(* PackageManager.ml - Hosted package resolution and persistent response cache.
   Resolves the same ProgramTypes package declarations exposed by Matter's
   package-manager HTTP API and renders them as compiler package source units. *)
[@@@warning "-4-30"]

type config = { server : string; cachePath : string }
type resolvedSource = { name : string; source : string }

type itemKind =
  | PackageType
  | PackageValue
  | PackageFunction
  | PackageTrait
  | PackageImpl

type locatedEntity = {
  kind : itemKind;
  hash : string;
  location : string;
  json : string;
}

type fetchResult = Found of string | Missing

let ( let* ) = Result.bind
let ( let+ ) result f = Result.map f result

let message = function
  | Failure text
  | Invalid_argument text
  | Yojson.Json_error text
  | Sqlite3.SqliteError text
  | Sqlite3.Error text ->
      text
  | Unix.Unix_error (error, _, _) -> Unix.error_message error
  | ex -> Printexc.to_string ex

(* Package expression trees have a finite nesting bound. Raw JSON keeps
   numeric literal spellings, including integers wider than OCaml's int. *)
let parse source =
  let depth = ref 0 and quoted = ref false and escaped = ref false in
  String.iter
    (fun c ->
      if !quoted then (
        if !escaped then escaped := false
        else if c = '\\' then escaped := true
        else if c = '"' then quoted := false)
      else
        match c with
        | '"' -> quoted := true
        | '[' | '{' ->
            incr depth;
            if !depth > 512 then
              raise (Yojson.Json_error "Package JSON exceeds nesting depth 512")
        | ']' | '}' -> decr depth
        | _ -> ())
    source;
  let json = Yojson.Raw.from_string source in
  let rec validate = function
    | `Assoc fields -> List.iter (fun (_, value) -> validate value) fields
    | `List values -> List.iter validate values
    | `Floatlit ("NaN" | "Infinity" | "-Infinity") ->
        raise (Yojson.Json_error "Package JSON contains a non-finite number")
    | `Null | `Bool _ | `Intlit _ | `Floatlit _ | `Stringlit _ -> ()
  in
  validate json;
  json

let defaultServer = "https://matter.darklang.com/"

let defaultCachePath () =
  let directory =
    match Sys.getenv_opt "XDG_DATA_HOME" with
    | Some path when path <> "" && not (Filename.is_relative path) -> path
    | _ -> (
        match Sys.getenv_opt "HOME" with
        | Some path when path <> "" -> Filename.concat path ".local/share"
        | _ -> "")
  in
  let directory =
    if Sys.file_exists directory && Sys.is_directory directory then directory
    else ""
  in
  Filename.concat (Filename.concat directory "dark-compiler") "packages.sqlite3"

let defaultConfig () =
  { server = defaultServer; cachePath = defaultCachePath () }

let kindPath = function
  | PackageType -> "type"
  | PackageValue -> "value"
  | PackageFunction -> "function"
  | PackageTrait -> "trait"
  | PackageImpl -> "impl"

let allKinds =
  [ PackageType; PackageValue; PackageFunction; PackageTrait; PackageImpl ]

let rec createDirectory path =
  if path <> "" && not (Sys.file_exists path) then (
    let parent = Filename.dirname path in
    if parent <> path then createDirectory parent;
    try Unix.mkdir path 0o777
    with Unix.Unix_error (Unix.EEXIST, _, _) when Sys.is_directory path -> ())

let withCache (config : config) action =
  try
    let directory = Filename.dirname config.cachePath in
    if directory <> "" then createDirectory directory;
    action ()
  with ex ->
    Error ("Package cache '" ^ config.cachePath ^ "' failed: " ^ message ex)

let cacheRead config key =
  withCache config (fun () ->
      match PackageIO.cacheRead config.cachePath key with
      | None -> Ok None
      | Some (200, body) -> Ok (Some (Found body))
      | Some (404, _) -> Ok (Some Missing)
      | Some (status, _) ->
          Error
            ("Package cache contains unsupported HTTP status "
           ^ string_of_int status ^ " for " ^ key))

let cacheWrite config key result =
  withCache config (fun () ->
      let status, body =
        match result with Found body -> (200, body) | Missing -> (404, "")
      in
      PackageIO.cacheWrite config.cachePath key status body;
      Ok ())

let requestNetwork client config path =
  let uri =
    Uri.to_string
      (Uri.resolve "http" (Uri.of_string config.server) (Uri.of_string path))
  in
  try
    let status, body = PackageIO.get client uri in
    if status >= 200 && status <= 299 then Ok (Found body)
    else if status = 404 then Ok Missing
    else
      Error
        ("Package server GET " ^ uri ^ " returned " ^ string_of_int status
       ^ ": " ^ body)
  with ex -> Error ("Package server GET " ^ uri ^ " failed: " ^ message ex)

let cacheKey config path =
  let rec trim index =
    if index > 0 && config.server.[index - 1] = '/' then trim (index - 1)
    else index
  in
  String.sub config.server 0 (trim (String.length config.server)) ^ path

let fetchByHash client config path =
  let key = cacheKey config path in
  let* cached = cacheRead config key in
  match cached with
  | Some result -> Ok result
  | None ->
      let* result = requestNetwork client config path in
      let+ () = cacheWrite config key result in
      result

let findByName client config path =
  let key = cacheKey config path in
  match requestNetwork client config path with
  | Ok result ->
      let+ () = cacheWrite config key result in
      result
  | Error networkError -> (
      match cacheRead config key with
      | Ok (Some result) -> Ok result
      | Ok None -> Error networkError
      | Error cacheError -> Error (networkError ^ "; " ^ cacheError))

let objectFields = function `Assoc fields -> fields | _ -> []
let tryField name element = List.assoc_opt name (objectFields element)
let arrayItems = function `List values -> values | _ -> []

let enumCase = function
  | `Assoc [ (name, `List fields) ] -> Ok (name, fields)
  | element ->
      Error ("Expected a package enum, got " ^ Yojson.Raw.to_string element)

let stringValue = function
  | `Stringlit literal -> (
      match Yojson.Basic.from_string literal with
      | `String value -> Ok value
      | _ -> Crash.crash "JSON string literal decoded as another kind")
  | element -> Error ("Expected a string, got " ^ Yojson.Raw.to_string element)

let booleanValue = function
  | `Bool value -> Ok value
  | element -> Error ("Expected a boolean, got " ^ Yojson.Raw.to_string element)

let numberText = function
  | `Intlit text | `Floatlit text -> Ok text
  | element -> Error ("Expected a number, got " ^ Yojson.Raw.to_string element)

let argumentIndex = function
  | `Intlit text -> Option.map Int32.to_int (Int32.of_string_opt text)
  | _ -> None

let replace oldText newText text =
  let output = Buffer.create (String.length text) in
  let rec loop index =
    if index < String.length text then
      if
        index + String.length oldText <= String.length text
        && String.sub text index (String.length oldText) = oldText
      then (
        Buffer.add_string output newText;
        loop (index + String.length oldText))
      else (
        Buffer.add_char output text.[index];
        loop (index + 1))
  in
  loop 0;
  Buffer.contents output

let escapedStringContents text =
  text |> replace "\\" "\\\\" |> replace "\"" "\\\"" |> replace "\n" "\\n"
  |> replace "\r" "\\r" |> replace "\t" "\\t"

let quoted text = "\"" ^ escapedStringContents text ^ "\""

let parseHashJson json =
  try
    let* name, fields = enumCase (parse json) in
    match (name, fields) with
    | "Hash", [ value ] -> stringValue value
    | _ -> Error ("Package find returned an invalid hash: " ^ json)
  with ex -> Error ("Package find returned invalid JSON: " ^ message ex)

let locationName element =
  match
    ( tryField "owner" element,
      tryField "modules" element,
      tryField "name" element )
  with
  | Some owner, Some modules, Some name ->
      let* owner = stringValue owner in
      let* modules = ResultList.mapResults stringValue (arrayItems modules) in
      let+ leaf = stringValue name in
      String.concat "." ((owner :: modules) @ [ leaf ])
  | _ -> Error ("Invalid package location: " ^ Yojson.Raw.to_string element)

let resolvedName element =
  match (tryField "resolved" element, tryField "originalName" element) with
  | Some resolved, Some originalName -> (
      let* name, fields = enumCase resolved in
      match (name, fields) with
      | "Ok", [ value ] -> (
          match tryField "location" value with
          | Some location -> (
              let* name, fields = enumCase location in
              match (name, fields) with
              | "Some", [ value ] -> locationName value
              | "None", [] ->
                  let+ names =
                    ResultList.mapResults stringValue (arrayItems originalName)
                  in
                  String.concat "." names
              | _ ->
                  Error
                    ("Invalid resolved package location: "
                    ^ Yojson.Raw.to_string location))
          | None ->
              Error
                ("Invalid resolved package name: " ^ Yojson.Raw.to_string value)
          )
      | "Error", _ ->
          let+ names =
            ResultList.mapResults stringValue (arrayItems originalName)
          in
          String.concat "." names
      | _ ->
          Error ("Invalid package resolution: " ^ Yojson.Raw.to_string resolved)
      )
  | _ ->
      Error ("Invalid package name resolution: " ^ Yojson.Raw.to_string element)

let rec renderType element =
  let* name, fields = enumCase element in
  let unary label =
    match fields with
    | [ inner ] ->
        let+ typ = renderType inner in
        label ^ "<" ^ typ ^ ">"
    | _ -> Error ("Invalid " ^ name ^ " type")
  in
  match (name, fields) with
  | "TVariable", [ value ] -> stringValue value
  | ( ( "TUnit" | "TBool" | "TInt8" | "TUInt8" | "TInt16" | "TUInt16" | "TInt32"
      | "TUInt32" | "TInt64" | "TUInt64" | "TInt128" | "TUInt128" | "TInt"
      | "TChar" | "TString" | "TDateTime" | "TUuid" | "TBlob" ),
      [] ) ->
      Ok (String.sub name 1 (String.length name - 1))
  | "TFloat", [] -> Ok "Float"
  | "TStream", _ -> unary "Stream"
  | "TList", _ -> unary "List"
  | "TDB", _ -> unary "DB"
  | "TDict", [ key; value ] ->
      let* key = renderType key in
      let+ value = renderType value in
      "Dict<" ^ key ^ ", " ^ value ^ ">"
  | "TTuple", [ first; second; rest ] ->
      let+ values =
        ResultList.mapResults renderType (first :: second :: arrayItems rest)
      in
      "(" ^ String.concat " * " values ^ ")"
  | "TFn", [ parameters; result ] ->
      let* parameters =
        ResultList.mapResults renderType (arrayItems parameters)
      in
      let+ result = renderType result in
      String.concat " -> " (parameters @ [ result ])
  | "TCustomType", [ name; typeArgs ] ->
      let* name = resolvedName name in
      let+ args = ResultList.mapResults renderType (arrayItems typeArgs) in
      name ^ if args = [] then "" else "<" ^ String.concat ", " args ^ ">"
  | _ -> Error ("Unsupported package type " ^ name)

let rec renderLetPattern element =
  let* name, fields = enumCase element in
  match (name, fields) with
  | "LPUnit", [ _ ] -> Ok "()"
  | "LPWildcard", [ _ ] -> Ok "_"
  | "LPVariable", [ _; name ] -> stringValue name
  | "LPTuple", [ _; first; second; rest ] ->
      let+ values =
        ResultList.mapResults renderLetPattern
          (first :: second :: arrayItems rest)
      in
      "(" ^ String.concat ", " values ^ ")"
  | _ -> Error ("Unsupported package let pattern " ^ name)

let scalar suffix value =
  let+ number = numberText value in
  number ^ suffix

let rec renderMatchPattern element =
  let* name, fields = enumCase element in
  match (name, fields) with
  | "MPVariable", [ _; name ] -> stringValue name
  | "MPUnit", [ _ ] -> Ok "()"
  | "MPBool", [ _; value ] ->
      let+ value = booleanValue value in
      string_of_bool value
  | "MPInt8", [ _; value ] -> scalar "y" value
  | "MPUInt8", [ _; value ] -> scalar "uy" value
  | "MPInt16", [ _; value ] -> scalar "s" value
  | "MPUInt16", [ _; value ] -> scalar "us" value
  | "MPInt32", [ _; value ] -> scalar "l" value
  | "MPUInt32", [ _; value ] -> scalar "ul" value
  | "MPInt64", [ _; value ] -> scalar "L" value
  | "MPUInt64", [ _; value ] -> scalar "UL" value
  | "MPInt128", [ _; value ] -> scalar "Q" value
  | "MPUInt128", [ _; value ] -> scalar "Z" value
  | "MPInt", [ _; value ] -> numberText value
  | "MPString", [ _; value ] ->
      let+ text = stringValue value in
      quoted text
  | "MPChar", [ _; value ] ->
      let+ text = stringValue value in
      "'" ^ replace "'" "\\'" text ^ "'"
  | "MPList", [ _; values ] ->
      let+ values =
        ResultList.mapResults renderMatchPattern (arrayItems values)
      in
      "[" ^ String.concat ", " values ^ "]"
  | "MPListCons", [ _; head; tail ] ->
      let* head = renderMatchPattern head in
      let+ tail = renderMatchPattern tail in
      "(" ^ head ^ " :: " ^ tail ^ ")"
  | "MPTuple", [ _; first; second; rest ] ->
      let+ values =
        ResultList.mapResults renderMatchPattern
          (first :: second :: arrayItems rest)
      in
      "(" ^ String.concat ", " values ^ ")"
  | "MPEnum", [ _; name; values ] ->
      let* name = stringValue name in
      let+ values =
        ResultList.mapResults renderMatchPattern (arrayItems values)
      in
      name ^ if values = [] then "" else "(" ^ String.concat ", " values ^ ")"
  | "MPOr", [ _; values ] ->
      let+ values =
        ResultList.mapResults renderMatchPattern (arrayItems values)
      in
      String.concat " | " values
  | _ -> Error ("Unsupported package match pattern " ^ name)

let infixText element =
  let* name, fields = enumCase element in
  match (name, fields) with
  | "BinOp", [ operation ] -> (
      let* name, fields = enumCase operation in
      match (name, fields) with
      | "BinOpAnd", [] -> Ok "&&"
      | "BinOpOr", [] -> Ok "||"
      | _ -> Error ("Unsupported binary operation " ^ name))
  | "InfixFnCall", [ operation ] -> (
      let* name, _ = enumCase operation in
      match name with
      | "ArithmeticPlus" -> Ok "+"
      | "ArithmeticMinus" -> Ok "-"
      | "ArithmeticMultiply" -> Ok "*"
      | "ArithmeticDivide" -> Ok "/"
      | "ArithmeticModulo" -> Ok "%"
      | "ArithmeticPower" -> Ok "^"
      | "ComparisonGreaterThan" -> Ok ">"
      | "ComparisonGreaterThanOrEqual" -> Ok ">="
      | "ComparisonLessThan" -> Ok "<"
      | "ComparisonLessThanOrEqual" -> Ok "<="
      | "ComparisonEquals" -> Ok "=="
      | "ComparisonNotEquals" -> Ok "!="
      | "StringConcat" -> Ok "++"
      | _ -> Error ("Unsupported infix function " ^ name))
  | _ -> Error ("Unsupported infix " ^ name)

let rec renderExpr parameters selfName element =
  let recurse = renderExpr parameters selfName in
  let renderMany = ResultList.mapResults recurse in
  let application name typeArgs args =
    let* types = ResultList.mapResults renderType (arrayItems typeArgs) in
    let+ args = renderMany (arrayItems args) in
    let applied =
      if types = [] then name else name ^ "<" ^ String.concat ", " types ^ ">"
    in
    applied ^ " (" ^ String.concat ") (" args ^ ")"
  in
  let* name, fields = enumCase element in
  match (name, fields) with
  | "EUnit", [ _ ] -> Ok "()"
  | "EBool", [ _; value ] ->
      let+ value = booleanValue value in
      string_of_bool value
  | "EInt8", [ _; value ] -> scalar "y" value
  | "EUInt8", [ _; value ] -> scalar "uy" value
  | "EInt16", [ _; value ] -> scalar "s" value
  | "EUInt16", [ _; value ] -> scalar "us" value
  | "EInt32", [ _; value ] -> scalar "l" value
  | "EUInt32", [ _; value ] -> scalar "ul" value
  | "EInt64", [ _; value ] -> scalar "L" value
  | "EUInt64", [ _; value ] -> scalar "UL" value
  | "EInt128", [ _; value ] -> scalar "Q" value
  | "EUInt128", [ _; value ] -> scalar "Z" value
  | "EInt", [ _; value ] -> numberText value
  | "EFloat", [ _; sign; whole; part ] ->
      let* sign, _ = enumCase sign in
      let* whole = stringValue whole in
      let+ part = stringValue part in
      (if sign = "Negative" then "-" else "") ^ whole ^ "." ^ part
  | "EChar", [ _; value ] ->
      let+ text = stringValue value in
      "'" ^ replace "'" "\\'" text ^ "'"
  | "EString", [ _; segments ] ->
      let segment element =
        let* name, fields = enumCase element in
        match (name, fields) with
        | "StringText", [ text ] ->
            let+ text = stringValue text in
            escapedStringContents text
        | "StringInterpolation", [ expr ] ->
            let+ text = recurse expr in
            "{" ^ text ^ "}"
        | _ -> Error ("Unsupported string segment " ^ name)
      in
      let+ segments = ResultList.mapResults segment (arrayItems segments) in
      "$\"" ^ String.concat "" segments ^ "\""
  | "EVariable", [ _; name ] -> stringValue name
  | "EArg", [ _; index ] -> (
      match argumentIndex index with
      | Some index ->
          if index >= 0 && index < List.length parameters then
            Ok (List.nth parameters index)
          else Error ("Invalid package argument index " ^ string_of_int index)
      | None -> Error "Invalid package argument index")
  | "ESelf", [ _ ] -> Ok selfName
  | "EFnName", [ _; name ] | "EFnName", [ _; name; _ ] | "EValue", [ _; name ]
    ->
      resolvedName name
  | "EList", [ _; values ] ->
      let+ values = renderMany (arrayItems values) in
      "[" ^ String.concat ", " values ^ "]"
  | "ETuple", [ _; first; second; rest ] ->
      let+ values = renderMany (first :: second :: arrayItems rest) in
      "(" ^ String.concat ", " values ^ ")"
  | "EDict", [ _; entries ] ->
      let entry element =
        match arrayItems element with
        | [ key; value ] ->
            let* key = recurse key in
            let+ value = recurse value in
            key ^ ": " ^ value
        | _ -> Error "Invalid package dictionary entry"
      in
      let+ entries = ResultList.mapResults entry (arrayItems entries) in
      "Dict { " ^ String.concat "; " entries ^ " }"
  | "ELet", [ _; pattern; value; body ] ->
      let* pattern = renderLetPattern pattern in
      let* value = recurse value in
      let+ body = recurse body in
      "(let " ^ pattern ^ " = " ^ value ^ " in " ^ body ^ ")"
  | "EIf", [ _; condition; yes; no ] -> (
      let* condition = recurse condition in
      let* yes = recurse yes in
      let* name, fields = enumCase no in
      match (name, fields) with
      | "Some", [ value ] ->
          let+ no = recurse value in
          "(if " ^ condition ^ " then " ^ yes ^ " else " ^ no ^ ")"
      | "None", [] -> Ok ("(if " ^ condition ^ " then " ^ yes ^ ")")
      | _ -> Error ("Invalid optional else case " ^ name))
  | "EInfix", [ _; infix; left; right ] ->
      let* op = infixText infix in
      let* left = recurse left in
      let+ right = recurse right in
      "(" ^ left ^ " " ^ op ^ " " ^ right ^ ")"
  | "EApply", [ _; fn; typeArgs; args ] -> (
      let* name, fields = enumCase fn in
      match (name, fields) with
      | "EFnName", [ _; name ] | "EFnName", [ _; name; _ ] ->
          let* name = resolvedName name in
          application name typeArgs args
      | _ ->
          let* fn = recurse fn in
          application ("(" ^ fn ^ ")") typeArgs args)
  | "ELambda", [ _; patterns; body ] ->
      let* patterns =
        ResultList.mapResults renderLetPattern (arrayItems patterns)
      in
      let+ body = recurse body in
      "(fun " ^ String.concat " " patterns ^ " -> " ^ body ^ ")"
  | "ERecord", [ _; name; typeArgs; values ] ->
      let* name = resolvedName name in
      let* types = ResultList.mapResults renderType (arrayItems typeArgs) in
      let field element =
        match arrayItems element with
        | [ name; value ] ->
            let* name = stringValue name in
            let+ value = recurse value in
            name ^ " = " ^ value
        | _ -> Error "Invalid package record field"
      in
      let+ fields = ResultList.mapResults field (arrayItems values) in
      let name =
        if types = [] then name else name ^ "<" ^ String.concat ", " types ^ ">"
      in
      name ^ " { " ^ String.concat "; " fields ^ " }"
  | "ERecordFieldAccess", [ _; record; field ] ->
      let* record = recurse record in
      let+ field = stringValue field in
      "(" ^ record ^ ")." ^ field
  | "ERecordUpdate", [ _; record; updates ] ->
      let update element =
        match arrayItems element with
        | [ name; value ] ->
            let* name = stringValue name in
            let+ value = recurse value in
            name ^ " = " ^ value
        | _ -> Error "Invalid package record update"
      in
      let* record = recurse record in
      let+ updates = ResultList.mapResults update (arrayItems updates) in
      "{ " ^ record ^ " with " ^ String.concat "; " updates ^ " }"
  | "EEnum", [ _; name; _typeArgs; enumName; values ] ->
      let* name = resolvedName name in
      let* leaf = stringValue enumName in
      let+ values = renderMany (arrayItems values) in
      name ^ "." ^ leaf
      ^ if values = [] then "" else "(" ^ String.concat ", " values ^ ")"
  | "EMatch", [ _; argument; cases ] ->
      let case element =
        match
          ( tryField "pat" element,
            tryField "whenCondition" element,
            tryField "rhs" element )
        with
        | Some pattern, Some guard, Some body -> (
            let* pattern = renderMatchPattern pattern in
            let* name, fields = enumCase guard in
            match (name, fields) with
            | "None", [] ->
                let+ body = recurse body in
                "| " ^ pattern ^ " -> " ^ body
            | "Some", [ condition ] ->
                let* guard = recurse condition in
                let+ body = recurse body in
                "| " ^ pattern ^ " when " ^ guard ^ " -> " ^ body
            | _ -> Error ("Invalid match guard " ^ name))
        | _ -> Error "Invalid package match case"
      in
      let* argument = recurse argument in
      let+ cases = ResultList.mapResults case (arrayItems cases) in
      "(match " ^ argument ^ " with " ^ String.concat " " cases ^ ")"
  | "EStatement", [ _; first; next ] ->
      let* first = recurse first in
      let+ next = recurse next in
      "(" ^ first ^ "; " ^ next ^ ")"
  | "EPipe", [ _; initial; parts ] ->
      let part element =
        let* name, fields = enumCase element in
        match (name, fields) with
        | "EPipeVariable", [ _; name; args ] ->
            let* name = stringValue name in
            let+ args = renderMany (arrayItems args) in
            name
            ^ if args = [] then "" else " (" ^ String.concat ") (" args ^ ")"
        | "EPipeLambda", [ _; patterns; body ] ->
            let* patterns =
              ResultList.mapResults renderLetPattern (arrayItems patterns)
            in
            let+ body = recurse body in
            "fun " ^ String.concat " " patterns ^ " -> " ^ body
        | "EPipeInfix", [ _; infix; argument ] ->
            let* op = infixText infix in
            let+ argument = recurse argument in
            "(" ^ op ^ ") (" ^ argument ^ ")"
        | "EPipeFnCall", [ _; name; typeArgs; args ]
        | "EPipeFnCall", [ _; name; typeArgs; args; _ ] ->
            let* name = resolvedName name in
            if arrayItems args <> [] then application name typeArgs args
            else
              let+ types =
                ResultList.mapResults renderType (arrayItems typeArgs)
              in
              name
              ^ if types = [] then "" else "<" ^ String.concat ", " types ^ ">"
        | "EPipeEnum", [ _; name; caseName; values ] ->
            let* name = resolvedName name in
            let* leaf = stringValue caseName in
            let+ values = renderMany (arrayItems values) in
            name ^ "." ^ leaf
            ^
            if values = [] then "" else " (" ^ String.concat ") (" values ^ ")"
        | _ -> Error ("Unsupported package pipe part " ^ name)
      in
      let* initial = recurse initial in
      let+ parts = ResultList.mapResults part (arrayItems parts) in
      "(" ^ String.concat " |> " (initial :: parts) ^ ")"
  | _ -> Error ("Unsupported package expression " ^ name)

let parseLocatedEntity kind hash json =
  try
    let root = parse json in
    match (tryField "entity" root, tryField "location" root) with
    | Some _, Some location ->
        let+ location = locationName location in
        { kind; hash; location; json }
    | _ ->
        Error ("Package server returned an invalid located entity for " ^ hash)
  with ex ->
    Error
      ("Package server returned invalid JSON for " ^ hash ^ ": " ^ message ex)

let tryPackageHash element =
  match (tryField "name" element, tryField "location" element) with
  | Some name, Some location -> (
      match (enumCase name, enumCase location) with
      | Ok ("Package", [ hash ]), Ok ("Some", [ location ]) -> (
          match (enumCase hash, locationName location) with
          | Ok ("Hash", [ hash ]), Ok name ->
              Option.map
                (fun hash -> (hash, name))
                (Result.to_option (stringValue hash))
          | _ -> None)
      | _ -> None)
  | _ -> None

let distinct values =
  let rec loop found = function
    | [] -> List.rev found
    | value :: rest ->
        if List.mem value found then loop found rest
        else loop (value :: found) rest
  in
  loop [] values

let dependencyRefs json =
  try
    let rec collect element =
      let own = Option.to_list (tryPackageHash element) in
      match element with
      | `Assoc fields ->
          own @ List.concat_map (fun (_, value) -> collect value) fields
      | `List values -> own @ List.concat_map collect values
      | _ -> own
    in
    Ok (distinct (collect (parse json)))
  with ex -> Error ("Could not inspect package dependencies: " ^ message ex)

let renderTraitRef value =
  match (tryField "trait_" value, tryField "typeArgs" value) with
  | Some trait_, Some args ->
      let* name = resolvedName trait_ in
      let+ args = ResultList.mapResults renderType (arrayItems args) in
      name ^ if args = [] then "" else "<" ^ String.concat ", " args ^ ">"
  | _ -> Error "Invalid package trait reference"

let renderTypeParameters types bounds =
  let* types = ResultList.mapResults stringValue (arrayItems types) in
  let* bounds =
    ResultList.mapResults
      (fun bound ->
        match (tryField "param" bound, tryField "trait_" bound) with
        | Some param, Some trait_ ->
            let* param = stringValue param in
            let+ trait_ = renderTraitRef trait_ in
            (param, trait_)
        | _ -> Error "Invalid package trait bound")
      (Option.fold ~none:[] ~some:arrayItems bounds)
  in
  if List.exists (fun (param, _) -> not (List.mem param types)) bounds then
    Error "Package bound refers to an undeclared type parameter"
  else
    Ok
      (if types = [] then ""
       else
         "<"
         ^ String.concat ", "
             (List.map
                (fun param ->
                  let constraints =
                    List.filter_map
                      (fun (name, trait_) ->
                        if name = param then Some trait_ else None)
                      bounds
                  in
                  "'" ^ param
                  ^
                  if constraints = [] then ""
                  else ": " ^ String.concat " + " constraints)
                types)
         ^ ">")

let renderParameters parameters =
  ResultList.mapResults
    (fun element ->
      match (tryField "name" element, tryField "typ" element) with
      | Some name, Some typ ->
          let* name = stringValue name in
          let name =
            NameSyntax.formatIdentifier (NameSyntax.identifierFromText name)
          in
          let+ typ = renderType typ in
          (name, "(" ^ name ^ ": " ^ typ ^ ")")
      | _ -> Error "Invalid package function parameter")
    (arrayItems parameters)

let renderEntity entity =
  try
    let root = parse entity.json in
    match tryField "entity" root with
    | None -> Error ("Missing package entity " ^ entity.hash)
    | Some value -> (
        let parts = String.split_on_char '.' entity.location in
        match List.rev parts with
        | [] | [ _ ] -> Error ("Invalid package location " ^ entity.location)
        | leaf :: reversedModule -> (
            let moduleName = String.concat "." (List.rev reversedModule) in
            let prefix body =
              {
                name = "package:" ^ entity.location ^ "@" ^ entity.hash;
                source = "module " ^ moduleName ^ "\n" ^ body;
              }
            in
            match entity.kind with
            | PackageTrait -> (
                match
                  (tryField "typeParams" value, tryField "methods" value)
                with
                | Some types, Some methods ->
                    let* types =
                      renderTypeParameters types (tryField "bounds" value)
                    in
                    let* methods =
                      ResultList.mapResults
                        (fun method_ ->
                          match
                            ( tryField "name" method_,
                              tryField "typeParams" method_,
                              tryField "parameters" method_,
                              tryField "returnType" method_ )
                          with
                          | Some name, Some types, Some parameters, Some result
                            ->
                              let* name = stringValue name in
                              let* types =
                                renderTypeParameters types
                                  (tryField "bounds" method_)
                              in
                              let* parameters = renderParameters parameters in
                              let+ result = renderType result in
                              "  let " ^ name ^ types ^ " "
                              ^ String.concat " " (List.map snd parameters)
                              ^ " : " ^ result
                          | _ -> Error "Invalid package trait method")
                        (arrayItems methods)
                    in
                    Ok
                      (prefix
                         ("trait " ^ leaf ^ types ^ " =\n"
                        ^ String.concat "\n" methods))
                | _ -> Error ("Invalid package trait " ^ entity.location))
            | PackageImpl -> (
                match
                  ( tryField "trait_" value,
                    tryField "traitTypeArgs" value,
                    tryField "self" value,
                    tryField "typeParams" value,
                    tryField "methods" value )
                with
                | Some trait_, Some args, Some self, Some types, Some methods ->
                    let* trait_ = resolvedName trait_ in
                    let* args =
                      ResultList.mapResults renderType (arrayItems args)
                    in
                    let* self = renderType self in
                    let* types =
                      renderTypeParameters types (tryField "bounds" value)
                    in
                    let+ methods =
                      ResultList.mapResults
                        (fun method_ ->
                          match arrayItems method_ with
                          | [ name; target ] ->
                              let* name = stringValue name in
                              let+ target = resolvedName target in
                              "  let " ^ name ^ " = " ^ target
                          | _ -> Error "Invalid package implementation method")
                        (arrayItems methods)
                    in
                    prefix
                      ("impl" ^ types ^ " " ^ trait_
                      ^ (if args = [] then ""
                         else "<" ^ String.concat ", " args ^ ">")
                      ^ " for (" ^ self ^ ") =\n" ^ String.concat "\n" methods)
                | _ ->
                    Error ("Invalid package implementation " ^ entity.location))
            | PackageFunction -> (
                match
                  ( tryField "parameters" value,
                    tryField "typeParams" value,
                    tryField "returnType" value,
                    tryField "body" value )
                with
                | Some parameters, Some typeParams, Some returnType, Some body
                  ->
                    let parameter element =
                      match
                        (tryField "name" element, tryField "typ" element)
                      with
                      | Some name, Some typ ->
                          let* name = stringValue name in
                          let sourceName =
                            NameSyntax.formatIdentifier
                              (NameSyntax.identifierFromText name)
                          in
                          let+ typ = renderType typ in
                          (sourceName, "(" ^ sourceName ^ ": " ^ typ ^ ")")
                      | _ -> Error "Invalid package function parameter"
                    in
                    let* parameters =
                      ResultList.mapResults parameter (arrayItems parameters)
                    in
                    let* types =
                      renderTypeParameters typeParams (tryField "bounds" value)
                    in
                    let* result = renderType returnType in
                    let+ body =
                      renderExpr (List.map fst parameters) entity.location body
                    in
                    prefix
                      ("let " ^ leaf ^ types ^ " "
                      ^ String.concat " " (List.map snd parameters)
                      ^ " : " ^ result ^ " = " ^ body)
                | _ -> Error ("Invalid package function " ^ entity.location))
            | PackageValue -> (
                match tryField "body" value with
                | Some body ->
                    let+ body = renderExpr [] entity.location body in
                    prefix ("val " ^ leaf ^ " = " ^ body)
                | None -> Error ("Invalid package value " ^ entity.location))
            | PackageType -> (
                match tryField "declaration" value with
                | None -> Error ("Invalid package type " ^ entity.location)
                | Some declaration -> (
                    match
                      ( tryField "typeParams" declaration,
                        tryField "definition" declaration )
                    with
                    | Some typeParams, Some definition -> (
                        let* types =
                          renderTypeParameters typeParams
                            (tryField "bounds" declaration)
                        in
                        let* name, fields = enumCase definition in
                        let head = "type " ^ leaf ^ types ^ " = " in
                        match (name, fields) with
                        | "Alias", [ target ] ->
                            let+ typ = renderType target in
                            prefix (head ^ typ)
                        | "Record", [ fields ] ->
                            let field element =
                              match
                                (tryField "name" element, tryField "typ" element)
                              with
                              | Some name, Some typ ->
                                  let* name = stringValue name in
                                  let+ typ = renderType typ in
                                  name ^ ": " ^ typ
                              | _ -> Error "Invalid package record field"
                            in
                            let+ fields =
                              ResultList.mapResults field (arrayItems fields)
                            in
                            prefix
                              (head ^ "{ " ^ String.concat "; " fields ^ " }")
                        | "Enum", [ cases ] ->
                            let case element =
                              match
                                ( tryField "name" element,
                                  tryField "fields" element )
                              with
                              | Some name, Some fields ->
                                  let* name = stringValue name in
                                  let field element =
                                    match tryField "typ" element with
                                    | Some typ -> renderType typ
                                    | None -> Error "Invalid package enum field"
                                  in
                                  let+ fields =
                                    ResultList.mapResults field
                                      (arrayItems fields)
                                  in
                                  name
                                  ^
                                  if fields = [] then ""
                                  else " of " ^ String.concat " * " fields
                              | _ -> Error "Invalid package enum case"
                            in
                            let+ cases =
                              ResultList.mapResults case (arrayItems cases)
                            in
                            prefix (head ^ String.concat " | " cases)
                        | _ ->
                            Error ("Unsupported package type definition " ^ name)
                        )
                    | _ ->
                        Error
                          ("Invalid package type declaration " ^ entity.location)
                    ))))
  with ex ->
    Error ("Could not render package " ^ entity.location ^ ": " ^ message ex)

let candidatePrefixes isKnownName names =
  names
  |> List.filter (fun name -> not (isKnownName name))
  |> List.concat_map (fun name ->
      let parts = String.split_on_char '.' name in
      List.init
        (max 0 (List.length parts - 1))
        (fun index ->
          String.concat "."
            (List.filteri (fun i _ -> i < List.length parts - index) parts)))
  |> distinct

let escapeDataString text =
  let escaped = Buffer.create (String.length text) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
          Buffer.add_char escaped c
      | _ -> Buffer.add_string escaped (Printf.sprintf "%%%02X" (Char.code c)))
    text;
  Buffer.contents escaped

(* The upstream /:location module browser returns complete LocatedItems and
   direct child modules, including traits and impls. Keep non-trait items in
   the hash index for dependency loading, without importing unrelated bodies. *)
let parseModuleInventory moduleName json =
  try
    let root = parse json in
    match tryField "submodules" root with
    | None -> Ok None
    | Some modules ->
        let* modules =
          ResultList.mapResults
            (fun parts ->
              let* parts =
                ResultList.mapResults stringValue (arrayItems parts)
              in
              let name = String.concat "." parts in
              if String.starts_with ~prefix:(moduleName ^ ".") name then Ok name
              else Error ("Invalid package child module '" ^ name ^ "'"))
            (arrayItems modules)
        in
        let* entities =
          ResultList.collectResults
            (fun (field, kind) ->
              ResultList.mapResults
                (fun item ->
                  match
                    Option.bind (tryField "entity" item) (tryField "hash")
                  with
                  | None -> Error "Package inventory item is missing its hash"
                  | Some hash ->
                      let* hash = parseHashJson (Yojson.Raw.to_string hash) in
                      parseLocatedEntity kind hash (Yojson.Raw.to_string item))
                (Option.fold ~none:[] ~some:arrayItems (tryField field root)))
            [
              ("types", PackageType);
              ("values", PackageValue);
              ("fns", PackageFunction);
              ("traits", PackageTrait);
              ("impls", PackageImpl);
            ]
        in
        Ok (Some (entities, modules))
  with ex -> Error ("Invalid package module inventory: " ^ message ex)

let packageModule location =
  let parts = String.split_on_char '.' location in
  String.concat "." (List.take (min 2 (max 1 (List.length parts - 1))) parts)

let resolveNames config resolutionEnv knownTraits names =
  let client = PackageIO.create () in
  Fun.protect
    ~finally:(fun () -> PackageIO.dispose client)
    (fun () ->
      let isKnownName name =
        List.mem name knownTraits
        || List.exists
             (fun context ->
               Result.is_ok (NameResolution.resolve context name resolutionEnv))
             [
               NameResolution.Type;
               NameResolution.Callable;
               NameResolution.Value;
             ]
      in
      let fetchLocated kind hash =
        let path =
          "/" ^ kindPath kind ^ "/get/with-location/" ^ escapeDataString hash
        in
        let* result = fetchByHash client config path in
        match result with
        | Missing -> Ok None
        | Found json ->
            let+ entity = parseLocatedEntity kind hash json in
            Some entity
      in
      let rec browse modules visited inventory =
        match modules with
        | [] -> Ok (visited, inventory)
        | name :: rest when StringOrder.Set.mem name visited ->
            browse rest visited inventory
        | name :: rest -> (
            let visited = StringOrder.Set.add name visited in
            match findByName client config ("/" ^ escapeDataString name) with
            | Error _ | Ok Missing -> browse rest visited inventory
            | Ok (Found json) -> (
                let* parsed = parseModuleInventory name json in
                match parsed with
                | None ->
                    let parent =
                      String.split_on_char '.' name
                      |> List.rev |> List.tl |> List.rev |> String.concat "."
                    in
                    browse
                      (if parent = "" then rest else parent :: rest)
                      visited inventory
                | Some (entities, children) ->
                    let* inventory =
                      List.fold_left
                        (fun result entity ->
                          let* inventory = result in
                          match
                            StringOrder.Map.find_opt entity.hash inventory
                          with
                          | Some previous when previous.kind <> entity.kind ->
                              Error
                                ("Package hash has conflicting item kinds: "
                               ^ entity.hash)
                          | Some _ -> Ok inventory
                          | None ->
                              Ok
                                (StringOrder.Map.add entity.hash entity
                                   inventory))
                        (Ok inventory) entities
                    in
                    browse (children @ rest) visited inventory))
      in
      let findRoots () =
        let attempts =
          List.concat_map
            (fun name ->
              List.map
                (fun kind ->
                  let path =
                    "/" ^ kindPath kind ^ "/find/" ^ escapeDataString name
                  in
                  let* result = findByName client config path in
                  match result with
                  | Missing -> Ok []
                  | Found json ->
                      let+ hash = parseHashJson json in
                      [ (kind, hash) ])
                allKinds)
            (List.filter
               (fun name -> not (isKnownName name))
               (candidatePrefixes isKnownName names))
        in
        let roots =
          List.concat_map
            (function Ok values -> values | Error _ -> [])
            attempts
        in
        if roots <> [] || List.for_all Result.is_ok attempts then
          Ok (distinct roots)
        else
          match List.find_opt Result.is_error attempts with
          | Some (Error error) -> Error error
          | _ -> Ok []
      in
      let rec load pending visited browsed inventory loaded =
        match pending with
        | [] -> Ok (List.rev loaded)
        | (_, hash, _) :: rest when StringOrder.Set.mem hash visited ->
            load rest visited browsed inventory loaded
        | (kind, hash, name) :: rest -> (
            let* browsed, inventory =
              match name with
              | Some name when not (StringOrder.Map.mem hash inventory) ->
                  browse [ packageModule name ] browsed inventory
              | _ -> Ok (browsed, inventory)
            in
            let* entity =
              match StringOrder.Map.find_opt hash inventory with
              | Some entity -> Ok (Some entity)
              | None -> (
                  match kind with
                  | Some kind -> fetchLocated kind hash
                  | None ->
                      let attempts =
                        List.map (fun kind -> fetchLocated kind hash) allKinds
                      in
                      Ok
                        (List.find_map
                           (function
                             | Ok (Some entity) -> Some entity | _ -> None)
                           attempts))
            in
            match entity with
            | None -> Error ("Package dependency " ^ hash ^ " was not found")
            | Some entity ->
                let* dependencies = dependencyRefs entity.json in
                let next =
                  List.filter_map
                    (fun (hash, name) ->
                      if isKnownName name then None
                      else Some (None, hash, Some name))
                    dependencies
                in
                let discovered =
                  StringOrder.Map.bindings inventory
                  |> List.filter_map (fun (hash, entity) ->
                      if
                        (entity.kind = PackageTrait || entity.kind = PackageImpl)
                        && not (StringOrder.Set.mem hash visited)
                      then Some (Some entity.kind, hash, Some entity.location)
                      else None)
                in
                load
                  (next @ rest @ discovered)
                  (StringOrder.Set.add hash visited)
                  browsed inventory (entity :: loaded))
      in
      let candidates =
        candidatePrefixes isKnownName names
        |> List.filter (fun name -> not (isKnownName name))
      in
      let* browsed, inventory =
        browse
          (List.map packageModule candidates |> distinct)
          StringOrder.Set.empty StringOrder.Map.empty
      in
      let* roots = findRoots () in
      let inventoryRoots =
        StringOrder.Map.bindings inventory
        |> List.filter_map (fun (hash, entity) ->
            if List.mem entity.location candidates then Some (entity.kind, hash)
            else None)
      in
      let roots = distinct (roots @ inventoryRoots) in
      let* rootEntities =
        ResultList.mapResults
          (fun (kind, hash) ->
            let* entity =
              match StringOrder.Map.find_opt hash inventory with
              | Some entity -> Ok (Some entity)
              | None -> fetchLocated kind hash
            in
            match entity with
            | Some entity -> Ok entity
            | None -> Error ("Package root was not found: " ^ hash))
          roots
      in
      let modules =
        List.map (fun entity -> packageModule entity.location) rootEntities
        |> distinct
      in
      let* browsed, inventory = browse modules browsed inventory in
      let inventory =
        List.fold_left
          (fun inventory entity ->
            StringOrder.Map.add entity.hash entity inventory)
          inventory rootEntities
      in
      let discovered =
        StringOrder.Map.bindings inventory
        |> List.filter_map (fun (hash, entity) ->
            if entity.kind = PackageTrait || entity.kind = PackageImpl then
              Some (Some entity.kind, hash, Some entity.location)
            else None)
      in
      let* entities =
        load
          (List.map
             (fun entity ->
               (Some entity.kind, entity.hash, Some entity.location))
             rootEntities
          @ discovered)
          StringOrder.Set.empty browsed inventory []
      in
      ResultList.mapResults renderEntity entities)

let resolveWritten ?(knownTraits = []) config resolutionEnv units =
  let* names = WrittenSource.qualifiedNames units in
  resolveNames config resolutionEnv knownTraits names
