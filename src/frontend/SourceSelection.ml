(* SourceSelection.ml - Keep upstream identities while selecting explicit compiler implementations. *)
module W = WrittenTypes
module I = SourceInventory
module A = CompilerAnnotations
module S = StringOrder.Set

type unavailable = { name : string; reason : string; excluded : bool }

type selected = {
  units : (string * Validation.validatedSourceFile) list;
  unavailable : unavailable list;
}

type node = {
  file : string;
  path : string list;
  name : string;
  declaration : W.declaration;
  disposition : A.disposition;
}

let ( let* ) = Result.bind

let kind = function
  | W.DFunction _ -> "function"
  | W.DValue _ -> "val"
  | W.DType _ -> "type"
  | W.DTrait _ -> "trait"
  | W.DImpl _ -> "implementation"
  | W.DModule _ -> "module"
  | W.DTest _ -> "test"
  | W.DAssertion _ -> "assertion"
  | W.DExpr _ -> "expression"

let position = function
  | W.DFunction fn -> fn.W.keywordLet.Tokenizer.start
  | W.DValue value -> value.W.keywordVal.Tokenizer.start
  | W.DType typ -> typ.W.keywordType.Tokenizer.start
  | W.DTrait trait_ -> trait_.W.keywordTrait.Tokenizer.start
  | W.DImpl impl -> impl.W.keywordImpl.Tokenizer.start
  | W.DTest test -> test.W.keywordTest.Tokenizer.start
  | W.DAssertion test -> test.W.range.Tokenizer.start
  | W.DExpr expr -> (W.exprRange expr).Tokenizer.start
  | W.DModule modul -> modul.W.keywordModule.Tokenizer.start

let nodes file (source : I.source) =
  let rec collect path declarations =
    List.concat_map
      (function
        | W.DModule modul ->
            collect (path @ W.moduleNameParts modul) modul.W.declarations
        | ( W.DFunction _ | W.DValue _ | W.DType _ | W.DTrait _ | W.DImpl _
          | W.DExpr _ | W.DAssertion _ | W.DTest _ ) as declaration -> (
            let entry =
              List.find_opt
                (fun (entry : I.entry) ->
                  entry.I.position = position declaration)
                source.I.entries
            in
            match entry with
            | None ->
                Crash.crash
                  "Parsed declaration missing from its source inventory"
            | Some entry ->
                [
                  {
                    file;
                    path;
                    name = entry.I.name;
                    declaration;
                    disposition = entry.I.disposition;
                  };
                ]))
      declarations
  in
  collect [] source.I.tree.W.declarations
  @ List.concat_map
      (fun expression -> collect [] [ W.DExpr expression ])
      source.I.tree.W.exprsToEval

let require selected name =
  match
    List.find_opt
      (fun (item : unavailable) -> item.name = name)
      selected.unavailable
  with
  | None -> Ok ()
  | Some item ->
      Error
        ("Unavailable declaration " ^ name ^ " ("
        ^ (if item.excluded then "excluded: " else "blocked: ")
        ^ item.reason ^ ")")

let select ~context ~roots ~supplements ~sources =
  let rec unique seen = function
    | [] -> Ok ()
    | (path, _) :: rest ->
        if S.mem path seen then Error ("Duplicate source path: " ^ path)
        else unique (S.add path seen) rest
  in
  let* () = unique S.empty sources in
  let* () = unique S.empty (List.map (fun path -> (path, ())) roots) in
  let* () = unique S.empty (List.map (fun path -> (path, ())) supplements) in
  let lookup path =
    match List.assoc_opt path sources with
    | Some source -> Ok source
    | None -> Error ("Missing source: " ^ path)
  in
  let* originalSources =
    ResultList.traverse
      (fun path ->
        let* source = lookup path in
        Ok (path, source))
      roots
  in
  let originals =
    List.concat_map (fun (path, source) -> nodes path source) originalSources
  in
  let originalNames =
    List.fold_left
      (fun names (node : node) ->
        S.add (kind node.declaration ^ ":" ^ node.name) names)
      S.empty originals
  in
  let replacement (node : node) target =
    match String.split_on_char '#' target with
    | [ file; name ] -> (
        let* source = lookup file in
        let matches =
          nodes file source
          |> List.filter (fun candidate ->
              kind candidate.declaration = kind node.declaration
              && (candidate.name = name
                 || String.concat "." (candidate.path @ [ name ])
                    = candidate.name))
        in
        match matches with
        | [] -> Error ("Missing replacement target: " ^ target)
        | [ _; _ ] | _ :: _ :: _ ->
            Error ("Ambiguous replacement target: " ^ target)
        | [ candidate ] -> (
            if candidate.name <> node.name then
              Error ("Replacement must preserve public identity " ^ node.name)
            else
              match candidate.disposition with
              | A.Run -> Ok candidate
              | A.Blocked _ | A.Excluded _ | A.Replace _ | A.Xfail _ ->
                  Error
                    ("Replacement target must be a concrete unannotated \
                      implementation: " ^ target)))
    | [] | [ _ ] | _ :: _ :: _ -> Error ("Invalid replacement target: " ^ target)
  in
  let memberBlock node =
    let* source = lookup node.file in
    let members =
      match node.declaration with
      | W.DImpl impl ->
          List.map
            (function
              | W.IMethod fn -> fn.W.keywordLet.Tokenizer.start
              | W.IAlias value -> value.W.keywordVal.Tokenizer.start)
            impl.W.members
      | W.DTrait trait_ ->
          List.map
            (fun (method_ : W.traitMethodDecl) ->
              method_.W.keywordLet.Tokenizer.start)
            trait_.W.methods
      | W.DFunction _ | W.DValue _ | W.DType _ | W.DModule _ | W.DTest _
      | W.DAssertion _ | W.DExpr _ ->
          []
    in
    let rec find = function
      | [] -> Ok []
      | position :: rest -> (
          match
            List.find_opt
              (fun (entry : I.entry) -> entry.I.position = position)
              source.I.entries
          with
          | None -> Crash.crash "Container member missing from source inventory"
          | Some entry -> (
              match entry.I.disposition with
              | A.Run -> find rest
              | A.Blocked { reason; _ } ->
                  let* following = find rest in
                  Ok
                    ({ name = entry.I.name; reason; excluded = false }
                    :: following)
              | A.Excluded reason ->
                  let* following = find rest in
                  Ok
                    ({ name = entry.I.name; reason; excluded = true }
                    :: following)
              | A.Replace _ ->
                  Error
                    ("Member replacement requires a replacement for its \
                      containing trait or implementation: " ^ entry.I.name)
              | A.Xfail _ -> Error "Expected failure on a package member"))
    in
    find members
  in
  let rec selectNodes kept unavailable contracts targets = function
    | [] -> Ok (List.rev kept, List.rev unavailable, List.rev contracts, targets)
    | node :: rest -> (
        match node.declaration with
        | W.DTest _ | W.DAssertion _ ->
            selectNodes kept unavailable contracts targets rest
        | W.DFunction _ | W.DValue _ | W.DType _ | W.DTrait _ | W.DImpl _
        | W.DExpr _ | W.DModule _ -> (
            match node.disposition with
            | A.Run -> (
                let* blocked = memberBlock node in
                match blocked with
                | [] ->
                    selectNodes (node :: kept) unavailable contracts targets
                      rest
                | member :: _ ->
                    selectNodes kept
                      (({ member with name = node.name } :: blocked)
                      @ unavailable)
                      contracts targets rest)
            | A.Blocked { reason; _ } | A.Excluded reason ->
                let excluded =
                  match node.disposition with
                  | A.Excluded _ -> true
                  | A.Run | A.Blocked _ | A.Replace _ | A.Xfail _ -> false
                in
                selectNodes kept
                  ({ name = node.name; reason; excluded } :: unavailable)
                  contracts targets rest
            | A.Replace { target; reason = _ } ->
                let* selected = replacement node target in
                selectNodes
                  ({ node with declaration = selected.declaration } :: kept)
                  unavailable
                  ((node.path, node.declaration, selected.declaration)
                  :: contracts)
                  (S.add selected.file targets)
                  rest
            | A.Xfail _ ->
                Error
                  "Expected-failure annotations cannot select a package \
                   declaration"))
  in
  let* chosen, unavailable, contracts, targetFiles =
    selectNodes [] [] [] S.empty originals
  in
  let claimed =
    List.fold_left
      (fun names node -> S.add (kind node.declaration ^ ":" ^ node.name) names)
      S.empty
      (chosen
      @ List.filter
          (fun node ->
            match node.disposition with
            | A.Excluded _ -> true
            | A.Run | A.Blocked _ | A.Replace _ | A.Xfail _ -> false)
          originals)
  in
  let* supplementalSources = ResultList.traverse lookup supplements in
  let compilerOrder =
    List.map2 nodes supplements supplementalSources |> List.concat
  in
  let supplementalNodes =
    compilerOrder
    |> List.filter (fun node ->
        not (S.mem (kind node.declaration ^ ":" ^ node.name) claimed))
  in
  let* supplemental, supplementalUnavailable, supplementalContracts, _ =
    selectNodes [] [] [] S.empty supplementalNodes
  in
  let* () =
    if supplementalContracts = [] then Ok ()
    else Error "Supplemental declarations cannot redirect replacements"
  in
  (* Replacement files may carry compiler-only helpers. Public siblings remain
     owned by their imported declarations and are never pulled in implicitly. *)
  let privateNode (node : node) =
    List.exists
      (String.starts_with ~prefix:"__")
      (String.split_on_char '.' node.name)
  in
  let* support =
    ResultList.traverse
      (fun file ->
        let* source = lookup file in
        Ok
          (nodes file source
          |> List.filter (fun node ->
              privateNode node
              && not
                   (S.mem
                      (kind node.declaration ^ ":" ^ node.name)
                      originalNames))))
      (S.elements targetFiles
      |> List.filter (fun file -> not (List.mem file supplements)))
  in
  let* support, supportUnavailable, supportContracts, _ =
    selectNodes [] [] [] S.empty (List.concat support)
  in
  if supportContracts <> [] then
    Error
      "Compiler-only replacement helpers cannot themselves have replacement \
       annotations"
  else
    let order =
      List.mapi
        (fun index node -> (kind node.declaration ^ ":" ^ node.name, index))
        compilerOrder
      |> StringOrder.Map.of_list
    in
    let rank node =
      Option.value ~default:max_int
        (StringOrder.Map.find_opt
           (kind node.declaration ^ ":" ^ node.name)
           order)
    in
    (* Values are inferred in source order. Preserve the order of explicitly
       supplied compiler implementation files when selecting replacements. *)
    let chosen =
      chosen @ supplemental @ support
      |> List.stable_sort (fun left right ->
          Int.compare (rank left) (rank right))
    in
    let rec checkNames seen = function
      | [] -> Ok ()
      | (node : node) :: rest ->
          let key = kind node.declaration ^ ":" ^ node.name in
          if S.mem key seen then
            Error ("Duplicate selected declaration: " ^ node.name)
          else checkNames (S.add key seen) rest
    in
    let* () = checkNames S.empty chosen in
    let wrap (node : node) =
      if node.path = [] then node.declaration
      else
        W.DModule
          {
            W.range = W.synthRange;
            name = (W.synthRange, String.concat "." node.path);
            declarations = [ node.declaration ];
            keywordModule = W.synthRange;
          }
    in
    let* units =
      ResultList.traverse
        (fun node ->
          let tree =
            {
              W.range = W.synthRange;
              declarations = [ wrap node ];
              exprsToEval = [];
            }
          in
          let* unit_ =
            Validation.validate Validation.Script tree
            |> Result.map_error (fun issues ->
                String.concat "; "
                  (List.map
                     (fun (issue : Validation.issue) ->
                       issue.Validation.message)
                     (NonEmptyList.toList issues)))
          in
          Ok (node.file, unit_))
        chosen
    in
    let* () =
      SourceContracts.check ~context ~sources:(List.map snd units) contracts
    in
    Ok
      {
        units;
        unavailable = unavailable @ supplementalUnavailable @ supportUnavailable;
      }
