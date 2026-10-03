(*
   Declarations.fs - Validate type declarations and summarize source inventories.
*)
(* Declarations.ml - Validate type declarations and summarize source inventories. *)
open! AST
open CheckingDiagnostics
module M = StringOrder.Map
module S = StringOrder.Set
let typeDefName = function RecordDef (name, _, _) | SumTypeDef (name, _, _) | TypeAlias (name, _, _) -> name
let typeDefTypeParams = function RecordDef (_, params, _) | SumTypeDef (_, params, _) | TypeAlias (_, params, _) -> params
let typeDefinitions topLevels =
 let definitions = List.filter_map (function TypeDef definition -> Some definition | FunctionDef _ | ValueDef _ | Expression _ -> None) topLevels in
 let _, definitions = List.fold_left (fun (seen, values) definition -> let name = typeDefName definition in
  if S.mem name seen then seen, values else S.add name seen, definition :: values) (S.empty, []) (List.rev definitions) in definitions
let duplicate names =
 let counts = List.fold_left (fun acc name -> M.add name (1 + Option.value (M.find_opt name acc) ~default:0) acc) M.empty names in
 List.find_opt (fun name -> M.find name counts > 1) names
(*
   Validate the nominal declaration namespace before building lookup maps.
   Every type name is visible during this pure validation phase, which permits
   recursive references without allowing later declarations to overwrite an
   earlier identity.
   The interpreter preserves duplicate declarations and
   resolves lookup against the first declaration.
*)
let validateTopLevelTypeDeclarations baseEnv topLevels =
 let ( let* ) = Result.bind in
 let definitions = typeDefinitions topLevels in
 let base = match baseEnv, definitions with
  | _, [] | None, _ -> M.empty
  | Some env, _ ->
    let records = M.map (fun (info : Types.recordTypeInfo) -> List.length info.Types.typeParams) env.Types.indexedTypeReg in
    let aliases = M.map (fun (params, _) -> List.length params) env.Types.aliasReg in
    let named = M.fold M.add aliases records in
    M.fold (fun _ (name, params, _, _) acc -> M.add name (List.length params) acc) env.Types.variantLookup named in
 let arities = List.fold_left (fun acc definition -> M.add (typeDefName definition) (List.length (typeDefTypeParams definition)) acc) base definitions in
 let declaringModule name = match NameResolution.tryQualifiedName name with None -> [] | Some qualified -> List.rev (List.tl (List.rev (NameResolution.qualifiedNameSegments qualified))) in
 let resolveArity owner name = List.find_map (fun name -> Option.map (fun arity -> name, arity) (M.find_opt name arities)) (NameResolution.candidateSpellings NameResolution.Type (declaringModule owner) name) in
 let rec validateReference owner typ =
  let all values = List.fold_left (fun result value -> let* () = result in validateReference owner value) (Ok ()) values in
  match typ with
  | TRecord (name, args) | TSum (name, args) -> (match resolveArity owner name with
    | None -> Error (GenericError ("Unknown type reference: " ^ name ^ " in " ^ owner))
    | Some (_, expected) when expected <> List.length args -> Error (GenericError ("Type argument arity mismatch: " ^ name ^ " expects " ^ string_of_int expected ^ ", got " ^ string_of_int (List.length args) ^ " in " ^ owner))
    | Some _ -> all args)
  | TFunction (params, ret) -> all (params @ [ret]) | TTuple types -> all types
  | TList value | TStream value -> validateReference owner value | TDict (key, value) -> all [key; value]
  | TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TNever | TInternalRawPtr -> Ok () in
 let aliasCycles () =
  let names = S.of_list (List.filter_map (function TypeAlias (name, _, _) -> Some name | RecordDef _ | SumTypeDef _ -> None) definitions) in
  let rec referenced typ =
   let combine values = List.fold_left (fun acc value -> S.union acc (referenced value)) S.empty values in
   match typ with
   | TRecord (name, args) | TSum (name, args) -> let nested = combine args in if S.mem name names then S.add name nested else nested
   | TFunction (params, ret) -> combine (ret :: params) | TTuple values -> combine values | TList value | TStream value -> referenced value | TDict (key, value) -> combine [key; value]
   | TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TNever | TInternalRawPtr -> S.empty in
  let graph = M.of_list (List.filter_map (function TypeAlias (name, _, target) -> Some (name, referenced target) | RecordDef _ | SumTypeDef _ -> None) definitions) in
  let rec visit name states = match M.find_opt name states with
   | Some AliasValidated -> Ok states
   | Some AliasVisiting -> Error (GenericError ("Invalid recursive type alias cycle involving: " ^ name))
   | None -> let visiting = M.add name AliasVisiting states in
     let* states = List.fold_left (fun result dependency -> let* states = result in visit dependency states) (Ok visiting) (S.elements (M.find name graph)) in Ok (M.add name AliasValidated states) in
  Result.map (fun _ -> ()) (List.fold_left (fun result (name, _) -> let* states = result in visit name states) (Ok M.empty) (M.bindings graph)) in
 match duplicate (List.map typeDefName definitions) with
 | Some name -> Error (GenericError ("Duplicate type declaration: " ^ name))
 | None ->
   let colliding = AST.collidingConstructorCaseNames definitions in
   let entries = List.concat_map (function SumTypeDef (name, _, variants) -> List.filter_map (fun (variant : AST.variant) -> if S.mem variant.name colliding then Some (AST.constructorRuntimeIdentity name variant.name, name ^ "." ^ variant.name) else None) variants | RecordDef _ | TypeAlias _ -> []) definitions in
   let module I = Map.Make (Int) in
   let grouped = List.fold_left (fun acc (identity, name) -> I.add identity (Option.value (I.find_opt identity acc) ~default:[] @ [name]) acc) I.empty entries in
   let identities = List.map fst entries |> List.fold_left (fun acc id -> if List.mem id acc then acc else acc @ [id]) [] in
   let collision = List.find_map (fun id -> let names = I.find id grouped |> List.fold_left (fun acc name -> if List.mem name acc then acc else acc @ [name]) [] in if List.length names > 1 then Some (id, names) else None) identities in
   match collision with
   | Some (id, names) -> Error (GenericError ("Constructor identity collision " ^ string_of_int id ^ ": " ^ String.concat ", " names))
   | None ->
     let rec validate = function
      | [] -> Ok ()
      | definition :: rest -> let owner = typeDefName definition in let params = typeDefTypeParams definition in
        (match duplicate params with
         | Some param -> Error (GenericError ("Duplicate type parameter: " ^ param ^ " in " ^ owner))
         | None ->
           let types = match definition with RecordDef (_, _, fields) -> List.map snd fields | SumTypeDef (_, _, variants) -> List.concat_map (fun (variant : AST.variant) -> variant.fields) variants | TypeAlias (_, _, target) -> [target] in
           let referenced = List.fold_left (fun acc typ -> Types.collectTypeVarsInType typ acc) [] types in
           match List.find_opt (fun name -> not (List.mem name params)) referenced with
           | Some param -> Error (GenericError ("Undeclared type parameter: '" ^ param ^ " in " ^ owner))
           | None ->
             let referenceResult = List.fold_left (fun result typ -> let* () = result in validateReference owner typ) (Ok ()) types in
             let declarationResult = match definition with
              | RecordDef (_, _, []) -> Error (GenericError ("Record declaration must contain at least one field: " ^ owner))
              | RecordDef _ -> Ok ()
              | SumTypeDef (_, _, []) -> Error (GenericError ("Enum declaration must contain at least one case: " ^ owner))
              | SumTypeDef (_, _, variants) -> (match duplicate (List.map (fun (variant : AST.variant) -> variant.name) variants) with Some name -> Error (GenericError ("Duplicate constructor declaration: " ^ owner ^ "." ^ name)) | None -> Ok ())
              | TypeAlias _ -> Ok () in
             let* () = referenceResult in let* () = declarationResult in validate rest) in
     let* () = aliasCycles () in validate definitions
(*
   Build all declaration registries after validation and name resolution have
   established unique nominal type and constructor identities.
*)
let summarizeTopLevelDeclarations topLevels =
 let open ResolveDeclarations in
 let empty = {typeReg = M.empty; recordTypeParams = M.empty; aliasReg = M.empty; variantLookup = M.empty; funcSigs = M.empty; funcParamNames = M.empty; genericFuncs = M.empty} in
 let colliding = AST.collidingConstructorCaseNames (typeDefinitions topLevels) in
 let key = function FunctionDef definition -> Some ("function", definition.name) | ValueDef value -> Some ("value", AST.valueDefName value) | TypeDef definition -> Some ("type", typeDefName definition) | Expression _ -> None in
 let indexed = List.mapi (fun index value -> index, value) topLevels in
 let winning = List.fold_left (fun acc (index, value) -> match key value with None -> acc | Some name -> (name, index) :: List.remove_assoc name acc) [] indexed in
 let declarations = List.filter (fun (index, value) -> match key value with None -> true | Some name -> List.assoc_opt name winning = Some index) indexed in
 List.fold_left (fun summary (_, value) -> match value with
  | TypeDef (RecordDef (name, params, fields)) -> {summary with typeReg = M.add name fields summary.typeReg; recordTypeParams = M.add name params summary.recordTypeParams}
  | TypeDef (TypeAlias (name, params, target)) -> {summary with aliasReg = M.add name (params, target) summary.aliasReg}
  | TypeDef (SumTypeDef (name, params, variants)) ->
    let lookup = List.mapi (fun ordinal (variant : AST.variant) -> ordinal, variant) variants |> List.fold_left (fun lookup (ordinal, (variant : AST.variant)) ->
     let tag = if S.mem variant.name colliding then AST.constructorRuntimeIdentity name variant.name else ordinal in
     let info = name, params, tag, variant.fields in M.add (name ^ "." ^ variant.name) info (M.add variant.name info lookup)) summary.variantLookup in {summary with variantLookup = lookup}
  | FunctionDef definition -> let params = NonEmptyList.toList definition.params in
    {summary with funcSigs = M.add definition.name (List.map snd params, definition.returnType) summary.funcSigs;
     funcParamNames = M.add definition.name (List.map fst params) summary.funcParamNames;
     genericFuncs = if definition.typeParams = [] then summary.genericFuncs else M.add definition.name definition.typeParams summary.genericFuncs}
  | ValueDef _ | Expression _ -> summary) empty declarations
