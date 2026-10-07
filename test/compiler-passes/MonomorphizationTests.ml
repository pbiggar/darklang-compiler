[@@@warning "-4-42"]

(* MonomorphizationTests.ml - Unit tests for AST monomorphization
   Ensures monomorphization preserves unresolved type variables in specializations. *)
open Dark_compiler
module C = CheckedAST
module S = SpecializationIdentity

type testResult = (unit, string) result

let testPreservesTypeVarsInSpecialization () =
  let xId, symbols = C.allocateBinding "x" (C.emptySymbols ()) in
  let id, symbols = C.internFunction "id" symbols in
  let funcDef : C.functionDef =
    {
      C.id;
      name = "id";
      typeParams = [ "t" ];
      params = C.checkedParams (NonEmptyList.singleton (xId, AST.TVar "t"));
      returnType = C.checkedType (AST.TVar "t");
      body = C.Local xId;
      recursion = None;
    }
  in
  let artifact : S.genericFunctionArtifact =
    { S.symbols; func = funcDef; directDependencies = S.FunctionSet.empty }
  in
  let specialized =
    Monomorphization.specializeFromSpecs symbols
      (StringOrder.Map.singleton "id" artifact)
      (S.SpecSet.singleton ("id", [ AST.TVar "t" ]))
  in
  match specialized.S.specializedFuncs with
  | [ specializedArtifact ] ->
      let definition = specializedArtifact.S.func in
      let parameterTypes =
        C.functionParameterTypes definition
        |> NonEmptyList.toList |> List.map snd
      in
      if
        definition.C.name = "id_t"
        && parameterTypes = [ AST.TVar "t" ]
        && C.functionReturnType definition = AST.TVar "t"
      then Ok ()
      else
        Error
          "Type-variable specialization changed the parameter or return type"
  | [] | _ :: _ :: _ -> Error "Expected one type-variable specialization"

let testReplaceTypeAppsWithRegistry () =
  let id, symbols = C.internFunction "id" (C.emptySymbols ()) in
  let specializedId, symbols = C.internFunction "id_i64" symbols in
  let expr =
    C.TypeApp
      ( id,
        [ C.checkedType AST.TInt64 ],
        NonEmptyList.singleton (C.Int64Literal 1L) )
  in
  let registry = S.SpecMap.singleton ("id", [ AST.TInt64 ]) "id_i64" in
  match Monomorphization.replaceTypeAppsWithRegistry symbols registry expr with
  | Ok (C.Call (name, args))
    when name = specializedId
         && NonEmptyList.toList args = [ C.Int64Literal 1L ] ->
      Ok ()
  | Ok result ->
      Error
        ("Unexpected replacement result: "
        ^ CheckedStructuralFormat.toString result)
  | Error message -> Error ("Unexpected error: " ^ message)

let testReplaceTypeAppsWithRegistryMissingSpec () =
  let id, symbols = C.internFunction "id" (C.emptySymbols ()) in
  let expr =
    C.TypeApp
      ( id,
        [ C.checkedType AST.TInt64 ],
        NonEmptyList.singleton (C.Int64Literal 1L) )
  in
  match
    Monomorphization.replaceTypeAppsWithRegistry symbols S.SpecMap.empty expr
  with
  | Ok _ -> Error "Expected missing specialization error"
  | Error _ -> Ok ()

let testSpecializeFromSpecs () =
  let xId, symbols = C.allocateBinding "x" (C.emptySymbols ()) in
  let id, symbols = C.internFunction "id" symbols in
  let funcDef : C.functionDef =
    {
      C.id;
      name = "id";
      typeParams = [ "t" ];
      params = C.checkedParams (NonEmptyList.singleton (xId, AST.TVar "t"));
      returnType = C.checkedType (AST.TVar "t");
      body = C.Local xId;
      recursion = None;
    }
  in
  let genericDefs =
    StringOrder.Map.singleton "id"
      {
        S.symbols;
        func = funcDef;
        directDependencies = S.directDependencies funcDef.C.body;
      }
  in
  let result =
    Monomorphization.specializeFromSpecs symbols genericDefs
      (S.SpecSet.singleton ("id", [ AST.TInt64 ]))
  in
  let hasFunction =
    List.exists
      (fun artifact -> artifact.S.func.C.name = "id_i64")
      result.S.specializedFuncs
  in
  let hasRegistry =
    S.SpecMap.mem ("id", [ AST.TInt64 ]) result.S.specRegistry
  in
  if hasFunction && hasRegistry then Ok ()
  else Error "Expected specializeFromSpecs to produce id_i64 and registry entry"

let tests =
  [
    ("Preserve TVar in monomorphization", testPreservesTypeVarsInSpecialization);
    ("Replace TypeApps with registry", testReplaceTypeAppsWithRegistry);
    ( "Replace TypeApps with registry missing spec",
      testReplaceTypeAppsWithRegistryMissingSpec );
    ("Specialize from specs", testSpecializeFromSpecs);
  ]
