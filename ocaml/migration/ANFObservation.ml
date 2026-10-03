open Dark_compiler
module A = InstrumentedANF
module E = SemanticANF
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let int value = `Assoc ["kind", `String "int32"; "value", `String (string_of_int value)]
let int64 value = `Assoc ["kind", `String "int64"; "value", `String (Int64.to_string value)]
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let observe source =
 let integers = [A.Int8 (-128); A.Int16 (-32768); A.Int32 Int32.min_int; A.Int64 Int64.min_int; A.UInt8 255; A.UInt16 65535; A.UInt32 4294967295L; A.UInt64 (-1L)] in
 let atoms = [A.UnitLiteral; A.IntLiteral (A.UInt64 (-1L)); A.BoolLiteral true; A.StringLiteral source; A.FloatLiteral (-0.); A.Var (A.TempId 3); A.FuncRef (AST.functionId (-1L))] in
 let tables = List.map (fun entries -> A.TypeMap.ofSeq (List.to_seq (List.map (fun (id, typ) -> A.TempId id, typ) entries)))
  [[]; [0, AST.TUnit]; [3, AST.TString; 1, AST.TInt64; 3, AST.TBool; 5, AST.TList AST.TString]; [2, AST.TVar source]; [2147483647, AST.TUnit]] in
 let ids = [-2147483648; -1; 0; 1; 2; 3; 4; 5; 6; 2147483646; 2147483647] in
 let table value = tuple [E.aNF_typeMap value; list (fun id -> option SemanticAST.semanticType (A.TypeMap.tryFind (A.TempId id) value)) ids] in
 let generators = list (fun id -> let temp, generator = A.freshVar (A.VarGen id) and expr, exprGenerator = A.freshExprId (A.ExprIdGen id) in tuple [E.aNF_tempId temp; E.aNF_varGen generator; int expr; E.aNF_exprIdGen exprGenerator]) ids in
 let coverage = List.fold_left (fun mapping id -> A.addCoverageEntry id source mapping) A.emptyCoverageMapping [3; 1; 3; -1; 2147483647] in
 let normalized = list (fun parameters -> list (fun expressions -> list (fun atoms -> list E.aNF_atom (InstrumentedSpecializationIdentity.normalizeSyntheticNullaryArgAtoms parameters expressions atoms)) ([] :: List.map (fun atom -> [atom]) atoms @ [atoms])) [[]; [InstrumentedCheckedAST.UnitLiteral]; [InstrumentedCheckedAST.StringLiteral source]]) [[]; [AST.TUnit]; [AST.TInt64]] in
 tuple [list (fun value -> tuple [E.aNF_sizedInt value; int64 (A.sizedIntToInt64 value); SemanticJson.string (A.sizedIntToString value); SemanticAST.semanticType (A.sizedIntToType value)]) integers;
  list E.aNF_atom atoms; list table tables; list (fun earlier -> list (fun later -> table (A.TypeMap.merge earlier later)) tables) tables;
  generators; E.aNF_coverageMapping coverage; normalized; IRFixtures.observe source]
