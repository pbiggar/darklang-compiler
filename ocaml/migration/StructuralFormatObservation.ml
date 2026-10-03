open Dark_compiler
let observe source =
 let scalar = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr] in
 let rec nested depth value = if depth = 0 then value else nested (depth - 1) (AST.TList value) in
 let samples = scalar @ [AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TFunction ([AST.TVar source; AST.TInt64], AST.TList AST.TString);
 AST.TTuple scalar; AST.TRecord (source, scalar); AST.TSum (source, scalar); AST.TList (AST.TVar source); AST.TStream (AST.TVar source);
 AST.TDict (AST.TRecord (source, []), AST.TSum (source, [AST.TVar source]));
 AST.TTuple (List.init 100 (fun _ -> AST.TUnit)); AST.TTuple (List.init 101 (fun _ -> AST.TUnit));
 nested 99 AST.TUnit; nested 100 AST.TUnit; nested 101 AST.TUnit] @ (if source = "" then [AST.TTuple (List.init 100 (fun _ -> AST.TTuple (List.init 100 (fun _ -> AST.TUnit))))] else []) @
 List.concat_map (fun length -> [AST.TRecord (String.make length 'x', [AST.TUnit; AST.TList AST.TInt64]); AST.TFunction ([AST.TVar (String.make length 'x')], AST.TString)]) [60; 61; 62; 63; 64; 65; 70; 75; 79; 80; 81] in
 `List (List.map (fun value -> SemanticJson.string (HostStructuralFormat.semanticType value)) samples)
