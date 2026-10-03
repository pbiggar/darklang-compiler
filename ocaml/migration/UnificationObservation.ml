open Dark_compiler
module U = Unification
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let bindings values = list (fun (name, value) -> tuple [str name; typ value]) values
let map values = `Assoc ["map", bindings (M.bindings values)]
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [str error]
let option encode = function Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] | None -> SemanticJson.union "FSharpOption" "None" []
let observe source =
 let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
 AST.TVar source; AST.TInferenceVar (source, "fixed"); AST.TVar "t$empty"; AST.TFunction ([AST.TVar "a"], AST.TVar "b"); AST.TTuple [AST.TVar "a"; AST.TInt64];
 AST.TRecord ("R", [AST.TVar "a"]); AST.TSum ("R", [AST.TVar "a"]); AST.TList (AST.TVar "a"); AST.TStream (AST.TVar "a"); AST.TDict (AST.TVar "a", AST.TVar "b");
 AST.TFunction ([AST.TInt64], AST.TBool); AST.TTuple [AST.TList (AST.TVar "t$empty"); AST.TInt]; AST.TTuple [AST.TList (AST.TVar "a"); AST.TInt]; AST.TList AST.TInt64; AST.TStream AST.TInt64;
 AST.TRecord ("R", []); AST.TSum ("S", []); AST.TFunction ([], AST.TUnit)] in
 let aliases = M.singleton "Alias" ([], AST.TRecord ("R", [])) in
 let pairs = List.concat_map (fun left -> List.map (fun right -> tuple [result bindings (U.matchConcrete left right); result bindings (U.matchTypes left right);
   result map (U.unifyTypes left right); `Bool (U.typesCompatible left right); `Bool (U.typesCompatibleWithAliases aliases left right);
   option typ (U.reconcileTypes None left right); option typ (U.reconcileTypes (Some aliases) left right)]) samples) samples in
 let cases = List.map (fun (left, right) -> [source, left; source, right; "tail", AST.TInt64]) (List.combine samples (List.rev samples)) @
   [["a", AST.TList (AST.TVar "b$0"); "a", AST.TList (AST.TVar "b")];
    ["a", AST.TList (AST.TVar "b"); "a", AST.TList (AST.TVar "b$0")];
    ["a", AST.TRecord ("R", [AST.TVar "b$0"]); "a", AST.TRecord ("R", [AST.TVar "b"])];
    ["a", AST.TInt64; "a", AST.TString]] in
 let inference = List.map (fun actual -> result (list typ) (U.inferTypeArgs ["a"; "b"; source] [AST.TVar "a"] [actual] (Some (AST.TVar "b")) (Some AST.TString))) samples @
   [result (list typ) (U.inferTypeArgs ["a"] [AST.TVar "a"] [] None None);
    result (list typ) (U.inferTypeArgs ["a"] [AST.TVar "a"; AST.TVar "a"] [AST.TInt64; AST.TString] None None)] in
 tuple [str U.emptyListElementVar; list (fun name -> `Bool (U.isInferenceVar name)) [source; "#infer:fixed"; "t$empty"; "binding_x"; "__x"; "recursiveParameter0"; "a"];
   list (fun value -> tuple [option str (U.unificationVar value); `Bool (U.containsTVar value)]) samples;
   `List pairs; list (fun values -> result map (U.consolidateBindings values)) cases; `List inference;
   option (fun (value, name) -> tuple [typ value; str name]) (U.tryLookupResolved source (M.singleton source AST.TInt64));
   option (fun (value, name) -> tuple [typ value; str name]) (U.tryLookupResolved source M.empty);
   list (fun index -> str (ExpressionSupport.paramNameForLegacyError (M.singleton source ["first"; "second"]) source index)) [-2147483648; -1; 0; 1; 2; 3; 2147483647]]
