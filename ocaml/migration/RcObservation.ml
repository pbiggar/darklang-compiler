[@@@warning "-4"]
open Dark_compiler
module A = ANF
module F = RcTypeFacts
module R = RcReturnAnalysis
module I = RcInsertExpression
module S = RcShapePlanning
module P = RefCountInsertion
module J = ProductionANF
module M = F.TempMap
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let attempt encode action = result encode (try Ok (action ()) with Failure message -> Error message | Invalid_argument message -> Error message)
let types values = `Assoc ["map", list (fun (id, typ) -> tuple [J.aNF_tempId id; SemanticAST.semanticType typ]) (M.bindings values)]
let expression (expr, gen, table) = tuple [J.aNF_aExpr expr; J.aNF_varGen gen; types table]
let func (func, gen, table) = tuple [J.aNF_functionDef func; J.aNF_varGen gen; types table]
let ssaLabel (SSAANF.Label id) = SemanticJson.union "Label" "Label" [SemanticJson.int32 id]
let ssaTerminator = function
 | SSAANF.Return atom -> SemanticJson.union "Terminator" "Return" [J.aNF_atom atom]
 | SSAANF.Jump (target, args) -> SemanticJson.union "Terminator" "Jump" [ssaLabel target; list J.aNF_atom args]
 | SSAANF.Branch (cond, yes, no) -> SemanticJson.union "Terminator" "Branch" [J.aNF_atom cond; ssaLabel yes; ssaLabel no]
let ssaBlock (block : SSAANF.block) = SemanticJson.record "Block" ["Label", ssaLabel block.SSAANF.label; "Parameters", list J.aNF_typedParam block.SSAANF.parameters; "Operations", list (fun (id, operation) -> tuple [J.aNF_tempId id; J.aNF_cExpr operation]) block.SSAANF.operations; "Terminator", ssaTerminator block.SSAANF.terminator]
let ssaFunction (func : SSAANF.functionDef) = SemanticJson.record "Function" ["Id", (let value = AST.functionIdValue func.SSAANF.id in let value = if value < 0L then Z.to_string (Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)) else Int64.to_string value in SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind", `String "uint64"; "value", `String value]]); "Name", SemanticJson.string func.SSAANF.name; "TypedParams", list J.aNF_typedParam func.SSAANF.typedParams; "ReturnType", SemanticAST.semanticType func.SSAANF.returnType; "ReturnOwnership", J.aNF_returnOwnership func.SSAANF.returnOwnership; "Entry", ssaLabel func.SSAANF.entry; "Blocks", `Assoc ["map", list (fun (label, block) -> tuple [ssaLabel label; ssaBlock block]) (SSAANF.LabelMap.bindings func.SSAANF.blocks)]; "FreshValueTypes", types func.SSAANF.freshValueTypes]
let id index = A.TempId index
let v index = A.Var (id index)
let fid index = AST.functionId (Int64.of_int index)
let observe source =
 let scalar = [AST.TUnit; AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TBool; AST.TChar; AST.TFloat64; AST.TInt; AST.TInt128; AST.TUInt128; AST.TString; AST.TBlob; AST.TDateTime; AST.TInternalRawPtr; AST.TNever; AST.TVar source] in
 let managed = [AST.TList AST.TString; AST.TList AST.TInt64; AST.TList (AST.TFunction ([AST.TUnit], AST.TString)); AST.TTuple [AST.TString; AST.TInt64]; AST.TDict (AST.TString, AST.TList AST.TString); AST.TStream AST.TString; AST.TFunction ([AST.TUnit], AST.TString); AST.TRecord ("R", []); AST.TSum ("S", []); AST.TRecord ("S", [])] in
 let record : TypeRegistries.recordTypeInfo = {TypeRegistries.typeParams = []; fields = [source, AST.TString; "next", AST.TList AST.TString]} in
 let sum : MemoryModel.rcSumShapeInfo = {MemoryModel.typeParams = []; payloads = [0, None; 1, Some AST.TString]; unaryPayloadTags = MemoryModel.IntSet.singleton 1} in
 let descriptor : A.recordDescriptor = {A.sourceTypeName = "R"; runtimeTypeName = "R"; typeArgs = []; fields = record.TypeRegistries.fields; valueType = AST.TRecord ("R", [])} in
 let cases typ =
  let bind operation body = A.Let (id 20, operation, body) in
  let make = A.Call (fid 200, []) in
  let ret = A.Return (v 20) and unit = A.Return A.UnitLiteral in
  [A.Return (v 10); bind (A.TypedAtom (v 10, typ)) ret; bind (A.Atom (v 10)) ret;
   bind make unit; bind make ret; bind make (A.Let (id 21, A.TypedAtom (v 20, typ), A.Return (v 21)));
   bind make (A.Let (id 21, A.TupleAlloc [v 20], A.Return (v 21)));
   bind make (A.Let (id 21, A.TupleAlloc [v 20; v 20], A.Return (v 21)));
   bind make (A.Let (id 21, A.RawSlotInit (v 12, A.IntLiteral (A.Int64 8L), v 20, typ), unit));
   bind make (A.Let (id 21, A.RawSlotInit (v 12, A.IntLiteral (A.Int64 8L), v 20, typ), ret));
   bind make (A.Let (id 21, A.TypedAtom (v 20, typ), A.Let (id 22, A.TupleAlloc [v 21], A.Return (v 22))));
   bind make (A.If (v 11, ret, unit));
   bind make (A.Join ({A.id = id 30; typ = AST.TInt64}, A.If (v 11, ret, A.Return (v 10)), A.Let (id 21, make, A.Jump (id 30, A.IntLiteral (A.Int64 7L)))));
   bind make (A.Let (id 21, A.Print (v 20, typ), ret));
   bind (A.BorrowedCall (fid 200, [])) unit; bind (A.BorrowedCall (fid 200, [])) ret;
   bind (A.IfValue (v 11, v 10, v 13)) ret;
   bind (A.RawGet (v 12, A.IntLiteral (A.Int64 8L), None)) (A.Let (id 21, A.TypedAtom (v 20, typ), A.Return (v 21)));
   bind (A.RawGet (v 12, A.IntLiteral (A.Int64 8L), None)) (A.Let (id 21, A.Atom (v 20), A.Let (id 22, A.RawSlotInit (v 12, A.IntLiteral (A.Int64 8L), v 21, typ), unit)));
   bind (A.TupleGet (v 14, 0)) ret; bind (A.RecordGet (descriptor, v 15, 0)) ret;
   bind (A.RecordAlloc (descriptor, [v 10; v 13])) unit;
   bind (A.RecordClone (descriptor, v 15, [v 10; v 13])) ret;
   bind (A.RecordReuse (descriptor, descriptor, v 15, [v 10; v 13])) ret;
   bind (A.ClosureAlloc (fid 203, [v 10])) (A.Let (id 21, A.ClosureCall (v 20, [A.UnitLiteral]), A.Return (v 21)));
   bind make (A.Let (id 21, A.TailCall (fid 100, [v 20; v 11]), A.Return (v 21)));
   bind make (A.Let (id 21, A.TailCall (fid 200, []), A.Return (v 21)));
   bind make (A.Let (id 21, A.Call (fid 202, [v 13; v 20]), A.Return (v 21)))] in
 let observeType typ =
  let functions = FunctionIdMap.ofList [fid 100, ("loop", AST.TFunction ([typ; AST.TBool], typ)); fid 200, ("make", AST.TFunction ([], typ)); fid 201, ("observe", AST.TFunction ([AST.TInt64], AST.TUnit)); fid 202, ("Darklang.Stdlib.List.__push_i64", AST.TFunction ([AST.TList typ; typ], AST.TList typ)); fid 203, ("closure", AST.TFunction ([AST.TUnit], typ))] in
  let initial = M.of_list [id 10, typ; id 11, AST.TBool; id 12, AST.TInternalRawPtr; id 13, typ; id 14, AST.TTuple [typ]; id 15, AST.TRecord ("R", [])] in
  let ctx : F.typeContext = {F.typeReg = StringOrder.Map.singleton "R" record; variantLookup = StringOrder.Map.empty; sumShapeReg = StringOrder.Map.singleton "S" sum; funcReg = functions; funcParams = StringOrder.Map.empty; tempTypes = initial; closureFuncs = M.empty; typePlanning = F.createRcTypePlanningContext ()} in
  tuple [attempt J.memoryModel_rcShape (fun () -> S.rcShapeForType ctx typ);
   list (fun body ->
    let analyzed = R.analyzeReturns M.empty M.empty body in
    let bindings = match analyzed with R.RLet (id, operation, rest, _) -> attempt SemanticAST.semanticType (fun () -> I.inferBindingType ctx id operation rest) | _ -> result SemanticAST.semanticType (Ok typ) in
    tuple [bindings; list (fun first -> attempt expression (fun () -> I.insertRCInternal ctx body (A.VarGen first) initial)) [100; 2147483647];
     list (fun name -> let definition : A.functionDef = {A.id = fid 100; name; typedParams = List.map (fun (id, typ) -> {A.id; typ}) (M.bindings initial); returnType = typ; returnOwnership = A.OwnedReturn; body} in
      tuple [attempt func (fun () -> P.insertRCInFunction ctx definition (A.VarGen 100));
       result ssaFunction (SSAANF.convertFunctionBeforeRC 30 ctx definition);
       result ssaFunction (SSAANF.convertFunction 30 (A.TypeMap.ofSeq (M.to_seq initial)) definition);
       list (fun ownership -> let contract : OwnedIR.callSignature = {OwnedIR.parameters = List.map (fun _ -> ownership) definition.A.typedParams; result = OwnedIR.ProducedCallResult} in result (fun () -> `Null) (P.verifyOwnershipContracts ctx (FunctionIdMap.ofList [fid 100, contract]) (A.Program ([definition], A.Return A.UnitLiteral)))) [OwnedIR.UnmanagedCallParameter; OwnedIR.BorrowedCallParameter; OwnedIR.ConsumedCallParameter; OwnedIR.UniqueCallParameter]]) ["loop"; "Darklang.Stdlib.List.__mapHelper_case"]]) (cases typ)] in
 list observeType (scalar @ managed)
