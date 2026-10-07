(* TypeFactTests.fs - Verify call-result type facts used by reference counting. *)
[@@@warning "-4"]
open Dark_compiler
module A = ANF
module F = RcTypeFacts
let fid = TestIds.functionIdForName
let context funcs : F.typeContext = {F.typeReg = StringOrder.Map.empty; variantLookup = StringOrder.Map.empty; sumShapeReg = StringOrder.Map.empty; funcReg = funcs; funcParams = StringOrder.Map.empty; tempTypes = F.TempMap.empty; closureFuncs = F.TempMap.empty; typePlanning = F.createRcTypePlanningContext ()}
let testInferCallReturnsFunctionReturnType () =
 let ctx = context (FunctionIdMap.ofList [fid "mkPair", ("mkPair", AST.TFunction ([AST.TInt64], AST.TTuple [AST.TInt64; AST.TInt64]))]) in
 match F.inferCExprType ctx (A.Call (fid "mkPair", [A.IntLiteral (A.Int64 1L)])) with
 | Some (AST.TTuple [AST.TInt64; AST.TInt64]) -> Ok ()
 | Some typ -> Error ("Expected inferCExprType Call to return tuple return type, got: " ^ StructuralFormat.semanticType typ)
 | None -> Error "Expected inferCExprType Call to return a concrete type, got None"
let testMalformedRawGetIntrinsicDoesNotInferInt64 () =
 let ctx = context FunctionIdMap.empty in
 match F.inferCExprType ctx (A.Call (fid "__raw_get_not_a_mangled_type", [A.Var (A.TempId 1); A.IntLiteral (A.Int64 0L)])) with
 | None -> Ok ()
 | Some typ -> Error ("Expected malformed __raw_get_ suffix to remain unknown, got: " ^ StructuralFormat.semanticType typ)
