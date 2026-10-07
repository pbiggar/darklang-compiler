(* ANF_Optimize.fs - Preserve ANF rewrite checks while production optimization uses SSA. *)
[@@@warning "-4"]
open ANF
open ANFConstants
open ANFExpressionOptimization
open ANFAccumulatorOptimization
module FS = SpecializationIdentity.FunctionSet
let rec rewriteInvertedBoolLiteralBranches varGen expr = match expr with
 | Jump _ | Return _ -> expr,varGen
 | Join (parameter,continuation,entry) -> let body,next=rewriteInvertedBoolLiteralBranches varGen continuation in let entry',final=rewriteInvertedBoolLiteralBranches next entry in Join (parameter,body,entry'),final
 | Let (tid,cexpr,body) -> let body',next=rewriteInvertedBoolLiteralBranches varGen body in Let (tid,cexpr,body'),next
 | If (condition,yes,no) ->
  let yes,afterYes=rewriteInvertedBoolLiteralBranches varGen yes in let no,afterNo=rewriteInvertedBoolLiteralBranches afterYes no in
  match yes,no with Return (BoolLiteral false),Return (BoolLiteral true) -> let result,next=freshVar afterNo in Let (result,UnaryPrim (Not,condition),Return (Var result)),next | _ -> If (condition,yes,no),afterNo
let rewriteInvertedBoolLiteralBranchesInProgram (Program (functions,main) as program) =
 let initial=freshVarGenForProgram program in
 let reversed,next=List.fold_left (fun (acc,vg) (func : functionDef) -> let body,next=rewriteInvertedBoolLiteralBranches vg func.body in {func with body}::acc,next) ([],initial) functions in
 let main,_=rewriteInvertedBoolLiteralBranches next main in Program (List.rev reversed,main)
(*
   Optimize a program with explicit options
   Copy propagation first removes administrative aliases introduced by the
   public `fun` syntax, exposing the exact local closure use set. Running
   devirtualization afterwards does not need another fixed-point iteration:
   it only removes a zero-capture allocation and changes known call forms.
   Optimize main expression
   This standalone rewrite-check API has no checked symbol table.
   Each rewrite can add helpers, so carry the fresh ID cursor through
   the ordered passes instead of rescanning every intermediate program.
*)
let optimizeProgramWithOptionsAndExternalFunctionsWithTrace recordTiming context options eligibleTailRecursionNames externalFunctions program =
 let measure name operation = match recordTiming with None -> operation () | Some record -> let started=(Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in let result=operation () in record name (((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. started));result in
 let program'=if options.enableConstFolding then measure "ANF Optimize detail: Boolean branch rewrite" (fun () -> rewriteInvertedBoolLiteralBranchesInProgram program) else program in
 let Program (functions,main)=program' in
 let functions'=measure "ANF Optimize detail: Function fixed points" (fun () -> List.map (fun func -> let optimized=optimizeToFixedPoint context options func 10 in {optimized with body=devirtualizeCaptureFreeClosures optimized.body}) functions) in
 let mainIds=AST.allocateFunctionIds (List.to_seq (List.map (fun (func : functionDef) -> func.id) functions)) (List.to_seq ["__dark_anf_optimization_main"]) in
 let mainId=match StringOrder.Map.find_opt "__dark_anf_optimization_main" mainIds with Some id -> id | None -> Crash.crash "ANF optimization main identity was not allocated" in
 let mainFunc={id=mainId;name="__main__";typedParams=[];returnType=AST.TUnit;returnOwnership=OwnedReturn;body=main} in
 let mainOptimized=measure "ANF Optimize detail: Main fixed point" (fun () -> optimizeToFixedPoint context options mainFunc 10) in
 let optimizedProgram=Program (functions',devirtualizeCaptureFreeClosures mainOptimized.body) in
 if options.enableTailRecursionModuloOperation then
  let programIds=FS.of_list (List.map (fun (func : functionDef) -> func.id) functions') in let activeEligible=FS.inter eligibleTailRecursionNames programIds in
  let helpers=measure "ANF Optimize detail: Accumulator helper planning" (fun () -> if FS.is_empty activeEligible then FunctionIdMap.empty else
   let names=List.fold_left (fun names (func : functionDef) -> FunctionIdMap.add func.id func.name names) context.functionNames functions' in
   let ids=List.fold_left (fun ids (func : functionDef) -> StringOrder.Map.add func.name func.id ids) context.functionIds functions' in
   let ordinal=FunctionIdMap.fold (fun next id _ -> let candidate=AST.nextFunctionIdOrdinal (AST.functionIdValue id) in if Int64.unsigned_compare candidate next > 0 then candidate else next) 0L names in
   planTailRecursionModuloHelpers ordinal names ids activeEligible) in
  if FunctionIdMap.isEmpty helpers then optimizedProgram else
  let vg=freshVarGenForProgram optimizedProgram in
  let current,vg=measure "ANF Optimize detail: Addition rewrite" (fun () -> transformTailRecursionModuloAddition helpers vg optimizedProgram) in
  let current,vg=measure "ANF Optimize detail: Subtraction rewrite" (fun () -> transformTailRecursionModuloSubtraction helpers vg current) in
  let current,vg=measure "ANF Optimize detail: Multiplication rewrite" (fun () -> transformTailRecursionModuloMultiplication helpers vg current) in
  let current,vg=measure "ANF Optimize detail: Fixed constructor rewrite" (fun () -> transformTailRecursionModuloFixedConstructors helpers vg current) in
  fst (measure "ANF Optimize detail: List constructor rewrite" (fun () -> transformTailRecursionModuloListConstructors helpers externalFunctions vg current))
 else optimizedProgram
let optimizeProgramWithOptionsAndExternalFunctions context options eligible externalFunctions program=optimizeProgramWithOptionsAndExternalFunctionsWithTrace None context options eligible externalFunctions program
let optimizeProgramWithOptions context options (Program (functions,_) as program) = let eligible=FS.of_list (List.map (fun (func : functionDef) -> func.id) functions) in optimizeProgramWithOptionsAndExternalFunctions context options eligible StringOrder.Map.empty program
(*
   Optimize a program with default options
*)
let optimizeProgram context program=optimizeProgramWithOptions context defaultOptimizeOptions program
let optimizeConstFolding context program=optimizeProgramWithOptions context {enableConstFolding=true;enableConstProp=false;enableCopyProp=false;enableDCE=false;enableCSE=false;enableStrengthReduction=false;enableTailRecursionModuloOperation=false} program
let optimizeCopyProp context program=optimizeProgramWithOptions context {enableConstFolding=false;enableConstProp=false;enableCopyProp=true;enableDCE=false;enableCSE=false;enableStrengthReduction=false;enableTailRecursionModuloOperation=false} program
let optimizeDCE context program=optimizeProgramWithOptions context {enableConstFolding=false;enableConstProp=false;enableCopyProp=false;enableDCE=true;enableCSE=false;enableStrengthReduction=false;enableTailRecursionModuloOperation=false} program
