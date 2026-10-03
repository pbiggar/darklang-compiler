(* ANFAccumulatorLowering.fs - Generate recursion helpers before SSA construction. *)
open ANF
module FS = SpecializationIdentity.FunctionSet
open ANFAccumulatorOptimization
(*
   These rewrites create helper functions from structured recursive arms.
   They remain at the ANF construction boundary until direct SSA lowering.
   Ownership lowering and synthesized entries can add functions after checking.
   Include their ordinals while collecting local candidates; the catalog cursor
   already reserves all external identities, including pruned declarations.
*)
let lower nextFunctionOrdinal (context : ANFConstants.optimizeContext) eligibleNames externalFunctions (Program (functions,_) as program) =
 let eligible,next=List.fold_left (fun (eligible,next) (func : functionDef) ->
  let eligible=if FS.mem func.id eligibleNames then FS.add func.id eligible else eligible in
  let ordinal=AST.nextFunctionIdOrdinal (AST.functionIdValue func.id) in eligible,(if Int64.unsigned_compare ordinal next > 0 then ordinal else next)) (FS.empty,nextFunctionOrdinal) functions in
 if FS.is_empty eligible then program else
 let helpers=planTailRecursionModuloHelpers next context.ANFConstants.functionNames context.ANFConstants.functionIds eligible in
 let vg=ANFExpressionOptimization.freshVarGenForProgram program in
 let current,vg=transformTailRecursionModuloAddition helpers vg program in
 let current,vg=transformTailRecursionModuloSubtraction helpers vg current in
 let current,vg=transformTailRecursionModuloMultiplication helpers vg current in
 let current,vg=transformTailRecursionModuloFixedConstructors helpers vg current in
 fst (transformTailRecursionModuloListConstructors helpers externalFunctions vg current)
