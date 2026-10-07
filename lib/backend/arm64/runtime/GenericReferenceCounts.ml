(* GenericReferenceCounts.fs - Outline reusable fixed-layout destruction helpers. *)
open ARM64CodeGenTypes
open ARM64Instructions
(*
   The normal generic-release lowering remains the single source of truth.
   Give borrowed helpers a unique Stdlib-shaped function identity so the
   existing single-payload sum ownership rule is preserved exactly.
   The body may call nested release helpers. Preserve the root and
   our caller's link register until the complete plan has finished.
*)
let generatePlannedGenericRefCountDecHelper helperLabel (spec:LIR.arm64PlannedGenericDecHelper) (ctx:codeGenContext)=
 let helperFunctionName=if spec.LIR.ownsSinglePayloadSum then helperLabel else "Darklang.Stdlib."^helperLabel in
 let helperCtx={ctx with functionName=helperFunctionName;instructionSite="root"} in
 let metadata={MemoryModel.releasePlanCacheKey=None;releasePlan=Some spec.LIR.releasePlan;sourceType=None} in
 match convertInstr helperCtx (LIR.RefCountDec (LIR.Physical LIR.X0,spec.LIR.payloadSize,LIR.GenericHeap,Some metadata)) with
 | Ok body -> [Symbolic.Label helperLabel;Symbolic.STP_pre (Symbolic.X0,Symbolic.X30,Symbolic.SP,-16)] @ body @ [Symbolic.LDP_post (Symbolic.X0,Symbolic.X30,Symbolic.SP,16);Symbolic.RET]
 | Error error -> Crash.crash ("ARM64 generic release helper generation failed for "^helperLabel^": "^error)
(*
   The compilation-session function cache also stores immutable generic
   release helpers. Their reserved stable label fully identifies the planned
   body; the cache separately keys target and codegen options.
*)
let plannedGenericRefCountDecHelperCacheKey helperId helperLabel : LIR.functionDef=
 let entry=LIR.Label "cache_entry" in
 let block={LIR.label=entry;instrs=[];terminator=LIR.Ret} in
 {LIR.id=helperId;name=helperLabel;typedParams=[];cfg={LIR.entry;blocks=LIR.LabelMap.singleton entry block};stackSize=0;usedCalleeSaved=[];codegenFacts=None}
let isPlannedGenericRefCountDecHelperCacheKey (func:LIR.functionDef)=
 Option.is_none func.LIR.codegenFacts && HostText.startsWith func.LIR.name plannedGenericRefCountDecHelperLabelPrefix
