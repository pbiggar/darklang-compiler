(* cache_regression.ml - Preserve cache segregation while bounding lookup allocation. *)
[@@@warning "-42"]
open Dark_compiler
module C=CompilationCacheIdentity
let require condition message=if not condition then failwith message
let functionFor i =
  let blocks=List.init 32 (fun index->let label=MIR.Label ("block_"^string_of_int index) in
    label,{MIR.label;instrs=[];terminator=MIR.Ret (MIR.Int64Const 0L)}) |> MIR.LabelMap.of_list in
  {MIR.id=AST.functionId (Int64.of_int i);name="cache_probe_"^string_of_int i;typedParams=[];
   returnType=AST.TUnit;cfg={MIR.entry=MIR.Label "block_0";blocks};floatRegs=MIR.IntSet.empty}
let key (func:MIR.functionDef):C.mirOptimizationKey={C.func;options=MIROptimizationFacts.defaultOptimizeOptions;effectFreeCalls=SpecializationIdentity.FunctionSet.empty}
let () =
  let session=new CompilationSession.compilationSession () in
  Fun.protect ~finally:(fun ()->session#dispose) (fun ()->
    let keys=List.init 256 (fun i->key (functionFor i)) in
    let generated=ref 0 in
    Gc.full_major ();
    let before=(Gc.quick_stat ()).Gc.minor_words in
    List.iter (fun key->ignore (session#optimizeMirFunction key (fun ()->incr generated;key.C.func))) keys;
    List.iter (fun (key:C.mirOptimizationKey)->
      (* Rebuild maps with another insertion order: equality is semantic, not
         the balancing history or object identity of persistent containers. *)
      let func=key.C.func in
      let blocks=MIR.LabelMap.bindings func.MIR.cfg.MIR.blocks |> List.rev |> MIR.LabelMap.of_list in
      let equivalent={key with C.func={func with MIR.cfg={func.MIR.cfg with MIR.blocks}}} in
      ignore (session#optimizeMirFunction equivalent (fun ()->failwith "Equivalent cache key missed"))) keys;
    let allocated=((Gc.quick_stat ()).Gc.minor_words-.before)*.8. in
    require (!generated=256 && session#cachedMirOptimizationCount=256 && session#mirOptimizationHitCount=256) "Cache counts differ";
    require (allocated<100e6) "Cache lookups allocate quadratically";
    let first=key (functionFor 0) in
    let changed={first with C.options={first.C.options with MIROptimizationFacts.enableSCCP=false}} in
    ignore (session#optimizeMirFunction changed (fun ()->incr generated;changed.C.func));
    let collision={first with C.func={first.C.func with MIR.id=AST.functionId 999L}} in
    ignore (session#optimizeMirFunction collision (fun ()->incr generated;collision.C.func));
    require (!generated=258) "Hash collision or option change incorrectly reused an entry";
    Printf.printf "Cache equality, collisions, options and allocation regressions passed (%.2f MB)\n" (allocated/.1e6))
