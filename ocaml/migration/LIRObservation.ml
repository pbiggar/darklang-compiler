(* Full block layout, carried codegen facts, RC keys, and constructor observations. *)
[@@@warning "-4"]
open Dark_compiler
module L = LIR
module J = ProductionLIR
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let label text = L.Label text
let block text instructions terminator : L.basicBlock = {L.label = label text; instrs = instructions; terminator}
let graph entry blocks : L.cfg = {L.entry = label entry; blocks = L.LabelMap.of_list (List.map (fun (value : L.basicBlock) -> value.L.label, value) blocks)}
let makeFunction cfg typedParams : L.functionDef = {L.id = AST.functionId (-1L); name = "fixture"; typedParams; cfg; stackSize = 32; usedCalleeSaved = [L.X19;L.X27]; codegenFacts = None}
let observe source =
 let terminators = [L.Ret;L.Jump (label "a");L.Branch (L.Virtual 0,label "b",label "a");L.BranchZero (L.Virtual 0,label "b",label "a");L.BranchBitZero (L.Virtual 0,3,label "b",label "a");L.BranchBitNonZero (L.Virtual 0,3,label "b",label "a");L.CondBranch (L.LT,label "b",label "a")] in
 let layouts = List.concat_map (fun term -> List.map (fun variant ->
  let aTerm = match variant with 0 -> L.Jump (label "ret") | 1 -> L.Jump (label source) | 2 -> L.Jump (label "missing") | 3 -> L.Branch (L.Virtual 1,label "ret",label "ret") | _ -> L.Ret in
  let bTerm = if variant = 3 then L.Ret else L.Jump (label "ret") in
  let cfg = graph source [block source [] term;block "a" [] aTerm;block "b" [] bTerm;block "ret" [] L.Ret;block "\xee\x80\x80" [] (L.Jump (label "\xf0\x90\x80\x80"));block "\xf0\x90\x80\x80" [] L.Ret] in
  tuple [J.cfg cfg;result (list J.basicBlock) (L.layoutBlocks cfg)]) [0;1;2;3;4]) terminators in
 let bad = [graph "missing" [block source [] L.Ret];graph "missing" [];graph source [block source [] (L.Jump (label "missing"))]] in
 let planTypes = [AST.TString;AST.TInt64;AST.TList AST.TString;AST.TRecord ("\xee\x80\x80",[]);AST.TRecord ("\xf0\x90\x80\x80",[]);AST.TTuple [AST.TString;AST.TList AST.TInt64]] in
 let basePlans = [MemoryModel.NoReleasePlan; MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer;MemoryModel.DynamicBufferRelease (MemoryModel.FixedSizeRoot (8,MemoryModel.GenericHeap));MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer] in
 let recursive = List.map (fun typ -> MemoryModel.RecursiveRelease typ) planTypes in
 let fields = List.mapi (fun i plan -> MemoryModel.FieldRelease (i*8,plan)) recursive in
 let payloads = [MemoryModel.NoPayloadRelease;MemoryModel.FixedBlockPayloadRelease (48,fields);MemoryModel.BoxedSumPayloadRelease (48,fields,[{MemoryModel.tag=3;fieldReleases=fields};{MemoryModel.tag=1;fieldReleases=[]}]);MemoryModel.TaggedListPayloadRelease (List.hd recursive);MemoryModel.DictPayloadRelease (List.hd recursive,List.nth recursive 1);MemoryModel.ClosurePayloadRelease fields] in
 let plans = basePlans @ recursive @ List.mapi (fun i payload -> MemoryModel.RootRelease (i*8,MemoryModel.GenericHeap,payload)) payloads in
 let metadata = None :: Some {MemoryModel.releasePlanCacheKey=None;releasePlan=None;sourceType=None} :: List.concat_map (fun plan -> List.map (fun cache -> Some {MemoryModel.releasePlanCacheKey=cache;releasePlan=Some plan;sourceType=Some AST.TString}) [None;Some source;Some "\xee\x80\x80";Some "\xf0\x90\x80\x80"]) plans in
 let kinds = [L.GenericHeap;L.StreamHeap;L.TaggedList;L.DictHeap;L.ClosureHeap] in
 let rcInstructions = List.concat_map (fun kind -> List.concat_map (fun metadata -> [L.RefCountDec (L.Virtual 1,16,kind,metadata);L.RefCountInc (L.Virtual 1,16,kind,metadata)]) metadata) kinds in
 let cliInstructions = [L.Execute;L.RunProcess;L.GetArgv;L.SpawnProcess;L.ProcessIO;L.TerminateProcess] |> List.map (fun operation -> L.CliNative (L.Virtual 1,operation,[])) in
 let instructions = LIRFixtures.instructions source in
 let paramLists = [[];[{L.reg=L.Virtual 0;typ=AST.TTuple []}];[{L.reg=L.Virtual 0;typ=AST.TTuple [AST.TInt64;AST.TString;AST.TList AST.TString]}];[{L.reg=L.Physical L.X0;typ=AST.TInt64}]] in
 let factCases = List.concat_map (fun instruction -> List.map (fun params ->
  let func = makeFunction (graph source [block source [instruction] L.Ret]) params in
  tuple [J.functionDef func;J.functionCodegenFacts (L.analyzeFunctionCodegenFacts func);J.functionDef (L.attachFunctionCodegenFacts func)]) paramLists) (instructions @ cliInstructions @ rcInstructions) in
 let allFunction = makeFunction (graph source [block source instructions L.Ret;block "\xee\x80\x80" rcInstructions L.Ret;block "\xf0\x90\x80\x80" cliInstructions L.Ret]) (List.nth paramLists 2) in
 let program = L.Program ([allFunction],StringOrder.Map.singleton source {L.typeParams=["a"];variants=[{L.name="C";tag=3;payload=Some AST.TString;fieldCount=1}]},StringOrder.Map.singleton source ["f",AST.TList AST.TInt64]) in
 let keys = List.map L.rcReleasePlanMemoKey metadata in
 let printingInstructions = instructions @ List.concat_map (fun size -> [L.PrintSum (L.Virtual 3,List.init size (fun i -> source ^ "\n\"",i,if i mod 2 = 0 then None else Some (AST.TTuple planTypes)),false);L.PrintRecord (L.Virtual 3,source,List.init size (fun i -> source ^ string_of_int i,AST.TTuple planTypes))]) [0;1;2;3;4;8] in
 let printerCases = List.concat_map (fun terminator -> List.map (fun instruction ->
  let first = makeFunction (graph source [block source [instruction] terminator;block "a" [] L.Ret]) [] in
  let second = {first with L.id=AST.functionId 2L;name="Other.\xf0\x90\x90\xa8";cfg=graph "a" [block "a" [] L.Ret]} in
  let program = L.Program ([first;second],StringOrder.Map.empty,StringOrder.Map.empty) in
  tuple [SemanticJson.string (LIRPrinter.formatLIR program);list (fun filter -> list (fun summary -> SemanticJson.string (LIRPrinter.formatLIRDump filter summary program)) [false;true]) [None;Some "fixture";Some "TURE";Some "absent";Some "\xf0\x90\x90\x80"] ]) printingInstructions) terminators in
 tuple [LIRFixtures.observe source;`List layouts;list (fun cfg -> result (list J.basicBlock) (L.layoutBlocks cfg)) bad;`List factCases;
  list J.rcReleasePlanMemoKey keys;list J.rcReleasePlanMemoKey (L.RcReleasePlanMemoKeySet.elements (L.RcReleasePlanMemoKeySet.of_list keys));
  J.program (L.attachCodegenFacts program);SemanticJson.int32 (L.countCoverageHits program);`List printerCases;
  SemanticJson.string (LIRPrinter.formatLIR (L.Program ([],StringOrder.Map.empty,StringOrder.Map.empty)))]
