(* BinaryOutput.ml - Select a validated target backend and assemble executable output. *)
[@@@warning "-4"]
module G=Backend_Arm64_CodeGen
module M=StringOrder.Map
module PhysicalFunctions=Hashtbl.Make(struct type t=LIR.functionDef let equal=(==) let hash=Hashtbl.hash end)
let (let*)=Result.bind
let unsigned value=Printf.sprintf "%Lu" value
let finalizeArm64GenericHelperIds (LIR.Program (functions,variants,records)) functionGroups metadataGroups=
 let sourceNames=List.fold_left (fun names (func:LIR.functionDef)->match FunctionIdMap.tryFind func.LIR.id names with Some existing when existing<>func.LIR.name->Crash.crash ("Executable assigns FunctionId "^unsigned (AST.functionIdValue func.LIR.id)^" to both '"^existing^"' and '"^func.LIR.name^"'")|_->FunctionIdMap.add func.LIR.id func.LIR.name names) FunctionIdMap.empty functions in
 let labels=List.concat_map (fun (func:LIR.functionDef)->Option.fold ~none:[] ~some:(fun (facts:LIR.functionCodegenFacts)->M.bindings facts.LIR.arm64GenericHelperIds |> List.map fst) func.LIR.codegenFacts) functions in
 let finalIds=AST.allocateFunctionIds (FunctionIdMap.keys sourceNames) (List.to_seq labels) in
 let finalizedId label=match M.find_opt label finalIds with Some id->id|None->Crash.crash ("ARM64 helper '"^label^"' has no final identity") in
 let finalizeFunction (func:LIR.functionDef)=
  let localIds=Option.fold ~none:M.empty ~some:(fun (facts:LIR.functionCodegenFacts)->facts.LIR.arm64GenericHelperIds) func.LIR.codegenFacts in if M.is_empty localIds then func else
  let replacements=M.fold (fun label oldId ids->let newId=finalizedId label in match FunctionIdMap.tryFind oldId ids with Some existing when existing<>newId->Crash.crash ("ARM64 helper identity "^unsigned (AST.functionIdValue oldId)^" names two helpers in '"^func.LIR.name^"'")|_->FunctionIdMap.add oldId newId ids) localIds FunctionIdMap.empty in
  let rewrite=function LIR.Call (dest,id,args)->LIR.Call (dest,Option.value ~default:id (FunctionIdMap.tryFind id replacements),args)|LIR.TailCall (id,args)->LIR.TailCall (Option.value ~default:id (FunctionIdMap.tryFind id replacements),args)|instr->instr in
  let blocks=LIR.LabelMap.map (fun (block:LIR.basicBlock)->{block with LIR.instrs=List.map rewrite block.LIR.instrs}) func.LIR.cfg.LIR.blocks in
  let facts=Option.map (fun (facts:LIR.functionCodegenFacts)->{facts with LIR.arm64GenericHelperIds=M.mapi (fun label _->finalizedId label) localIds}) func.LIR.codegenFacts in {func with LIR.cfg={func.LIR.cfg with LIR.blocks};codegenFacts=facts} in
 let finalized=List.map finalizeFunction functions in
 let byId=List.fold_left (fun groups ((original:LIR.functionDef),finalized)->FunctionIdMap.change original.LIR.id (fun values->Some (Option.value ~default:[] values @ [original,finalized])) groups) FunctionIdMap.empty (List.combine functions finalized) in
 let remap (func:LIR.functionDef)=match Option.bind (FunctionIdMap.tryFind func.LIR.id byId) (List.find_map (fun (original,finalized)->if original==func then Some finalized else None)) with Some value->value|None->Crash.crash ("Executable group contains unknown function '"^func.LIR.name^"'") in
 let functionGroups=List.map (fun (group:G.functionGroup)->{group with G.functions=List.map remap group.G.functions}) functionGroups in
 let metadataGroups=List.map (fun (group:G.metadataGroup)->{group with G.functions=List.map remap group.G.functions}) metadataGroups in LIR.Program (finalized,variants,records),functionGroups,metadataGroups
let formatLabel text name=
 let token="{format}" in let output=Buffer.create (String.length text) in
 let rec loop index=if index<String.length text then if index+String.length token<=String.length text && String.sub text index (String.length token)=token then (Buffer.add_string output name;loop (index+String.length token)) else (Buffer.add_char output text.[index];loop (index+1)) in loop 0;Buffer.contents output
let elapsedDetail verbosity duration=if verbosity>=2 then (let scaled=duration*.10. in let lower=Float.floor scaled in let rounded=if scaled-.lower=0.5 then (if Float.rem lower 2.=0. then lower else lower+.1.) else Float.round scaled in Output.println ("        "^FloatFormat.roundTrip (rounded/.10.)^"ms"))
(* Run codegen, encoding, and binary generation. *)
let generateBinary target verbosity (options:CompilerOptions.compilerOptions) elapsed recorder codegenLabel emitLabel dumpAsm dumpMachineCode session programContextIdentity functionGroups metadataGroups sumShapes knownWrites allocatedProgram=
 let record name duration=PipelineDiagnostics.recordPassTiming recorder name duration in
 match target with
 |Platform.LinuxX86_64->
  (* x86-64 backend *)
  if verbosity>=1 then Output.println codegenLabel;let start=elapsed () in
  let* instructions=CodeGen_X86_64.translateProgram allocatedProgram options.CompilerOptions.enableLeakCheck |> Result.map_error (fun error->"x86-64 code generation error: "^error) in
  let duration=elapsed ()-.start in record "Code Generation" duration;elapsedDetail verbosity duration;
  if dumpAsm && verbosity>=3 then (Output.println "=== x86-64 Assembly Instructions ===";List.iteri (fun index instr->Output.println ("  "^string_of_int index^": "^MachineDiagnostic.x64 instr)) instructions;Output.println "");
  if verbosity>=1 then Output.println (formatLabel emitLabel "ELF");let start=elapsed () in
  let pool=X86_64_Resolve.collectStringPool instructions in
  let encode=match session with Some (current:CompilationSession.compilationSession)->current#encodeX64Instruction|None->X86_64_Encoding.encodeInstruction in
  let* resolved=X86_64_Resolve.resolveAndEncodeWith encode instructions |> Result.map_error (fun error->"x86-64 resolve error: "^error) in
  (* Patch data labels (e.g., leak counter) if there are deferred fixups. *)
  let* resolved=(if resolved.X86_64_Resolve.deferredFixups=[] then Ok resolved else let offset=64+56 in let size=Bytes.length resolved.X86_64_Resolve.machineCode in let labels=X86_64_Resolve.dataLabelOffsets offset size pool in X86_64_Resolve.patchDataLabels resolved labels offset) |> Result.map_error (fun error->"x86-64 data label error: "^error) in
  let* entry=X86_64_Resolve.requireLabelPosition "_start" resolved.X86_64_Resolve.labelPositions |> Result.map_error (fun error->"x86-64 resolve error: "^error) in
  let binary=Binary_Generation_ELF_X86_64.createExecutableWithPools resolved.X86_64_Resolve.machineCode pool LiteralPool.emptyFloatPool options.CompilerOptions.enableLeakCheck entry in
  let duration=elapsed ()-.start in record "x86-64 Emit" duration;elapsedDetail verbosity duration;Ok binary
 |Platform.ARM64Backend armTarget->
  (* ARM64 backend (original) *)
  let allocatedProgram,functionGroups,metadataGroups=finalizeArm64GenericHelperIds allocatedProgram functionGroups metadataGroups in
  if verbosity>=1 then Output.println codegenLabel;let start=elapsed () in
  let coverageExprCount=if options.CompilerOptions.enableCoverage then LIR.countCoverageHits allocatedProgram else 0 in
  let codegenOptions={ARM64CodeGenTypes.disableFreeList=options.CompilerOptions.disableFreeList;enableCoverage=options.CompilerOptions.enableCoverage;coverageExprCount;enableLeakCheck=options.CompilerOptions.enableLeakCheck} in
  let arm64Target=ARM64.targetConfigFor armTarget in
  let contexts=PhysicalFunctions.create 16 in List.iter (fun (group:G.metadataGroup)->List.iter (fun func->PhysicalFunctions.replace contexts func group.G.contextIdentity) group.G.functions) metadataGroups;
  let reusableSession=if options.CompilerOptions.enableCoverage then None else session in
  let functionCache=Option.map (fun (current:CompilationSession.compilationSession) func generate->let context=if GenericReferenceCounts.isPlannedGenericRefCountDecHelperCacheKey func then current#arm64GenericReleaseHelperContextIdentity else Option.value ~default:programContextIdentity (PhysicalFunctions.find_opt contexts func) in current#codegenFunction context arm64Target codegenOptions func generate) reusableSession in
  let metadataGroupCache=Option.map (fun (current:CompilationSession.compilationSession) identity functions summarize->current#arm64MetadataGroup identity functions summarize) session in
  let functionGroupCache=Option.map (fun (current:CompilationSession.compilationSession) identity functions generate->current#codegenFunctionGroup identity arm64Target codegenOptions functions generate) reusableSession in
  let refinementCache=Option.map (fun (current:CompilationSession.compilationSession) func callees refine->current#refineArm64LirFunction func callees refine) reusableSession in
  let helperCache=Option.map (fun (current:CompilationSession.compilationSession) key generate->current#arm64Helpers programContextIdentity arm64Target codegenOptions key generate) reusableSession in
  let phaseRecorder=Option.map (fun record name elapsed->record {CompilerOptions.pass=name;elapsed=(Int64.of_float (elapsed *. 1e6))}) recorder in
  let opRecorder=Option.bind session (fun (current:CompilationSession.compilationSession)->current#arm64LirOpExpansionRecorder) in
  let* program=G.generateARM64WithOptionsAndCaches arm64Target codegenOptions (Some sumShapes) (Some knownWrites) functionCache refinementCache functionGroupCache functionGroups metadataGroupCache helperCache metadataGroups opRecorder phaseRecorder allocatedProgram |> Result.map_error (fun error->"Code generation error: "^error) in
  let duration=elapsed ()-.start in record "Code Generation" duration;elapsedDetail verbosity duration;
  if dumpAsm && verbosity>=3 then (Output.println "=== ARM64 Assembly Instructions ===";List.iteri (fun index instr->Output.println ("  "^string_of_int index^": "^MachineDiagnostic.symbolic instr)) (G.generatedProgramInstructions program);Output.println "");
  let os=ARM64.targetOS arm64Target in let formatName=match os with Platform.MacOS->"Mach-O"|Platform.Linux->"ELF" in
  if verbosity>=1 then Output.println (formatLabel emitLabel formatName);let start=elapsed () in
  let prepare=Option.map (fun (current:CompilationSession.compilationSession) chunk generate->current#prepareArm64EmissionChunk chunk generate) session in
  let prepareGroup=Option.map (fun (current:CompilationSession.compilationSession) chunks generate->current#prepareArm64EmissionChunkGroup chunks generate) session in
  let emitted=Emit.emitBinary program os options.CompilerOptions.enableLeakCheck prepare prepareGroup phaseRecorder in
  let duration=elapsed ()-.start in record "ARM64 Emit" duration;elapsedDetail verbosity duration;
  if dumpMachineCode && verbosity>=3 then (Output.println "=== Machine Code (hex) ===";let words=emitted.Emit.machineCode in for index=0 to (Array.length words-1)/4 do let offset=index*4 in if offset+3<Array.length words then Output.println (Printf.sprintf "  %04X: %02lx %02lx %02lx %02lx" offset words.(offset) words.(offset+1) words.(offset+2) words.(offset+3)) done;Output.println ("Total: "^string_of_int (Array.length words)^" bytes\n"));Ok emitted.Emit.binary
