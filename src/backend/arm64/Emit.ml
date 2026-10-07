(* Emit.ml - ARM64 Emission (Encoding + Binary Generation)
   Resolves symbolic data labels into literal pools, encodes ARM64 instructions,
   and produces a platform-specific binary in a single pass. *)
type emitResult={machineCode:ARM64.machineCode array;binary:bytes}
(* Resolve label refs, encode machine code, and generate a binary for the target OS *)
let emitBinary program os enableLeakCheck prepareCachedChunk prepareCachedChunkGroup phaseRecorder=
 let startPhase ()=Option.map (fun _ -> (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)) phaseRecorder in
 let recordPhase name timer=match phaseRecorder,timer with Some record,Some started->record name ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. started)|_->() in
 let prepareTimer=startPhase () in
 let preparedChunks=Backend_Arm64_CodeGen.generatedProgramChunks program |> List.map (fun (chunk:Backend_Arm64_CodeGen.generatedChunk) ->
  let preparePart instructions=
   let prepare ()=ARM64_Encoding.prepareSymbolicChunk instructions in
   match prepareCachedChunk with Some cache when chunk.Backend_Arm64_CodeGen.reusableAcrossCompilations->cache instructions prepare|_->prepare () in
  match chunk.Backend_Arm64_CodeGen.instructionParts with
  | [instructions]->preparePart instructions
  | instructionParts->
   let prepareGroup ()=List.map preparePart instructionParts |> ARM64_Encoding.combinePreparedChunks in
   match prepareCachedChunkGroup with Some cache when chunk.Backend_Arm64_CodeGen.reusableAcrossCompilations->cache instructionParts prepareGroup|_->prepareGroup ()) in
 recordPhase "ARM64 Emit Chunk Preparation" prepareTimer;
 let poolTimer=startPhase () in
 let stringPool,floatPool=preparedChunks |> List.to_seq |> Seq.flat_map (fun chunk -> Array.to_seq chunk.ARM64_Encoding.poolLabelRefs) |> ARM64_Resolve.collectPoolsFromLabelRefs in
 recordPhase "ARM64 Emit Pool Collection" poolTimer;
 let encodingTimer=startPhase () in
 let machineCode=ARM64_Encoding.encodePreparedChunksWithPools preparedChunks stringPool floatPool os enableLeakCheck in
 recordPhase "ARM64 Emit Encoding" encodingTimer;
 let binaryTimer=startPhase () in
 let binary=match os with Platform.MacOS->Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck|Platform.Linux->Backend_Arm64_Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck in
 recordPhase "ARM64 Emit Binary Assembly" binaryTimer;
 {machineCode;binary}
