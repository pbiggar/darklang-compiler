(*
   LIRExecutionTestRunner.fs - Compiles and executes single-block LIR programs.
   Checks typed x64 failures and provides shared backend executable support.
*)
[@@@warning "-4-42"]
open Dark_compiler
open LIRExecutionFormat
let (let*)=Result.bind
let executableProgram (LIR.Program (functions,variants,records))=
 let entries,others=List.partition (fun (func:LIR.functionDef)->func.LIR.name="_start") functions in
 match entries with
 |[func]->(match LIR.LabelMap.bindings func.LIR.cfg.LIR.blocks with
 |[_,body]->let entryLabel=LIR.Label "_start_entry" and bodyLabel=LIR.Label "_start_body" in
 let entryBlock:LIR.basicBlock={LIR.label=entryLabel;instrs=[];terminator=LIR.Jump bodyLabel} in
 let bodyBlock={body with LIR.label=bodyLabel} in
 let executableFunction={func with LIR.name="_start";cfg={LIR.entry=entryLabel;blocks=LIR.LabelMap.of_list [entryLabel,entryBlock;bodyLabel,bodyBlock]}} in
 Ok (LIR.Program (executableFunction::others,variants,records))
 |blocks->Error (Printf.sprintf "Executable LIR fixture requires one input block, got %d" (List.length blocks)))
 |entries->Error (Printf.sprintf "Executable LIR fixture requires one _start function, got %d" (List.length entries))
let patchDeferredLabels stringPool (resolved:X86_64_Resolve.resolveResult)=
 if resolved.X86_64_Resolve.deferredFixups=[] then Ok resolved else
 let codeFileOffset=64+56 in let codeSize=Bytes.length resolved.X86_64_Resolve.machineCode in
 X86_64_Resolve.patchDataLabels resolved (X86_64_Resolve.dataLabelOffsets codeFileOffset codeSize stringPool) codeFileOffset
let writeAndRun target binary=
 let path=Filename.concat (Filename.get_temp_dir_name ()) (HostGuid.newGuidN ()) in
 Fun.protect ~finally:(fun ()->SourcePreparation.tryDeleteFile path) (fun ()->try
 let fd=Unix.openfile path [Unix.O_WRONLY;Unix.O_CLOEXEC;Unix.O_CREAT;Unix.O_TRUNC] 0o666 in
 Fun.protect ~finally:(fun ()->Unix.close fd) (fun ()->let rec write offset=if offset<Bytes.length binary then let count=Unix.write fd binary offset (Bytes.length binary-offset) in write (offset+count) in write 0;Unix.fsync fd);
 let permissions=(Unix.stat path).Unix.st_perm in Unix.chmod path (permissions lor 0o100);
 let file,args=match target,Platform.detectArch () with
 |Platform.LinuxX86_64,Ok Platform.X86_64|Platform.ARM64Backend _,Ok Platform.ARM64->path,[]
 |Platform.LinuxX86_64,_->"/opt/dcb/qemu/qemu-x86_64",[path]
 |Platform.ARM64Backend _,_->"/opt/dcb/qemu/qemu-aarch64",[path] in
 TestProcess.capture file args 10000
 with exn->Error ("Execution failed: "^HostFile.errorMessage path exn))
let translate program leakCheck=let* executable=executableProgram program in CodeGen_X86_64.translateProgram executable (leakCheck=LeakCheckEnabled)
let executeX64Program program leakCheck=
 let enableLeakCheck=leakCheck=LeakCheckEnabled in
 let* instructions=Result.map_error (fun msg->"Codegen error: "^msg) (translate program leakCheck) in
 let stringPool=X86_64_Resolve.collectStringPool instructions in
 let* resolved=Result.map_error (fun msg->"Resolve error: "^msg) (X86_64_Resolve.resolveAndEncode instructions) in
 let* resolved=patchDeferredLabels stringPool resolved in
 writeAndRun Platform.LinuxX86_64 (Binary_Generation_ELF_X86_64.createExecutableWithPools resolved.X86_64_Resolve.machineCode stringPool LiteralPool.emptyFloatPool enableLeakCheck 0)
let executeARM64Program armTarget program leakCheck=
 let enableLeakCheck=leakCheck=LeakCheckEnabled in let target=ARM64.targetConfigFor armTarget in
 let options={ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.enableLeakCheck} in
 let* generated=Result.map_error (fun msg->"Codegen error: "^msg) (let* executable=executableProgram program in Backend_Arm64_CodeGen.generateARM64WithOptions target options (ARM64PrepareFunctions.prepareARM64Program executable)) in
 let instructions=Backend_Arm64_CodeGen.generatedProgramInstructions generated in
 let labels=List.filter_map (function Symbolic.Label label->Some label|_->None) instructions |> StringOrder.Set.of_list in
 let* ()=match List.find_map (function Symbolic.BL label when not (StringOrder.Set.mem label labels)->Some label|_->None) instructions with Some label->Error ("Codegen error: ARM64 call target '"^label^"' was not generated")|None->Ok () in
 let emitted=Emit.emitBinary generated (ARM64.targetOS target) enableLeakCheck None None None in writeAndRun (Platform.ARM64Backend armTarget) emitted.Emit.binary
let executeProgram target program leakCheck=match target with Platform.LinuxX86_64->executeX64Program program leakCheck|Platform.ARM64Backend armTarget->executeARM64Program armTarget program leakCheck
let checkExpectation (exitCode,stdout,stderr)=function
 |ExpectedExitCode expected->if exitCode=expected then Ok () else Error (Printf.sprintf "Expected exit code %d, got %d" expected exitCode)
 |ExpectedStdout expected->let actual=HostText.trim stdout in if actual=expected then Ok () else Error ("Expected stdout '"^expected^"', got '"^actual^"'")
 |ExpectedStderr expected->let actual=HostText.trim stderr in if actual=expected then Ok () else Error ("Expected stderr '"^expected^"', got '"^actual^"'")
let checkProcessExpectations test expectations=let* actual=executeProgram Platform.LinuxX86_64 test.program test.leakCheck in let* _=ResultList.traverse (checkExpectation actual) expectations in Ok ()
let checkCodegenError test expected=match translate test.program test.leakCheck with Error msg when HostText.contains msg expected->Ok ()|Error msg->Error ("Expected codegen error containing '"^expected^"', got '"^msg^"'")|Ok _->Error ("Expected codegen error containing '"^expected^"', but translation succeeded")
let runLIRExecutionTest test=match test.expectation with ExpectedProcessResult expectations->checkProcessExpectations test expectations|ExpectedCodegenError expected->checkCodegenError test expected
let loadLIRExecutionTests path=if not (TestFileIO.exists path) then Error ("LIR-execution test file not found: "^path) else try parseLIRExecutionFileContent path (HostFile.readText path) with exn->Error ("Failed to read LIR-execution test file "^path^": "^HostFile.errorMessage path exn)
let tests testFiles=let testsForFile path=match loadLIRExecutionTests path with Error msg->["parse "^Filename.basename path,(fun ()->Error msg)]|Ok cases->List.map (fun test->test.name,(fun ()->runLIRExecutionTest test)) cases in Array.to_list testFiles |> List.sort StringOrder.compare |> List.concat_map testsForFile
