(*
   X86_64CodeGenTests.fs - Tests for x86-64 code generation from LIR

   Verifies that LIR programs translate to working x86-64 executables.
   Build and run a LIR program with process arguments, returning exit code,
   stdout, and stderr.
   Build and run a LIR program, returning exit code, stdout, and stderr.
   Build and run a LIR program, returning the exit code
   Create a minimal LIR function with a single basic block
   Literal strings belong in the executable's immutable literal pool. Emitting
   bump-allocation instructions here leaks one heap object per execution and can
   exhaust the runtime heap in string-heavy code.
   Initializing a string literal field uses RCX internally. A separate live X3
   value must survive when the containing record is held in another register.
   RawSlotInit computes its destination through R11. When the retained value is
   also in X12/R11, ownership must be established before that computation.
   CLI argv is implemented by an in-binary runtime helper. Its call must
   resolve as a code label rather than being deferred as an ELF data fixup.
   Native argv entries must be copied into a managed String and returned in a
   nullable String pointer, matching the LIR type of Stdlib.Cli.__argv.
   Run the x64 kernel boundary under QEMU: hostname and environment values
   must be managed strings, CPU affinity must be counted, and kill(2) must
   preserve EINVAL without consulting a command-line utility.
   The host is ARM64, so inspect the x64 syscall lowering directly. DateTime
   clock values retain nanosecond-derived precision as 100ns Unix ticks.
   Float arguments are parallel moves. A swap must retain both original values
   rather than letting the first MOVSD overwrite the source of the second one.
   Test: malformed x64 CFGs should be reported as codegen errors rather than throwing Map.find.
   Test: condition consumers cannot inherit float comparison state from an earlier translation.
   Test: conditional branch
   Test: x64 generic boxed-sum RefCountDec dispatches mixed payload cleanup by tag.
   Test: x64 generic fixed-block RefCountDec dispatches nested mixed boxed-sum cleanup by tag.
   Test: x64 DictHeap RefCountDec selects a planned helper for nested dict/list payload cleanup.
   Test: x64 tagged-list generic tuple payloads stay on planned list helpers.
   Test: x64 tagged-list generic record payloads stay on planned list helpers.
   Test: x64 DictHeap RefCountDec releases every managed payload in collision nodes.
   Test: x64 DictHeap RefCountDec releases every managed key and recursive tuple/list value in collision nodes.
   Test: x64 DictHeap RefCountDec keeps recursive string-key tuple values on planned dict helpers.
   Test: x64 higher-arity tuple list payloads stay on planned release helpers.
   Test: x64 higher-field record list payloads stay on planned release helpers.
   Test: releasing one closure preserves other live closures in x64 argument registers.
   Test: x64 closure RefCountDec dispatches captured mixed boxed-sum cleanup by tag.
   Test: x64 tagged-list RefCountDec releases closure payloads in stdlib helper contexts.
   Test: x64 tagged-list RefCountDec releases every tuple3 dynamic field combination.
   Test: x64 tagged-list RefCountDec releases nested tuple dynamic payloads at later offsets.
   Test: x64 tagged-list RefCountDec releases every record3 dynamic field combination.
   Test: x64 tagged-list RefCountDec dispatches mixed boxed-sum dynamic payload cleanup by tag.
   Test: x64 tagged-list RefCountDec releases boxed sum tuple2 dynamic combinations.
   Test: x64 tagged-list RefCountDec releases boxed sum tuple3 dynamic combinations.
   Test: x64 tagged-list RefCountDec releases boxed sum record3 dynamic payload combinations.
*)
[@@@warning "-4-42"]
open Dark_compiler
open LIR
module M=StringOrder.Map
module X=X86_64
let (let*)=Result.bind
let require condition error=if condition then Ok () else Error error
let mergeFixtureVariantRegistries left right=M.fold (fun name variants acc->match M.find_opt name acc with None->M.add name variants acc|Some existing when existing=variants->acc|Some _->Crash.crash ("Conflicting inferred test variant metadata for "^name)) right left
let rec inferFixtureVariantsFromType=function
 |AST.TSum (name,args)->
  let variants:typeVariants=match args with
  |[]->{LIR.typeParams=[];variants=[{LIR.name=name^"_case";tag=0;payload=None;fieldCount=0}]}
  |[_]->{LIR.typeParams=["a"];variants=[{LIR.name=name^"_payload";tag=0;payload=Some (AST.TVar "a");fieldCount=1}]}
  |_->Crash.crash ("Cannot infer test variant metadata for multi-argument sum "^name) in
  List.map inferFixtureVariantsFromType args |> List.fold_left mergeFixtureVariantRegistries (M.singleton name variants)
 |AST.TTuple fields|AST.TRecord (_,fields)->List.map inferFixtureVariantsFromType fields |> List.fold_left mergeFixtureVariantRegistries M.empty
 |AST.TList typ|AST.TStream typ->inferFixtureVariantsFromType typ
 |AST.TDict (key,value)->mergeFixtureVariantRegistries (inferFixtureVariantsFromType key) (inferFixtureVariantsFromType value)
 |AST.TFunction (args,result)->List.map inferFixtureVariantsFromType (result::args) |> List.fold_left mergeFixtureVariantRegistries M.empty
 |AST.TInt8|AST.TInt16|AST.TInt32|AST.TInt64|AST.TInt128|AST.TInt|AST.TUInt8|AST.TUInt16|AST.TUInt32|AST.TUInt64|AST.TUInt128|AST.TBool|AST.TFloat64|AST.TString|AST.TBlob|AST.TChar|AST.TDateTime|AST.TUnit|AST.TInternalRawPtr|AST.TNever|AST.TVar _|AST.TInferenceVar _->M.empty
let inferFixtureVariantsFromRcMetadata metadata=Option.bind metadata (fun value->value.MemoryModel.sourceType) |> Option.map inferFixtureVariantsFromType |> Option.value ~default:M.empty
let inferFixtureVariantsFromInstr=function
 |Phi (_,_,Some typ)|HeapStore (_,_,_,Some typ)|RawSlotInit (_,_,_,typ)|PrintList (_,typ)->inferFixtureVariantsFromType typ
 |RefCountInc (_,_,_,metadata)|RefCountDec (_,_,_,metadata)->inferFixtureVariantsFromRcMetadata metadata
 |PrintSum (_,variants,_)->List.filter_map (fun (_,_,payload)->payload) variants |> List.map inferFixtureVariantsFromType |> List.fold_left mergeFixtureVariantRegistries M.empty
 |PrintRecord (_,_,fields)->List.map (fun (_,typ)->inferFixtureVariantsFromType typ) fields |> List.fold_left mergeFixtureVariantRegistries M.empty
 |_->M.empty
let inferFixtureVariantsFromFunction (func:functionDef)=
 let params=List.map (fun (param:typedLIRParam)->inferFixtureVariantsFromType param.LIR.typ) func.LIR.typedParams |> List.fold_left mergeFixtureVariantRegistries M.empty in
 LabelMap.bindings func.LIR.cfg.LIR.blocks |> List.concat_map (fun (_,block)->block.LIR.instrs) |> List.map inferFixtureVariantsFromInstr |> List.fold_left mergeFixtureVariantRegistries params
let completeFixtureVariants (Program (functions,variants,records))=
 let mergeInferred explicit inferred=M.fold (fun name value acc->if M.mem name acc then acc else M.add name value acc) inferred explicit in
 let inferred=List.map inferFixtureVariantsFromFunction functions |> List.fold_left mergeInferred M.empty in Program (functions,mergeInferred variants inferred,records)
let translate program leak=CodeGen_X86_64.translateProgram (completeFixtureVariants program) leak
let binaryFor program leak alwaysPatch=
 let* instructions=Result.map_error (fun error->"Codegen error: "^error) (translate program leak) in
 let pool=X86_64_Resolve.collectStringPool instructions in
 let* resolved=Result.map_error (fun error->"Resolve error: "^error) (X86_64_Resolve.resolveAndEncode instructions) in
 let* resolved=if not alwaysPatch && resolved.X86_64_Resolve.deferredFixups=[] then Ok resolved else
  let offsets=X86_64_Resolve.dataLabelOffsets 120 (Bytes.length resolved.X86_64_Resolve.machineCode) pool in Result.map_error (fun error->"Data label error: "^error) (X86_64_Resolve.patchDataLabels resolved offsets 120) in
 Ok (Binary_Generation_ELF_X86_64.createExecutableWithPools resolved.X86_64_Resolve.machineCode pool LiteralPool.emptyFloatPool leak 0)
let runLIRProgramFullWithOptionsAndArgs program leak args=
 let* binary=binaryFor program leak false in
 let temp=Filename.temp_file "dark-x64-" "" in
 let outcome=try
 Out_channel.with_open_bin temp (fun stream->Out_channel.output_bytes stream binary;Out_channel.flush stream;Unix.fsync (Unix.descr_of_out_channel stream));
 Unix.chmod temp ((Unix.stat temp).Unix.st_perm lor 0o100);
 let command,args=match Platform.detectArch () with Ok Platform.X86_64->temp,args|_->"/opt/dcb/qemu/qemu-x86_64",temp::args in
 TestProcess.capture command args 10000
 with ex->Error ("Execution failed: "^HostFile.errorMessage temp ex) in
 (try Sys.remove temp with Sys_error _->());outcome
let runLIRProgramFullWithOptions program leak=runLIRProgramFullWithOptionsAndArgs program leak []
let runLIRProgram program=Result.bind (binaryFor program false true) X86_64BinaryTests.runElfBinary
let generatedCallLabels program=Result.map (List.filter_map (function X.CALL label->Some label|_->None)) (Result.map_error (fun error->"Codegen error: "^error) (translate program false))
let assertCallsHelper prefix context program=
 let* labels=generatedCallLabels program in
 require (List.exists (fun label->HostText.startsWith label prefix) labels) (context^" did not call a planned "^(if prefix="__dark_list_rc_dec_plan_" then "list" else "dict")^" helper; calls were "^HostStructuralFormat.format (HostStructuralFormat.Sequence (List.map (fun s->HostStructuralFormat.Text s) labels)))
let assertCallsPlannedListHelper=assertCallsHelper "__dark_list_rc_dec_plan_"
let assertCallsPlannedDictHelper=assertCallsHelper "__dark_dict_rc_dec_plan_"
let rcMetadata typ:MemoryModel.rcMetadata={MemoryModel.releasePlanCacheKey=None;releasePlan=None;sourceType=Some typ}
let rcMetadataWithSumShapes sums typ:MemoryModel.rcMetadata=
 let plan=MemoryPlanning.rcReleasePlanOfTypeWithSums M.empty sums typ in {MemoryModel.releasePlanCacheKey=ReleasePlanFingerprint.rcReleasePlanCacheKey typ plan;releasePlan=Some plan;sourceType=Some typ}
let completeRcMetadata records=function
 |Some ({MemoryModel.releasePlan=None;sourceType=Some typ;_} as value)->let plan=MemoryPlanning.rcReleasePlanOfType records typ in Some {value with MemoryModel.releasePlanCacheKey=ReleasePlanFingerprint.rcReleasePlanCacheKey typ plan;releasePlan=Some plan}
 |metadata->metadata
let completeRcInstrMetadata records=function RefCountInc (addr,size,kind,meta)->RefCountInc (addr,size,kind,completeRcMetadata records meta)|RefCountDec (addr,size,kind,meta)->RefCountDec (addr,size,kind,completeRcMetadata records meta)|instr->instr
let block label instrs terminator:basicBlock={LIR.label;instrs;terminator}
let functionWith name typedParams entry blocks:functionDef={LIR.id=TestIds.functionIdForName name;name;typedParams;cfg={LIR.entry;blocks=LabelMap.of_list blocks};stackSize=0;usedCalleeSaved=[];codegenFacts=None}
let makeSimpleProgramWithRecords instrs term records=
 let entry=Label "_start_entry" and body=Label "_start_body" in
 Program ([functionWith "_start" [] entry [entry,block entry [] (Jump body);body,block body (List.map (completeRcInstrMetadata records) instrs) term]],M.empty,records)
let makeSimpleProgram instrs term=makeSimpleProgramWithRecords instrs term M.empty
let runInNamedFunction name instrs term=match makeSimpleProgram [Call (Physical X0,TestIds.functionIdForName name,[])] Ret with
 |Program ([entry],variants,records)->let label=Label (name^"_entry") in let callee=functionWith name [] label [label,block label (List.map (completeRcInstrMetadata records) instrs) term] in Program ([entry;callee],variants,records)
 |_->Crash.crash "Test fixture expected a single entry function"
let makeEmptyFunction name typedParams=let label=Label (name^"_entry") in functionWith name typedParams label [label,block label [] Ret]
let xInstructions instructions=HostStructuralFormat.format (HostStructuralFormat.Sequence (List.map MachineDiagnostic.x64Instr instructions))

let testStringLiteralUsesStaticStorage ()=
 let program=makeSimpleProgram [ Mov (Physical X1, StringSymbol "pooled") ] Ret in
 let* instrs=CodeGen_X86_64.translateProgram program false in
 require (List.exists (function X.LEA_rip (_,label) when HostText.startsWith label "__dark_string_literal_"->true|_->false) instrs) "Expected x86 string literal to be loaded from static storage"
let testStringLiteralHeapStorePreservesX3 ()=
 let program=makeSimpleProgram [ Mov (Physical X3, Imm 123L); HeapAlloc (Physical X4, 8); HeapStore (Physical X4, 0, StringSymbol "field", Some AST.TString); Mov (Physical X0, Reg (Physical X3)); PrintInt64 (Physical X0) ] Ret in
 let* _,stdout,stderr=runLIRProgramFullWithOptions program false in let output=HostText.trim stdout in
 require (output="123" && stderr="") (Printf.sprintf "Expected string field initialization to preserve X3=123, got stdout '%s' and stderr '%s'" output stderr)
let testRawSlotInitRetainsX12Value ()=
 let tupleType=AST.TTuple [AST.TString] in
 let program=makeSimpleProgram [ HeapAlloc (Physical X3, 8); HeapStore (Physical X3, 0, StringSymbol "owned", Some AST.TString); Mov (Physical X19, Imm 8L); RawAlloc (Physical X20, Physical X19); Mov (Physical X19, Imm 0L); Mov (Physical X12, Reg (Physical X3)); RawSlotInit (Physical X20, Physical X19, Physical X12, tupleType); RefCountDec (Physical X3, 8, GenericHeap, Some (rcMetadata tupleType)); HeapLoad (Physical X1, Physical X20, 0); HeapLoad (Physical X1, Physical X1, 0); PrintHeapStringNoNewline (Physical X1) ] Ret in
 let* exitCode,stdout,stderr=runLIRProgramFullWithOptions program false in
 let* ()=require (exitCode=0) (Printf.sprintf "Expected exit code 0, got %d: %s" exitCode stderr) in
 require (stdout="owned") (Printf.sprintf "Expected X12 RawSlotInit value to remain owned, got stdout '%s' and stderr '%s'" stdout stderr)
let testStringConcatLoadsStackSlotOperand ()=
 let program=makeSimpleProgram [ StringConcat (Physical X1, StringSymbol "1", StringSymbol "", []); Store (-8, Physical X1); StringConcat (Physical X2, StringSymbol "2", StringSymbol "", []); Store (-16, Physical X2); Mov (Physical X12, StackSlot (-8)); StringConcat (Physical X11, Reg (Physical X12), StackSlot (-16), []); Mov (Physical X0, Reg (Physical X11)); PrintHeapStringNoNewline (Physical X0) ] Ret in
 let program=match program with Program ([func],variants,records)->Program ([{func with LIR.stackSize=16}],variants,records)|_->Crash.crash "StringConcat stack-slot fixture expected one function" in
 let* exitCode,stdout,stderr=runLIRProgramFullWithOptions program false in
 let* ()=require (exitCode=0) (Printf.sprintf "Expected exit code 0, got %d: %s" exitCode stderr) in
 require (stdout="12") (Printf.sprintf "Expected x64 stack-slot concatenation to print '12', got stdout '%s' and stderr '%s'" stdout stderr)
let testCliArgvHelperResolvesAsCodeLabel ()=
 let program=makeSimpleProgram [CliNative (Physical X0, GetArgv, [Imm 0L])] Ret in
 let* instrs=Result.map_error (fun e->"CLI argv x64 lowering failed: "^e) (translate program false) in
 let* resolved=Result.map_error (fun e->"CLI argv x64 resolution failed: "^e) (X86_64_Resolve.resolveAndEncode instrs) in
 require (not (List.exists (fun fixup->fixup.X86_64_Resolve.targetLabel="__dark_cli_argv") resolved.X86_64_Resolve.deferredFixups)) "CLI argv helper call was deferred as an ELF data fixup"
let testCliArgvReturnsNullableString ()=
 let program=makeSimpleProgram [ CliNative (Physical X1, GetArgv, [Imm 0L]); PrintHeapStringNoNewline (Physical X1) ] Ret in
 let* exitCode,stdout,stderr=runLIRProgramFullWithOptionsAndArgs program false ["hello"] in
 let* ()=require (exitCode=0) (Printf.sprintf "Expected exit code 0, got %d: %s" exitCode stderr) in
 require (stdout="hello") (Printf.sprintf "Expected nullable Some(hello), got '%s'" stdout)
let testCliHostOperationsExecute ()=
 let program=makeSimpleProgram [ CliNative (Physical X0, CpuCount, []); PrintInt64 (Physical X0); CliNative (Physical X0, Hostname, []); HeapLoad (Physical X1, Physical X0, 8); PrintHeapString (Physical X1); CliNative (Physical X0, GetEnv, [StringSymbol "PATH"]); PrintHeapString (Physical X0); CliNative (Physical X0, GetPid, []); CliNative (Physical X0, Kill, [Reg (Physical X0); Imm 99999L]); HeapLoad (Physical X1, Physical X0, 8); HeapLoad (Physical X2, Physical X1, 0); PrintInt64 (Physical X2) ] Ret in
 let* exitCode,stdout,stderr=runLIRProgramFullWithOptions program false in
 let error prefix=Printf.sprintf "%s x64 CLI host output: exit=%d, stdout='%s', stderr='%s'" prefix exitCode stdout stderr in
 match String.split_on_char '\n' stdout with
 |cpu::hostname::path::errno::_->(match Int64.of_string_opt (HostText.trim cpu),Int64.of_string_opt (HostText.trim errno) with Some count,Some 22L when exitCode=0 && count>0L && hostname<>"" && path<>"" && stderr=""->Ok ()|_->Error (error "Unexpected"))
 |_->Error (error "Incomplete")
let testCliNativePreservesLiveCallerRegister ()=
 let program=makeSimpleProgram [ Mov (Physical X3, Imm 42L); SaveRegs ([X3], []); CliNative (Physical X0, CpuCount, []); RestoreRegs ([X3], []); Mov (Physical X0, Reg (Physical X3)); PrintInt64 (Physical X0) ] Ret in
 let* exitCode,stdout,stderr=runLIRProgramFullWithOptions program false in
 require (exitCode=0 && stdout="42\n" && stderr="") (Printf.sprintf "CLI native call corrupted a live x64 caller register: exit=%d, stdout='%s', stderr='%s'" exitCode stdout stderr)
let testDateTimeNowLowersTo100nsUnixTicks ()=
 let program=makeSimpleProgram [DateTimeNow (Physical X0)] Ret in
 let* instrs=Result.map_error (fun e->"DateTimeNow x64 lowering failed: "^e) (translate program false) in
 let hasTicks=List.exists (function X.IMUL_imm (_,_,10000000l)->true|_->false) instrs in
 let hasNanoseconds=List.mem (X.MOV_imm32 (X.RDI,100l)) instrs in
 require (hasTicks && hasNanoseconds && List.mem (X.IDIV X.RDI) instrs) ("DateTimeNow did not lower to 100ns Unix ticks: "^xInstructions instrs)
let testSleepLowersToNormalizedInterruptSafeNanosleep ()=
 let program=makeSimpleProgram [Sleep (41, FPhysical D0)] Ret in
 let* instrs=Result.map_error (fun e->"Sleep x64 lowering failed: "^e) (translate program false) in
 let normalization=List.mem (X.MULSD (X.XMM1,X.XMM0)) instrs && List.mem (X.CVTTSD2SI (X.R11,X.XMM1)) instrs && List.mem (X.IDIV X.R10) instrs in
 let rec syscall=function
 |X.MOV_imm32 (X.RAX,n)::X.SYSCALL::_ when n=Int32.of_int Platform.linuxX86_64SyscallNumbers.Platform.nanosleep->true
 |X.MOV_imm (X.RAX,n)::X.SYSCALL::_ when n=Int64.of_int Platform.linuxX86_64SyscallNumbers.Platform.nanosleep->true
 |_::rest->syscall rest|[]->false in
 let retry=List.mem (X.CMP_imm (X.RAX,-4l)) instrs && List.mem (X.MOV_load (X.R10,X.RSP,16l)) instrs && List.mem (X.MOV_load (X.R10,X.RSP,24l)) instrs in
 require (normalization && syscall instrs && retry) ("Sleep did not lower to normalized interrupt-safe x64 nanosleep: "^xInstructions instrs)
let testFloatArgumentMovesResolveCycles ()=
 let program=makeSimpleProgram [ FLoad (FPhysical D1, 1.0); FLoad (FPhysical D2, 2.0); FArgMoves [ (D1, FPhysical D2); (D2, FPhysical D1) ]; FloatToInt64 (Physical X1, FPhysical D1); FloatToInt64 (Physical X2, FPhysical D2); Mov (Physical X3, Imm 10L); Mul (Physical X0, Physical X1, Physical X3); Add (Physical X0, Physical X0, Reg (Physical X2)); PrintInt64 (Physical X0) ] Ret in
 let* _,stdout,stderr=runLIRProgramFullWithOptions program false in let output=HostText.trim stdout in
 require (output="21" && stderr="") (Printf.sprintf "Expected swapped float arguments to print 21, got stdout '%s' and stderr '%s'" output stderr)
let testHighFloatRegistersExecute ()=
 let program=makeSimpleProgram [ FLoad (FPhysical D0, 0.01); FMov (FPhysical D15, FPhysical D0); FLoad (FPhysical D2, 2000.0); FMul (FPhysical D15, FPhysical D15, FPhysical D2); FloatToInt64 (Physical X0, FPhysical D15); PrintInt64 (Physical X0) ] Ret in
 let* _,stdout,stderr=runLIRProgramFullWithOptions program false in let output=HostText.trim stdout in
 require (output="20" && stderr="") (Printf.sprintf "Expected high x64 float registers to print 20, got stdout '%s' and stderr '%s'" output stderr)
let testNonCommutativeFloatAliasesPreserveScratch ()=
 let program=makeSimpleProgram [ FLoad (FPhysical D0, 7.0); FLoad (FPhysical D1, 10.0); FLoad (FPhysical D2, 2.0); FSub (FPhysical D2, FPhysical D1, FPhysical D2); FloatToInt64 (Physical X1, FPhysical D2); FLoad (FPhysical D2, 2.0); FDiv (FPhysical D2, FPhysical D1, FPhysical D2); FloatToInt64 (Physical X2, FPhysical D2); FloatToInt64 (Physical X3, FPhysical D0); Mov (Physical X4, Imm 100L); Mul (Physical X1, Physical X1, Physical X4); Mov (Physical X4, Imm 10L); Mul (Physical X2, Physical X2, Physical X4); Add (Physical X0, Physical X1, Reg (Physical X2)); Add (Physical X0, Physical X0, Reg (Physical X3)); PrintInt64 (Physical X0) ] Ret in
 let* _,stdout,stderr=runLIRProgramFullWithOptions program false in let output=HostText.trim stdout in
 require (output="857" && stderr="") (Printf.sprintf "Expected aliased x64 float operations to print 857, got stdout '%s' and stderr '%s'" output stderr)

let testBranchFalseEdgeFallsThrough ()=
 let entry=Label "x64_layout_entry" and yes=Label "x64_layout_true" and no=Label "x64_layout_false" in
 let func=functionWith "x64_layout" [] entry [entry,block entry [] (Branch (Physical X0,yes,no));yes,block yes [] Ret;no,block no [] Ret] in
 let* instrs=CodeGen_X86_64.translateProgram (Program ([func],M.empty,M.empty)) false in
 let epilogueJumps=List.filter ((=) (X.JMP "_epilogue_x64_layout")) instrs |> List.length in
 let* ()=require (not (List.mem (X.JMP "x64_layout_false") instrs)) "x64 emitted a jump to the immediately following false block" in
 require (epilogueJumps=1) (Printf.sprintf "x64 emitted %d jumps to the epilogue; expected one before the final fallthrough" epilogueJumps)
let testReportsMissingEntryBlock ()=
 let entry=Label "_start_entry" and body=Label "_start_body" in
 let func=functionWith "_start" [] entry [body,block body [] Ret] in
 match translate (Program ([func],M.empty,M.empty)) false with
 |Error e when HostText.contains e "missing entry block"->Ok ()
 |Error e->Error (Printf.sprintf "Expected missing entry block error, got '%s'" e)
 |Ok _->Error "Expected x64 codegen to reject a CFG whose entry block is absent"
let testRejectsConditionsWithoutBlockComparison ()=
 let translate instrs term=translate (makeSimpleProgram instrs term) false in
 let* _=Result.map_error (fun e->Printf.sprintf "Expected float comparison primer to translate, got '%s'" e) (translate [FCmp (FPhysical D0,FPhysical D1)] Ret) in
 let target=Label "_start_target" in
 let rec runCases=function
 |[]->Ok ()
 |(name,instrs,term)::rest->match translate instrs term with
  |Error e when HostText.contains e "without a preceding comparison in the same block"->runCases rest
  |Error e->Error (Printf.sprintf "Expected %s comparison-context error, got '%s'" name e)
  |Ok _->Error (Printf.sprintf "Expected x64 codegen to reject %s without a block-local comparison" name) in
 runCases ["Cset",[Cset (Physical X0,EQ)],Ret;"CondBranch",[],CondBranch (EQ,target,target)]
let testBranch ()=
 let entry=Label "_start_entry" and test=Label "_start_test" and yes=Label "_start_true" and no=Label "_start_false" in
 let func=functionWith "_start" [] entry [entry,block entry [] (Jump test);test,block test [Mov (Physical X2,Imm 10L);Cmp (Physical X2,Imm 5L)] (CondBranch (GT,yes,no));yes,block yes [Mov (Physical X1,Imm 42L);LIR.Exit] Ret;no,block no [Mov (Physical X1,Imm 0L);LIR.Exit] Ret] in
 let* exitCode=runLIRProgram (Program ([func],M.empty,M.empty)) in require (exitCode=42) (Printf.sprintf "Expected exit code 42, got %d" exitCode)
let variant name tag payload fieldCount:variantInfo={LIR.name;tag;payload;fieldCount}
let variantRegistry name variants=M.singleton name {LIR.typeParams=[];variants}
let sumShapes variants=M.map (fun (types:typeVariants)->{MemoryModel.typeParams=types.LIR.typeParams;unaryPayloadTags=List.filter_map (fun (v:variantInfo)->if v.LIR.fieldCount=1 then Some v.LIR.tag else None) types.LIR.variants |> MemoryModel.IntSet.of_list;payloads=List.sort (fun (a:variantInfo) (b:variantInfo)->Int.compare a.LIR.tag b.LIR.tag) types.LIR.variants |> List.map (fun (v:variantInfo)->v.LIR.tag,v.LIR.payload)}) variants
let withVariants variants (Program (functions,_,records))=Program (functions,variants,records)
let testGenericRefCountDecMixedSumPayloadUsesVariantDispatch ()=
 let name="X64MixedSumPayloadDispatch" in let typ=AST.TSum (name,[]) in
 let variants=variantRegistry name [variant "X64MixedSumBytesPayload" 0 (Some AST.TBlob) 1;variant "X64MixedSumListPayload" 1 (Some (AST.TList AST.TInt64)) 1] in
 let program=makeSimpleProgram [RefCountDec (Physical X3,16,GenericHeap,Some (rcMetadataWithSumShapes (sumShapes variants) typ))] Ret |> withVariants variants in
 let* instrs=translate program false in
 let rec branchAppearsBeforeSecondCase seen=function []->false|X.CMP_imm (_,0l)::rest->branchAppearsBeforeSecondCase true rest|X.CMP_imm (_,1l)::_ when seen->false|X.JMP _::_ when seen->true|_::rest->branchAppearsBeforeSecondCase seen rest in
 require (branchAppearsBeforeSecondCase false instrs) "x64 generic mixed boxed-sum payload release did not branch past remaining variant cases after a match"
let testGenericRefCountDecNestedMixedSumPayloadUsesVariantDispatch ()=
 let name="X64NestedMixedSumPayloadDispatch" in let typ=AST.TSum (name,[]) in let parentType=AST.TTuple [typ] in
 let variants=variantRegistry name [variant "X64NestedMixedSumNoPayload" 0 None 0;variant "X64NestedMixedSumListPayload" 1 (Some (AST.TList AST.TBlob)) 1] in
 let program=makeSimpleProgram [RefCountDec (Physical X3,8,GenericHeap,Some (rcMetadataWithSumShapes (sumShapes variants) parentType))] Ret |> withVariants variants in
 let* instrs=translate program false in require (List.exists (function X.MOV_load (_,X.RDX,0l)->true|_->false) instrs) "x64 generic fixed-block nested mixed boxed-sum payload release did not dispatch on the child variant tag"
let testDictRefCountDecDictListValueUsesPlannedHelper ()=
 let typ=AST.TDict (AST.TInt64,AST.TDict (AST.TInt64,AST.TList AST.TInt64)) in
 let program=makeSimpleProgram [RefCountDec (Physical X0,0,DictHeap,Some (rcMetadata typ))] Ret in
 let* labels=generatedCallLabels program in
 let display=HostStructuralFormat.format (HostStructuralFormat.Sequence (List.map (fun s->HostStructuralFormat.Text s) labels)) in
 let* ()=require (List.exists (fun label->HostText.startsWith label "__dark_dict_rc_dec_plan_") labels) ("Nested dict/list RefCountDec did not call a planned dict helper; calls were "^display) in
 require (not (List.mem "__dark_dict_rc_dec_dict_list_value_helper" labels)) ("Nested dict/list RefCountDec still called the dict-list matrix helper; calls were "^display)
let plannedTuple typ context=makeSimpleProgram [RefCountDec (Physical X0,0,TaggedList,Some (rcMetadata (AST.TList typ)))] Ret |> assertCallsPlannedListHelper context
let testTaggedListTuplePayloadUsesPlannedHelper ()=plannedTuple (AST.TTuple [AST.TString;AST.TList AST.TInt64;AST.TDict (AST.TInt64,AST.TInt64)]) "Tuple list payload"
let testTaggedListTuple5PayloadUsesPlannedHelper ()=plannedTuple (AST.TTuple [AST.TString;AST.TBlob;AST.TList AST.TInt64;AST.TDict (AST.TInt64,AST.TList AST.TInt64);AST.TFunction ([AST.TInt64],AST.TInt64)]) "List tuple5 payload"
let plannedRecord name fields context=
 let typ=AST.TRecord (name,[]) in let records=M.singleton name fields in
 makeSimpleProgramWithRecords [RefCountDec (Physical X0,0,TaggedList,Some (rcMetadata (AST.TList typ)))] Ret records |> assertCallsPlannedListHelper context
let testTaggedListRecordPayloadUsesPlannedHelper ()=plannedRecord "X64PlannedListRecordPayload" ["name",AST.TString;"items",AST.TList AST.TInt64] "Record list payload"
let testTaggedListRecord5PayloadUsesPlannedHelper ()=plannedRecord "X64PlannedListRecord5Payload" ["name",AST.TString;"blob",AST.TBlob;"items",AST.TList AST.TInt64;"lookup",AST.TDict (AST.TInt64,AST.TList AST.TInt64);"fn",AST.TFunction ([AST.TInt64],AST.TInt64)] "List record5 payload"
let testDictRefCountDecStringKeyTupleValueUsesPlannedHelper ()=
 let typ=AST.TDict (AST.TString,AST.TTuple [AST.TString;AST.TList AST.TInt64]) in
 makeSimpleProgram [RefCountDec (Physical X0,0,DictHeap,Some (rcMetadata typ))] Ret |> assertCallsPlannedDictHelper "Dict string key tuple value"

let testDictRefCountDecStringCollisionKeysAndValues ()=
 let dictType=AST.TDict (AST.TString,AST.TString) in
 let program=makeSimpleProgram [ StringConcat (Physical X2, StringSymbol "key", StringSymbol "1", []); StringConcat (Physical X3, StringSymbol "value", StringSymbol "1", []); StringConcat (Physical X4, StringSymbol "key", StringSymbol "2", []); StringConcat (Physical X5, StringSymbol "value", StringSymbol "2", []); HeapAlloc (Physical X6, 40); HeapStore (Physical X6, 0, Imm 2L, None); HeapStore (Physical X6, 8, Reg (Physical X2), Some AST.TString); HeapStore (Physical X6, 16, Reg (Physical X3), Some AST.TString); HeapStore (Physical X6, 24, Reg (Physical X4), Some AST.TString); HeapStore (Physical X6, 32, Reg (Physical X5), Some AST.TString); Mov (Physical X7, Imm 3L); Orr (Physical X7, Physical X6, Physical X7); RefCountDec (Physical X7, 0, DictHeap, Some (rcMetadata dictType)) ] Ret in
 let* _,_,stderr=runLIRProgramFullWithOptions program true in let leaks=HostText.trim stderr in
 require (leaks="") (Printf.sprintf "Expected dict collision string keys and values to be released, got stderr '%s'" leaks)
let testDictRefCountDecStringCollisionKeysAndTupleListValues ()=
 let listType=AST.TList AST.TInt64 in let tupleType=AST.TTuple [AST.TString;listType] in let dictType=AST.TDict (AST.TString,tupleType) in
 let program=makeSimpleProgram [ StringConcat (Physical X2, StringSymbol "key", StringSymbol "1", []); StringConcat (Physical X3, StringSymbol "value", StringSymbol "1", []); HeapAlloc (Physical X4, 8); HeapStore (Physical X4, 0, Imm 42L, None); Mov (Physical X5, Imm 2L); Orr (Physical X5, Physical X4, Physical X5); HeapAlloc (Physical X6, 16); HeapStore (Physical X6, 0, Reg (Physical X3), Some AST.TString); HeapStore (Physical X6, 8, Reg (Physical X5), Some listType); Mov (Physical X19, Reg (Physical X6)); Mov (Physical X20, Reg (Physical X2)); StringConcat (Physical X2, StringSymbol "key", StringSymbol "2", []); StringConcat (Physical X3, StringSymbol "value", StringSymbol "2", []); HeapAlloc (Physical X4, 8); HeapStore (Physical X4, 0, Imm 99L, None); Mov (Physical X5, Imm 2L); Orr (Physical X5, Physical X4, Physical X5); HeapAlloc (Physical X6, 16); HeapStore (Physical X6, 0, Reg (Physical X3), Some AST.TString); HeapStore (Physical X6, 8, Reg (Physical X5), Some listType); HeapAlloc (Physical X7, 40); HeapStore (Physical X7, 0, Imm 2L, None); HeapStore (Physical X7, 8, Reg (Physical X20), Some AST.TString); HeapStore (Physical X7, 16, Reg (Physical X19), Some tupleType); HeapStore (Physical X7, 24, Reg (Physical X2), Some AST.TString); HeapStore (Physical X7, 32, Reg (Physical X6), Some tupleType); Mov (Physical X21, Imm 3L); Orr (Physical X21, Physical X7, Physical X21); RefCountDec (Physical X21, 0, DictHeap, Some (rcMetadata dictType)) ] Ret in
 let* _,_,stderr=runLIRProgramFullWithOptions program true in let leaks=HostText.trim stderr in
 require (leaks="") (Printf.sprintf "Expected dict collision string keys and tuple/list values to be released, got stderr '%s'" leaks)
let testTaggedListRefCountDecClosurePayloadInStdlibFunction ()=
 let closureType=AST.TFunction ([AST.TInt64],AST.TInt64) in
 let program=runInNamedFunction "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_acc_fn_i64" [ ClosureAlloc (Physical X2, TestIds.functionIdForName "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_acc_fn_i64", []); HeapAlloc (Physical X3, 8); HeapStore (Physical X3, 0, Reg (Physical X2), Some closureType); Mov (Physical X4, Imm 2L); Orr (Physical X4, Physical X3, Physical X4); RefCountDec (Physical X4, 0, TaggedList, Some (rcMetadata ((AST.TList closureType)))) ] Ret in
 let* exitCode,_,stderr=runLIRProgramFullWithOptions program true in let leaks=HostText.trim stderr in
 let* ()=require (exitCode=0) (Printf.sprintf "Expected stdlib list closure payload release to exit 0, got %d, stderr '%s'" exitCode leaks) in
 require (leaks="") (Printf.sprintf "Expected stdlib list closure payload release to balance leak counter, got stderr '%s'" leaks)
let testClosureRefCountDecPreservesLiveArgumentClosures ()=
 let closureType=AST.TFunction ([AST.TInt64],AST.TInt64) in let tupleType=AST.TTuple [AST.TInt64;AST.TInt64] in
 let capturedFunction name=makeEmptyFunction name [{LIR.reg=Physical X0;typ=tupleType}] in
 let first=capturedFunction "x64_preserved_closure_first" and second=capturedFunction "x64_preserved_closure_second" in
 let main=makeSimpleProgram [ClosureAlloc (Physical X5,first.LIR.id,[Imm 11L]);ClosureAlloc (Physical X7,second.LIR.id,[Imm 22L]);RefCountDec (Physical X7,16,ClosureHeap,Some (rcMetadata closureType));RefCountDec (Physical X5,16,ClosureHeap,Some (rcMetadata closureType))] Ret in
 let main=match main with Program ([func],variants,records)->Program ([func;first;second],variants,records)|other->other in
 let* exitCode,_,stderr=runLIRProgramFullWithOptions main true in let leaks=HostText.trim stderr in
 require (exitCode=0 && leaks="") (Printf.sprintf "Expected both live closures to release cleanly, got exit %d and stderr '%s'" exitCode leaks)
let testClosureRefCountDecMixedSumCaptureUsesVariantDispatch ()=
 let name="X64ClosureMixedSumCaptureDispatch" in let sumType=AST.TSum (name,[]) in let tupleType=AST.TTuple [AST.TInt64;sumType] in
 let variants=variantRegistry name [variant "X64ClosureMixedSumNoPayload" 0 None 0;variant "X64ClosureMixedSumBytesPayload" 1 (Some AST.TBlob) 1;variant "X64ClosureMixedSumIntegerPayload" 2 (Some AST.TInt64) 1] in
 let captured=makeEmptyFunction "x64_mixed_sum_capture_fn" [{LIR.reg=Physical X0;typ=tupleType}] in
 let main=makeSimpleProgram [ClosureAlloc (Physical X4,TestIds.functionIdForName "x64_mixed_sum_capture_fn",[Reg (Physical X3)]);RefCountDec (Physical X4,16,ClosureHeap,Some (rcMetadata (AST.TFunction ([AST.TInt64],AST.TInt64))))] Ret in
 let main=match main with Program ([func],_,records)->Program ([func;captured],variants,records)|other->other in
 let* instrs=translate main false in require (List.mem (X.MOV_load (X.R10,X.RDX,0l)) instrs) "x64 closure mixed boxed-sum capture release did not dispatch on the captured sum variant tag"
let testTaggedListRefCountDecMixedSumDynamicPayloadUsesVariantDispatch ()=
 let name="X64ListMixedSumDynamicDispatch" in let sumType=AST.TSum (name,[]) in
 let variants=variantRegistry name [variant "X64ListMixedSumNoPayload" 0 None 0;variant "X64ListMixedSumBytesPayload" 1 (Some AST.TBlob) 1;variant "X64ListMixedSumIntegerPayload" 2 (Some AST.TInt64) 1] in
 let program=makeSimpleProgram [RefCountDec (Physical X5,0,TaggedList,Some (rcMetadataWithSumShapes (sumShapes variants) (AST.TList sumType)))] Ret |> withVariants variants in
 let* instrs=translate program false in
 let rec seesTagCheckBeforeDynamicRelease saw=function []->false|X.MOV_load (X.R10,X.RDX,0l)::rest->seesTagCheckBeforeDynamicRelease true rest|X.CMP_imm (X.R10,1l)::_ when saw->true|_::rest->seesTagCheckBeforeDynamicRelease saw rest in
 require (seesTagCheckBeforeDynamicRelease false instrs) "x64 tagged-list mixed boxed-sum dynamic payload release did not check the active variant tag"

type threeFieldFixture=Tuple3|Record3|SumTuple3|SumRecord3
let dynamicField=function AST.TString|AST.TBlob->true|_->false
let dynamicReg context=function 0->X2|1->X3|2->X4|index->Crash.crash (Printf.sprintf "Unexpected %s field index %d" context index)
let rec runCases run=function []->Ok ()|case::rest->let* ()=run case in runCases run rest
let runThreeFieldCase fixture (name,fields)=
 let isRecord=match fixture with Record3|SumRecord3->true|Tuple3|SumTuple3->false in
 let isSum=match fixture with SumTuple3|SumRecord3->true|Tuple3|Record3->false in
 let context=match fixture with Tuple3->"tuple3"|Record3->"record3"|SumTuple3->"sum tuple3"|SumRecord3->"sum record3" in
 let recordName=(if fixture=SumRecord3 then "X64ListRcSumRecord3" else "X64ListRcRecord3")^name in
 let payloadType=if isRecord then AST.TRecord (recordName,[]) else AST.TTuple fields in
 let records=if isRecord then M.singleton recordName (List.mapi (fun i typ->"field"^string_of_int i,typ) fields) else M.empty in
 let sumName=if fixture=SumRecord3 then recordName^"Wrapper" else "X64ListRcSumTuple3"^name in
 let valueType=if isSum then AST.TSum (sumName,[payloadType]) else payloadType in
 let allocs=List.mapi (fun i typ->if dynamicField typ then Some (StringConcat (Physical (dynamicReg context i),StringSymbol ("left"^name^string_of_int i),StringSymbol ("right"^name^string_of_int i),[])) else None) fields |> List.filter_map Fun.id in
 let stores=List.mapi (fun i typ->if dynamicField typ then HeapStore (Physical X5,i*8,Reg (Physical (dynamicReg context i)),Some typ) else HeapStore (Physical X5,i*8,Imm (Int64.of_int (i+1)),None)) fields in
 let suffix=if isSum then [HeapAlloc (Physical X6,16);HeapStore (Physical X6,0,Imm 0L,None);HeapStore (Physical X6,8,Reg (Physical X5),Some payloadType);HeapAlloc (Physical X7,8);HeapStore (Physical X7,0,Reg (Physical X6),Some valueType);Mov (Physical X8,Imm 2L);Orr (Physical X8,Physical X7,Physical X8);RefCountDec (Physical X8,0,TaggedList,Some (rcMetadata (AST.TList valueType)))] else [HeapAlloc (Physical X6,8);HeapStore (Physical X6,0,Reg (Physical X5),Some payloadType);Mov (Physical X7,Imm 2L);Orr (Physical X7,Physical X6,Physical X7);RefCountDec (Physical X7,0,TaggedList,Some (rcMetadata (AST.TList payloadType)))] in
 let program=makeSimpleProgramWithRecords (allocs @ [HeapAlloc (Physical X5,24)] @ stores @ suffix) Ret records in
 let* _,_,stderr=runLIRProgramFullWithOptions program true in let leaks=HostText.trim stderr in
 let dynamic=if fixture=SumTuple3 then "" else "dynamic " in
 require (leaks="") (Printf.sprintf "Expected list %s %s %spayload release to balance leak counter, got stderr '%s'" context name dynamic leaks)
let runTuple2Case sum (name,nestedType,setup,stores)=
 let valueType=if sum then AST.TSum ("X64ListRcSumTuple"^name,[nestedType]) else AST.TTuple [AST.TInt64;nestedType] in
 let first=HeapStore (Physical X5,0,Imm (if sum then 0L else 42L),None) in
 let suffix=[HeapAlloc (Physical X5,16);first;HeapStore (Physical X5,8,Reg (Physical X4),Some nestedType);HeapAlloc (Physical X6,8);HeapStore (Physical X6,0,Reg (Physical X5),Some valueType);Mov (Physical X7,Imm 2L);Orr (Physical X7,Physical X6,Physical X7);RefCountDec (Physical X7,0,TaggedList,Some (rcMetadata (AST.TList valueType)))] in
 let program=makeSimpleProgram (setup @ [HeapAlloc (Physical X4,16)] @ stores @ suffix) Ret in
 let* _,_,stderr=runLIRProgramFullWithOptions program true in let leaks=HostText.trim stderr in
 let context=if sum then "sum tuple2" else "tuple2 nested tuple" in let dynamic=if sum then "" else "dynamic " in
 require (leaks="") (Printf.sprintf "Expected list %s %s %spayload release to balance leak counter, got stderr '%s'" context name dynamic leaks)
let testTaggedListRefCountDecTuple3DynamicPayloadCombinations ()=runCases (runThreeFieldCase Tuple3) [ ("first", [AST.TString; AST.TInt64; AST.TInt64]); ("third", [AST.TInt64; AST.TInt64; AST.TString]); ("first-second", [AST.TString; AST.TBlob; AST.TInt64]); ("second-third", [AST.TInt64; AST.TString; AST.TBlob]); ("all", [AST.TString; AST.TBlob; AST.TString]) ]
let testTaggedListRefCountDecRecord3DynamicPayloadCombinations ()=runCases (runThreeFieldCase Record3) [ ("First", [AST.TString; AST.TInt64; AST.TInt64]); ("Third", [AST.TInt64; AST.TInt64; AST.TString]); ("FirstSecond", [AST.TString; AST.TBlob; AST.TInt64]); ("SecondThird", [AST.TInt64; AST.TString; AST.TBlob]); ("All", [AST.TString; AST.TBlob; AST.TString]) ]
let testTaggedListRefCountDecSumTuple3DynamicPayloadCombinations ()=runCases (runThreeFieldCase SumTuple3) [ ("First", [AST.TString; AST.TInt64; AST.TInt64]); ("Second", [AST.TInt64; AST.TString; AST.TInt64]); ("Third", [AST.TInt64; AST.TInt64; AST.TString]); ("FirstSecond", [AST.TString; AST.TBlob; AST.TInt64]); ("SecondThird", [AST.TInt64; AST.TString; AST.TBlob]); ("FirstThird", [AST.TString; AST.TInt64; AST.TBlob]); ("All", [AST.TString; AST.TBlob; AST.TString]) ]
let testTaggedListRefCountDecSumRecord3DynamicPayloadCombinations ()=runCases (runThreeFieldCase SumRecord3) [ ("First", [AST.TString; AST.TInt64; AST.TInt64]); ("Third", [AST.TInt64; AST.TInt64; AST.TString]); ("FirstSecond", [AST.TString; AST.TBlob; AST.TInt64]); ("SecondThird", [AST.TInt64; AST.TString; AST.TBlob]); ("FirstThird", [AST.TString; AST.TInt64; AST.TBlob]); ("All", [AST.TString; AST.TBlob; AST.TString]) ]
let testTaggedListRefCountDecTuple2NestedTupleDynamicPayloadCombinations ()=runCases (runTuple2Case false) [ ("Second", AST.TTuple [AST.TInt64; AST.TString], [StringConcat (Physical X2, StringSymbol "left", StringSymbol "right", [])], [HeapStore (Physical X4, 0, Imm 7L, None); HeapStore (Physical X4, 8, Reg (Physical X2), Some AST.TString)]); ("Both", AST.TTuple [AST.TString; AST.TBlob], [StringConcat (Physical X2, StringSymbol "left", StringSymbol "right", []); StringConcat (Physical X3, StringSymbol "bytes", StringSymbol "payload", [])], [HeapStore (Physical X4, 0, Reg (Physical X2), Some AST.TString); HeapStore (Physical X4, 8, Reg (Physical X3), Some AST.TBlob)]) ]
let testTaggedListRefCountDecSumTuple2DynamicPayloadCombinations ()=runCases (runTuple2Case true) [ ("second", AST.TTuple [AST.TInt64; AST.TString], [StringConcat (Physical X3, StringSymbol "leftSecond", StringSymbol "rightSecond", [])], [HeapStore (Physical X4, 0, Imm 7L, None); HeapStore (Physical X4, 8, Reg (Physical X3), Some AST.TString)]); ("both", AST.TTuple [AST.TString; AST.TBlob], [StringConcat (Physical X2, StringSymbol "leftBoth0", StringSymbol "rightBoth0", []); StringConcat (Physical X3, StringSymbol "leftBoth1", StringSymbol "rightBoth1", [])], [HeapStore (Physical X4, 0, Reg (Physical X2), Some AST.TString); HeapStore (Physical X4, 8, Reg (Physical X3), Some AST.TBlob)]) ]

let tests=[
 "x64 branch false edge falls through",testBranchFalseEdgeFallsThrough;
 "LIR CLI argv x64 helper resolves as code label",testCliArgvHelperResolvesAsCodeLabel;
 "LIR CLI argv x64 returns nullable String",testCliArgvReturnsNullableString;
 "LIR CLI host operations execute under x64",testCliHostOperationsExecute;
 "LIR CLI native call preserves live x64 caller register",testCliNativePreservesLiveCallerRegister;
 "LIR string literal x64 uses static storage",testStringLiteralUsesStaticStorage;
 "LIR string literal x64 heap store preserves X3",testStringLiteralHeapStorePreservesX3;
 "LIR x64 RawSlotInit retains X12 value",testRawSlotInitRetainsX12Value;
 "LIR StringConcat x64 loads stack-slot operand",testStringConcatLoadsStackSlotOperand;
 "LIR x64 codegen reports missing entry block",testReportsMissingEntryBlock;
 "LIR x64 codegen rejects conditions without block comparison",testRejectsConditionsWithoutBlockComparison;
 "LIR DateTimeNow x64 lowering uses 100ns Unix ticks",testDateTimeNowLowersTo100nsUnixTicks;
 "LIR Sleep x64 lowering normalizes timeout and retries nanosleep",testSleepLowersToNormalizedInterruptSafeNanosleep;
 "LIR float x64 argument moves resolve cycles",testFloatArgumentMovesResolveCycles;
 "LIR high x64 float registers execute",testHighFloatRegistersExecute;
 "LIR x64 noncommutative float aliases preserve scratch",testNonCommutativeFloatAliasesPreserveScratch;
 "LIR conditional branch",testBranch;
 "LIR generic RefCountDec dispatches mixed sum payload cleanup",testGenericRefCountDecMixedSumPayloadUsesVariantDispatch;
 "LIR generic RefCountDec dispatches nested mixed sum payload cleanup",testGenericRefCountDecNestedMixedSumPayloadUsesVariantDispatch;
 "LIR DictHeap RefCountDec uses planned helper for nested dict list leaf values",testDictRefCountDecDictListValueUsesPlannedHelper;
 "LIR tagged list RefCountDec uses planned helper for tuple payload",testTaggedListTuplePayloadUsesPlannedHelper;
 "LIR tagged list RefCountDec uses planned helper for record payload",testTaggedListRecordPayloadUsesPlannedHelper;
 "LIR tagged list RefCountDec uses planned helper for tuple5 payload",testTaggedListTuple5PayloadUsesPlannedHelper;
 "LIR tagged list RefCountDec uses planned helper for record5 payload",testTaggedListRecord5PayloadUsesPlannedHelper;
 "LIR DictHeap RefCountDec releases string collision keys and values",testDictRefCountDecStringCollisionKeysAndValues;
 "LIR DictHeap RefCountDec releases collision string keys and tuple/list values",testDictRefCountDecStringCollisionKeysAndTupleListValues;
 "LIR DictHeap RefCountDec uses planned helper for string keys and tuple/list values",testDictRefCountDecStringKeyTupleValueUsesPlannedHelper;
 "LIR closure RefCountDec preserves live argument closures",testClosureRefCountDecPreservesLiveArgumentClosures;
 "LIR closure RefCountDec dispatches mixed sum capture cleanup",testClosureRefCountDecMixedSumCaptureUsesVariantDispatch;
 "LIR tagged list RefCountDec releases closure payload in stdlib helper",testTaggedListRefCountDecClosurePayloadInStdlibFunction;
 "LIR tagged list RefCountDec releases tuple3 dynamic payload combinations",testTaggedListRefCountDecTuple3DynamicPayloadCombinations;
 "LIR tagged list RefCountDec releases tuple2 nested tuple dynamic combinations",testTaggedListRefCountDecTuple2NestedTupleDynamicPayloadCombinations;
 "LIR tagged list RefCountDec releases record3 dynamic payload combinations",testTaggedListRefCountDecRecord3DynamicPayloadCombinations;
 "LIR tagged list RefCountDec dispatches mixed sum dynamic payload cleanup",testTaggedListRefCountDecMixedSumDynamicPayloadUsesVariantDispatch;
 "LIR tagged list RefCountDec releases sum tuple2 dynamic payload combinations",testTaggedListRefCountDecSumTuple2DynamicPayloadCombinations;
 "LIR tagged list RefCountDec releases sum tuple3 dynamic payload combinations",testTaggedListRefCountDecSumTuple3DynamicPayloadCombinations;
 "LIR tagged list RefCountDec releases sum record3 dynamic payload combinations",testTaggedListRefCountDecSumRecord3DynamicPayloadCombinations;
]
