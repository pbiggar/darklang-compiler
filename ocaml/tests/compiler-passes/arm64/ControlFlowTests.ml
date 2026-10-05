(*
   ControlFlowTests.fs - Verify target return transfers, print control flow, and RC instruction costs.
   A diamond needs one jump over the sibling branch, but neither a backward
   jump from that sibling nor a return-to-epilogue jump. Count emitted transfers
   rather than merely asserting a particular order of LIR labels.
   Count the whole RC operation, including literal protection and scratch
   preservation. Executable RC tests cover the heap/literal outcomes.
   The DFS work stack remains necessary. Only spills and copies
   protecting node state across payload destruction are redundant.
   Test: malformed ARM64 CFGs should be reported as codegen errors instead of silently dropping the entry.
*)
[@@@warning "-4-42"]
open Dark_compiler
open Fixtures
module L=LIR
module S=Symbolic
module M=StringOrder.Map
let (let*)=Result.bind
let uint64ZeroBranchTargetsDigit instrs=
 let destination=instrs |> List.mapi (fun i instr->i,instr) |> List.find_map (function index,ARM64.CBZ_offset (ARM64.X2,offset)->Some (index+offset)|_->None)
 in Option.bind destination (fun target->if target<0 then None else List.nth_opt instrs target)
 |> Option.fold ~none:false ~some:(function ARM64.MOVZ (ARM64.X2,48,0)->true|_->false)
let testPrintUInt64RuntimeZeroBranches ()=
 if not (uint64ZeroBranchTargetsDigit (PrintValues.generatePrintUInt64NoExit target)) then Error "ARM64 UInt64 newline printer zero branch does not target the zero digit handler"
 else if not (uint64ZeroBranchTargetsDigit (PrintValues.generatePrintUInt64NoNewline target)) then Error "ARM64 UInt64 no-newline printer zero branch does not target the zero digit handler" else Ok ()
let testPrintUInt64RuntimePreservesNewline ()=
 let rec preserves=function ARM64.MOVZ (ARM64.X3,10,0)::ARM64.STRB (ARM64.X3,ARM64.X1,0)::ARM64.SUB_imm (ARM64.X1,ARM64.X1,1)::_ ->true|_::rest->preserves rest|[]->false in
 if preserves (PrintValues.generatePrintUInt64NoExit target) then Ok () else Error "ARM64 UInt64 newline printer does not move the digit cursor before conversion"
let block label instrs terminator:L.basicBlock={L.label;instrs;terminator}
let functionWithBlocks name typedParams entry blocks stackSize:L.functionDef={L.id=TestIds.functionIdForName name;name;typedParams;cfg={L.entry;blocks=L.LabelMap.of_list blocks};stackSize;usedCalleeSaved=[];codegenFacts=None}
let context name stackSize usedCalleeSaved:ARM64CodeGenTypes.codeGenContext={ARM64CodeGenTypes.target;options=ARM64CodeGenTypes.defaultOptions;sumShapeRegistry=M.empty;recordRegistry=M.empty;rawSlotInitRetainTargets=None;closurePayloadSizes=M.empty;closureCaptureTypes=M.empty;functionNames=FunctionIdMap.empty;functionName=name;instructionSite="";stackSize;usedCalleeSaved;usedCalleeSavedF=[];heapOverflowLabel="__heap_oom_"^name;recordLirOpExpansion=None}
let indexOf expected xs=List.mapi (fun i v->i,v) xs |> List.find_map (fun (i,v)->if v=expected then Some i else None)
let testBranchFalseEdgeFallsThrough ()=
 let entry=L.Label "arm64_layout_entry" and yes=L.Label "arm64_layout_true" and no=L.Label "arm64_layout_false" in
 let func=functionWithBlocks "arm64_layout" [] entry [entry,block entry [] (L.Branch (L.Physical L.X0,yes,no));yes,block yes [] L.Ret;no,block no [L.HeapAlloc (L.Physical L.X1,8)] L.Ret] 0 in
 let* instrs=ARM64Functions.convertFunction [] (context func.L.name 0 []) func in
 let jumps=List.filter ((=) (S.B_label "_epilogue_arm64_layout")) instrs |> List.length in
 if List.mem (S.B_label "arm64_layout_false") instrs then Error "ARM64 emitted a jump to the immediately following false block"
 else if jumps<>1 then Error (Printf.sprintf "ARM64 emitted %d jumps to the epilogue; expected one before the final fallthrough" jumps) else
 match indexOf (S.Label "_epilogue_arm64_layout") instrs,indexOf (S.Label "__heap_oom_arm64_layout") instrs with
 |Some epilogue,Some overflow when epilogue<overflow->Ok ()
 |Some epilogue,Some overflow->Error (Printf.sprintf "ARM64 heap-overflow trap at %d blocks final return fallthrough to epilogue at %d" overflow epilogue)
 |_->Error "ARM64 allocation fixture did not emit both epilogue and heap-overflow labels"
let testSharedReturnTransferCost ()=
 let entry=L.Label "common_return_entry" and yes=L.Label "a_common_return_true" and no=L.Label "z_common_return_false" and join=L.Label "common_return_join" in
 let func=functionWithBlocks "common_return" [] entry [entry,block entry [] (L.Branch (L.Physical L.X0,yes,no));yes,block yes [L.Mov (L.Physical L.X0,L.Imm 11L)] (L.Jump join);no,block no [L.Mov (L.Physical L.X0,L.Imm 22L)] (L.Jump join);join,block join [] L.Ret] 0 in
 let* instrs=ARM64Functions.convertFunction [] (context func.L.name 0 []) func in
 if List.length (List.filter (function S.B_label _->true|_->false) instrs)=1 then Ok () else Error "Common-return diamond needs one unconditional transfer"
let testDynamicBufferRcInstructionCost ()=
 let ctx=context "buffer_rc_cost" 0 [] in
 let operations=[(fun operand->L.RefCountIncString operand);(fun operand->L.RefCountDecString operand);(fun operand->L.RefCountIncBlob operand);(fun operand->L.RefCountDecBlob operand)] in
 let operands=[L.X0,6;L.X13,7;L.X14,6;L.X15,7] in
 List.concat_map (fun operation->List.map (fun (reg,cost)->operation (L.Reg (L.Physical reg)),cost) operands) operations
 |> List.fold_left (fun result (operation,cost)->let* ()=result in let* instrs=ARM64Instructions.convertInstr ctx operation in if List.length instrs=cost then Ok () else Error (Printf.sprintf "Expected %d instructions, got %d" cost (List.length instrs))) (Ok ())
let testPrimitiveListPayloadPreservationCost ()=
 let helper="__dark_list_refcount_dec_helper" in
 let program=makeSimpleProgramWithVariants [L.RefCountDec (L.Physical L.X0,0,L.TaggedList,Some (rcMetadata (AST.TList AST.TInt64)))] M.empty in
 let* instrs=generatePreparedARM64 target program in
 let rec after=function []->[]|instr::rest->if instr=S.Label helper then instr::rest else after rest in
 match after instrs with []->Error "Primitive list release code was not emitted"|_::rest->
 let rec body=function (S.Label name)::_ when not (HostText.startsWithCurrentCulture name (helper^"_"))->[]|instr::rest->instr::body rest|[]->[] in
 let preservation=body rest |> List.filter (function S.STP_pre (S.X19,S.X20,S.SP,_)|S.LDP_post (S.X19,S.X20,S.SP,_)|S.STR (S.X21,S.SP,_)|S.LDR (S.X21,S.SP,_)|S.MOV_reg ((S.X19|S.X20|S.X21),_)|S.MOV_reg (_,(S.X19|S.X20|S.X21))->true|_->false) in
 if preservation=[] then Ok () else Error (Printf.sprintf "Primitive list release needs no payload preservation instructions, got %d" (List.length preservation))
let makeEmptyFunction name typedParams=let label=L.Label (name^"_entry") in functionWithBlocks name typedParams label [label,block label [] L.Ret] 0
let makeAllocatedEntryFunction name typedParams instrs stackSize=let label=L.Label (name^"_entry") in functionWithBlocks name typedParams label [label,block label instrs L.Ret] stackSize
let generatedEntryTransfers (func:L.functionDef)=ARM64Functions.convertFunction [] (context func.L.name func.L.stackSize func.L.usedCalleeSaved) func |> Result.map (List.filter (function S.MOV_reg (S.X29,S.SP)->false|S.MOV_reg _|S.FMOV_reg _|S.STUR _->true|_->false))
let assertGeneratedEntryTransfers caseName func expected=match generatedEntryTransfers func with
 |Error e->Error (caseName^" failed code generation: "^e)|Ok actual when actual=expected->Ok ()|Ok actual->let render xs=List.map PassTestRunner.prettyPrintARM64Instr xs |> String.concat "; " in Error (caseName^" expected ["^render expected^"], got ["^render actual^"]")
let testGeneratedEntryUsesAllocatorTransfersOnly ()=
 let intParam reg:L.typedLIRParam={L.reg=L.Physical reg;typ=AST.TInt64} in let floatParam:L.typedLIRParam={L.reg=L.Physical L.X0;typ=AST.TFloat64} in
 let identity=makeAllocatedEntryFunction "arm64_entry_identity" [intParam L.X0] [] 0 in
 let mixed=makeAllocatedEntryFunction "arm64_entry_mixed_spill" [intParam L.X0;floatParam;intParam L.X1;floatParam] [L.FMov (L.FPhysical L.D4,L.FPhysical L.D0);L.FMov (L.FPhysical L.D5,L.FPhysical L.D1);L.Store (-8,L.Physical L.X1);L.Mov (L.Physical L.X3,L.Reg (L.Physical L.X0))] 16 in
 let swap=makeAllocatedEntryFunction "arm64_entry_swap" [intParam L.X0;intParam L.X1] [L.Mov (L.Physical L.X16,L.Reg (L.Physical L.X0));L.Mov (L.Physical L.X0,L.Reg (L.Physical L.X1));L.Mov (L.Physical L.X1,L.Reg (L.Physical L.X16))] 0 in
 let eight=makeAllocatedEntryFunction "arm64_entry_eight_args" (List.map intParam [L.X0;L.X1;L.X2;L.X3;L.X4;L.X5;L.X6;L.X7]) [L.Store (-8,L.Physical L.X7);L.Mov (L.Physical L.X7,L.Reg (L.Physical L.X6))] 16 in
 let* ()=assertGeneratedEntryTransfers "identity parameter" identity [] in
 let* ()=assertGeneratedEntryTransfers "mixed integer/float parameters with spill" mixed [S.FMOV_reg (S.D4,S.D0);S.FMOV_reg (S.D5,S.D1);S.STUR (S.X1,S.X29,-8);S.MOV_reg (S.X3,S.X0)] in
 let* ()=assertGeneratedEntryTransfers "parallel-move swap" swap [S.MOV_reg (S.X16,S.X0);S.MOV_reg (S.X0,S.X1);S.MOV_reg (S.X1,S.X16)] in
 assertGeneratedEntryTransfers "eight integer arguments" eight [S.STUR (S.X7,S.X29,-8);S.MOV_reg (S.X7,S.X6)]
let testReportsMissingEntryBlock ()=
 let entry=L.Label "_start_entry" and body=L.Label "_start_body" in
 let func=functionWithBlocks "_start" [] entry [body,block body [] L.Ret] 0 in
 match generatePreparedARM64 target (L.Program ([func],M.empty,M.empty)) with
 |Error e when HostText.contains e "missing entry block"->Ok ()|Error e->Error ("Expected missing entry block error, got '"^e^"'")|Ok _->Error "Expected ARM64 codegen to reject a CFG whose entry block is absent"
