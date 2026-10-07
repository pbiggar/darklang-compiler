(*
   X86_64_Resolve.ml - x86-64 Label Resolution and Fixup
   Resolves symbolic labels (CALL, JMP, Jcc, LEA_rip) into concrete
   relative offsets. Uses a two-pass approach:
   Pass 1: Encode all instructions to get their byte sizes, record
   label positions and fixup locations.
   Pass 2: Patch rel32 fields with correct relative offsets.
   x86-64 relative branches are offset from the END of the instruction
   (i.e., the address of the next instruction), not the start.
*)
[@@@warning "-4"]
open X86_64
(*
   A fixup records where a rel32 placeholder needs to be patched
   Byte offset in the output where the rel32 starts
   Byte offset of the instruction AFTER this one (where PC will be when executing)
   The label name this fixup targets
*)
type fixup = {patchOffset:int;nextInstrOffset:int;targetLabel:string}
(*
   Encode a list of x86-64 instructions with label resolution.
   Result of resolving and encoding
   Fixups deferred for data labels resolved after code size is known
*)
type resolveResult = {machineCode:bytes;labelPositions:int StringOrder.Map.t;deferredFixups:fixup list}
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b=Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
(*
   Require a code label position when downstream binary layout depends on it.
*)
let requireLabelPosition label labelPositions=match StringOrder.Map.find_opt label labelPositions with Some offset -> Ok offset | None -> Error ("Missing required label: "^label)
(*
   Patch a signed rel32 displacement into already-encoded machine code.
*)
let patchRel32 machineCode patchOffset rel =
 Bytes.set_int32_le machineCode patchOffset (Int32.of_int rel)
type patchState = {errors:string list}
(*
   Collect every symbolic string reference in first-use order. The empty string
   is always first because runtime helpers also address that canonical buffer.
*)
let collectStringPool instructions=LiteralPool.createStringPool (Seq.cons "" (Seq.filter_map (function LEA_rip (_,label) -> X86_64.tryStringLiteralValue label | _ -> None) (List.to_seq instructions)))
let stringEntrySize length=add 16 ((add length 7) land (lnot 7))
(*
   Resolve symbolic literal/runtime labels against the data segment layout used
   by Binary_Generation_ELF_X86_64.
*)
let dataLabelOffsets codeFileOffset codeSize (stringPool:LiteralPool.stringPool) =
 let dataStart=(add (add codeFileOffset codeSize) 7) land (lnot 7) in
 let dataEnd,literalLabels=Array.fold_left (fun (offset,labels) (value,length) -> let labels=StringOrder.Map.add (X86_64.stringLiteralLabel value) offset labels in add offset (stringEntrySize length),labels) (dataStart,StringOrder.Map.empty) stringPool.LiteralPool.strings in
 let emptyOffset=Option.value ~default:dataStart (StringOrder.Map.find_opt (X86_64.stringLiteralLabel "") literalLabels) in
 StringOrder.Map.add "_leak_count" (RuntimeDataLayout.elfCounterOffset dataEnd) (StringOrder.Map.add "_empty_dynamic_buffer" emptyOffset literalLabels)
(*
   Returns the final machine code bytes and label positions.
   Pass 1: encode all instructions, collect label positions and fixups
   Copy buffered instruction bytes into the patchable output
   Pass 2: apply fixups (defer unknown labels for data label patching later)
   rel32 = target - nextInstr
*)
let resolveAndEncodeWith encode instructions =
 let output=Buffer.create 4096 in
 let rec loop labelPositions fixups offset = function
 | [] ->
  let result=Buffer.to_bytes output in
  let deferred=List.fold_left (fun deferred fixup -> match StringOrder.Map.find_opt fixup.targetLabel labelPositions with
   | None -> fixup::deferred
   | Some targetOffset -> let rel=sub targetOffset fixup.nextInstrOffset in patchRel32 result fixup.patchOffset rel;deferred) [] fixups in
  Ok {machineCode=result;labelPositions;deferredFixups=List.rev deferred}
 | Label name::rest ->
  if StringOrder.Map.mem name labelPositions then Error ("Duplicate label: "^name)
  else loop (StringOrder.Map.add name offset labelPositions) fixups offset rest
 | instr::rest ->
  let bytes=encode instr in
  let nextOffset=add offset (Bytes.length bytes) in
  let fixups=match instr with
   | CALL targetLabel | JMP targetLabel -> {patchOffset=add offset 1;nextInstrOffset=nextOffset;targetLabel}::fixups
   | Jcc (_,targetLabel) -> {patchOffset=add offset 2;nextInstrOffset=nextOffset;targetLabel}::fixups
   | LEA_rip (_,targetLabel) -> {patchOffset=add offset 3;nextInstrOffset=nextOffset;targetLabel}::fixups
   | _ -> fixups in
  Buffer.add_bytes output bytes;
  loop labelPositions fixups nextOffset rest in
 loop StringOrder.Map.empty [] 0 instructions
let resolveAndEncode instructions=resolveAndEncodeWith X86_64_Encoding.encodeInstruction instructions
let patchDataLabel dataLabels codeFileOffset machineCode (state:patchState) fixup = match StringOrder.Map.find_opt fixup.targetLabel dataLabels with
 | None -> {errors=("Undefined label: "^fixup.targetLabel)::state.errors}
 | Some fileOffset -> let targetCodeOffset=sub fileOffset codeFileOffset in let rel=sub targetCodeOffset fixup.nextInstrOffset in patchRel32 machineCode fixup.patchOffset rel;state
(*
   Patch deferred fixups with data label positions.
   dataLabels maps label names to file offsets. codeFileOffset is where code starts in the file.
*)
let patchDataLabels (result:resolveResult) dataLabels codeFileOffset =
 let patchState=List.fold_left (patchDataLabel dataLabels codeFileOffset result.machineCode) {errors=[]} result.deferredFixups in
 if patchState.errors=[] then Ok {result with deferredFixups=[]} else Error (String.concat "\n" (List.rev patchState.errors))
