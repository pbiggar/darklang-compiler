[@@@warning "-4-42"]
(* RuntimeDataLayoutTests.ml - ELF relocation/image agreement for writable counters. *)
open Dark_compiler
type testResult=(unit,string) result
let checkCounter image offset=
 if offset mod 65536<>0 then Error "Writable ELF counter shares a code page"
 else if Bytes.length image<>offset+8 then Error "Counter relocation disagrees with ELF image extent"
 else if String.exists (fun value->value<>'\000') (Bytes.sub_string image offset (Bytes.length image-offset)) then Error "ELF counter is not initialized to zero" else Ok ()
let checkArm64 stringLength ()=
 let strings=LiteralPool.createStringPool (List.to_seq [String.make stringLength 'x']) in
 let code=[|0xd65f03c0l|] in (* RET; only data layout is under test. *)
 let labels=ARM64_Encoding.computeLeakCounterLabel Platform.Linux 120 4 0 (ARM64_Encoding.getStringPoolSize strings) in
 let image=Backend_Arm64_Binary_Generation_ELF.createExecutableWithPools code strings LiteralPool.emptyFloatPool true in
 match StringOrder.Map.find_opt Symbolic.leakCounterLabelName labels with Some offset->checkCounter image offset|None->Error "ARM64 counter relocation is absent"
let checkX86 stringLength ()=
 let strings=LiteralPool.createStringPool (List.to_seq [String.make stringLength 'x']) in
 let code=Bytes.of_string "\195" in (* RET; only data layout is under test. *)
 let labels=X86_64_Resolve.dataLabelOffsets 120 (Bytes.length code) strings in
 let image=Binary_Generation_ELF_X86_64.createExecutableWithPools code strings LiteralPool.emptyFloatPool true 0 in
 match StringOrder.Map.find_opt "_leak_count" labels with Some offset->checkCounter image offset|None->Error "x86 counter relocation is absent"
let tests=List.concat_map (fun length->[Printf.sprintf "ARM64 counter relocation after %d string bytes" length,checkArm64 length;Printf.sprintf "x86 counter relocation after %d string bytes" length,checkX86 length]) [0;4090;16380;65530]
