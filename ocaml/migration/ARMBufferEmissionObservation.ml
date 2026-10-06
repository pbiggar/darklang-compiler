(* Whole buffer selections, concat trees and exact recoverable operand diagnostics. *)
open Dark_compiler
module E=ARM64EmitBuffers
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false; (match f () with Ok xs -> SemanticJson.union "FSharpResult" "Ok" [`List (List.map J.symInstr xs)] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error])] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP]
let observe source=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=ARMPrintingObservation.context source target false in
 let gps=List.map (fun p -> LIR.Physical p) physical@List.map (fun n -> LIR.Virtual n) [-1;0;2147483647] in
 let selected=List.map (fun p -> LIR.Physical p) [LIR.X8;LIR.X9;LIR.X11;LIR.X14;LIR.X19;LIR.SP]@[LIR.Virtual (-1)] in
 let strings=List.map (fun text -> LIR.StringSymbol text) [source;"";"hé😀";"a\000b";HostText.ofScalars [|0xfffd;0x61;0xfffd|]] in
 let floats=[0.;-0.;0.1;infinity;neg_infinity;nan;Int64.float_of_bits 1L;Float.max_float] in
 let operands=List.map (fun reg -> LIR.Reg reg) gps@strings@List.map (fun offset -> LIR.StackSlot offset) [-2147483648;-4096;-4095;-257;-256;-1;0;255;256;4095;4096;2147483647]@List.map (fun n -> LIR.Imm n) [Int64.min_int;-1L;0L;Int64.max_int]@List.concat_map (fun value -> [LIR.FloatImm value;LIR.FloatSymbol value]) floats@List.map (fun id -> LIR.FuncAddr (AST.functionId id)) [0L;1L;-1L] in
 let kinds=[MemoryModel.Utf8String;MemoryModel.NullableUtf8String;MemoryModel.GraphemeCluster;MemoryModel.NullableGraphemeCluster] in
 let equality=list (fun kind -> list (fun dest -> list (fun left -> list (fun right -> call (fun () -> E.emitCanonicalBufferEq ctx kind dest left right)) [left;LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X8);LIR.Reg (LIR.Physical LIR.X9);LIR.StringSymbol source;LIR.Imm 0L]) operands) selected) kinds in
 let equalityDestinations=list (fun dest -> list (fun kind -> list (fun left -> list (fun right -> call (fun () -> E.emitCanonicalBufferEq ctx kind dest left right)) [LIR.Reg dest;left;LIR.Reg (LIR.Physical LIR.X9);LIR.StringSymbol source]) [LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X8);LIR.Reg (LIR.Physical LIR.X11);LIR.StringSymbol source]) kinds) gps in
 let concat=list (fun enabled -> let ctx=ARMPrintingObservation.context source target enabled in tuple [list (fun dest -> list (fun left -> list (fun right -> call (fun () -> E.emitStringConcat ctx dest left right [])) [left;LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X9);LIR.Reg (LIR.Physical LIR.X11);LIR.StringSymbol source]) operands) selected;list (fun dest -> list (fun left -> list (fun right -> call (fun () -> E.emitStringConcat ctx dest left right [])) [left;LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X14);LIR.StringSymbol source]) [LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X9);LIR.Reg (LIR.Physical LIR.X11);LIR.StringSymbol source]) gps;list (fun dest -> list (fun operand -> tuple [call (fun () -> E.emitStringConcat ctx dest operand (LIR.StringSymbol source) [LIR.StringSymbol "tail"]);call (fun () -> E.emitStringConcat ctx dest (LIR.StringSymbol source) operand [LIR.StringSymbol "tail"]);call (fun () -> E.emitStringConcat ctx dest (LIR.StringSymbol source) (LIR.StringSymbol "") [operand]);call (fun () -> E.emitStringConcat ctx dest (LIR.StringSymbol source) (LIR.Reg dest) [LIR.StackSlot 0;operand;LIR.StringSymbol source])]) operands) selected;list (fun dest -> list (fun count -> call (fun () -> E.emitStringConcat ctx dest (LIR.StringSymbol source) (LIR.StringSymbol "é") (List.init count (fun n -> List.nth strings (n mod List.length strings))))) [1;2;3;4;8;17;65]) gps]) [false;true] in
 tuple [equality;equalityDestinations;concat]
