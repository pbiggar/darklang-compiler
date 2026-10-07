(*
   ARM64Operands.ml - Materialize target operands, registers, and immediates.
*)
[@@@warning "-4"]
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b=Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let neg a=Int32.to_int (Int32.neg (Int32.of_int a))
(*
   Convert LIR.PhysReg to ARM64Symbolic.Reg
*)
let lirPhysRegToARM64Reg = function
 | LIR.X0 -> Symbolic.X0
 | LIR.X1 -> Symbolic.X1
 | LIR.X2 -> Symbolic.X2
 | LIR.X3 -> Symbolic.X3
 | LIR.X4 -> Symbolic.X4
 | LIR.X5 -> Symbolic.X5
 | LIR.X6 -> Symbolic.X6
 | LIR.X7 -> Symbolic.X7
 | LIR.X8 -> Symbolic.X8
 | LIR.X9 -> Symbolic.X9
 | LIR.X10 -> Symbolic.X10
 | LIR.X11 -> Symbolic.X11
 | LIR.X12 -> Symbolic.X12
 | LIR.X13 -> Symbolic.X13
 | LIR.X14 -> Symbolic.X14
 | LIR.X15 -> Symbolic.X15
 | LIR.X16 -> Symbolic.X16
 | LIR.X17 -> Symbolic.X17
 | LIR.X19 -> Symbolic.X19
 | LIR.X20 -> Symbolic.X20
 | LIR.X21 -> Symbolic.X21
 | LIR.X22 -> Symbolic.X22
 | LIR.X23 -> Symbolic.X23
 | LIR.X24 -> Symbolic.X24
 | LIR.X25 -> Symbolic.X25
 | LIR.X26 -> Symbolic.X26
 | LIR.X27 -> Symbolic.X27
 | LIR.X29 -> Symbolic.X29
 | LIR.X30 -> Symbolic.X30
 | LIR.SP -> Symbolic.SP
(*
   Convert LIR.PhysFPReg to ARM64Symbolic.FReg
*)
let lirPhysFPRegToARM64FReg = function
 | LIR.D0 -> Symbolic.D0
 | LIR.D1 -> Symbolic.D1
 | LIR.D2 -> Symbolic.D2
 | LIR.D3 -> Symbolic.D3
 | LIR.D4 -> Symbolic.D4
 | LIR.D5 -> Symbolic.D5
 | LIR.D6 -> Symbolic.D6
 | LIR.D7 -> Symbolic.D7
 | LIR.D8 -> Symbolic.D8
 | LIR.D9 -> Symbolic.D9
 | LIR.D10 -> Symbolic.D10
 | LIR.D11 -> Symbolic.D11
 | LIR.D12 -> Symbolic.D12
 | LIR.D13 -> Symbolic.D13
 | LIR.D14 -> Symbolic.D14
 | LIR.D15 -> Symbolic.D15
(*
   Convert LIR.FReg to ARM64Symbolic.FReg
   For FVirtual, we use a two-tier allocation scheme to avoid collisions:
   - Negative FVirtual IDs are reserved for spill and cycle scratch registers.
   - FVirtual 0-7 -> D2-D9 (dedicated 1:1 mapping for parameters)
   - FVirtual 8+ -> D10-D13 (4 temps with modulo, for SSA temps and locals)
   The two-tier scheme ensures that parameter VRegs (0-7) never collide with
   SSA-generated temps (which have high IDs like 12001). Parameters get D2-D9,
   while temps get D10-D13 with modulo 4.
   Special temp registers for specific purposes
   Float return across RestoreRegs
   Left spill scratch
   Right spill scratch
   Third spill scratch
   Parallel-move cycle scratch
   Parameters (VRegs 0-7) get dedicated D2-D9 mapping
   This prevents collisions with SSA-generated temps
   ANF-level VRegs (8-9999): function params and local bindings
   These come from ANF TempIds which are sequential across functions.
   Pool: D0, D1, D10-D15, D27-D31 (13 registers)
   Using direct index: (n - 8) % 13
   MIR intermediates (VRegs 10000+): computation temps from freshReg
   Use same pool but with offset to reduce collisions with ANF-level VRegs
   The offset of 7 ensures that if ANF VReg k and MIR VReg (10000+k) exist,
   they map to different registers (since 7 and 13 are coprime)
*)
let lirFRegToARM64FReg = function
 | LIR.FPhysical physReg -> Ok (lirPhysFPRegToARM64FReg physReg)
 | LIR.FVirtual (-1) -> Ok Symbolic.D16
 | LIR.FVirtual (-1000) -> Ok Symbolic.D18
 | LIR.FVirtual (-1001) -> Ok Symbolic.D17
 | LIR.FVirtual (-1002) -> Ok Symbolic.D27
 | LIR.FVirtual (-2000) -> Ok Symbolic.D16
 | LIR.FVirtual n when n>=0 && n<=7 -> Ok (match n with 0 -> Symbolic.D2 | 1 -> Symbolic.D3 | 2 -> Symbolic.D4 | 3 -> Symbolic.D5 | 4 -> Symbolic.D6 | 5 -> Symbolic.D7 | 6 -> Symbolic.D8 | _ -> Symbolic.D9)
 | LIR.FVirtual n when n<10000 ->
 let tempRegs=[|Symbolic.D0;Symbolic.D1;Symbolic.D10;Symbolic.D11;Symbolic.D12;Symbolic.D13;Symbolic.D14;Symbolic.D15;Symbolic.D27;Symbolic.D28;Symbolic.D29;Symbolic.D30;Symbolic.D31|] in
 let regIdx=sub n 8 mod Array.length tempRegs in Ok tempRegs.(regIdx)
 | LIR.FVirtual n ->
 let tempRegs=[|Symbolic.D0;Symbolic.D1;Symbolic.D10;Symbolic.D11;Symbolic.D12;Symbolic.D13;Symbolic.D14;Symbolic.D15;Symbolic.D27;Symbolic.D28;Symbolic.D29;Symbolic.D30;Symbolic.D31|] in
 let regIdx=add (sub n 10000) 7 mod Array.length tempRegs in Ok tempRegs.(regIdx)
(*
   Convert LIR.Reg to ARM64Symbolic.Reg (assumes physical registers only)
*)
let lirRegToARM64Reg = function
 | LIR.Physical physReg -> Ok (lirPhysRegToARM64Reg physReg)
 | LIR.Virtual vreg -> Error (Printf.sprintf "Virtual register %d should have been allocated" vreg)
(*
   Convert LIR.Reg (Virtual) to LIR.FReg (FVirtual) for float HeapStore
   This is used when a float value is stored via HeapStore - the register
   ID is shared between Virtual and FVirtual address spaces
   Map GP physical registers to FP physical registers for edge cases
*)
let virtualToFVirtual = function
 | LIR.Virtual n -> LIR.FVirtual n
 | LIR.Physical p -> LIR.FPhysical (match p with LIR.X0 -> LIR.D0 | LIR.X1 -> LIR.D1 | LIR.X2 -> LIR.D2 | LIR.X3 -> LIR.D3 | LIR.X4 -> LIR.D4 | LIR.X5 -> LIR.D5 | LIR.X6 -> LIR.D6 | LIR.X7 -> LIR.D7 | LIR.X8 | LIR.X9 | LIR.X10 | LIR.X11 | LIR.X12 | LIR.X13 | LIR.X14 | LIR.X15 | LIR.X16 | LIR.X17 | LIR.X19 | LIR.X20 | LIR.X21 | LIR.X22 | LIR.X23 | LIR.X24 | LIR.X25 | LIR.X26 | LIR.X27 | LIR.X29 | LIR.X30 | LIR.SP -> LIR.D15)
(*
   Generate ARM64 instructions to load an immediate into a register
   Load 64-bit immediate using MOVZ/MOVN + MOVK sequence
   For negative numbers, MOVN (move NOT) can be more efficient
   Extract each 16-bit chunk
   Count how many chunks are all-zeros vs all-ones
   Use MOVN if more chunks are 0xFFFF (inverted gives more zeros)
   Use MOVN: start with first non-0xFFFF chunk, then MOVK for remaining non-0xFFFF chunks
   MOVN Xd, #imm, LSL #shift sets Xd = NOT(imm << shift), filling rest with 1s
   Find first chunk that is NOT 0xFFFF (so inverting gives a meaningful value)
   Start with MOVN using inverted first non-0xFFFF chunk
   All chunks are 0xFFFF, use MOVN #0 to get all 1s (-1)
   Use MOVZ: find first non-zero chunk, then MOVK for remaining non-zero chunks
   MOVZ Xd, #imm, LSL #shift sets Xd = imm << shift, zeros elsewhere
   Start with MOVZ using first non-zero chunk
   All chunks are zero, just use MOVZ #0
*)
let loadImmediate dest value =
 let chunk shift=Int64.to_int (Int64.logand (Int64.shift_right value shift) 0xffffL) in
 let chunk0=chunk 0 in let chunk1=chunk 16 in let chunk2=chunk 32 in let chunk3=chunk 48 in
 let zeroCount=(if chunk0=0 then 1 else 0)+(if chunk1=0 then 1 else 0)+(if chunk2=0 then 1 else 0)+(if chunk3=0 then 1 else 0) in
 let onesCount=(if chunk0=0xffff then 1 else 0)+(if chunk1=0xffff then 1 else 0)+(if chunk2=0xffff then 1 else 0)+(if chunk3=0xffff then 1 else 0) in
 let chunks=[chunk0,0;chunk1,16;chunk2,32;chunk3,48] in
 if onesCount>zeroCount then
 (match List.find_opt (fun (c,_) -> c<>0xffff) chunks with
 | Some (firstChunk,firstShift) ->
 let invFirstChunk=(lnot firstChunk) land 0xffff in
 [Symbolic.MOVN (dest,invFirstChunk,firstShift)]
 @(if firstShift<>0 && chunk0<>0xffff then [Symbolic.MOVK (dest,chunk0,0)] else [])
 @(if firstShift<>16 && chunk1<>0xffff then [Symbolic.MOVK (dest,chunk1,16)] else [])
 @(if firstShift<>32 && chunk2<>0xffff then [Symbolic.MOVK (dest,chunk2,32)] else [])
 @(if firstShift<>48 && chunk3<>0xffff then [Symbolic.MOVK (dest,chunk3,48)] else [])
 | None -> [Symbolic.MOVN (dest,0,0)])
 else
 (match List.find_opt (fun (c,_) -> c<>0) chunks with
 | Some (firstChunk,firstShift) ->
 [Symbolic.MOVZ (dest,firstChunk,firstShift)]
 @(if firstShift<>0 && chunk0<>0 then [Symbolic.MOVK (dest,chunk0,0)] else [])
 @(if firstShift<>16 && chunk1<>0 then [Symbolic.MOVK (dest,chunk1,16)] else [])
 @(if firstShift<>32 && chunk2<>0 then [Symbolic.MOVK (dest,chunk2,32)] else [])
 @(if firstShift<>48 && chunk3<>0 then [Symbolic.MOVK (dest,chunk3,48)] else [])
 | None -> [Symbolic.MOVZ (dest,0,0)])
(*
   Generate ARM64 instructions to load a stack slot into a register
   Stack slots are accessed relative to FP (X29)
   Uses LDUR for small offsets (-256 to +255), computes address for larger offsets
   Small offset: use LDUR directly
   Larger negative offset: compute address into X10, then load
   X10 = X29 - (-offset), then LDR dest, [X10, #0]
   Larger positive offset: compute address into X10, then load
*)
let loadStackSlot dest offset =
 if offset>= -256 && offset<=255 then Ok [Symbolic.LDUR (dest,Symbolic.X29,offset)]
 else if offset<0 && neg offset<=4095 then Ok [Symbolic.SUB_imm (Symbolic.X10,Symbolic.X29,(neg offset) land 0xffff);Symbolic.LDR (dest,Symbolic.X10,0)]
 else if offset>0 && offset<=4095 then Ok [Symbolic.ADD_imm (Symbolic.X10,Symbolic.X29,offset land 0xffff);Symbolic.LDR (dest,Symbolic.X10,0)]
 else Error (Printf.sprintf "Stack offset %d exceeds supported range (-4095 to +4095)" offset)
(*
   Load an integer or managed-string operand for a native CLI helper call.
*)
let loadCliOperand dest = function
 | LIR.Imm value -> Ok (loadImmediate dest value)
 | LIR.Reg source -> Result.map (fun sourceReg -> if sourceReg=dest then [] else [Symbolic.MOV_reg (dest,sourceReg)]) (lirRegToARM64Reg source)
 | LIR.StackSlot offset -> loadStackSlot dest offset
 | LIR.StringSymbol value -> Ok (HeapAllocation.loadStringLiteralPointer dest value)
 | LIR.FloatImm _ | LIR.FloatSymbol _ | LIR.FuncAddr _ -> Error "CLI native operation received a non-integer operand"
(*
   Generate ARM64 instructions to store a register to a stack slot
   Stack slots are accessed relative to FP (X29)
   Uses STUR for small offsets (-256 to +255), computes address for larger offsets
   Small offset: use STUR directly
   Larger negative offset: compute address into X10, then store
   X10 = X29 - (-offset), then STR src, [X10, #0]
   Larger positive offset: compute address into X10, then store
*)
let storeStackSlot src offset =
 if offset>= -256 && offset<=255 then Ok [Symbolic.STUR (src,Symbolic.X29,offset)]
 else if offset<0 && neg offset<=4095 then Ok [Symbolic.SUB_imm (Symbolic.X10,Symbolic.X29,(neg offset) land 0xffff);Symbolic.STR (src,Symbolic.X10,0)]
 else if offset>0 && offset<=4095 then Ok [Symbolic.ADD_imm (Symbolic.X10,Symbolic.X29,offset land 0xffff);Symbolic.STR (src,Symbolic.X10,0)]
 else Error (Printf.sprintf "Stack offset %d exceeds supported range (-4095 to +4095)" offset)
