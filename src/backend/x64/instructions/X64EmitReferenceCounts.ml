(* X64EmitReferenceCounts.ml - Emit x64 instructions for referencecounts operations. *)
[@@@warning "-4"]
open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open FieldReferenceCounts
open X64ListReferenceCounts
module X=X86_64
let callPreserving registers addrReg helperLabel =
 let saves=List.map (fun reg->X.PUSH reg) registers in
 let restores=List.map (fun reg->X.POP reg) (List.rev registers) in
 saves@[X.MOV_reg (X.RAX,addrReg);X.CALL helperLabel]@restores
let emitRefCountInc (_ctx:funcCtx) addr payloadSize kind =
 Result.map (fun addrReg->match kind with
 | LIR.TaggedList -> callPreserving [X.RAX;X.RCX;X.RDX;X.RDI;X.R10] addrReg listRefCountIncHelperLabel
 | LIR.DictHeap -> callPreserving [X.RAX;X.RCX;X.RDX;X.RDI;X.RSI;X.R8;X.R9;X.R10] addrReg dictRefCountIncHelperLabel
 | LIR.ClosureHeap -> callPreserving [X.RAX;X.RCX;X.RDX;X.RDI;X.R10] addrReg closureRefCountIncHelperLabel
 | LIR.GenericHeap | LIR.StreamHeap -> genRefCountIncGeneric addrReg payloadSize) (resolveReg addr)
let emitRefCountDec ctx addr payloadSize kind metadata =
 Result.map (fun addrReg->match kind with
 | LIR.TaggedList ->
   (* TaggedList RefCountDec calls the iterative skew-list DFS helper. *)
   let helperLabel=listDecHelperForReleasePlan (requiredRcMetadataReleasePlan "TaggedList RefCountDec" metadata) in
   callPreserving [X.RAX;X.RCX;X.RDX;X.RDI;X.RSI;X.R8;X.R9;X.R10;scratch] addrReg helperLabel
 | LIR.DictHeap ->
   let helperLabel=dictDecHelperForReleasePlan (requiredRcMetadataReleasePlan "DictHeap RefCountDec" metadata) in
   callPreserving [X.RAX;X.RCX;X.RDX;X.RDI;X.RSI;X.R8;X.R9;X.R10;scratch] addrReg helperLabel
 | LIR.ClosureHeap -> callPreserving [X.RAX;X.RCX;X.RDX;X.RDI;X.RSI;X.R8;X.R9;X.R10;scratch] addrReg closureRefCountDecHelperLabel
 | LIR.StreamHeap -> callPreserving [X.RAX;X.RCX;X.RDX;X.RDI;X.R10;scratch] addrReg streamRefCountDecHelperLabel
 | LIR.GenericHeap -> genRefCountDecGeneric ctx addrReg payloadSize metadata) (resolveReg addr)
let emitRefCountIncBuffer (_ctx:funcCtx) skipTagged str = match str with
 | LIR.Imm 0L -> Ok []
 | LIR.StringSymbol _ -> Ok [] (* Literal string - no refcount *)
 | LIR.Reg reg -> Result.map (fun addrReg->
   (* Dynamic buffer: [refcount:8][length:8][data:N][padding:P]
      Sentinel value INT64_MAX means literal (read-only) *)
   let skipLabel=freshLabel "rcinc_str_skip" in
   let literalLabel=freshLabel "rcinc_str_lit" in
   let refAddrReg,refValueReg,preserveRegs,restoreRegs=
    if addrReg=scratch then
     X.RCX,X.RDX,[X.PUSH scratch;X.PUSH X.RCX;X.PUSH X.RDX;X.MOV_reg (X.RCX,addrReg)],
     [X.POP X.RDX;X.POP X.RCX;X.POP scratch]
    else
     let refValueReg=if addrReg=X.RCX then X.RDX else X.RCX in
     addrReg,refValueReg,[X.PUSH refValueReg],[X.POP refValueReg] in
   let taggedGuard=if skipTagged then [X.MOV_reg (scratch,refAddrReg);X.AND_imm (scratch,1l);X.Jcc (X.NE,literalLabel)] else [] in
   preserveRegs@[X.TEST_reg (refAddrReg,refAddrReg);X.Jcc (X.EQ,literalLabel)]@taggedGuard@
   [X.MOV_load (refValueReg,refAddrReg,0l)]@loadImm64 scratch 0x7FFFFFFFFFFFFFFFL (* scratch = INT64_MAX *)@
   [X.CMP_reg (refValueReg,scratch);X.Jcc (X.EQ,literalLabel); (* skip if literal *)
    X.ADD_imm (refValueReg,1l);X.MOV_store (refAddrReg,0l,refValueReg);X.Label literalLabel;X.Label skipLabel]@restoreRegs) (resolveReg reg)
 | _ -> Error "dynamic buffer RefCountInc requires StringSymbol or Reg operand"
let emitRefCountDecBuffer ctx skipTagged str = match str with
 | LIR.Imm 0L -> Ok []
 | LIR.StringSymbol _ -> Ok [] (* Literal string - no refcount *)
 | LIR.Reg reg -> Result.map (fun addrReg->
   let skipLabel=freshLabel "rcdec_str_skip" in
   let literalLabel=freshLabel "rcdec_str_lit" in
   let noFreeLabel=freshLabel "rcdec_str_nofree" in
   let leakDec=genLeakCounterDec ctx in
   let refAddrReg,refValueReg,preserveRegs,restoreRegs=
    if addrReg=scratch then
     X.RCX,X.RDX,[X.PUSH scratch;X.PUSH X.RCX;X.PUSH X.RDX;X.MOV_reg (X.RCX,addrReg)],
     [X.POP X.RDX;X.POP X.RCX;X.POP scratch]
    else
     let refValueReg=if addrReg=X.RCX then X.RDX else X.RCX in
     addrReg,refValueReg,[X.PUSH refValueReg],[X.POP refValueReg] in
   let taggedGuard=if skipTagged then [X.MOV_reg (scratch,refAddrReg);X.AND_imm (scratch,1l);X.Jcc (X.NE,literalLabel)] else [] in
   preserveRegs@[X.TEST_reg (refAddrReg,refAddrReg);X.Jcc (X.EQ,literalLabel)]@taggedGuard@
   [X.MOV_load (refValueReg,refAddrReg,0l)]@loadImm64 scratch 0x7FFFFFFFFFFFFFFFL@
   [X.CMP_reg (refValueReg,scratch);X.Jcc (X.EQ,literalLabel);X.SUB_imm (refValueReg,1l);
    X.MOV_store (refAddrReg,0l,refValueReg);X.TEST_reg (refValueReg,refValueReg);X.Jcc (X.NE,noFreeLabel)]
   (* String refcount hit zero - decrement leak counter *)
   @leakDec@[X.Label noFreeLabel;X.Label literalLabel;X.Label skipLabel]@restoreRegs) (resolveReg reg)
 | _ -> Error "dynamic buffer RefCountDec requires StringSymbol or Reg operand"
let emitRefCountIncString ctx str=emitRefCountIncBuffer ctx false str
let emitRefCountDecString ctx str=emitRefCountDecBuffer ctx false str
let emitRefCountIncInt ctx value=emitRefCountIncBuffer ctx true value
let emitRefCountDecInt ctx value=emitRefCountDecBuffer ctx true value
