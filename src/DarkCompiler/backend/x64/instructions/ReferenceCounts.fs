// ReferenceCounts.fs - Emit x64 instructions for referencecounts operations.

module X64EmitReferenceCounts

open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open X64FieldReferenceCounts
open X64ListReferenceCounts

let internal emitRefCountInc (ctx: FuncCtx) (addr: LIR.Reg) (payloadSize: int) (kind: LIR.RcKind) : Result<X86_64.Instr list, string> =
    resolveReg addr
    |> Result.map (fun addrReg ->
        match kind with
        | LIR.TaggedList ->
            let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.R10]
            let saves = saveRegs |> List.map X86_64.PUSH
            let restores = saveRegs |> List.rev |> List.map X86_64.POP
            saves
            @ [X86_64.MOV_reg (X86_64.RAX, addrReg); X86_64.CALL listRefCountIncHelperLabel]
            @ restores
        | LIR.DictHeap ->
            let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10]
            let saves = saveRegs |> List.map X86_64.PUSH
            let restores = saveRegs |> List.rev |> List.map X86_64.POP
            saves
            @ [X86_64.MOV_reg (X86_64.RAX, addrReg); X86_64.CALL dictRefCountIncHelperLabel]
            @ restores
        | LIR.ClosureHeap ->
            let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.R10]
            let saves = saveRegs |> List.map X86_64.PUSH
            let restores = saveRegs |> List.rev |> List.map X86_64.POP
            saves
            @ [X86_64.MOV_reg (X86_64.RAX, addrReg); X86_64.CALL closureRefCountIncHelperLabel]
            @ restores
        | LIR.GenericHeap
        | LIR.StreamHeap ->
            genRefCountIncGeneric addrReg payloadSize)

let internal emitRefCountDec (ctx: FuncCtx) (addr: LIR.Reg) (payloadSize: int) (kind: LIR.RcKind) (metadata: MemoryModel.RcMetadata option) : Result<X86_64.Instr list, string> =
    resolveReg addr
    |> Result.map (fun addrReg ->
        match kind with
        | LIR.TaggedList ->
            // TaggedList RefCountDec calls the iterative skew-list DFS helper.
            let helperLabel =
                metadata
                |> requiredRcMetadataReleasePlan "TaggedList RefCountDec"
                |> listDecHelperForReleasePlan
            let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10; scratch]
            let saves = saveRegs |> List.map X86_64.PUSH
            let restores = saveRegs |> List.rev |> List.map X86_64.POP
            saves
            @ [X86_64.MOV_reg (X86_64.RAX, addrReg); X86_64.CALL helperLabel]
            @ restores
        | LIR.DictHeap ->
            let helperLabel =
                metadata
                |> requiredRcMetadataReleasePlan "DictHeap RefCountDec"
                |> dictDecHelperForReleasePlan
            let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10; scratch]
            let saves = saveRegs |> List.map X86_64.PUSH
            let restores = saveRegs |> List.rev |> List.map X86_64.POP
            saves
            @ [X86_64.MOV_reg (X86_64.RAX, addrReg); X86_64.CALL helperLabel]
            @ restores
        | LIR.ClosureHeap ->
            let saveRegs =
                [ X86_64.RAX
                  X86_64.RCX
                  X86_64.RDX
                  X86_64.RDI
                  X86_64.RSI
                  X86_64.R8
                  X86_64.R9
                  X86_64.R10
                  scratch ]
            let saves = saveRegs |> List.map X86_64.PUSH
            let restores = saveRegs |> List.rev |> List.map X86_64.POP
            saves
            @ [X86_64.MOV_reg (X86_64.RAX, addrReg); X86_64.CALL closureRefCountDecHelperLabel]
            @ restores
        | LIR.StreamHeap ->
            let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.R10; scratch]
            let saves = saveRegs |> List.map X86_64.PUSH
            let restores = saveRegs |> List.rev |> List.map X86_64.POP
            saves
            @ [X86_64.MOV_reg (X86_64.RAX, addrReg); X86_64.CALL streamRefCountDecHelperLabel]
            @ restores
        | LIR.GenericHeap ->
            genRefCountDecGeneric ctx addrReg payloadSize metadata)

let private emitRefCountIncBuffer (ctx: FuncCtx) (skipTagged: bool) (str: LIR.Operand) : Result<X86_64.Instr list, string> =
    match str with
    | LIR.StringSymbol _ -> Ok []  // Literal string - no refcount
    | LIR.Reg reg ->
        resolveReg reg
        |> Result.map (fun addrReg ->
            // Dynamic buffer: [refcount:8][length:8][data:N][padding:P]
            // Sentinel value INT64_MAX means literal (read-only)
            let skipLabel = freshLabel "rcinc_str_skip"
            let literalLabel = freshLabel "rcinc_str_lit"
            let refAddrReg, refValueReg, preserveRegs, restoreRegs =
                if addrReg = scratch then
                    X86_64.RCX,
                    X86_64.RDX,
                    [X86_64.PUSH scratch
                     X86_64.PUSH X86_64.RCX
                     X86_64.PUSH X86_64.RDX
                     X86_64.MOV_reg (X86_64.RCX, addrReg)],
                    [X86_64.POP X86_64.RDX; X86_64.POP X86_64.RCX; X86_64.POP scratch]
                else
                    let refValueReg =
                        if addrReg = X86_64.RCX then X86_64.RDX else X86_64.RCX
                    addrReg, refValueReg, [X86_64.PUSH refValueReg], [X86_64.POP refValueReg]
            let taggedGuard =
                if skipTagged then
                    [X86_64.MOV_reg (scratch, refAddrReg)
                     X86_64.AND_imm (scratch, 1)
                     X86_64.Jcc (X86_64.NE, skipLabel)]
                else
                    []
            preserveRegs
            @ [X86_64.TEST_reg (refAddrReg, refAddrReg)
               X86_64.Jcc (X86_64.EQ, skipLabel)]
            @ taggedGuard
            @ [X86_64.MOV_load (refValueReg, refAddrReg, 0)]
            @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL        // scratch = INT64_MAX
            @ [X86_64.CMP_reg (refValueReg, scratch)
               X86_64.Jcc (X86_64.EQ, literalLabel)        // skip if literal
               X86_64.ADD_imm (refValueReg, 1)
               X86_64.MOV_store (refAddrReg, 0, refValueReg)
               X86_64.Label literalLabel]
            @ restoreRegs
            @ [X86_64.Label skipLabel])
    | _ -> Error "dynamic buffer RefCountInc requires StringSymbol or Reg operand"

let private emitRefCountDecBuffer (ctx: FuncCtx) (skipTagged: bool) (str: LIR.Operand) : Result<X86_64.Instr list, string> =
    match str with
    | LIR.StringSymbol _ -> Ok []  // Literal string - no refcount
    | LIR.Reg reg ->
        resolveReg reg
        |> Result.map (fun addrReg ->
            let skipLabel = freshLabel "rcdec_str_skip"
            let literalLabel = freshLabel "rcdec_str_lit"
            let noFreeLabel = freshLabel "rcdec_str_nofree"
            let leakDec = genLeakCounterDec ctx
            let refAddrReg, refValueReg, preserveRegs, restoreRegs =
                if addrReg = scratch then
                    X86_64.RCX,
                    X86_64.RDX,
                    [X86_64.PUSH scratch
                     X86_64.PUSH X86_64.RCX
                     X86_64.PUSH X86_64.RDX
                     X86_64.MOV_reg (X86_64.RCX, addrReg)],
                    [X86_64.POP X86_64.RDX; X86_64.POP X86_64.RCX; X86_64.POP scratch]
                else
                    let refValueReg =
                        if addrReg = X86_64.RCX then X86_64.RDX else X86_64.RCX
                    addrReg, refValueReg, [X86_64.PUSH refValueReg], [X86_64.POP refValueReg]
            let taggedGuard =
                if skipTagged then
                    [X86_64.MOV_reg (scratch, refAddrReg)
                     X86_64.AND_imm (scratch, 1)
                     X86_64.Jcc (X86_64.NE, skipLabel)]
                else
                    []
            preserveRegs
            @ [X86_64.TEST_reg (refAddrReg, refAddrReg)
               X86_64.Jcc (X86_64.EQ, skipLabel)]
            @ taggedGuard
            @ [X86_64.MOV_load (refValueReg, refAddrReg, 0)]
            @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
            @ [X86_64.CMP_reg (refValueReg, scratch)
               X86_64.Jcc (X86_64.EQ, literalLabel)
               X86_64.SUB_imm (refValueReg, 1)
               X86_64.MOV_store (refAddrReg, 0, refValueReg)
               X86_64.TEST_reg (refValueReg, refValueReg)
               X86_64.Jcc (X86_64.NE, noFreeLabel)]
            // String refcount hit zero - decrement leak counter
            @ leakDec
            @ [X86_64.Label noFreeLabel
               X86_64.Label literalLabel]
            @ restoreRegs
            @ [X86_64.Label skipLabel])
    | _ -> Error "dynamic buffer RefCountDec requires StringSymbol or Reg operand"

let internal emitRefCountIncString (ctx: FuncCtx) (str: LIR.Operand) =
    emitRefCountIncBuffer ctx false str

let internal emitRefCountDecString (ctx: FuncCtx) (str: LIR.Operand) =
    emitRefCountDecBuffer ctx false str

let internal emitRefCountIncInt (ctx: FuncCtx) (value: LIR.Operand) =
    emitRefCountIncBuffer ctx true value

let internal emitRefCountDecInt (ctx: FuncCtx) (value: LIR.Operand) =
    emitRefCountDecBuffer ctx true value
