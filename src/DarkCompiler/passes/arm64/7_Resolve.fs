// 7_ARM64_Resolve.fs - ARM64 symbolic literal-pool collection
//
// Collects literal pools for symbolic string and float label references before
// ARM64 encoding assigns concrete offsets.

module ARM64_Resolve

type private PoolState = {
    StringPool: LiteralPool.StringPool
    FloatPool: LiteralPool.FloatPool
}

let private addLabelRefToPools
    (state: PoolState)
    (labelRef: ARM64Symbolic.LabelRef)
    : PoolState =
    match labelRef with
    | ARM64Symbolic.CodeLabel _ -> state
    | ARM64Symbolic.DataLabel dataRef ->
        match dataRef with
        | ARM64Symbolic.Named _ -> state
        | ARM64Symbolic.StringLiteral value ->
            let (_, pool') = LiteralPool.addString state.StringPool value
            { state with StringPool = pool' }
        | ARM64Symbolic.FloatLiteral value ->
            let (_, pool') = LiteralPool.addFloat state.FloatPool value
            { state with FloatPool = pool' }

let collectPools
    (instructions: ARM64Symbolic.Instr list)
    : LiteralPool.StringPool * LiteralPool.FloatPool =
    let initialState = { StringPool = LiteralPool.emptyStringPool; FloatPool = LiteralPool.emptyFloatPool }

    let updatePools (state: PoolState) (instr: ARM64Symbolic.Instr) : PoolState =
        match instr with
        | ARM64Symbolic.ADRP (_, labelRef)
        | ARM64Symbolic.ADD_label (_, _, labelRef)
        | ARM64Symbolic.ADR (_, labelRef) ->
            addLabelRefToPools state labelRef
        | _ -> state

    let rec loop state remaining =
        match remaining with
        | [] -> state
        | instr :: rest ->
            let nextState = updatePools state instr
            loop nextState rest
    let pools = loop initialState instructions
    (pools.StringPool, pools.FloatPool)
