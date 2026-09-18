// Aggregates.fs - Build skew-list storage and bind typed deconstruction patterns.

module LoweringAggregates

open MemoryModel
open ANF
open TypeRegistries
open LiftExpressions
open LiftFunctions

let internal buildSkewListLiteral
    (listType: AST.Type)
    (elements: (ANF.Atom * AST.Type) list)
    (varGen: ANF.VarGen)
    (initialBindings: (ANF.TempId * ANF.CExpr) list)
    : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =

    let tagRawPtr
        (tag: int64)
        (ptrVar: ANF.TempId)
        (vg: ANF.VarGen)
        (bindingsRev: (ANF.TempId * ANF.CExpr) list)
        : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
        let (taggedRawVar, vg1) = ANF.freshVar vg
        let tagExpr = ANF.Prim (ANF.BitOr, ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 tag))
        let (taggedVar, vg2) = ANF.freshVar vg1
        let typedExpr = ANF.TypedAtom (ANF.Var taggedRawVar, listType)
        (ANF.Var taggedVar, (taggedVar, typedExpr) :: (taggedRawVar, tagExpr) :: bindingsRev, vg2)

    let allocLeaf
        (value: ANF.Atom)
        (valueType: AST.Type)
        (vg: ANF.VarGen)
        (bindingsRev: (ANF.TempId * ANF.CExpr) list)
        : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
        let (ptrVar, vg1) = ANF.freshVar vg
        let (valueVar, vg2) = ANF.freshVar vg1
        let (rcVar, vg3) = ANF.freshVar vg2
        let nextBindings =
            (rcVar, ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), ANF.IntLiteral (ANF.Int64 1L)))
            :: (valueVar, ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), value, valueType))
            :: (ptrVar, ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 16L)))
            :: bindingsRev
        tagRawPtr 2L ptrVar vg3 nextBindings

    let allocNode
        (value: ANF.Atom)
        (valueType: AST.Type)
        (left: ANF.Atom)
        (right: ANF.Atom)
        (vg: ANF.VarGen)
        (bindingsRev: (ANF.TempId * ANF.CExpr) list)
        : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
        let (ptrVar, vg1) = ANF.freshVar vg
        let (valueVar, vg2) = ANF.freshVar vg1
        let (leftVar, vg3) = ANF.freshVar vg2
        let (rightVar, vg4) = ANF.freshVar vg3
        let (rcVar, vg5) = ANF.freshVar vg4
        let nextBindings =
            (rcVar, ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 24L), ANF.IntLiteral (ANF.Int64 1L)))
            :: (rightVar, ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), right, listType))
            :: (leftVar, ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), left, listType))
            :: (valueVar, ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), value, valueType))
            :: (ptrVar, ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 32L)))
            :: bindingsRev
        tagRawPtr 3L ptrVar vg5 nextBindings

    let allocDigit
        (weight: int)
        (length: int)
        (tree: ANF.Atom)
        (rest: ANF.Atom)
        (vg: ANF.VarGen)
        (bindingsRev: (ANF.TempId * ANF.CExpr) list)
        : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
        let (ptrVar, vg1) = ANF.freshVar vg
        let (weightVar, vg2) = ANF.freshVar vg1
        let (lengthVar, vg3) = ANF.freshVar vg2
        let (treeVar, vg4) = ANF.freshVar vg3
        let (restVar, vg5) = ANF.freshVar vg4
        let (rcVar, vg6) = ANF.freshVar vg5
        let nextBindings =
            (rcVar, ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 32L), ANF.IntLiteral (ANF.Int64 1L)))
            :: (restVar, ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 24L), rest, listType))
            :: (treeVar, ANF.RawSlotInit (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 16L), tree, listType))
            :: (lengthVar, ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 8L), ANF.IntLiteral (ANF.Int64 (int64 length))))
            :: (weightVar, ANF.RawWriteWord (ANF.Var ptrVar, ANF.IntLiteral (ANF.Int64 0L), ANF.IntLiteral (ANF.Int64 (int64 weight))))
            :: (ptrVar, ANF.RawAlloc (ANF.IntLiteral (ANF.Int64 40L)))
            :: bindingsRev
        tagRawPtr 1L ptrVar vg6 nextBindings

    let rec buildTrees
        (remaining: (ANF.Atom * AST.Type) list)
        (vg: ANF.VarGen)
        (bindings: (ANF.TempId * ANF.CExpr) list)
        : (int * ANF.Atom) list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
        match remaining with
        | [] -> ([], bindings, vg)
        | (value, valueType) :: rest ->
            let (trees, bindings1, vg1) = buildTrees rest vg bindings
            match trees with
            | (firstWeight, firstTree) :: (secondWeight, secondTree) :: suffix when firstWeight = secondWeight ->
                let (tree, bindings2, vg2) = allocNode value valueType firstTree secondTree vg1 bindings1
                ((firstWeight + secondWeight + 1, tree) :: suffix, bindings2, vg2)
            | _ ->
                let (tree, bindings2, vg2) = allocLeaf value valueType vg1 bindings1
                ((1, tree) :: trees, bindings2, vg2)

    let rec buildDigits
        (trees: (int * ANF.Atom) list)
        (vg: ANF.VarGen)
        (bindings: (ANF.TempId * ANF.CExpr) list)
        : ANF.Atom * int * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
        match trees with
        | [] -> (ANF.IntLiteral (ANF.Int64 0L), 0, bindings, vg)
        | (weight, tree) :: rest ->
            let (restAtom, restLength, bindings1, vg1) = buildDigits rest vg bindings
            let length = weight + restLength
            let (digit, bindings2, vg2) = allocDigit weight length tree restAtom vg1 bindings1
            (digit, length, bindings2, vg2)

    let (trees, treeBindings, treeVarGen) = buildTrees elements varGen (List.rev initialBindings)
    let (root, _, bindings, finalVarGen) = buildDigits trees treeVarGen treeBindings
    (root, List.rev bindings, finalVarGen)

/// Prepare every projection for one binding before its continuation is
/// lowered. The type checker has already proved the complete unit/tuple shape.
let rec internal lowerLetPatternBindings
    (pattern: CheckedAST.LetPattern)
    (sourceAtom: ANF.Atom)
    (sourceType: AST.Type)
    (env: VarEnv)
    (bindingsRev: (ANF.TempId * ANF.CExpr) list)
    (varGen: ANF.VarGen)
    : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
    match pattern with
    | CheckedAST.LPUnit | CheckedAST.LPWildcard -> Ok (env, bindingsRev, varGen)
    | CheckedAST.LPVariable name ->
        let (bindingId, nextVarGen) = ANF.freshVar varGen
        let env' = Map.add name (bindingId, sourceType) env
        Ok (env', (bindingId, ANF.TypedAtom (sourceAtom, sourceType)) :: bindingsRev, nextVarGen)
    | CheckedAST.LPTuple (first, second, rest) ->
        let patterns = first :: second :: rest
        match sourceType with
        | AST.TTuple elementTypes when List.length patterns = List.length elementTypes ->
            List.zip patterns elementTypes
            |> List.indexed
            |> List.fold (fun result (index, (innerPattern, innerType)) ->
                result
                |> Result.bind (fun (currentEnv, currentBindings, currentVarGen) ->
                    let (rawId, afterRaw) = ANF.freshVar currentVarGen
                    let (typedId, afterTyped) = ANF.freshVar afterRaw
                    let projectionBindings =
                        (typedId, ANF.TypedAtom (ANF.Var rawId, innerType))
                        :: (rawId, ANF.TupleGet (sourceAtom, index))
                        :: currentBindings
                    lowerLetPatternBindings
                        innerPattern
                        (ANF.Var typedId)
                        innerType
                        currentEnv
                        projectionBindings
                        afterTyped)) (Ok (env, bindingsRev, varGen))
        | _ -> Error "Let tuple pattern reached ANF lowering with an incompatible type"

let rec internal letPatternAcceptsType
    (pattern: CheckedAST.LetPattern)
    (valueType: AST.Type)
    : bool =
    match pattern, valueType with
    | CheckedAST.LPVariable _, _ | CheckedAST.LPWildcard, _ -> true
    | CheckedAST.LPUnit, AST.TUnit -> true
    | CheckedAST.LPTuple (first, second, rest), AST.TTuple elementTypes ->
        let patterns = first :: second :: rest
        List.length patterns = List.length elementTypes
        && List.forall2 letPatternAcceptsType patterns elementTypes
    | _ -> false
