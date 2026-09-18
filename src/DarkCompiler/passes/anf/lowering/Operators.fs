// Operators.fs - Lower numeric operators and structural equality into ANF.

module LoweringOperators

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open TypeSubstitution
open ClosureAnalysis
open LiftExpressions
open LiftFunctions

let convertBinOp (op: AST.BinOp) : ANF.BinOp =
    match op with
    | AST.Add -> ANF.Add
    | AST.Sub -> ANF.Sub
    | AST.Mul -> ANF.Mul
    | AST.Div -> ANF.Div
    | AST.Mod -> ANF.Mod
    | AST.Pow -> Crash.crash "Exponentiation must lower through the canonical numeric power function"
    | AST.Shl -> ANF.Shl
    | AST.Shr -> ANF.Shr
    | AST.BitAnd -> ANF.BitAnd
    | AST.BitOr -> ANF.BitOr
    | AST.BitXor -> ANF.BitXor
    | AST.Eq -> ANF.Eq
    | AST.Neq -> ANF.Neq
    | AST.Lt -> ANF.Lt
    | AST.Gt -> ANF.Gt
    | AST.Lte -> ANF.Lte
    | AST.Gte -> ANF.Gte
    | AST.And -> ANF.And
    | AST.Or -> ANF.Or
    | AST.StringConcat -> ANF.Add  // Never reached - StringConcat handled as CExpr

/// Arbitrary-precision Int values use canonical decimal-string storage. Route
/// operations through the pure Int stdlib implementation instead of native
/// machine-word primitives.
let internal integerFunctionForBinOp (operandType: AST.Type) (op: AST.BinOp) : string option =
    let moduleName =
        match op, operandType with
        | AST.Pow, AST.TInt -> Some "Stdlib.Int"
        | AST.Pow, AST.TInt8 -> Some "Stdlib.Int8"
        | AST.Pow, AST.TInt16 -> Some "Stdlib.Int16"
        | AST.Pow, AST.TInt32 -> Some "Stdlib.Int32"
        | AST.Pow, AST.TInt64 -> Some "Stdlib.Int64"
        | AST.Pow, AST.TUInt8 -> Some "Stdlib.UInt8"
        | AST.Pow, AST.TUInt16 -> Some "Stdlib.UInt16"
        | AST.Pow, AST.TUInt32 -> Some "Stdlib.UInt32"
        | AST.Pow, AST.TUInt64 -> Some "Stdlib.UInt64"
        | AST.Pow, AST.TFloat64 -> Some "Stdlib.Float"
        | AST.Mod, AST.TFloat64 -> Some "Stdlib.Float"
        | _, AST.TInt -> Some "Stdlib.Int"
        | _, AST.TInt128 -> Some "Stdlib.Int128"
        | _, AST.TUInt128 -> Some "Stdlib.UInt128"
        | _ -> None
    let functionName =
        match op, operandType with
        | AST.Add, _ -> Some "add"
        | AST.Sub, _ -> Some "subtract"
        | AST.Mul, _ -> Some "multiply"
        | AST.Div, _ -> Some "divide"
        | AST.Mod, _ -> Some "mod"
        | AST.Pow, _ -> Some "power"
        | AST.Shl, _ -> Some "shiftLeft"
        | AST.Shr, _ -> Some "shiftRight"
        | AST.BitAnd, _ -> Some "bitwiseAnd"
        | AST.BitOr, _ -> Some "bitwiseOr"
        | AST.BitXor, _ -> Some "bitwiseXor"
        | AST.Lt, _ -> Some "lessThan"
        | AST.Gt, _ -> Some "greaterThan"
        | AST.Lte, _ -> Some "lessThanOrEqualTo"
        | AST.Gte, _ -> Some "greaterThanOrEqualTo"
        | AST.Eq, _ | AST.Neq, _ | AST.And, _ | AST.Or, _ | AST.StringConcat, _ -> None
    match moduleName, functionName with
    | Some moduleName, Some functionName -> Some $"{moduleName}.{functionName}"
    | _ -> None

/// Convert AST.UnaryOp to ANF.UnaryOp
let convertUnaryOp (op: AST.UnaryOp) : ANF.UnaryOp =
    match op with
    | AST.Neg -> ANF.Neg
    | AST.Not -> ANF.Not
    | AST.BitNot -> ANF.BitNot

/// Check if a type requires structural equality (compound types)
let isCompoundType (typ: AST.Type) : bool =
    match typ with
    | AST.TTuple _ -> true
    | AST.TRecord _ -> true
    | AST.TSum _ -> true
    | _ -> false

/// Generate structural equality comparison for compound types.
/// Returns a list of bindings and the final result atom that holds the comparison result.
let rec generateStructuralEquality
    (leftAtom: ANF.Atom)
    (rightAtom: ANF.Atom)
    (typ: AST.Type)
    (varGen: ANF.VarGen)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    : (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen =
    // Keep bindings in reverse order during construction to avoid quadratic
    // list appends when comparing deeply nested structures.
    let addForwardBindingsToRev
        (accRev: (ANF.TempId * ANF.CExpr) list)
        (bindings: (ANF.TempId * ANF.CExpr) list)
        : (ANF.TempId * ANF.CExpr) list =
        List.fold (fun acc binding -> binding :: acc) accRev bindings

    let combineComparisonResult
        (accResult: ANF.Atom option)
        (nextResult: ANF.Atom)
        (accBindingsRev: (ANF.TempId * ANF.CExpr) list)
        (vg: ANF.VarGen)
        : (ANF.Atom option * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) =
        match accResult with
        | None ->
            (Some nextResult, accBindingsRev, vg)
        | Some previousResult ->
            let (andVar, vg') = ANF.freshVar vg
            let andExpr = ANF.Prim (ANF.And, previousResult, nextResult)
            let updatedBindingsRev = (andVar, andExpr) :: accBindingsRev
            (Some (ANF.Var andVar), updatedBindingsRev, vg')

    let finalizeBindings
        (accResult: ANF.Atom option)
        (accBindingsRev: (ANF.TempId * ANF.CExpr) list)
        (vg: ANF.VarGen)
        : (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen =
        match accResult with
        | Some resultAtom ->
            (List.rev accBindingsRev, resultAtom, vg)
        | None ->
            let (trueVar, vg') = ANF.freshVar vg
            let bindingsRev = (trueVar, ANF.Atom (ANF.BoolLiteral true)) :: accBindingsRev
            (List.rev bindingsRev, ANF.Var trueVar, vg')

    let primitiveEquality (valueType: AST.Type) (left: ANF.Atom) (right: ANF.Atom) : ANF.CExpr =
        match valueType with
        | AST.TInt128 -> ANF.Call ("Stdlib.Int128.__equals", [left; right])
        | AST.TUInt128 -> ANF.Call ("Stdlib.UInt128.__equals", [left; right])
        | AST.TString | AST.TChar | AST.TInt -> ANF.Call ("__string_eq", [left; right])
        | _ -> ANF.Prim (ANF.Eq, left, right)

    match typ with
    | AST.TTuple elemTypes ->
        let rec compareElements
            (index: int)
            (types: AST.Type list)
            (accResult: ANF.Atom option)
            (accBindingsRev: (ANF.TempId * ANF.CExpr) list)
            (vg: ANF.VarGen)
            : (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen =
            match types with
            | [] ->
                finalizeBindings accResult accBindingsRev vg
            | elemType :: restTypes ->
                let (leftElemVar, vg1) = ANF.freshVar vg
                let leftGet = ANF.TupleGet (leftAtom, index)
                let (rightElemVar, vg2) = ANF.freshVar vg1
                let rightGet = ANF.TupleGet (rightAtom, index)
                let withElemBindingsRev =
                    addForwardBindingsToRev
                        accBindingsRev
                        [ (leftElemVar, leftGet); (rightElemVar, rightGet) ]

                let (elementResult, withComparisonBindingsRev, vg3) =
                    if isCompoundType elemType then
                        let (nestedBindings, nestedResult, vgNested) =
                            generateStructuralEquality
                                (ANF.Var leftElemVar)
                                (ANF.Var rightElemVar)
                                elemType
                                vg2
                                typeReg
                                variantLookup
                        let updatedBindingsRev =
                            addForwardBindingsToRev withElemBindingsRev nestedBindings
                        (nestedResult, updatedBindingsRev, vgNested)
                    else
                        let (cmpVar, vgCmp) = ANF.freshVar vg2
                        let cmpExpr = primitiveEquality elemType (ANF.Var leftElemVar) (ANF.Var rightElemVar)
                        let updatedBindingsRev = (cmpVar, cmpExpr) :: withElemBindingsRev
                        (ANF.Var cmpVar, updatedBindingsRev, vgCmp)

                let (updatedResult, updatedBindingsRev, vg4) =
                    combineComparisonResult accResult elementResult withComparisonBindingsRev vg3

                compareElements (index + 1) restTypes updatedResult updatedBindingsRev vg4

        compareElements 0 elemTypes None [] varGen

    | AST.TRecord (typeName, typeArgs) ->
        match Map.tryFind typeName typeReg with
        | None ->
            let (cmpVar, vg') = ANF.freshVar varGen
            ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')
        | Some recordInfo ->
            let descriptor =
                recordDescriptor
                    {
                        TypeName = typeName
                        TypeArgs = typeArgs
                    }
                    recordInfo
            let concreteFields =
                match buildDeclaredRecordFieldSubst recordInfo typeArgs with
                | Some subst ->
                    recordInfo.Fields
                    |> List.map (fun (name, fieldType) -> (name, applySubstToType subst fieldType))
                | None ->
                    recordInfo.Fields

            let rec compareFields
                (index: int)
                (fieldList: (string * AST.Type) list)
                (accResult: ANF.Atom option)
                (accBindingsRev: (ANF.TempId * ANF.CExpr) list)
                (vg: ANF.VarGen)
                : (ANF.TempId * ANF.CExpr) list * ANF.Atom * ANF.VarGen =
                match fieldList with
                | [] ->
                    finalizeBindings accResult accBindingsRev vg
                | (_, fieldType) :: restFields ->
                    let (leftFieldVar, vg1) = ANF.freshVar vg
                    let leftGet = ANF.RecordGet (descriptor, leftAtom, index)
                    let (rightFieldVar, vg2) = ANF.freshVar vg1
                    let rightGet = ANF.RecordGet (descriptor, rightAtom, index)
                    let withFieldBindingsRev =
                        addForwardBindingsToRev
                            accBindingsRev
                            [ (leftFieldVar, leftGet); (rightFieldVar, rightGet) ]

                    let (fieldResult, withComparisonBindingsRev, vg3) =
                        if isCompoundType fieldType then
                            let (nestedBindings, nestedResult, vgNested) =
                                generateStructuralEquality
                                    (ANF.Var leftFieldVar)
                                    (ANF.Var rightFieldVar)
                                    fieldType
                                    vg2
                                    typeReg
                                    variantLookup
                            let updatedBindingsRev =
                                addForwardBindingsToRev withFieldBindingsRev nestedBindings
                            (nestedResult, updatedBindingsRev, vgNested)
                        else
                            let (cmpVar, vgCmp) = ANF.freshVar vg2
                            let cmpExpr = primitiveEquality fieldType (ANF.Var leftFieldVar) (ANF.Var rightFieldVar)
                            let updatedBindingsRev = (cmpVar, cmpExpr) :: withFieldBindingsRev
                            (ANF.Var cmpVar, updatedBindingsRev, vgCmp)

                    let (updatedResult, updatedBindingsRev, vg4) =
                        combineComparisonResult accResult fieldResult withComparisonBindingsRev vg3

                    compareFields (index + 1) restFields updatedResult updatedBindingsRev vg4

            compareFields 0 concreteFields None [] varGen

    | AST.TSum (typeName, _) ->
        let hasAnyPayload =
            variantLookup
            |> Map.exists (fun _ (tName, _, _, payloadType) ->
                tName = typeName && payloadType.IsSome)

        if not hasAnyPayload then
            let (cmpVar, vg') = ANF.freshVar varGen
            ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')
        else
            let (leftTagVar, vg1) = ANF.freshVar varGen
            let (rightTagVar, vg2) = ANF.freshVar vg1
            let (tagEqVar, vg3) = ANF.freshVar vg2
            let (leftPayloadVar, vg4) = ANF.freshVar vg3
            let (rightPayloadVar, vg5) = ANF.freshVar vg4
            let (payloadEqVar, vg6) = ANF.freshVar vg5
            let (resultVar, vg7) = ANF.freshVar vg6

            // UUID is a nominal single-case sum over an immutable UInt128
            // block, so its payload needs value equality rather than pointer
            // equality.
            // Other sums retain the established primitive payload comparison;
            // multi-variant, heterogeneous payload dispatch is a separate
            // structural-equality design boundary.
            let payloadComparison =
                if typeName = "Uuid" then
                    ANF.Call ("Stdlib.UInt128.__equals", [ANF.Var leftPayloadVar; ANF.Var rightPayloadVar])
                else
                    ANF.Prim (ANF.Eq, ANF.Var leftPayloadVar, ANF.Var rightPayloadVar)

            let bindings = [
                (leftTagVar, ANF.TupleGet (leftAtom, 0))
                (rightTagVar, ANF.TupleGet (rightAtom, 0))
                (tagEqVar, ANF.Prim (ANF.Eq, ANF.Var leftTagVar, ANF.Var rightTagVar))
                (leftPayloadVar, ANF.TupleGet (leftAtom, 1))
                (rightPayloadVar, ANF.TupleGet (rightAtom, 1))
                (payloadEqVar, payloadComparison)
                (resultVar, ANF.Prim (ANF.And, ANF.Var tagEqVar, ANF.Var payloadEqVar))
            ]
            (bindings, ANF.Var resultVar, vg7)

    | _ ->
        let (cmpVar, vg') = ANF.freshVar varGen
        ([(cmpVar, ANF.Prim (ANF.Eq, leftAtom, rightAtom))], ANF.Var cmpVar, vg')

/// Infer the type of an expression using type environment and registries
/// Used for type-directed field lookup in record access
