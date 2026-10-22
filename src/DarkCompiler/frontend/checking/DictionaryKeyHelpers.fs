// DictionaryKeyHelpers.fs - Generate structural hashes consistent with canonical key comparison.

module DictionaryKeyHelpers

open AST
open CheckingTypes
open ComparisonPlanning
open EqualityHelpers

let internal hashHelperNameWithPolicy allowBlob typ =
    let typ = canonicalEqualityType Map.empty typ
    match typ with
    | TInt64 -> "__hash_i64"
    | TBool -> "__hash_bool"
    | TString -> "__hash_str"
    | TBlob when allowBlob -> "__hash_blob"
    | _ -> compareHelperName typ + (if allowBlob then "_hash" else "_public_hash")

let internal hashHelperName typ = hashHelperNameWithPolicy true typ

/// Primitive public keys need no runtime validation, even before nominal metadata exists.
let internal isPrimitivePublicKey typ =
    match typ with
    | TUnit | TBool | TString | TChar | TDateTime | TFloat64
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 -> true
    | _ -> false

/// Private HAMTs permit Blob identity keys; public keys reject present Blob values.
let internal keyMayContainBlob aliasReg (records: IndexedTypeRegistry) (sums: IndexedSumTypeRegistry) typ =
    let rec inspect seen typ =
        let typ = resolveType aliasReg typ
        if Set.contains typ seen then false
        else
            let seen = Set.add typ seen
            let any types = types |> List.exists (inspect seen)
            match typ with
            | TBlob -> true
            | TList element -> inspect seen element
            | TTuple types | TEnumFields types -> any types
            | TDict (key, value) -> any [key; value]
            | TRecord (name, args) ->
                match Map.tryFind name records with
                | None -> Crash.crash $"Dictionary key record metadata missing: {name}"
                | Some info ->
                    match buildSubstitution info.TypeParams args with
                    | Error error -> Crash.crash error
                    | Ok subst -> info.Fields |> List.map (snd >> applyTypeArguments subst) |> any
            | TSum (name, args) ->
                match Map.tryFind name sums with
                | None -> Crash.crash $"Dictionary key sum metadata missing: {name}"
                | Some info ->
                    match buildSubstitution info.TypeParams args with
                    | Error error -> Crash.crash error
                    | Ok subst -> info.Variants |> List.choose (fun variant -> variant.Payload |> Option.map (applyTypeArguments subst)) |> any
            | _ -> false
    inspect Set.empty typ

/// Hash equal keys identically, including NaNs and both representations of zero.
/// Compound hashes follow canonical field/entry order and hash their children.
let internal generateHashHelpers allowBlob aliasReg (records: IndexedTypeRegistry) (sums: IndexedSumTypeRegistry) roots =
    let call name args = Call (name, NonEmptyList.fromList args)
    let nameFor = hashHelperNameWithPolicy allowBlob
    let hash typ value = call (nameFor typ) [value]
    let combine left right = BinOp (Add, BinOp (Mul, left, Int64Literal 31L), right)
    let rec ensure typ generated =
        let typ = resolveType aliasReg typ |> canonicalEqualityType Map.empty
        let name = nameFor typ
        match typ with
        | TInt64 | TBool | TString -> generated
        | TBlob when allowBlob -> generated
        | _ when Map.containsKey name generated -> generated
        | _ ->
            let value = Var "__key"
            let placeholder : FunctionDef = {
                Name = name; TypeParams = []; Params = NonEmptyList.singleton ("__key", typ)
                ReturnType = TInt64; Body = Int64Literal 0L; Recursion = None
            }
            let reserved = Map.add name placeholder generated
            let hashChildren children state =
                let next = children |> List.fold (fun current (childType, _) -> ensure childType current) state
                let body = children |> List.fold (fun acc (childType, expr) -> combine acc (hash childType expr)) (Int64Literal 17L)
                body, next
            let body, next =
                match typ with
                | TFloat64 ->
                    If (BinOp (Eq, value, FloatLiteral 0.0), Int64Literal 0L,
                        If (BinOp (Neq, value, value), Int64Literal 1L,
                            call "__string_hash" [call "Stdlib.Float.toString" [value]])), reserved
                | TUnit -> Int64Literal 0L, reserved
                | TTuple types | TEnumFields types ->
                    types |> List.mapi (fun i t -> t, TupleAccess (value, i)) |> fun children -> hashChildren children reserved
                | TList element ->
                    let next = ensure element reserved
                    Match (value, [
                        makeSimpleMatchCase (PList []) (Int64Literal 17L)
                        makeSimpleMatchCase (PListCons ([PVar "__head"], PVar "__tail"))
                            (combine (hash typ (Var "__tail")) (hash element (Var "__head")))
                    ]), next
                | TRecord (recordName, typeArgs) ->
                    match Map.tryFind recordName records with
                    | None -> Crash.crash $"Dictionary key metadata missing for {typ}"
                    | Some info ->
                        match buildSubstitution info.TypeParams typeArgs with
                        | Error error -> Crash.crash error
                        | Ok subst ->
                            info.Fields
                            |> List.sortBy fst
                            |> List.map (fun (field, fieldType) -> resolveType aliasReg (applyTypeArguments subst fieldType), RecordAccess (value, field))
                            |> fun children -> hashChildren children reserved
                | TSum (sumName, typeArgs) ->
                    match Map.tryFind sumName sums with
                    | None -> Crash.crash $"Dictionary key metadata missing for {typ}"
                    | Some info ->
                        match buildSubstitution info.TypeParams typeArgs with
                        | Error error -> Crash.crash error
                        | Ok subst ->
                            let cases, final =
                                info.Variants
                                |> List.sortBy (fun variant -> variant.Name)
                                |> List.mapi (fun i variant -> i, variant)
                                |> List.fold (fun (cases, current) (i, variant) ->
                                    let tag = Int64Literal (int64 i)
                                    let payload, body, next =
                                        match variant.Payload with
                                        | None -> None, tag, current
                                        | Some fieldType ->
                                            let concrete = resolveType aliasReg (applyTypeArguments subst fieldType)
                                            Some (PVar "__payload"), combine tag (hash concrete (Var "__payload")), ensure concrete current
                                    makeSimpleMatchCase (PConstructor ($"{sumName}.{variant.Name}", payload)) body :: cases, next) ([], reserved)
                            Match (value, List.rev cases), final
                | TDict (key, item) ->
                    let entries = TList (TTuple [key; item])
                    let next = ensure entries reserved
                    hash entries (TypeApp ("Stdlib.Dict.toList", [key; item], NonEmptyList.singleton value)), next
                | TBlob | TFunction _ | TStream _ | TRawPtr | TRuntimeError | TVar _ ->
                    RuntimeError "Unsupported dictionary key type", reserved
                | _ ->
                    let renderer, renderers = ValueRendering.keyStringRenderer records sums typ
                    let next = renderers |> List.fold (fun current helper -> Map.add helper.Name helper current) reserved
                    call "__string_hash" [call renderer [value]], next
            Map.add name { placeholder with Body = body } next
    roots |> Set.fold (fun generated typ -> ensure typ generated) Map.empty
