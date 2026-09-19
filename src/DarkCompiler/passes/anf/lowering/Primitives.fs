// Primitives.fs - Resolve intrinsic calls and primitive source representations.

module LoweringPrimitives

open MemoryModel
open ANF

let internal eqHelperDispatchMarker = "__dark_internal_eq_helper_dispatch"

let internal canonicalBufferKindForType (typ: AST.Type) : MemoryModel.CanonicalBufferKind option =
    match typ with
    | AST.TString -> Some MemoryModel.Utf8String
    | AST.TChar -> Some MemoryModel.GraphemeCluster
    | _ -> None

let internal materializeComparisonPlan (targetType: AST.Type) (args: CheckedAST.Expr list) : CheckedAST.Expr =
    match args with
    | [leftExpr; rightExpr] ->
        match targetType with
        | AST.TList _
        | AST.TDict _
        | AST.TTuple _
        | AST.TRecord _
        | AST.TSum _ ->
            CheckedAST.Call (
                ComparisonPlanning.eqHelperName targetType,
                AST.NonEmptyList.fromList [leftExpr; rightExpr]
            )
        | AST.TFunction _ ->
            let leftName = "__comparison_plan_left"
            let rightName = "__comparison_plan_right"
            CheckedAST.Let (
                CheckedAST.LPVariable leftName,
                leftExpr,
                CheckedAST.Let (
                    CheckedAST.LPVariable rightName,
                    rightExpr,
                    CheckedAST.If (
                        CheckedAST.BinOp (
                            AST.Eq,
                            CheckedAST.TupleAccess (CheckedAST.Var leftName, 1),
                            CheckedAST.TupleAccess (CheckedAST.Var rightName, 1)
                        ),
                        CheckedAST.IndirectApply (
                            CheckedAST.TupleAccess (CheckedAST.Var leftName, 1),
                            AST.NonEmptyList.fromList [CheckedAST.Var leftName; CheckedAST.Var rightName]
                        ),
                        CheckedAST.BoolLiteral false
                    )
                )
            )
        | AST.TInt ->
            CheckedAST.Call (
                "Darklang.Stdlib.Int.__equals",
                AST.NonEmptyList.fromList [leftExpr; rightExpr]
            )
        | _ -> CheckedAST.BinOp (AST.Eq, leftExpr, rightExpr)
    | _ -> Crash.crash "Comparison plan expected exactly two operands"

/// Variant lookup - maps variant names to (type name, type params, tag index, field types)
type VariantLookup = Map<string, (string * string list * int * AST.Type list)>

let sumTypeNamesFromVariantLookup (variantLookup: VariantLookup) : Set<string> =
    variantLookup
    |> Map.fold (fun names _ (typeName, _, _, _) -> Set.add typeName names) Set.empty

let internal tryFindVariant
    (constructorReference: CheckedAST.ConstructorReference)
    (variantName: string)
    (variantLookup: VariantLookup)
    : (string * string list * int * AST.Type list) option =
    Map.tryFind $"{constructorReference.TypeName}.{variantName}" variantLookup

let internal tryFindVariantForType
    (variantName: string)
    (sourceType: AST.Type)
    (variantLookup: VariantLookup)
    : (string * string list * int * AST.Type list) option =
    match sourceType with
    | AST.TSum (typeName, _)
    | AST.TRecord (typeName, _) ->
        Map.tryFind $"{typeName}.{variantName}" variantLookup
        |> Option.orElseWith (fun () -> Map.tryFind variantName variantLookup)
    | _ -> Map.tryFind variantName variantLookup

let internal int128ToCanonicalString (value: System.Int128) : string =
    value.ToString(System.Globalization.CultureInfo.InvariantCulture)

let internal uint128ToCanonicalString (value: System.UInt128) : string =
    value.ToString(System.Globalization.CultureInfo.InvariantCulture)

let private uint128Words (value: System.UInt128) : uint64 * uint64 =
    (uint64 value, uint64 (value >>> 64))

let private int128Words (value: System.Int128) : uint64 * uint64 =
    (uint64 value, uint64 (value >>> 64))

let internal int128Construction (value: System.Int128) : ANF.CExpr =
    let (low, high) = int128Words value
    ANF.Call ("Darklang.Stdlib.Int128.__fromWords", [ANF.IntLiteral (ANF.UInt64 low); ANF.IntLiteral (ANF.UInt64 high)])

let internal uint128Construction (value: System.UInt128) : ANF.CExpr =
    let (low, high) = uint128Words value
    ANF.Call ("Darklang.Stdlib.UInt128.__fromWords", [ANF.IntLiteral (ANF.UInt64 low); ANF.IntLiteral (ANF.UInt64 high)])

let internal int128LiteralComparison (valueAtom: ANF.Atom) (value: System.Int128) : ANF.CExpr =
    let (low, high) = int128Words value
    ANF.Call ("Darklang.Stdlib.Int128.__equalsWords", [valueAtom; ANF.IntLiteral (ANF.UInt64 low); ANF.IntLiteral (ANF.UInt64 high)])

let internal uint128LiteralComparison (valueAtom: ANF.Atom) (value: System.UInt128) : ANF.CExpr =
    let (low, high) = uint128Words value
    ANF.Call ("Darklang.Stdlib.UInt128.__equalsWords", [valueAtom; ANF.IntLiteral (ANF.UInt64 low); ANF.IntLiteral (ANF.UInt64 high)])

/// Convert AST.Type to a string for specialization keys
let rec typeToString (ty: AST.Type) : string =
    match ty with
    | AST.TInt64 -> "i64"
    | AST.TInt128 -> "i128"
    | AST.TInt -> "int"
    | AST.TInt32 -> "i32"
    | AST.TInt16 -> "i16"
    | AST.TInt8 -> "i8"
    | AST.TUInt64 -> "u64"
    | AST.TUInt128 -> "u128"
    | AST.TUInt32 -> "u32"
    | AST.TUInt16 -> "u16"
    | AST.TUInt8 -> "u8"
    | AST.TBool -> "bool"
    | AST.TString -> "str"
    | AST.TBlob -> "blob"
    | AST.TChar -> "char"
    | AST.TDateTime -> "datetime"
    | AST.TFloat64 -> "f64"
    | AST.TUnit -> "unit"
    | AST.TRuntimeError -> "runtime_error"
    | AST.TRawPtr -> "ptr"
    | AST.TVar name -> name
    | AST.TRecord (name, args) -> name + (if List.isEmpty args then "" else "<" + (args |> List.map typeToString |> String.concat ",") + ">")
    | AST.TSum (name, args) -> name + "<" + (args |> List.map typeToString |> String.concat ",") + ">"
    | AST.TList elemType -> "List<" + typeToString elemType + ">"
    | AST.TStream elemType -> "Stream<" + typeToString elemType + ">"
    | AST.TDict (keyType, valueType) -> "Dict<" + typeToString keyType + "," + typeToString valueType + ">"
    | AST.TFunction (paramTypes, retType) ->
        "(" + (paramTypes |> List.map typeToString |> String.concat ",") + ")->" + typeToString retType
    | AST.TTuple types -> "(" + (types |> List.map typeToString |> String.concat "*") + ")"

/// Convert a literal pattern into an ANF sized integer
let patternLiteralToSizedInt (pattern: AST.Pattern) : ANF.SizedInt option =
    match pattern with
    | AST.PInt64 n -> Some (ANF.Int64 n)
    | AST.PInt8Literal n -> Some (ANF.Int8 n)
    | AST.PInt16Literal n -> Some (ANF.Int16 n)
    | AST.PInt32Literal n -> Some (ANF.Int32 n)
    | AST.PUInt8Literal n -> Some (ANF.UInt8 n)
    | AST.PUInt16Literal n -> Some (ANF.UInt16 n)
    | AST.PUInt32Literal n -> Some (ANF.UInt32 n)
    | AST.PUInt64Literal n -> Some (ANF.UInt64 n)
    | _ -> None

/// Try to convert a function call to a file I/O intrinsic CExpr
/// Returns Some CExpr if it's a file intrinsic, None otherwise
let tryFileIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Darklang.Stdlib.File.currentDirectory", ([] | [ANF.UnitLiteral]) ->
        Some (ANF.CliNative (ANF.DirectoryCurrent, []))
    | "Darklang.Stdlib.File.listDirectoryPacked", [pathAtom] ->
        Some (ANF.CliNative (ANF.DirectoryListPacked, [pathAtom]))
    | "Darklang.Stdlib.File.readBlob", [pathAtom] ->
        Some (ANF.FileReadBlob pathAtom)
    | "Darklang.Stdlib.File.exists", [pathAtom] ->
        Some (ANF.FileExists pathAtom)
    | "Darklang.Stdlib.File.isDirectory", [pathAtom] ->
        Some (ANF.CliNative (ANF.FileIsDirectory, [pathAtom]))
    | "Darklang.Stdlib.File.writeBlob", [pathAtom; contentAtom] ->
        Some (ANF.FileWriteBlob (pathAtom, contentAtom))
    | "Darklang.Stdlib.File.appendText", [pathAtom; contentAtom] ->
        Some (ANF.FileAppendText (pathAtom, contentAtom))
    | "Darklang.Stdlib.File.delete", [pathAtom] ->
        Some (ANF.FileDelete pathAtom)
    | "Darklang.Stdlib.File.createDirectory", [pathAtom] ->
        Some (ANF.FileCreateDirectory pathAtom)
    | "Darklang.Stdlib.File.setExecutable", [pathAtom] ->
        Some (ANF.FileSetExecutable pathAtom)
    | "Darklang.Stdlib.File.writeFromPtr", [pathAtom; ptrAtom; lengthAtom] ->
        Some (ANF.FileWriteFromPtr (pathAtom, ptrAtom, lengthAtom))
    | _ -> None

let tryCliIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Darklang.Stdlib.Cli.__sleep", [delayMs] -> Some (ANF.Sleep delayMs)
    | _ ->
        let operation =
            match funcName with
            | "Darklang.Stdlib.Cli.__execute" -> Some ANF.Execute
            | "Darklang.Stdlib.Cli.__runProcess" -> Some ANF.RunProcess
            | "Darklang.Stdlib.Cli.__hostOSCode" -> Some ANF.HostOS
            | "Darklang.Stdlib.Cli.__hostArchitectureCode" -> Some ANF.HostArchitecture
            | "Darklang.Stdlib.Cli.__hostname" -> Some ANF.Hostname
            | "Darklang.Stdlib.Cli.__getenv" -> Some ANF.GetEnv
            | "Darklang.Stdlib.Cli.__environmentPacked" -> Some ANF.GetEnvironmentPacked
            | "Darklang.Stdlib.Cli.__setenv" -> Some ANF.SetEnv
            | "Darklang.Stdlib.Cli.__unsetenv" -> Some ANF.UnsetEnv
            | "Darklang.Stdlib.Cli.__argv" -> Some ANF.GetArgv
            | "Darklang.Stdlib.Cli.__kill" -> Some ANF.Kill
            | "Darklang.Stdlib.Cli.__getpid" -> Some ANF.GetPid
            | "Darklang.Stdlib.Cli.__getuid" -> Some ANF.GetUid
            | "Darklang.Stdlib.Cli.__cpuCount" -> Some ANF.CpuCount
            | "Darklang.Stdlib.Cli.__spawnProcess" -> Some ANF.SpawnProcess
            | "Darklang.Stdlib.Cli.__processIO" -> Some ANF.ProcessIO
            | "Darklang.Stdlib.Cli.__terminateProcess" -> Some ANF.TerminateProcess
            | _ -> None
        operation |> Option.map (fun op -> ANF.CliNative (op, args))

let internal normalizeNullaryIntrinsicArgs (args: ANF.Atom list) : ANF.Atom list =
    match args with
    | [ANF.UnitLiteral] -> []
    | _ -> args

/// Convert the public console builtins into explicit ordered effects.
let tryPresentationIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Builtin.print", [value] -> Some (ANF.StdoutWrite (value, false))
    | "Builtin.printLine", [value] -> Some (ANF.StdoutWrite (value, true))
    | "Builtin.stdinReadLine", [ANF.UnitLiteral] -> Some ANF.StdinReadLine
    | _ -> None

/// Parse a mangled type name (from typeToMangledName) into an AST type.
/// Returns Error if the mangled form is ambiguous or unsupported.
let internal tryParseMangledTypeWithSumTypeNames
    (sumTypeNames: Set<string>)
    (mangled: string)
    : Result<AST.Type, string> =
    let tokens = mangled.Split('_') |> Array.toList

    let mkNamedType (name: string) (args: AST.Type list) : AST.Type =
        if Set.contains name sumTypeNames then AST.TSum (name, args) else AST.TRecord (name, args)

    let isFreshenedTypeVarName (tok: string) : bool =
        tok.Contains("$")

    let tryPrimitive (tok: string) : AST.Type option =
        match tok with
        | "i8" -> Some AST.TInt8
        | "i16" -> Some AST.TInt16
        | "i32" -> Some AST.TInt32
        | "i64" -> Some AST.TInt64
        | "i128" -> Some AST.TInt128
        | "int" -> Some AST.TInt
        | "u8" -> Some AST.TUInt8
        | "u16" -> Some AST.TUInt16
        | "u32" -> Some AST.TUInt32
        | "u64" -> Some AST.TUInt64
        | "u128" -> Some AST.TUInt128
        | "bool" -> Some AST.TBool
        | "f64" -> Some AST.TFloat64
        | "str" -> Some AST.TString
        | "blob" -> Some AST.TBlob
        | "char" -> Some AST.TChar
        | "datetime" -> Some AST.TDateTime
        | "unit" -> Some AST.TUnit
        | "rawptr" -> Some AST.TRawPtr
        | _ -> None

    let rec parseType (toks: string list) : (AST.Type * string list) list =
        match toks with
        | [] -> []
        | tok :: rest ->
            match tok with
            | "list" ->
                parseType rest |> List.map (fun (elemT, rem) -> (AST.TList elemT, rem))
            | "stream" ->
                parseType rest |> List.map (fun (elemT, rem) -> (AST.TStream elemT, rem))
            | "dict" ->
                parseType rest
                |> List.collect (fun (keyT, rem1) ->
                    parseType rem1 |> List.map (fun (valueT, rem2) -> (AST.TDict (keyT, valueT), rem2)))
            | tupleToken when tupleToken.StartsWith("tup") && tupleToken.Length > 3 ->
                match System.Int32.TryParse(tupleToken.Substring(3)) with
                | true, arity when arity >= 0 ->
                    parseExactly arity rest
                    |> List.map (fun (elems, rem) -> (AST.TTuple elems, rem))
                | _ -> []
            | "tup" ->
                parseTupleElems rest |> List.map (fun (elems, rem) -> (AST.TTuple elems, rem))
            | "fn" ->
                parseFunction rest
            | _ ->
                match tryPrimitive tok with
                | Some prim -> [ (prim, rest) ]
                | None when isFreshenedTypeVarName tok || (tok.Length > 0 && System.Char.IsLower tok[0]) ->
                    [ (AST.TVar tok, rest) ]
                | None ->
                    let baseType = (mkNamedType tok [], rest)
                    let withArgs =
                        parseTupleElems rest
                        |> List.map (fun (args, rem) -> (mkNamedType tok args, rem))
                    baseType :: withArgs

    and parseExactly (count: int) (toks: string list) : (AST.Type list * string list) list =
        if count = 0 then
            [([], toks)]
        else
            parseType toks
            |> List.collect (fun (firstT, rem1) ->
                parseExactly (count - 1) rem1
                |> List.map (fun (restTs, rem2) -> (firstT :: restTs, rem2)))

    and parseTupleElems (toks: string list) : (AST.Type list * string list) list =
        parseType toks
        |> List.collect (fun (firstT, rem1) ->
            let single = ([firstT], rem1)
            let more =
                parseTupleElems rem1
                |> List.map (fun (restTs, rem2) -> (firstT :: restTs, rem2))
            single :: more)

    and parseFunction (toks: string list) : (AST.Type * string list) list =
        let rec splitParams (acc: string list) (remaining: string list) =
            match remaining with
            | [] -> None
            | "to" :: rest -> Some (List.rev acc, rest)
            | tok :: rest -> splitParams (tok :: acc) rest
        match splitParams [] toks with
        | None -> []
        | Some (paramTokens, retTokens) ->
            let paramParses =
                parseTupleElems paramTokens
                |> List.filter (fun (_, rem) -> rem = [])
                |> List.map fst
            let retParses =
                parseType retTokens
                |> List.filter (fun (_, rem) -> rem = [])
                |> List.map fst
            paramParses
            |> List.collect (fun paramTypes ->
                retParses |> List.map (fun ret -> (AST.TFunction (paramTypes, ret), [])))

    match parseType tokens |> List.filter (fun (_, rem) -> rem = []) with
    | [ (typ, _) ] -> Ok typ
    | [] -> Error $"Could not parse mangled type: {mangled}"
    | _ -> Error $"Ambiguous mangled type: {mangled}"

let tryParseMangledType (variantLookup: VariantLookup) (mangled: string) : Result<AST.Type, string> =
    tryParseMangledTypeWithSumTypeNames
        (sumTypeNamesFromVariantLookup variantLookup)
        mangled

/// Parse mangled type names used by monomorphized raw intrinsics.
let private tryParseMangledTypeForRawIntrinsic
    (sumTypeNames: Set<string>)
    (mangled: string)
    : AST.Type option =
    match tryParseMangledTypeWithSumTypeNames sumTypeNames mangled with
    | Ok typ -> Some typ
    | Error _ -> None

/// Try to convert a function call to a Float intrinsic CExpr
/// Returns Some CExpr if it's a Float intrinsic, None otherwise
let tryFloatIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Darklang.Stdlib.Float.sqrt", [xAtom] ->
        Some (ANF.FloatSqrt xAtom)
    | "Darklang.Stdlib.Float.negate", [xAtom] ->
        Some (ANF.FloatNeg xAtom)
    | "Darklang.Stdlib.Int64.toFloat", [xAtom] ->
        Some (ANF.Int64ToFloat xAtom)
    // NOTE: Float.toString is now implemented in Dark, not as an intrinsic
    | "Darklang.Stdlib.Float.__toBits", [xAtom] ->
        Some (ANF.FloatToBits xAtom)
    | "Darklang.Stdlib.Float.__toInt64Unchecked", [xAtom] ->
        Some (ANF.FloatToInt64 xAtom)
    | _ -> None

/// Canonical named APIs whose AOT implementation maps directly to backend
/// Boolean and fixed-width integer primitives.
let tryCanonicalPrimitiveIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    match funcName, args with
    | "Darklang.Stdlib.Bool.not", [value] -> Some (ANF.UnaryPrim (ANF.Not, value))
    | _ ->
        let fixedWidthModules =
            set [ "Darklang.Stdlib.Int8"; "Darklang.Stdlib.Int16"; "Darklang.Stdlib.Int32"; "Darklang.Stdlib.Int64"
                  "Darklang.Stdlib.UInt8"; "Darklang.Stdlib.UInt16"; "Darklang.Stdlib.UInt32"; "Darklang.Stdlib.UInt64" ]
        let nameParts = funcName.Split('.')
        let moduleName = nameParts |> Array.rev |> Array.skip 1 |> Array.rev |> String.concat "."
        let operationName = nameParts |> Array.tryLast
        match Set.contains moduleName fixedWidthModules, operationName, args with
        | true, Some "bitwiseAnd", [left; right] -> Some (ANF.Prim (ANF.BitAnd, left, right))
        | true, Some "bitwiseOr", [left; right] -> Some (ANF.Prim (ANF.BitOr, left, right))
        | true, Some "bitwiseXor", [left; right] -> Some (ANF.Prim (ANF.BitXor, left, right))
        | true, Some "shiftLeft", [left; right] -> Some (ANF.Prim (ANF.Shl, left, right))
        | true, Some "shiftRight", [left; right] -> Some (ANF.Prim (ANF.Shr, left, right))
        | true, Some "bitwiseNot", [value] -> Some (ANF.UnaryPrim (ANF.BitNot, value))
        | _ -> None

/// Try to convert a function call to a raw memory intrinsic CExpr
/// These are internal-only functions for implementing HAMT data structures
/// Returns Some CExpr if it's a raw memory intrinsic, None otherwise
/// Note: __raw_get and __raw_slot_init are generic and become monomorphized names like
/// __raw_get_i64, __raw_get_str, __raw_slot_init_i64, etc.
let tryRawMemoryIntrinsic
    (sumTypeNames: Set<string>)
    (funcName: string)
    (args: ANF.Atom list)
    : ANF.CExpr option =
    let args = normalizeNullaryIntrinsicArgs args
    let tryMonomorphizedValueType (prefix: string) (name: string) : AST.Type option =
        if name = prefix then
            None
        elif name.StartsWith(prefix + "_") then
            let mangled = name.Substring(prefix.Length + 1)
            tryParseMangledTypeForRawIntrinsic sumTypeNames mangled
        else
            None
    let tryMonomorphizedSuffix (prefix: string) (name: string) : string option =
        if name.StartsWith(prefix + "_") then
            Some (name.Substring(prefix.Length + 1))
        else
            None
    let dictTypeFromRawPtrIntrinsicName (name: string) : AST.Type =
        match tryMonomorphizedSuffix "__rawptr_to_dict" name with
        | Some suffix ->
            match tryParseMangledTypeForRawIntrinsic sumTypeNames $"dict_{suffix}" with
            | Some dictType -> dictType
            | None -> Crash.crash $"Could not recover Dict type from intrinsic '{name}'"
        | None ->
            AST.TDict (AST.TVar "k", AST.TVar "v")
    let listTypeFromRawPtrIntrinsicName (name: string) : AST.Type =
        match tryMonomorphizedSuffix "__rawptr_to_list" name with
        | Some suffix ->
            match tryParseMangledTypeForRawIntrinsic sumTypeNames suffix with
            | Some elemType -> AST.TList elemType
            | None -> Crash.crash $"Could not recover List type from intrinsic '{name}'"
        | None ->
            AST.TList (AST.TVar "a")
    match funcName, args with
    | "__raw_alloc", [numBytesAtom] ->
        Some (ANF.RawAlloc numBytesAtom)
    | "__mapped_alloc", [numBytesAtom] ->
        Some (ANF.MappedAlloc numBytesAtom)
    | "__raw_free", [ptrAtom] ->
        Some (ANF.RawFree ptrAtom)
    | "__mapped_free", [ptrAtom] ->
        Some (ANF.MappedFree ptrAtom)
    | "__list_array_release_small", [ptrAtom] ->
        Some (LowerListRegions.releaseRuntimeSmall ptrAtom)
    | "__raw_get_byte", [ptrAtom; offsetAtom] ->
        // Read single byte at offset, returns Int64 (zero-extended)
        // IMPORTANT: Must come before the generic __raw_get_* pattern
        Some (ANF.RawGetByte (ptrAtom, offsetAtom))
    | name, [ptrAtom; offsetAtom] when name = "__raw_get" || name.StartsWith("__raw_get_") ->
        // Generic __raw_get<v> monomorphizes to __raw_get_<mangled-type>.
        // Preserve recovered value type so downstream RC/codegen can handle heap payloads correctly.
        let valueType = tryMonomorphizedValueType "__raw_get" name
        Some (ANF.RawGet (ptrAtom, offsetAtom, valueType))
    | name, [ptrAtom; offsetAtom] when name = "__raw_take" || name.StartsWith("__raw_take_") ->
        let valueType = tryMonomorphizedValueType "__raw_take" name
        Some (ANF.RawTake (ptrAtom, offsetAtom, valueType))
    | "__raw_write_byte", [ptrAtom; offsetAtom; valueAtom] ->
        // Write single byte at offset
        Some (ANF.RawWriteByte (ptrAtom, offsetAtom, valueAtom))
    | "__raw_write_word", [ptrAtom; offsetAtom; valueAtom] ->
        // Write one unmanaged machine word. This intentionally has no typed edge semantics.
        Some (ANF.RawWriteWord (ptrAtom, offsetAtom, valueAtom))
    | name, [ptrAtom; offsetAtom; valueAtom] when name = "__raw_slot_init" || name.StartsWith("__raw_slot_init_") ->
        // Generic __raw_slot_init<v> monomorphizes to __raw_slot_init_<mangled-type>.
        // Preserve recovered value type so codegen can update ownership for stored heap values.
        match tryMonomorphizedValueType "__raw_slot_init" name with
        | Some valueType -> Some (ANF.RawSlotInit (ptrAtom, offsetAtom, valueAtom, valueType))
        | None -> Crash.crash "__raw_slot_init requires a concrete slot type"
    // String refcount intrinsics (for Dict with string keys)
    | "__refcount_inc_string", [strAtom] ->
        Some (ANF.RefCountIncString strAtom)
    | "__refcount_dec_string", [strAtom] ->
        Some (ANF.RefCountDecString strAtom)
    // Dynamic buffer backing-pointer views and raw allocation adoption.
    | "__string_to_rawptr", [strAtom] ->
        Some (ANF.StringToRawPtr strAtom)
    | "__rawptr_to_string", [ptrAtom] ->
        Some (ANF.RawPtrToString ptrAtom)
    | "__int128_to_rawptr", [valueAtom]
    | "__uint128_to_rawptr", [valueAtom] ->
        Some (ANF.TypedAtom (valueAtom, AST.TRawPtr))
    | "__rawptr_to_int128", [ptrAtom] ->
        Some (ANF.RawPtrToInt128 ptrAtom)
    | "__rawptr_to_uint128", [ptrAtom] ->
        Some (ANF.RawPtrToUInt128 ptrAtom)
    | "__string_concat_raw", [leftAtom; rightAtom] ->
        let representable atom =
            match atom with
            | ANF.UnitLiteral ->
                // A preceding RuntimeError never returns, but its unreachable
                // continuation still has the ANF Unit placeholder.
                ANF.StringLiteral ""
            | _ -> atom
        Some (ANF.StringConcat (representable leftAtom, representable rightAtom, []))
    | "__int_to_string", [valueAtom]
    | "__string_to_int", [valueAtom]
    | "__int64_to_int8", [valueAtom]
    | "__int64_to_int16", [valueAtom]
    | "__int64_to_int32", [valueAtom]
    | "__int64_to_uint8", [valueAtom]
    | "__int64_to_uint16", [valueAtom]
    | "__int64_to_uint32", [valueAtom] ->
        Some (ANF.Atom valueAtom)
    | "__int128_to_int", [valueAtom] ->
        Some (ANF.Call ("Darklang.Stdlib.Int128.__toInt", [valueAtom]))
    | "__uint128_to_int", [valueAtom] ->
        Some (ANF.Call ("Darklang.Stdlib.UInt128.__toInt", [valueAtom]))
    | "__int_to_int128", [valueAtom] ->
        Some (ANF.Call ("Darklang.Stdlib.Int128.__fromInt", [valueAtom]))
    | "__int_to_uint128", [valueAtom] ->
        Some (ANF.Call ("Darklang.Stdlib.UInt128.__fromInt", [valueAtom]))
    | "__blob_to_rawptr", [bytesAtom] ->
        Some (ANF.BlobToRawPtr bytesAtom)
    | "__rawptr_to_blob", [ptrAtom] ->
        Some (ANF.RawPtrToBlob ptrAtom)
    | name, [streamAtom] when name = "__stream_to_rawptr" || name.StartsWith("__stream_to_rawptr_") ->
        Some (ANF.TypedAtom (streamAtom, AST.TRawPtr))
    | name, [ptrAtom] when name = "__rawptr_to_stream" || name.StartsWith("__rawptr_to_stream_") ->
        let streamType =
            if name = "__rawptr_to_stream" then AST.TStream (AST.TVar "a")
            else
                let suffix = name.Substring("__rawptr_to_stream_".Length)
                match tryParseMangledTypeForRawIntrinsic sumTypeNames suffix with
                | Some elemType -> AST.TStream elemType
                | None -> Crash.crash $"Could not recover Stream type from intrinsic '{name}'"
        Some (ANF.TypedAtom (ptrAtom, streamType))

    // Dict intrinsics - for type-safe Dict<k, v> operations
    // __empty_dict<k, v> returns 0 (null pointer)
    | name, [] when name = "__empty_dict" || name.StartsWith("__empty_dict_") ->
        Some (ANF.Atom (ANF.IntLiteral (ANF.Int64 0L)))
    // __dict_is_null<k, v> checks if pointer is 0
    | name, [dictAtom] when name = "__dict_is_null" || name.StartsWith("__dict_is_null_") ->
        Some (ANF.Prim (ANF.Eq, dictAtom, ANF.IntLiteral (ANF.Int64 0L)))
    // __dict_get_tag<k, v> extracts low 2 bits (dict & 3)
    | name, [dictAtom] when name = "__dict_get_tag" || name.StartsWith("__dict_get_tag_") ->
        Some (ANF.Prim (ANF.BitAnd, dictAtom, ANF.IntLiteral (ANF.Int64 3L)))
    // __dict_to_rawptr<k, v> clears tag bits (dict & -4)
    | name, [dictAtom] when name = "__dict_to_rawptr" || name.StartsWith("__dict_to_rawptr_") ->
        Some (ANF.DictToRawPtr dictAtom)
    // __rawptr_to_dict<k, v> combines pointer + tag (ptr | tag)
    | name, [ptrAtom; tagAtom] when name = "__rawptr_to_dict" || name.StartsWith("__rawptr_to_dict_") ->
        Some (ANF.RawPtrToDict (ptrAtom, tagAtom, dictTypeFromRawPtrIntrinsicName name))

    // List intrinsics for the direct-payload skew RAL implementation.
    // __list_is_null<a> checks if list pointer is 0 (empty)
    | name, [listAtom] when name = "__list_is_null" || name.StartsWith("__list_is_null_") ->
        Some (ANF.Prim (ANF.Eq, listAtom, ANF.IntLiteral (ANF.Int64 0L)))
    // __list_get_tag<a> extracts the low three skew-node tag bits.
    | name, [listAtom] when name = "__list_get_tag" || name.StartsWith("__list_get_tag_") ->
        Some (ANF.Prim (ANF.BitAnd, listAtom, ANF.IntLiteral (ANF.Int64 7L)))
    // __list_to_rawptr<a> clears tag bits (list & -8) to get raw pointer
    | name, [listAtom] when name = "__list_to_rawptr" || name.StartsWith("__list_to_rawptr_") ->
        Some (ANF.ListToRawPtr listAtom)
    // __rawptr_to_list<a> combines pointer + tag (ptr | tag) to create tagged list
    | name, [ptrAtom; tagAtom] when name = "__rawptr_to_list" || name.StartsWith("__rawptr_to_list_") ->
        Some (ANF.RawPtrToList (ptrAtom, tagAtom, listTypeFromRawPtrIntrinsicName name))
    // __list_empty<a> returns 0 (the null skew-list root).
    | name, [] when name = "__list_empty" || name.StartsWith("__list_empty_") ->
        Some (ANF.Atom (ANF.IntLiteral (ANF.Int64 0L)))

    | _ -> None

/// Try to convert a function call to a random intrinsic CExpr
/// Returns Some CExpr if it's a random intrinsic, None otherwise
let tryRandomIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    let args = normalizeNullaryIntrinsicArgs args
    match funcName, args with
    | "Darklang.Stdlib.Int.__randomInt64Word", [] ->
        Some ANF.RandomInt64
    | _ -> None

/// Try to convert a function call to a DateTime intrinsic CExpr.
let tryDateTimeIntrinsic (funcName: string) (args: ANF.Atom list) : ANF.CExpr option =
    let args = normalizeNullaryIntrinsicArgs args
    match funcName, args with
    | "Darklang.Stdlib.DateTime.__now", [] ->
        Some ANF.DateTimeNow
    | "Darklang.Stdlib.DateTime.__fromUnixTimeTicks", [ticks] ->
        Some (ANF.TypedAtom (ticks, AST.TDateTime))
    | "Darklang.Stdlib.DateTime.__toUnixTimeTicks", [date] ->
        Some (ANF.TypedAtom (date, AST.TInt64))
    | _ -> None

let isBuiltinUnwrapName (funcName: string) : bool =
    funcName = "Builtin.unwrap"

let isBuiltinTestRuntimeErrorName (funcName: string) : bool =
    funcName = "Builtin.testRuntimeError"

/// Source programs use `crash`; the older builtin is retained for tests only.
let isSourceCrashName (funcName: string) : bool =
    funcName = "Builtin.crash"

let isRuntimeFailureName (funcName: string) : bool =
    isBuiltinTestRuntimeErrorName funcName || isSourceCrashName funcName

let isBuiltinTestNanName (name: string) : bool =
    name = "Builtin.testNan"

let isBuiltinTestInfinityName (name: string) : bool =
    name = "Builtin.testInfinity"

let isBuiltinBlobEmptyName (name: string) : bool =
    name = "Builtin.blobEmpty"

/// Look up a name already resolved and canonicalized by type checking.
let internal tryLookupResolved (name: string) (m: Map<string, 'a>) : ('a * string) option =
    Map.tryFind name m |> Option.map (fun value -> (value, name))

let internal unwrapErrorPayloadToString (expr: CheckedAST.Expr) : string option =
    match expr with
    | CheckedAST.UnitLiteral -> Some "()"
    | CheckedAST.Int64Literal n -> Some $"{n}"
    | CheckedAST.Int128Literal n -> Some (int128ToCanonicalString n)
    | CheckedAST.Int8Literal n -> Some $"{n}"
    | CheckedAST.Int16Literal n -> Some $"{n}"
    | CheckedAST.Int32Literal n -> Some $"{n}"
    | CheckedAST.UInt8Literal n -> Some $"{n}"
    | CheckedAST.UInt16Literal n -> Some $"{n}"
    | CheckedAST.UInt32Literal n -> Some $"{n}"
    | CheckedAST.UInt64Literal n -> Some $"{n}"
    | CheckedAST.UInt128Literal n -> Some (uint128ToCanonicalString n)
    | CheckedAST.BoolLiteral true -> Some "true"
    | CheckedAST.BoolLiteral false -> Some "false"
    | CheckedAST.FloatLiteral f -> Some $"{f}"
    | CheckedAST.StringLiteral s -> Some s
    | CheckedAST.CharLiteral s -> Some s
    | _ -> None

/// Record metadata retained through lowering. Declared parameter order cannot
/// be reconstructed from fields because parameters may be phantom.
