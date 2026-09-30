// DirectCallFacts.fs - Value, call, and rewrite facts for SSA direct-call specialization.

module DirectCallFacts

open MemoryModel
open ANF
open ANFEffects

type internal ParameterRewrite =
    | KeepParameter
    | ReplaceParameterWith of Atom

type internal ProgramAnalysis = {
    DirectCalls: FunctionIdMap<Atom list list>
    IndirectTargets: Set<AST.FunctionId>
}

type internal ScalarLiteral =
    | UnitScalar
    | IntScalar of SizedInt
    | BoolScalar of bool
    | FloatScalar of int64
    | StringScalar of string

type internal KnownValue =
    | LiteralValue of ScalarLiteral
    | Int128Value of constructor:AST.FunctionId * low:uint64 * high:uint64
    | UInt128Value of constructor:AST.FunctionId * low:uint64 * high:uint64
    | TupleValue of ScalarLiteral list
    | RecordValue of RecordDescriptor * ScalarLiteral list

type internal LiteralPattern = (int * KnownValue) list

type internal LiteralClone = {
    OriginalId: AST.FunctionId
    CloneId: AST.FunctionId
    CloneName: string
    Pattern: LiteralPattern
}

// Cloning is deliberately a small whole-program transform: without profile
// data, larger clone families are not justified by the saved scalar setup.
let internal maxLiteralClonesPerFunction = 4
let internal maxLiteralClonesPerProgram = 16

let internal emptyAnalysis = {
    DirectCalls = FunctionIdMap.empty
    IndirectTargets = Set.empty
}

// A function reference already names the complete target, so retaining an
// indirect call would hide a direct-call specialization opportunity without
// preserving any dynamic dispatch. Closure calls remain indirect because their
// hidden capture argument uses a different calling convention.
let internal exposeKnownIndirectCExpr (cexpr: CExpr) : CExpr =
    match cexpr with
    | IndirectCall (FuncRef name, args) -> Call (name, args)
    | IndirectTailCall (FuncRef name, args) -> TailCall (name, args)
    | _ -> cexpr

let internal addDirectCall
    (name: AST.FunctionId)
    (args: Atom list)
    (analysis: ProgramAnalysis)
    : ProgramAnalysis =
    let existing = FunctionIdMap.tryFind name analysis.DirectCalls |> Option.defaultValue []
    { analysis with DirectCalls = FunctionIdMap.add name (args :: existing) analysis.DirectCalls }

let internal analyzeAtom (atom: Atom) (analysis: ProgramAnalysis) : ProgramAnalysis =
    match atom with
    | FuncRef name -> { analysis with IndirectTargets = Set.add name analysis.IndirectTargets }
    | _ -> analysis

let internal analyzeAtoms (atoms: Atom list) (analysis: ProgramAnalysis) : ProgramAnalysis =
    atoms |> List.fold (fun state atom -> analyzeAtom atom state) analysis

let internal analyzeCExpr (cexpr: CExpr) (analysis: ProgramAnalysis) : ProgramAnalysis =
    let analyze = analyzeAtom
    let analyzeMany = analyzeAtoms
    match cexpr with
    | Atom atom
    | TypedAtom (atom, _)
    | UnaryPrim (_, atom)
    | RefCountInc (atom, _, _, _)
    | RefCountDec (atom, _, _, _)
    | Print (atom, _)
    | StdoutWrite (atom, _)
    | FileReadBlob atom
    | FileExists atom
    | FileDelete atom
    | FileCreateDirectory atom
    | FileSetExecutable atom
    | FloatSqrt atom
    | FloatAbs atom
    | FloatNeg atom
    | Int64ToFloat atom
    | FloatToInt64 atom
    | FloatToBits atom
    | RawAlloc atom
    | MappedAlloc atom
    | RawFree atom
    | MappedFree atom
    | RawGetByte (atom, _)
    | StringToRawPtr atom
    | RawPtrToString atom
    | BlobToRawPtr atom
    | RawPtrToBlob atom
    | RawPtrToInt128 atom
    | RawPtrToUInt128 atom
    | DictToRawPtr atom
    | ListToRawPtr atom
    | FixedBlockToRawPtr atom
    | RefCountIncString atom
    | RefCountDecString atom
    | RefCountIncBlob atom
    | RefCountDecBlob atom
    | RefCountIncInt atom
    | RefCountDecInt atom
    | FloatToString atom
    | Sleep atom
    | RuntimeErrorString atom -> analyze atom analysis
    | StringConcat (first, second, remaining) ->
        analyzeMany (first :: second :: remaining) analysis
    | Prim (_, left, right)
    | CanonicalBufferEq (_, left, right)
    | FileWriteBlob (left, right)
    | FileAppendText (left, right)
    | RawGet (left, right, _)
    | RawTake (left, right, _)
    | RawPtrToDict (left, right, _)
    | RawPtrToList (left, right, _) -> analyzeMany [left; right] analysis
    | IfValue (condition, thenValue, elseValue) ->
        analyzeMany [condition; thenValue; elseValue] analysis
    | Call (name, args)
    | BorrowedCall (name, args)
    | TailCall (name, args) ->
        analysis |> addDirectCall name args |> analyzeMany args
    | IndirectCall (func, args)
    | IndirectTailCall (func, args)
    | ClosureCall (func, args)
    | ClosureTailCall (func, args) -> analyzeMany (func :: args) analysis
    | ClosureAlloc (name, captures) ->
        { analysis with IndirectTargets = Set.add name analysis.IndirectTargets }
        |> analyzeMany captures
    | TupleAlloc atoms -> analyzeMany atoms analysis
    | RecordAlloc (_, atoms) -> analyzeMany atoms analysis
    | RecordClone (_, record, fields)
    | RecordReuse (_, _, record, fields) -> analyzeMany (record :: fields) analysis
    | CliNative (_, args) -> analyzeMany args analysis
    | TupleGet (tuple, _) -> analyze tuple analysis
    | RecordGet (_, record, _) -> analyze record analysis
    | FileWriteFromPtr (path, ptr, length) -> analyzeMany [path; ptr; length] analysis
    | RawWriteWord (ptr, offset, value)
    | RawWriteByte (ptr, offset, value) -> analyzeMany [ptr; offset; value] analysis
    | RawSlotInit (ptr, offset, value, _) -> analyzeMany [ptr; offset; value] analysis
    | RandomInt64
    | DateTimeNow
    | StdinReadLine
    | RuntimeError _ -> analysis
let internal scalarLiteralAtom (atom: Atom) : ScalarLiteral option =
    match atom with
    | UnitLiteral -> Some UnitScalar
    | IntLiteral value -> Some (IntScalar value)
    | BoolLiteral value -> Some (BoolScalar value)
    | FloatLiteral value -> Some (FloatScalar (System.BitConverter.DoubleToInt64Bits value))
    | StringLiteral value -> Some (StringScalar value)
    | Var _
    | FuncRef _ -> None

let internal atomForScalarLiteral (literal: ScalarLiteral) : Atom =
    match literal with
    | UnitScalar -> UnitLiteral
    | IntScalar value -> IntLiteral value
    | BoolScalar value -> BoolLiteral value
    | FloatScalar bits -> FloatLiteral (System.BitConverter.Int64BitsToDouble bits)
    | StringScalar value -> StringLiteral value

let internal isScalarLiteralType (typ: AST.SemanticType) : bool =
    match typ with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TBool
    | AST.TFloat64
    | AST.TString
    | AST.TChar
    | AST.TDateTime
    | AST.TUnit
    | AST.TSum _ -> true
    | _ -> false

let internal isConstructionValueType (typ: AST.SemanticType) : bool =
    match typ with
    | AST.TInt128
    | AST.TUInt128 -> true
    | AST.TTuple fields -> List.length fields <= 3
    | AST.TRecord _ -> true
    | _ -> false

let internal isSpecializableValueType (typ: AST.SemanticType) : bool =
    isScalarLiteralType typ || isConstructionValueType typ

let rec internal scalarLiteralMatchesType (typ: AST.SemanticType) (literal: ScalarLiteral) : bool =
    match typ, literal with
    | AST.TUnit, UnitScalar
    | AST.TInt8, IntScalar (Int8 _)
    | AST.TInt16, IntScalar (Int16 _)
    | AST.TInt32, IntScalar (Int32 _)
    | AST.TInt64, IntScalar (Int64 _)
    | AST.TUInt8, IntScalar (UInt8 _)
    | AST.TUInt16, IntScalar (UInt16 _)
    | AST.TUInt32, IntScalar (UInt32 _)
    | AST.TUInt64, IntScalar (UInt64 _)
    | AST.TBool, BoolScalar _
    | AST.TFloat64, FloatScalar _
    | AST.TString, StringScalar _
    | AST.TChar, StringScalar _
    | AST.TDateTime, IntScalar (Int64 _)
    | AST.TSum _, IntScalar (Int64 _) -> true
    | _ -> false

let internal knownValueMatchesType (typ: AST.SemanticType) (value: KnownValue) : bool =
    match typ, value with
    | _, LiteralValue literal -> scalarLiteralMatchesType typ literal
    | AST.TInt128, Int128Value _
    | AST.TUInt128, UInt128Value _ -> true
    | AST.TTuple fieldTypes, TupleValue fields ->
        List.length fieldTypes = List.length fields
        && List.forall2 scalarLiteralMatchesType fieldTypes fields
    | AST.TRecord (typeName, _), RecordValue (descriptor, fields) ->
        (typeName = descriptor.SourceTypeName || typeName = descriptor.RuntimeTypeName)
        && List.length descriptor.Fields = List.length fields
        && List.forall2
            scalarLiteralMatchesType
            (descriptor.Fields |> List.map snd)
            fields
    | AST.TSum _, TupleValue [_; _] -> true
    | _ -> false
let internal rewriteAtom (substitutions: Map<TempId, Atom>) (atom: Atom) : Atom =
    match atom with
    | Var id -> Map.tryFind id substitutions |> Option.defaultValue atom
    | _ -> atom

let internal rewriteCallArgs
    (rewriteMap: FunctionIdMap<ParameterRewrite list>)
    (name: AST.FunctionId)
    (args: Atom list)
    : Atom list =
    match FunctionIdMap.tryFind name rewriteMap with
    | None -> args
    | Some rewrites ->
        let rec loop rewrites args rewritten =
            match rewrites, args with
            | [], [] -> List.rev rewritten
            | rewrite :: restRewrites, arg :: restArgs ->
                match rewrite with
                | KeepParameter -> loop restRewrites restArgs (arg :: rewritten)
                | ReplaceParameterWith _ -> loop restRewrites restArgs rewritten
            | _ -> Crash.crash $"Direct-call argument count mismatch for '{name}'"
        loop rewrites args []

let internal rewriteCExpr
    (rewriteMap: FunctionIdMap<ParameterRewrite list>)
    (substitutions: Map<TempId, Atom>)
    (cexpr: CExpr)
    : CExpr =
    let rewrite = rewriteAtom substitutions
    let rewriteMany = List.map rewrite
    let directArgs name args = args |> rewriteMany |> rewriteCallArgs rewriteMap name
    match cexpr with
    | Atom atom -> Atom (rewrite atom)
    | TypedAtom (atom, typ) -> TypedAtom (rewrite atom, typ)
    | Prim (op, left, right) -> Prim (op, rewrite left, rewrite right)
    | UnaryPrim (op, atom) -> UnaryPrim (op, rewrite atom)
    | IfValue (condition, thenValue, elseValue) -> IfValue (rewrite condition, rewrite thenValue, rewrite elseValue)
    | Call (name, args) -> Call (name, directArgs name args)
    | BorrowedCall (name, args) -> BorrowedCall (name, directArgs name args)
    | TailCall (name, args) -> TailCall (name, directArgs name args)
    | IndirectCall (func, args) -> IndirectCall (rewrite func, rewriteMany args)
    | IndirectTailCall (func, args) -> IndirectTailCall (rewrite func, rewriteMany args)
    | ClosureAlloc (name, captures) -> ClosureAlloc (name, rewriteMany captures)
    | ClosureCall (closure, args) -> ClosureCall (rewrite closure, rewriteMany args)
    | ClosureTailCall (closure, args) -> ClosureTailCall (rewrite closure, rewriteMany args)
    | TupleAlloc atoms -> TupleAlloc (rewriteMany atoms)
    | TupleGet (tuple, index) -> TupleGet (rewrite tuple, index)
    | RecordAlloc (descriptor, fields) -> RecordAlloc (descriptor, rewriteMany fields)
    | RecordGet (descriptor, record, index) -> RecordGet (descriptor, rewrite record, index)
    | RecordClone (descriptor, record, fields) ->
        RecordClone (descriptor, rewrite record, rewriteMany fields)
    | RecordReuse (sourceDescriptor, targetDescriptor, record, fields) ->
        RecordReuse (sourceDescriptor, targetDescriptor, rewrite record, rewriteMany fields)
    | StringConcat (first, second, remaining) ->
        StringConcat (rewrite first, rewrite second, rewriteMany remaining)
    | CanonicalBufferEq (kind, left, right) ->
        match rewrite left, rewrite right with
        | StringLiteral leftValue, StringLiteral rightValue ->
            Atom (BoolLiteral (leftValue = rightValue))
        | left', right' -> CanonicalBufferEq (kind, left', right')
    | RefCountInc (atom, size, kind, metadata) -> RefCountInc (rewrite atom, size, kind, metadata)
    | RefCountDec (atom, size, kind, metadata) -> RefCountDec (rewrite atom, size, kind, metadata)
    | Print (atom, typ) -> Print (rewrite atom, typ)
    | StdoutWrite (atom, appendNewline) -> StdoutWrite (rewrite atom, appendNewline)
    | StdinReadLine -> StdinReadLine
    | FileReadBlob path -> FileReadBlob (rewrite path)
    | FileExists path -> FileExists (rewrite path)
    | FileWriteBlob (path, content) -> FileWriteBlob (rewrite path, rewrite content)
    | FileAppendText (path, content) -> FileAppendText (rewrite path, rewrite content)
    | FileDelete path -> FileDelete (rewrite path)
    | FileCreateDirectory path -> FileCreateDirectory (rewrite path)
    | FileSetExecutable path -> FileSetExecutable (rewrite path)
    | FileWriteFromPtr (path, ptr, length) -> FileWriteFromPtr (rewrite path, rewrite ptr, rewrite length)
    | FloatSqrt atom -> FloatSqrt (rewrite atom)
    | FloatAbs atom -> FloatAbs (rewrite atom)
    | FloatNeg atom -> FloatNeg (rewrite atom)
    | Int64ToFloat atom -> Int64ToFloat (rewrite atom)
    | FloatToInt64 atom -> FloatToInt64 (rewrite atom)
    | FloatToBits atom -> FloatToBits (rewrite atom)
    | RawAlloc atom -> RawAlloc (rewrite atom)
    | MappedAlloc atom -> MappedAlloc (rewrite atom)
    | RawFree atom -> RawFree (rewrite atom)
    | MappedFree atom -> MappedFree (rewrite atom)
    | RawGet (ptr, offset, typ) -> RawGet (rewrite ptr, rewrite offset, typ)
    | RawTake (ptr, offset, typ) -> RawTake (rewrite ptr, rewrite offset, typ)
    | RawGetByte (ptr, offset) -> RawGetByte (rewrite ptr, offset)
    | RawWriteWord (ptr, offset, value) -> RawWriteWord (rewrite ptr, rewrite offset, rewrite value)
    | RawWriteByte (ptr, offset, value) -> RawWriteByte (rewrite ptr, rewrite offset, rewrite value)
    | RawSlotInit (ptr, offset, value, typ) -> RawSlotInit (rewrite ptr, rewrite offset, rewrite value, typ)
    | StringToRawPtr atom -> StringToRawPtr (rewrite atom)
    | RawPtrToString atom -> RawPtrToString (rewrite atom)
    | BlobToRawPtr atom -> BlobToRawPtr (rewrite atom)
    | RawPtrToBlob atom -> RawPtrToBlob (rewrite atom)
    | RawPtrToInt128 atom -> RawPtrToInt128 (rewrite atom)
    | RawPtrToUInt128 atom -> RawPtrToUInt128 (rewrite atom)
    | DictToRawPtr atom -> DictToRawPtr (rewrite atom)
    | RawPtrToDict (ptr, tag, typ) -> RawPtrToDict (rewrite ptr, rewrite tag, typ)
    | ListToRawPtr atom -> ListToRawPtr (rewrite atom)
    | FixedBlockToRawPtr atom -> FixedBlockToRawPtr (rewrite atom)
    | RawPtrToList (ptr, tag, typ) -> RawPtrToList (rewrite ptr, rewrite tag, typ)
    | RefCountIncString atom -> RefCountIncString (rewrite atom)
    | RefCountDecString atom -> RefCountDecString (rewrite atom)
    | RefCountIncBlob atom -> RefCountIncBlob (rewrite atom)
    | RefCountDecBlob atom -> RefCountDecBlob (rewrite atom)
    | RefCountIncInt atom -> RefCountIncInt (rewrite atom)
    | RefCountDecInt atom -> RefCountDecInt (rewrite atom)
    | RandomInt64 -> RandomInt64
    | DateTimeNow -> DateTimeNow
    | Sleep delayMs -> Sleep (rewrite delayMs)
    | CliNative (operation, args) -> CliNative (operation, rewriteMany args)
    | FloatToString atom -> FloatToString (rewrite atom)
    | RuntimeError message -> RuntimeError message
    | RuntimeErrorString atom -> RuntimeErrorString (rewrite atom)
type internal ValueEnv = Map<TempId, KnownValue>

let internal knownValueForAtom (env: ValueEnv) (atom: Atom) : KnownValue option =
    match scalarLiteralAtom atom with
    | Some literal -> Some (LiteralValue literal)
    | None ->
        match atom with
        | Var id -> Map.tryFind id env
        | _ -> None

let internal knownLiteralsForAtoms (env: ValueEnv) (atoms: Atom list) : ScalarLiteral list option =
    let rec loop remaining literals =
        match remaining with
        | [] -> Some (List.rev literals)
        | atom :: rest ->
            match knownValueForAtom env atom with
            | Some (LiteralValue literal) -> loop rest (literal :: literals)
            | _ -> None
    loop atoms []

let internal knownValueForCExpr
    (functionNames: FunctionIdMap<string>)
    (env: ValueEnv)
    (cexpr: CExpr)
    : KnownValue option =
    let words name =
        match FunctionIdMap.tryFind name functionNames with
        | Some "Darklang.Stdlib.Int128.__fromWords" ->
            Some (fun low high -> Int128Value (name, low, high))
        | Some "Darklang.Stdlib.UInt128.__fromWords" ->
            Some (fun low high -> UInt128Value (name, low, high))
        | _ -> None
    match cexpr with
    | Atom atom
    | TypedAtom (atom, _) -> knownValueForAtom env atom
    | Call (name, [IntLiteral (UInt64 low); IntLiteral (UInt64 high)]) ->
        words name |> Option.map (fun build -> build low high)
    | TupleAlloc atoms when List.length atoms <= 3 ->
        knownLiteralsForAtoms env atoms |> Option.map TupleValue
    | RecordAlloc (descriptor, fields) when List.length fields <= 3 ->
        knownLiteralsForAtoms env fields
        |> Option.map (fun literals -> RecordValue (descriptor, literals))
    | _ -> None

let internal addKnownBinding functionNames (id: TempId) (cexpr: CExpr) (env: ValueEnv) : ValueEnv =
    match knownValueForCExpr functionNames env cexpr with
    | Some value -> Map.add id value env
    | None -> Map.remove id env

let internal addKnownCall
    (name: AST.FunctionId)
    (args: Atom list)
    (env: ValueEnv)
    (calls: FunctionIdMap<KnownValue option list list>)
    : FunctionIdMap<KnownValue option list list> =
    let values = args |> List.map (knownValueForAtom env)
    let existing = FunctionIdMap.tryFind name calls |> Option.defaultValue []
    FunctionIdMap.add name (values :: existing) calls
let internal literalPatternAt
    (eligibleIndices: Set<int>)
    (values: KnownValue option list)
    : LiteralPattern =
    values
    |> List.mapi (fun index value ->
        if Set.contains index eligibleIndices then
            value |> Option.map (fun known -> (index, known))
        else
            None)
    |> List.choose id
let internal boundedCloneGroups
    (groups: (AST.FunctionId * string * LiteralPattern list) list)
    : (AST.FunctionId * string * LiteralPattern list) list =
    groups
    |> List.fold (fun (selected, remaining) (id, name, patterns) ->
        let count = List.length patterns
        if count <= remaining then
            ((id, name, patterns) :: selected, remaining - count)
        else
            (selected, remaining)
    ) ([], maxLiteralClonesPerProgram)
    |> fst
    |> List.rev

let internal buildLiteralClones
    (existingIds: AST.FunctionId seq)
    (existingNames: Set<string>)
    (groups: (AST.FunctionId * string * LiteralPattern list) list)
    : LiteralClone list =
    let proposedSpecs =
        groups
        |> List.collect (fun (id, name, patterns) ->
            patterns
            |> List.mapi (fun index pattern ->
                let cloneName = $"{name}__literal_{index}"
                (id, cloneName, pattern)))
    let proposedNames = proposedSpecs |> List.map (fun (_, name, _) -> name)
    let namesAreUnique = List.length proposedNames = (proposedNames |> List.distinct |> List.length)
    if namesAreUnique && proposedNames |> List.forall (fun name -> not (Set.contains name existingNames)) then
        let allocated = AST.allocateFunctionIds existingIds proposedNames
        proposedSpecs
        |> List.map (fun (originalId, cloneName, pattern) ->
            let cloneId =
                Map.tryFind cloneName allocated
                |> Option.defaultWith (fun () -> Crash.crash "Literal clone identity was not allocated")
            { OriginalId = originalId
              CloneId = cloneId
              CloneName = cloneName
              Pattern = pattern })
    else
        []

let internal removePatternArguments
    (pattern: LiteralPattern)
    (args: Atom list)
    : Atom list =
    let removedIndices = pattern |> List.map fst |> Set.ofList
    args
    |> List.mapi (fun index arg ->
        if Set.contains index removedIndices then None else Some arg)
    |> List.choose id

let internal routeDirectCall
    (clonesByName: FunctionIdMap<LiteralClone list>)
    (env: ValueEnv)
    (name: AST.FunctionId)
    (args: Atom list)
    : AST.FunctionId * Atom list =
    let matchesPattern pattern =
        pattern
        |> List.forall (fun (index, value) ->
            List.tryItem index args |> Option.bind (knownValueForAtom env) = Some value)
    let matchingClone =
        FunctionIdMap.tryFind name clonesByName
        |> Option.bind (List.tryFind (fun clone -> matchesPattern clone.Pattern))
    match matchingClone with
    | Some clone -> (clone.CloneId, removePatternArguments clone.Pattern args)
    | None -> (name, args)

let internal routeCExpr
    (clonesByName: FunctionIdMap<LiteralClone list>)
    (env: ValueEnv)
    (cexpr: CExpr)
    : CExpr =
    match cexpr with
    | Call (name, args) ->
        let (target, routedArgs) = routeDirectCall clonesByName env name args
        Call (target, routedArgs)
    | BorrowedCall (name, args) ->
        let (target, routedArgs) = routeDirectCall clonesByName env name args
        BorrowedCall (target, routedArgs)
    | TailCall (name, args) ->
        let (target, routedArgs) = routeDirectCall clonesByName env name args
        TailCall (target, routedArgs)
    | _ -> cexpr
let internal cexprForKnownValue (value: KnownValue) : CExpr =
    let atoms literals = literals |> List.map atomForScalarLiteral
    match value with
    | LiteralValue literal -> Atom (atomForScalarLiteral literal)
    | Int128Value (constructor, low, high) ->
        Call (
            constructor,
            [IntLiteral (UInt64 low); IntLiteral (UInt64 high)]
        )
    | UInt128Value (constructor, low, high) ->
        Call (
            constructor,
            [IntLiteral (UInt64 low); IntLiteral (UInt64 high)]
        )
    | TupleValue fields -> TupleAlloc (atoms fields)
    | RecordValue (descriptor, fields) -> RecordAlloc (descriptor, atoms fields)
let internal isRematerializedValue functionNames (cexpr: CExpr) : bool =
    match knownValueForCExpr functionNames Map.empty cexpr with
    | Some _ -> true
    | None -> false
