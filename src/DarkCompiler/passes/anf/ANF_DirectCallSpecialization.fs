// ANF_DirectCallSpecialization.fs - Specialize internal direct-call signatures.
//
// Rewrites parameters only when every call site is present in the current ANF
// program. Functions used as raw addresses or closure targets retain their
// original calling convention.

module ANF_DirectCallSpecialization

open MemoryModel

open ANF
open ANFEffects

type private ParameterRewrite =
    | KeepParameter
    | ReplaceParameterWith of Atom

type private ProgramAnalysis = {
    DirectCalls: Map<string, Atom list list>
    IndirectTargets: Set<string>
}

type private ScalarLiteral =
    | UnitScalar
    | IntScalar of SizedInt
    | BoolScalar of bool
    | FloatScalar of int64
    | StringScalar of string

type private KnownValue =
    | LiteralValue of ScalarLiteral
    | Int128Value of low:uint64 * high:uint64
    | UInt128Value of low:uint64 * high:uint64
    | TupleValue of ScalarLiteral list
    | RecordValue of RecordDescriptor * ScalarLiteral list

type private LiteralPattern = (int * KnownValue) list

type private LiteralClone = {
    OriginalName: string
    CloneName: string
    Pattern: LiteralPattern
}

// Cloning is deliberately a small whole-program transform: without profile
// data, larger clone families are not justified by the saved scalar setup.
let private maxLiteralClonesPerFunction = 4
let private maxLiteralClonesPerProgram = 16

let private emptyAnalysis = {
    DirectCalls = Map.empty
    IndirectTargets = Set.empty
}

// A function reference already names the complete target, so retaining an
// indirect call would hide a direct-call specialization opportunity without
// preserving any dynamic dispatch. Closure calls remain indirect because their
// hidden capture argument uses a different calling convention.
let private exposeKnownIndirectCExpr (cexpr: CExpr) : CExpr =
    match cexpr with
    | IndirectCall (FuncRef name, args) -> Call (name, args)
    | IndirectTailCall (FuncRef name, args) -> TailCall (name, args)
    | _ -> cexpr

let rec private exposeKnownIndirectExpr (expr: AExpr) : AExpr =
    match expr with
    | Let (id, cexpr, body) ->
        Let (id, exposeKnownIndirectCExpr cexpr, exposeKnownIndirectExpr body)
    | Return _
    | Jump _ -> expr
    | Join (parameter, continuation, entry) ->
        Join (
            parameter,
            exposeKnownIndirectExpr continuation,
            exposeKnownIndirectExpr entry
        )
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            exposeKnownIndirectExpr thenBranch,
            exposeKnownIndirectExpr elseBranch
        )

let private exposeKnownIndirectTargets (Program (functions, main)) : Program =
    let functions' =
        functions
        |> List.map (fun func -> { func with Body = exposeKnownIndirectExpr func.Body })
    Program (functions', exposeKnownIndirectExpr main)

let private addDirectCall
    (name: string)
    (args: Atom list)
    (analysis: ProgramAnalysis)
    : ProgramAnalysis =
    let existing = Map.tryFind name analysis.DirectCalls |> Option.defaultValue []
    { analysis with DirectCalls = Map.add name (args :: existing) analysis.DirectCalls }

let private analyzeAtom (atom: Atom) (analysis: ProgramAnalysis) : ProgramAnalysis =
    match atom with
    | FuncRef name -> { analysis with IndirectTargets = Set.add name analysis.IndirectTargets }
    | _ -> analysis

let private analyzeAtoms (atoms: Atom list) (analysis: ProgramAnalysis) : ProgramAnalysis =
    atoms |> List.fold (fun state atom -> analyzeAtom atom state) analysis

let private analyzeCExpr (cexpr: CExpr) (analysis: ProgramAnalysis) : ProgramAnalysis =
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
    | FileReadText atom
    | FileExists atom
    | FileDelete atom
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
    | FloatToString atom
    | Sleep atom
    | RuntimeErrorString atom -> analyze atom analysis
    | Prim (_, left, right)
    | StringConcat (left, right)
    | CanonicalBufferEq (_, left, right)
    | FileWriteText (left, right)
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
    | RecordReuse (_, record, fields) -> analyzeMany (record :: fields) analysis
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

let rec private analyzeExpr (expr: AExpr) (analysis: ProgramAnalysis) : ProgramAnalysis =
    match expr with
    | Let (_, cexpr, body) -> analysis |> analyzeCExpr cexpr |> analyzeExpr body
    | Return atom -> analyzeAtom atom analysis
    | Jump (_, atom) -> analyzeAtom atom analysis
    | Join (_, continuation, entry) -> analysis |> analyzeExpr continuation |> analyzeExpr entry
    | If (condition, thenBranch, elseBranch) ->
        analysis
        |> analyzeAtom condition
        |> analyzeExpr thenBranch
        |> analyzeExpr elseBranch

let private analyzeProgram (functions: Function list) (main: AExpr) : ProgramAnalysis =
    functions
    |> List.fold (fun analysis func -> analyzeExpr func.Body analysis) emptyAnalysis
    |> analyzeExpr main

let private scalarLiteralAtom (atom: Atom) : ScalarLiteral option =
    match atom with
    | UnitLiteral -> Some UnitScalar
    | IntLiteral value -> Some (IntScalar value)
    | BoolLiteral value -> Some (BoolScalar value)
    | FloatLiteral value -> Some (FloatScalar (System.BitConverter.DoubleToInt64Bits value))
    | StringLiteral value -> Some (StringScalar value)
    | Var _
    | FuncRef _ -> None

let private atomForScalarLiteral (literal: ScalarLiteral) : Atom =
    match literal with
    | UnitScalar -> UnitLiteral
    | IntScalar value -> IntLiteral value
    | BoolScalar value -> BoolLiteral value
    | FloatScalar bits -> FloatLiteral (System.BitConverter.Int64BitsToDouble bits)
    | StringScalar value -> StringLiteral value

let private isScalarLiteralType (typ: AST.Type) : bool =
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

let private isConstructionValueType (typ: AST.Type) : bool =
    match typ with
    | AST.TInt128
    | AST.TUInt128 -> true
    | AST.TTuple fields -> List.length fields <= 3
    | AST.TRecord _ -> true
    | _ -> false

let private isSpecializableValueType (typ: AST.Type) : bool =
    isScalarLiteralType typ || isConstructionValueType typ

let rec private scalarLiteralMatchesType (typ: AST.Type) (literal: ScalarLiteral) : bool =
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

let private knownValueMatchesType (typ: AST.Type) (value: KnownValue) : bool =
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

let private uniformLiteralAt (index: int) (calls: Atom list list) : Atom option =
    let literals =
        calls
        |> List.map (fun args -> List.tryItem index args |> Option.bind scalarLiteralAtom)
    match literals with
    | Some first :: rest when rest |> List.forall (fun literal -> literal = Some first) ->
        Some (atomForScalarLiteral first)
    | _ -> None

let private rewritesForFunction
    (analysis: ProgramAnalysis)
    (func: Function)
    : ParameterRewrite list option =
    match Map.tryFind func.Name analysis.DirectCalls with
    | None -> None
    | Some _ when Set.contains func.Name analysis.IndirectTargets -> None
    | Some calls ->
        func.TypedParams
        |> List.mapi (fun index parameter ->
            match isScalarLiteralType parameter.Type, uniformLiteralAt index calls with
            | false, _ -> KeepParameter
            | true, Some literal ->
                match scalarLiteralAtom literal with
                | Some value when scalarLiteralMatchesType parameter.Type value ->
                    ReplaceParameterWith literal
                | _ -> KeepParameter
            | true, None -> KeepParameter)
        |> Some

let private buildRewriteMap
    (analysis: ProgramAnalysis)
    (functions: Function list)
    : Map<string, ParameterRewrite list> =
    functions
    |> List.choose (fun func ->
        rewritesForFunction analysis func
        |> Option.bind (fun rewrites ->
            if rewrites |> List.forall (fun rewrite -> rewrite = KeepParameter) then None
            else Some (func.Name, rewrites)))
    |> Map.ofList

let private rewriteAtom (substitutions: Map<TempId, Atom>) (atom: Atom) : Atom =
    match atom with
    | Var id -> Map.tryFind id substitutions |> Option.defaultValue atom
    | _ -> atom

let private rewriteCallArgs
    (rewriteMap: Map<string, ParameterRewrite list>)
    (name: string)
    (args: Atom list)
    : Atom list =
    match Map.tryFind name rewriteMap with
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

let private rewriteCExpr
    (rewriteMap: Map<string, ParameterRewrite list>)
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
    | RecordReuse (descriptor, record, fields) ->
        RecordReuse (descriptor, rewrite record, rewriteMany fields)
    | StringConcat (left, right) -> StringConcat (rewrite left, rewrite right)
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
    | FileReadText path -> FileReadText (rewrite path)
    | FileExists path -> FileExists (rewrite path)
    | FileWriteText (path, content) -> FileWriteText (rewrite path, rewrite content)
    | FileAppendText (path, content) -> FileAppendText (rewrite path, rewrite content)
    | FileDelete path -> FileDelete (rewrite path)
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
    | RandomInt64 -> RandomInt64
    | DateTimeNow -> DateTimeNow
    | Sleep delayMs -> Sleep (rewrite delayMs)
    | CliNative (operation, args) -> CliNative (operation, rewriteMany args)
    | FloatToString atom -> FloatToString (rewrite atom)
    | RuntimeError message -> RuntimeError message
    | RuntimeErrorString atom -> RuntimeErrorString (rewrite atom)

let rec private rewriteExpr
    (rewriteMap: Map<string, ParameterRewrite list>)
    (substitutions: Map<TempId, Atom>)
    (expr: AExpr)
    : AExpr =
    match expr with
    | Let (id, cexpr, body) ->
        Let (id, rewriteCExpr rewriteMap substitutions cexpr, rewriteExpr rewriteMap substitutions body)
    | Return atom -> Return (rewriteAtom substitutions atom)
    | Jump (target, atom) -> Jump (target, rewriteAtom substitutions atom)
    | Join (parameter, continuation, entry) ->
        Join (parameter, rewriteExpr rewriteMap (Map.remove parameter.Id substitutions) continuation, rewriteExpr rewriteMap substitutions entry)
    | If (condition, thenBranch, elseBranch) ->
        If (
            rewriteAtom substitutions condition,
            rewriteExpr rewriteMap substitutions thenBranch,
            rewriteExpr rewriteMap substitutions elseBranch
        )

let private rewriteFunction
    (rewriteMap: Map<string, ParameterRewrite list>)
    (func: Function)
    : Function =
    match Map.tryFind func.Name rewriteMap with
    | None -> { func with Body = rewriteExpr rewriteMap Map.empty func.Body }
    | Some rewrites ->
        let rec pairParameters parameters rewrites pairs =
            match parameters, rewrites with
            | [], [] -> List.rev pairs
            | parameter :: restParameters, rewrite :: restRewrites ->
                pairParameters restParameters restRewrites ((parameter, rewrite) :: pairs)
            | _ -> Crash.crash $"Direct-call parameter rewrite count mismatch for '{func.Name}'"
        let parameterRewrites = pairParameters func.TypedParams rewrites []
        let parameters =
            parameterRewrites
            |> List.choose (fun (parameter, rewrite) ->
                match rewrite with
                | KeepParameter -> Some parameter
                | ReplaceParameterWith _ -> None)
        let substitutions =
            parameterRewrites
            |> List.choose (fun (parameter, rewrite) ->
                match rewrite with
                | ReplaceParameterWith literal -> Some (parameter.Id, literal)
                | KeepParameter -> None)
            |> Map.ofList
        { func with
            TypedParams = parameters
            Body = rewriteExpr rewriteMap substitutions func.Body }

type private ValueEnv = Map<TempId, KnownValue>

let private knownValueForAtom (env: ValueEnv) (atom: Atom) : KnownValue option =
    match scalarLiteralAtom atom with
    | Some literal -> Some (LiteralValue literal)
    | None ->
        match atom with
        | Var id -> Map.tryFind id env
        | _ -> None

let private knownLiteralsForAtoms (env: ValueEnv) (atoms: Atom list) : ScalarLiteral list option =
    let rec loop remaining literals =
        match remaining with
        | [] -> Some (List.rev literals)
        | atom :: rest ->
            match knownValueForAtom env atom with
            | Some (LiteralValue literal) -> loop rest (literal :: literals)
            | _ -> None
    loop atoms []

let private knownValueForCExpr (env: ValueEnv) (cexpr: CExpr) : KnownValue option =
    let words name =
        match name with
        | "Stdlib.Int128.__fromWords" -> Some (fun low high -> Int128Value (low, high))
        | "Stdlib.UInt128.__fromWords" -> Some (fun low high -> UInt128Value (low, high))
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

let private addKnownBinding (id: TempId) (cexpr: CExpr) (env: ValueEnv) : ValueEnv =
    match knownValueForCExpr env cexpr with
    | Some value -> Map.add id value env
    | None -> Map.remove id env

let private addKnownCall
    (name: string)
    (args: Atom list)
    (env: ValueEnv)
    (calls: Map<string, KnownValue option list list>)
    : Map<string, KnownValue option list list> =
    let values = args |> List.map (knownValueForAtom env)
    let existing = Map.tryFind name calls |> Option.defaultValue []
    Map.add name (values :: existing) calls

let rec private collectKnownCalls
    (env: ValueEnv)
    (expr: AExpr)
    (calls: Map<string, KnownValue option list list>)
    : Map<string, KnownValue option list list> =
    match expr with
    | Return _
    | Jump _ -> calls
    | Let (id, cexpr, body) ->
        let calls' =
            match cexpr with
            | Call (name, args)
            | BorrowedCall (name, args)
            | TailCall (name, args) -> addKnownCall name args env calls
            | _ -> calls
        collectKnownCalls (addKnownBinding id cexpr env) body calls'
    | Join (parameter, continuation, entry) ->
        let calls' = collectKnownCalls (Map.remove parameter.Id env) continuation calls
        collectKnownCalls env entry calls'
    | If (_, thenBranch, elseBranch) ->
        let calls' = collectKnownCalls env thenBranch calls
        collectKnownCalls env elseBranch calls'

let private knownCallsInProgram (functions: Function list) (main: AExpr) =
    functions
    |> List.fold (fun calls func -> collectKnownCalls Map.empty func.Body calls) Map.empty
    |> collectKnownCalls Map.empty main

let private literalPatternAt
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

let rec private directCallsTo (target: string) (expr: AExpr) : Atom list list =
    match expr with
    | Jump _ | Return _ -> []
    | Join (_, continuation, entry) -> directCallsTo target continuation @ directCallsTo target entry
    | Let (_, cexpr, body) ->
        let current =
            match cexpr with
            | Call (name, args)
            | BorrowedCall (name, args)
            | TailCall (name, args) when name = target -> [args]
            | _ -> []
        current @ directCallsTo target body
    | If (_, thenBranch, elseBranch) ->
        directCallsTo target thenBranch @ directCallsTo target elseBranch

let private cloneableParameterIndices (func: Function) : Set<int> =
    let allIndices =
        func.TypedParams
        |> List.mapi (fun index parameter ->
            if isSpecializableValueType parameter.Type then Some index else None)
        |> List.choose id
        |> Set.ofList
    match directCallsTo func.Name func.Body with
    | [] -> allIndices
    | selfCalls ->
        func.TypedParams
        |> List.mapi (fun index parameter ->
            let isPassedThrough =
                isSpecializableValueType parameter.Type
                && (selfCalls
                    |> List.forall (fun args ->
                        List.tryItem index args = Some (Var parameter.Id)))
            if isPassedThrough then Some index else None)
        |> List.choose id
        |> Set.ofList

let private cloneGroups
    (analysis: ProgramAnalysis)
    (knownCalls: Map<string, KnownValue option list list>)
    (functions: Function list)
    : (string * LiteralPattern list) list =
    functions
    |> List.choose (fun func ->
        match Map.tryFind func.Name knownCalls with
        | None -> None
        | Some _ when Set.contains func.Name analysis.IndirectTargets -> None
        | Some calls ->
            let eligibleIndices = cloneableParameterIndices func
            let isRecursive = not (List.isEmpty (directCallsTo func.Name func.Body))
            let valueBenefit value =
                match value with
                | LiteralValue _ -> 1
                | Int128Value _
                | UInt128Value _ -> 2
                | TupleValue fields
                | RecordValue (_, fields) -> 1 + List.length fields
            let patterns =
                calls
                |> List.map (literalPatternAt eligibleIndices)
                |> List.map (fun pattern ->
                    pattern
                    |> List.filter (fun (index, value) ->
                        match List.tryItem index func.TypedParams with
                        | Some parameter -> knownValueMatchesType parameter.Type value
                        | None -> false))
                // Rematerializing an allocated value at the top of a recursive
                // clone would allocate once per iteration instead of once per
                // entry call. Recursive cloning therefore remains immediate-
                // value only; construction facts still specialize leaf calls.
                |> List.map (fun pattern ->
                    if isRecursive then
                        pattern
                        |> List.filter (fun (_, value) ->
                            match value with
                            | LiteralValue _ -> true
                            | _ -> false)
                    else
                        pattern)
                |> List.filter (not << List.isEmpty)
                |> List.countBy id
                // Spend the bounded clone budget on call-site savings first;
                // the pattern tie-break keeps names and output deterministic.
                |> List.sortBy (fun (pattern, occurrences) ->
                    let savedWork =
                        pattern
                        |> List.sumBy (fun (_, value) -> valueBenefit value)
                        |> (*) occurrences
                    (-savedWork, pattern))
                |> List.map fst
                |> List.truncate maxLiteralClonesPerFunction
            if List.length patterns < 2 then None
            else Some (func.Name, patterns))
    |> List.sortBy fst

let private boundedCloneGroups
    (groups: (string * LiteralPattern list) list)
    : (string * LiteralPattern list) list =
    groups
    |> List.fold (fun (selected, remaining) (name, patterns) ->
        let count = List.length patterns
        if count <= remaining then
            ((name, patterns) :: selected, remaining - count)
        else
            (selected, remaining)
    ) ([], maxLiteralClonesPerProgram)
    |> fst
    |> List.rev

let private buildLiteralClones
    (existingNames: Set<string>)
    (groups: (string * LiteralPattern list) list)
    : LiteralClone list =
    let proposed =
        groups
        |> List.collect (fun (name, patterns) ->
            patterns
            |> List.mapi (fun index pattern ->
                { OriginalName = name
                  CloneName = $"{name}__literal_{index}"
                  Pattern = pattern }))
    let proposedNames = proposed |> List.map (fun clone -> clone.CloneName)
    let namesAreUnique = List.length proposedNames = (proposedNames |> List.distinct |> List.length)
    if namesAreUnique
       && proposedNames |> List.forall (fun name -> not (Set.contains name existingNames)) then
        proposed
    else
        []

let private removePatternArguments
    (pattern: LiteralPattern)
    (args: Atom list)
    : Atom list =
    let removedIndices = pattern |> List.map fst |> Set.ofList
    args
    |> List.mapi (fun index arg ->
        if Set.contains index removedIndices then None else Some arg)
    |> List.choose id

let private routeDirectCall
    (clonesByName: Map<string, LiteralClone list>)
    (env: ValueEnv)
    (name: string)
    (args: Atom list)
    : string * Atom list =
    let matchesPattern pattern =
        pattern
        |> List.forall (fun (index, value) ->
            List.tryItem index args |> Option.bind (knownValueForAtom env) = Some value)
    let matchingClone =
        Map.tryFind name clonesByName
        |> Option.bind (List.tryFind (fun clone -> matchesPattern clone.Pattern))
    match matchingClone with
    | Some clone -> (clone.CloneName, removePatternArguments clone.Pattern args)
    | None -> (name, args)

let private routeCExpr
    (clonesByName: Map<string, LiteralClone list>)
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

let rec private routeExpr
    (clonesByName: Map<string, LiteralClone list>)
    (env: ValueEnv)
    (expr: AExpr)
    : AExpr =
    match expr with
    | Jump _ -> expr
    | Join (parameter, continuation, entry) ->
        Join (
            parameter,
            routeExpr clonesByName (Map.remove parameter.Id env) continuation,
            routeExpr clonesByName env entry
        )
    | Let (id, cexpr, body) ->
        let cexpr' = routeCExpr clonesByName env cexpr
        Let (id, cexpr', routeExpr clonesByName (addKnownBinding id cexpr env) body)
    | Return atom -> Return atom
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            routeExpr clonesByName env thenBranch,
            routeExpr clonesByName env elseBranch
        )

let private cexprForKnownValue (value: KnownValue) : CExpr =
    let atoms literals = literals |> List.map atomForScalarLiteral
    match value with
    | LiteralValue literal -> Atom (atomForScalarLiteral literal)
    | Int128Value (low, high) ->
        Call ("Stdlib.Int128.__fromWords", [IntLiteral (UInt64 low); IntLiteral (UInt64 high)])
    | UInt128Value (low, high) ->
        Call ("Stdlib.UInt128.__fromWords", [IntLiteral (UInt64 low); IntLiteral (UInt64 high)])
    | TupleValue fields -> TupleAlloc (atoms fields)
    | RecordValue (descriptor, fields) -> RecordAlloc (descriptor, atoms fields)

let private cloneFunction
    (clonesByName: Map<string, LiteralClone list>)
    (functionsByName: Map<string, Function>)
    (clone: LiteralClone)
    : Function =
    let original =
        match Map.tryFind clone.OriginalName functionsByName with
        | Some func -> func
        | None -> Crash.crash $"Missing direct-call clone source '{clone.OriginalName}'"
    let literalsByIndex = clone.Pattern |> Map.ofList
    let parameters =
        original.TypedParams
        |> List.mapi (fun index parameter ->
            if Map.containsKey index literalsByIndex then None else Some parameter)
        |> List.choose id
    let substitutions =
        original.TypedParams
        |> List.mapi (fun index parameter ->
            Map.tryFind index literalsByIndex
            |> Option.bind (fun value ->
                match value with
                | LiteralValue literal -> Some (parameter.Id, atomForScalarLiteral literal)
                | _ -> None))
        |> List.choose id
        |> Map.ofList
    let materializations =
        original.TypedParams
        |> List.mapi (fun index parameter ->
            Map.tryFind index literalsByIndex
            |> Option.bind (fun value ->
                match value with
                | LiteralValue _ -> None
                | _ -> Some (parameter.Id, cexprForKnownValue value)))
        |> List.choose id
    let substitutedBody =
        let body = rewriteExpr Map.empty substitutions original.Body
        List.foldBack (fun (id, cexpr) nested -> Let (id, cexpr, nested)) materializations body
    { original with
        Name = clone.CloneName
        TypedParams = parameters
        Body = routeExpr clonesByName Map.empty substitutedBody }

let rec private exprUsesTemp (id: TempId) (expr: AExpr) : bool =
    match expr with
    | Return atom
    | Jump (_, atom) -> atomUsesTemp id atom
    | Let (boundId, cexpr, body) ->
        cexprUsesTemp id cexpr || (boundId <> id && exprUsesTemp id body)
    | Join (parameter, continuation, entry) ->
        (parameter.Id <> id && exprUsesTemp id continuation)
        || exprUsesTemp id entry
    | If (condition, thenBranch, elseBranch) ->
        atomUsesTemp id condition
        || exprUsesTemp id thenBranch
        || exprUsesTemp id elseBranch

let private isRematerializedValue (cexpr: CExpr) : bool =
    match knownValueForCExpr Map.empty cexpr with
    | Some _ -> true
    | None -> false

// Routing a construction-valued argument removes its sole use. Eliminate only
// the exact, side-effect-free recipes that this pass knows how to recreate;
// arbitrary calls and allocations retain their original evaluation boundary.
let rec private removeUnusedRematerializedValues (expr: AExpr) : AExpr =
    match expr with
    | Return _
    | Jump _ -> expr
    | Let (id, cexpr, body) ->
        let body' = removeUnusedRematerializedValues body
        if isRematerializedValue cexpr && not (exprUsesTemp id body') then body'
        else Let (id, cexpr, body')
    | Join (parameter, continuation, entry) ->
        Join (
            parameter,
            removeUnusedRematerializedValues continuation,
            removeUnusedRematerializedValues entry
        )
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            removeUnusedRematerializedValues thenBranch,
            removeUnusedRematerializedValues elseBranch
        )

let private specializeFiniteLiterals (Program (functions, main)) : Program =
    let analysis = analyzeProgram functions main
    let knownCalls = knownCallsInProgram functions main
    let clones =
        cloneGroups analysis knownCalls functions
        |> boundedCloneGroups
        |> buildLiteralClones (functions |> List.map (fun func -> func.Name) |> Set.ofList)
    let clonesByName =
        clones
        |> List.groupBy (fun clone -> clone.OriginalName)
        |> Map.ofList
    let functionsByName = functions |> List.map (fun func -> (func.Name, func)) |> Map.ofList
    let clonedFunctions = clones |> List.map (cloneFunction clonesByName functionsByName)
    let routedFunctions =
        functions
        |> List.map (fun func ->
            { func with
                Body =
                    routeExpr clonesByName Map.empty func.Body
                    |> removeUnusedRematerializedValues })
    let main' =
        routeExpr clonesByName Map.empty main
        |> removeUnusedRematerializedValues
    Program (clonedFunctions @ routedFunctions, main')

let specializeProgram (program: Program) : Program =
    let (Program (functions, main)) = exposeKnownIndirectTargets program
    let analysis = analyzeProgram functions main
    let rewriteMap = buildRewriteMap analysis functions
    let functions' = functions |> List.map (rewriteFunction rewriteMap)
    let main' = rewriteExpr rewriteMap Map.empty main
    specializeFiniteLiterals (Program (functions', main'))
