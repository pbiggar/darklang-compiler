// 2.4.5_ANF_DirectCallSpecialization.fs - Specialize internal direct-call signatures.
//
// Rewrites parameters only when every call site is present in the current ANF
// program. Functions used as raw addresses or closure targets retain their
// original calling convention.

module ANF_DirectCallSpecialization

open ANF

type private ParameterRewrite =
    | KeepParameter
    | ReplaceParameterWith of Atom

type private ProgramAnalysis = {
    DirectCalls: Map<string, Atom list list>
    IndirectTargets: Set<string>
}

let private emptyAnalysis = {
    DirectCalls = Map.empty
    IndirectTargets = Set.empty
}

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
    | RawFree atom
    | RawGetByte (atom, _)
    | StringToRawPtr atom
    | RawPtrToString atom
    | BlobToRawPtr atom
    | RawPtrToBlob atom
    | DictToRawPtr atom
    | ListToRawPtr atom
    | RefCountIncString atom
    | RefCountDecString atom
    | RefCountIncBlob atom
    | RefCountDecBlob atom
    | FloatToString atom
    | Sleep atom
    | RuntimeErrorString atom -> analyze atom analysis
    | Prim (_, left, right)
    | StringConcat (left, right)
    | FileWriteText (left, right)
    | FileAppendText (left, right)
    | RawGet (left, right, _)
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
    | CliNative (_, args) -> analyzeMany args analysis
    | TupleGet (tuple, _) -> analyze tuple analysis
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
    | If (condition, thenBranch, elseBranch) ->
        analysis
        |> analyzeAtom condition
        |> analyzeExpr thenBranch
        |> analyzeExpr elseBranch

let private analyzeProgram (functions: Function list) (main: AExpr) : ProgramAnalysis =
    functions
    |> List.fold (fun analysis func -> analyzeExpr func.Body analysis) emptyAnalysis
    |> analyzeExpr main

let private literalAtom (atom: Atom) : Atom option =
    match atom with
    | UnitLiteral
    | IntLiteral _
    | BoolLiteral _
    | StringLiteral _
    | FloatLiteral _ -> Some atom
    | Var _
    | FuncRef _ -> None

let private uniformLiteralAt (index: int) (calls: Atom list list) : Atom option =
    let literals =
        calls
        |> List.map (fun args -> List.tryItem index args |> Option.bind literalAtom)
    match literals with
    | Some first :: rest when rest |> List.forall (fun literal -> literal = Some first) -> Some first
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
        |> List.mapi (fun index _parameter ->
            match uniformLiteralAt index calls with
            | Some literal -> ReplaceParameterWith literal
            | None -> KeepParameter)
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
    | StringConcat (left, right) -> StringConcat (rewrite left, rewrite right)
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
    | RawFree atom -> RawFree (rewrite atom)
    | RawGet (ptr, offset, typ) -> RawGet (rewrite ptr, rewrite offset, typ)
    | RawGetByte (ptr, offset) -> RawGetByte (rewrite ptr, offset)
    | RawWriteWord (ptr, offset, value) -> RawWriteWord (rewrite ptr, rewrite offset, rewrite value)
    | RawWriteByte (ptr, offset, value) -> RawWriteByte (rewrite ptr, rewrite offset, rewrite value)
    | RawSlotInit (ptr, offset, value, typ) -> RawSlotInit (rewrite ptr, rewrite offset, rewrite value, typ)
    | StringToRawPtr atom -> StringToRawPtr (rewrite atom)
    | RawPtrToString atom -> RawPtrToString (rewrite atom)
    | BlobToRawPtr atom -> BlobToRawPtr (rewrite atom)
    | RawPtrToBlob atom -> RawPtrToBlob (rewrite atom)
    | DictToRawPtr atom -> DictToRawPtr (rewrite atom)
    | RawPtrToDict (ptr, tag, typ) -> RawPtrToDict (rewrite ptr, rewrite tag, typ)
    | ListToRawPtr atom -> ListToRawPtr (rewrite atom)
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

let specializeProgram (Program (functions, main)) : Program =
    let analysis = analyzeProgram functions main
    let rewriteMap = buildRewriteMap analysis functions
    let functions' = functions |> List.map (rewriteFunction rewriteMap)
    let main' = rewriteExpr rewriteMap Map.empty main
    Program (functions', main')
