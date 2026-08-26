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

type private ScalarLiteral =
    | UnitScalar
    | IntScalar of SizedInt
    | BoolScalar of bool
    | FloatScalar of int64

type private LiteralPattern = (int * ScalarLiteral) list

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
    | RecordClone (_, record, fields) -> analyzeMany (record :: fields) analysis
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
    | StringLiteral _
    | Var _
    | FuncRef _ -> None

let private atomForScalarLiteral (literal: ScalarLiteral) : Atom =
    match literal with
    | UnitScalar -> UnitLiteral
    | IntScalar value -> IntLiteral value
    | BoolScalar value -> BoolLiteral value
    | FloatScalar bits -> FloatLiteral (System.BitConverter.Int64BitsToDouble bits)

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
    | AST.TUnit -> true
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
            | true, Some literal -> ReplaceParameterWith literal
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
    | RawTake (ptr, offset, typ) -> RawTake (rewrite ptr, rewrite offset, typ)
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

let private literalPatternAt (eligibleIndices: Set<int>) (args: Atom list) : LiteralPattern =
    args
    |> List.mapi (fun index atom ->
        if Set.contains index eligibleIndices then
            scalarLiteralAtom atom |> Option.map (fun literal -> (index, literal))
        else
            None)
    |> List.choose id

let rec private directCallsTo (target: string) (expr: AExpr) : Atom list list =
    match expr with
    | Return _ -> []
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
            if isScalarLiteralType parameter.Type then Some index else None)
        |> List.choose id
        |> Set.ofList
    match directCallsTo func.Name func.Body with
    | [] -> allIndices
    | selfCalls ->
        func.TypedParams
        |> List.mapi (fun index parameter ->
            let isPassedThrough =
                isScalarLiteralType parameter.Type
                && (selfCalls
                    |> List.forall (fun args ->
                        List.tryItem index args = Some (Var parameter.Id)))
            if isPassedThrough then Some index else None)
        |> List.choose id
        |> Set.ofList

let private cloneGroups
    (analysis: ProgramAnalysis)
    (functions: Function list)
    : (string * LiteralPattern list) list =
    functions
    |> List.choose (fun func ->
        match Map.tryFind func.Name analysis.DirectCalls with
        | None -> None
        | Some _ when Set.contains func.Name analysis.IndirectTargets -> None
        | Some calls ->
            let eligibleIndices = cloneableParameterIndices func
            let patterns =
                calls
                |> List.map (literalPatternAt eligibleIndices)
                |> List.filter (not << List.isEmpty)
                |> List.distinct
                |> List.sort
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
    (name: string)
    (args: Atom list)
    : string * Atom list =
    let matchesPattern pattern =
        pattern
        |> List.forall (fun (index, literal) ->
            List.tryItem index args |> Option.bind scalarLiteralAtom = Some literal)
    let matchingClone =
        Map.tryFind name clonesByName
        |> Option.bind (List.tryFind (fun clone -> matchesPattern clone.Pattern))
    match matchingClone with
    | Some clone -> (clone.CloneName, removePatternArguments clone.Pattern args)
    | None -> (name, args)

let private routeCExpr
    (clonesByName: Map<string, LiteralClone list>)
    (cexpr: CExpr)
    : CExpr =
    match cexpr with
    | Call (name, args) ->
        let (target, routedArgs) = routeDirectCall clonesByName name args
        Call (target, routedArgs)
    | BorrowedCall (name, args) ->
        let (target, routedArgs) = routeDirectCall clonesByName name args
        BorrowedCall (target, routedArgs)
    | TailCall (name, args) ->
        let (target, routedArgs) = routeDirectCall clonesByName name args
        TailCall (target, routedArgs)
    | _ -> cexpr

let rec private routeExpr
    (clonesByName: Map<string, LiteralClone list>)
    (expr: AExpr)
    : AExpr =
    match expr with
    | Let (id, cexpr, body) ->
        Let (id, routeCExpr clonesByName cexpr, routeExpr clonesByName body)
    | Return atom -> Return atom
    | If (condition, thenBranch, elseBranch) ->
        If (condition, routeExpr clonesByName thenBranch, routeExpr clonesByName elseBranch)

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
            |> Option.map (fun literal -> (parameter.Id, atomForScalarLiteral literal)))
        |> List.choose id
        |> Map.ofList
    let substitutedBody = rewriteExpr Map.empty substitutions original.Body
    { original with
        Name = clone.CloneName
        TypedParams = parameters
        Body = routeExpr clonesByName substitutedBody }

let private specializeFiniteLiterals (Program (functions, main)) : Program =
    let analysis = analyzeProgram functions main
    let clones =
        cloneGroups analysis functions
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
        |> List.map (fun func -> { func with Body = routeExpr clonesByName func.Body })
    Program (clonedFunctions @ routedFunctions, routeExpr clonesByName main)

let specializeProgram (Program (functions, main)) : Program =
    let analysis = analyzeProgram functions main
    let rewriteMap = buildRewriteMap analysis functions
    let functions' = functions |> List.map (rewriteFunction rewriteMap)
    let main' = rewriteExpr rewriteMap Map.empty main
    specializeFiniteLiterals (Program (functions', main'))
