// NameResolution.fs - Canonical semantic name resolution
//
// Builds an immutable inventory of compiler-visible symbols and resolves parsed
// qualified names according to the interpreter's context-specific precedence.
// No spelling recovery is performed here: every accepted spelling must be an
// explicit candidate in the inventory.

module NameResolution

open AST

[<StructuralEquality; StructuralComparison>]
type QualifiedName = private QualifiedName of NonEmptyList<string>

[<RequireQualifiedAccess>]
type ResolutionContext =
    | Value
    | Callable
    | Constructor
    | Type

[<StructuralEquality; StructuralComparison>]
type NamespaceIdentity =
    | RootNamespace
    | ModuleNamespace of path:NonEmptyList<string>
    | PackageNamespace of owner:string * modules:string list
    | BuiltinNamespace

[<StructuralEquality; StructuralComparison>]
type SymbolIdentity =
    | LocalValue of name:string
    | ModuleValue of namespaceIdentity:NamespaceIdentity * name:string
    | PackageValue of namespaceIdentity:NamespaceIdentity * name:string
    | BuiltinValue of name:string * version:int
    | ModuleFunction of namespaceIdentity:NamespaceIdentity * name:string * declarationId:string
    | PackageFunction of namespaceIdentity:NamespaceIdentity * name:string * declarationId:string
    | BuiltinFunction of name:string * version:int
    | ConstructorSymbol of declaringType:string * caseName:string
    | UserType of qualifiedName:string
    | BuiltinType of name:string

[<StructuralEquality; StructuralComparison>]
type CandidateProvenance =
    | LexicalBinding of name:string
    | SourceDeclaration of qualifiedName:string
    | ModuleDeclaration of qualifiedName:string
    | PackageDeclaration of qualifiedName:string
    | BuiltinRegistration of qualifiedName:string
    | CompilerExtension of qualifiedName:string

type Candidate = {
    VisibleName: QualifiedName
    Identity: SymbolIdentity
    Provenance: CandidateProvenance
}

type ResolutionEnvironment = private ResolutionEnvironment of Candidate list

type SuccessfulResolution = {
    OriginalName: QualifiedName
    Context: ResolutionContext
    Identity: SymbolIdentity
    Provenance: CandidateProvenance
}

type ResolutionError =
    | InvalidQualifiedName of originalName:string * context:ResolutionContext
    | UnresolvedName of originalName:QualifiedName * context:ResolutionContext
    | AmbiguousReference of
        originalName:QualifiedName *
        context:ResolutionContext *
        orderedCandidates:SymbolIdentity list

let tryQualifiedName (spelling: string) : QualifiedName option =
    NameSyntax.tryParseLegacySpelling spelling
    |> Option.bind (fun parsed ->
        let segments =
            parsed
            |> NameSyntax.segments
            |> List.map NameSyntax.identifierText
        if List.isEmpty segments || List.exists (fun segment -> segment = "") segments then None
        else segments |> NonEmptyList.tryFromList |> Option.map QualifiedName)

let qualifiedNameFromSegments (segments: NonEmptyList<string>) : QualifiedName =
    QualifiedName segments

let qualifiedNameSegments (QualifiedName segments) : string list =
    NonEmptyList.toList segments

let qualifiedNameToString (name: QualifiedName) : string =
    name |> qualifiedNameSegments |> String.concat "."

let private namespaceToString (identity: NamespaceIdentity) : string =
    match identity with
    | RootNamespace -> ""
    | ModuleNamespace path -> path |> NonEmptyList.toList |> String.concat "."
    | PackageNamespace (owner, modules) -> String.concat "." (owner :: modules)
    | BuiltinNamespace -> "Builtin"

let symbolIdentityToString (identity: SymbolIdentity) : string =
    match identity with
    | LocalValue name -> $"local value {name}"
    | ModuleValue (RootNamespace, name) -> $"value {name}"
    | PackageValue (RootNamespace, name) -> $"value {name}"
    | ModuleValue (ns, name) -> $"module value {namespaceToString ns}.{name}"
    | PackageValue (ns, name) -> $"package value {namespaceToString ns}.{name}"
    | BuiltinValue (name, version) -> $"builtin value Builtin.{name}_v{version}"
    | ModuleFunction (RootNamespace, name, _) -> $"function {name}"
    | PackageFunction (RootNamespace, name, _) -> $"function {name}"
    | ModuleFunction (ns, name, _) -> $"function {namespaceToString ns}.{name}"
    | PackageFunction (ns, name, _) -> $"function {namespaceToString ns}.{name}"
    | BuiltinFunction (name, version) -> $"builtin function Builtin.{name}_v{version}"
    | ConstructorSymbol (declaringType, caseName) -> $"constructor {declaringType}.{caseName}"
    | UserType name -> $"user type {name}"
    | BuiltinType name -> $"builtin type {name}"

let canonicalSpelling (identity: SymbolIdentity) : string =
    match identity with
    | LocalValue name -> name
    | ModuleValue (RootNamespace, name)
    | PackageValue (RootNamespace, name)
    | ModuleFunction (RootNamespace, name, _)
    | PackageFunction (RootNamespace, name, _) -> name
    | ModuleValue (ns, name)
    | PackageValue (ns, name)
    | ModuleFunction (ns, name, _)
    | PackageFunction (ns, name, _) -> $"{namespaceToString ns}.{name}"
    | BuiltinValue (name, _)
    | BuiltinFunction (name, _) -> $"Builtin.{name}"
    | ConstructorSymbol (declaringType, caseName) -> $"{declaringType}.{caseName}"
    | UserType name
    | BuiltinType name -> name

let empty : ResolutionEnvironment = ResolutionEnvironment []

let candidates (ResolutionEnvironment candidates) : Candidate list = candidates

let addCandidate (candidate: Candidate) (ResolutionEnvironment candidates) : ResolutionEnvironment =
    ResolutionEnvironment (candidate :: candidates)

let addCandidates (newCandidates: Candidate list) (ResolutionEnvironment candidates) : ResolutionEnvironment =
    ResolutionEnvironment (newCandidates @ candidates)

let merge (ResolutionEnvironment baseCandidates) (ResolutionEnvironment overlayCandidates) : ResolutionEnvironment =
    let imported (candidate: Candidate) : Candidate =
        let provenance =
            match candidate.Provenance with
            | SourceDeclaration name
            | ModuleDeclaration name -> PackageDeclaration name
            | other -> other
        { candidate with Provenance = provenance }
    ResolutionEnvironment (overlayCandidates @ List.map imported baseCandidates)

let candidate
    (visibleName: string)
    (identity: SymbolIdentity)
    (provenance: CandidateProvenance)
    : Candidate option =
    tryQualifiedName visibleName
    |> Option.map (fun name ->
        { VisibleName = name
          Identity = identity
          Provenance = provenance })

let private identityCategory (identity: SymbolIdentity) : string =
    match identity with
    | LocalValue _ -> "local"
    | ModuleValue _ | PackageValue _ | BuiltinValue _ -> "value"
    | ModuleFunction _ | PackageFunction _ | BuiltinFunction _ -> "function"
    | ConstructorSymbol _ -> "constructor"
    | UserType _ | BuiltinType _ -> "type"

let private precedence (context: ResolutionContext) (identity: SymbolIdentity) : int option =
    match context, identityCategory identity with
    | ResolutionContext.Value, "local" -> Some 0
    | ResolutionContext.Value, "value" -> Some 1
    | ResolutionContext.Value, "function" -> Some 2
    | ResolutionContext.Callable, "local" -> Some 0
    | ResolutionContext.Callable, "function" -> Some 1
    | ResolutionContext.Callable, "value" -> Some 2
    | ResolutionContext.Constructor, "constructor" -> Some 0
    | ResolutionContext.Type, "type" -> Some 0
    | _ -> None

let private provenancePrecedence (provenance: CandidateProvenance) : int =
    match provenance with
    | LexicalBinding _ -> 0
    | SourceDeclaration _ -> 1
    | ModuleDeclaration _ -> 2
    | PackageDeclaration _ -> 3
    | BuiltinRegistration _ -> 4
    | CompilerExtension _ -> 5

let private orderedDistinctCandidates (candidates: Candidate list) : Candidate list =
    candidates
    |> List.groupBy (fun candidate -> candidate.Identity)
    |> List.map (fun (_, sameIdentity) ->
        sameIdentity
        |> List.sortBy (fun candidate -> candidate.Provenance)
        |> function
            | first :: _ -> first
            | [] -> Crash.crash "Candidate identity group was unexpectedly empty")
    |> List.sortBy (fun candidate -> symbolIdentityToString candidate.Identity)

let resolveQualified
    (context: ResolutionContext)
    (name: QualifiedName)
    (ResolutionEnvironment inventory)
    : Result<SuccessfulResolution, ResolutionError> =
    let matching =
        inventory
        |> List.filter (fun candidate -> candidate.VisibleName = name)
        |> List.choose (fun candidate ->
            precedence context candidate.Identity
            |> Option.map (fun rank -> ((rank, provenancePrecedence candidate.Provenance), candidate)))

    match matching with
    | [] -> Error (UnresolvedName (name, context))
    | _ ->
        let winningRank =
            match matching with
            | (firstRank, _) :: rest -> rest |> List.fold (fun best (rank, _) -> min best rank) firstRank
            | [] -> Crash.crash "Resolver matching candidates unexpectedly became empty"
        let winners =
            matching
            |> List.choose (fun (rank, candidate) ->
                if rank = winningRank then Some candidate else None)
            |> orderedDistinctCandidates

        match winners with
        | [winner] ->
            Ok
                { OriginalName = name
                  Context = context
                  Identity = winner.Identity
                  Provenance = winner.Provenance }
        | _ ->
            winners
            |> List.map (fun candidate -> candidate.Identity)
            |> fun identities -> Error (AmbiguousReference (name, context, identities))

let resolve
    (context: ResolutionContext)
    (spelling: string)
    (environment: ResolutionEnvironment)
    : Result<SuccessfulResolution, ResolutionError> =
    match tryQualifiedName spelling with
    | Some name -> resolveQualified context name environment
    | None -> Error (InvalidQualifiedName (spelling, context))

let contextToString (context: ResolutionContext) : string =
    match context with
    | ResolutionContext.Value -> "value"
    | ResolutionContext.Callable -> "callable"
    | ResolutionContext.Constructor -> "constructor"
    | ResolutionContext.Type -> "type"

let errorToString (error: ResolutionError) : string =
    match error with
    | InvalidQualifiedName (name, context) ->
        $"Invalid {contextToString context} name: {name}"
    | UnresolvedName (name, context) ->
        $"Unresolved {contextToString context} name: {qualifiedNameToString name}"
    | AmbiguousReference (name, context, identities) ->
        let candidates = identities |> List.map symbolIdentityToString |> String.concat ", "
        $"Ambiguous {contextToString context} reference '{qualifiedNameToString name}': {candidates}"
