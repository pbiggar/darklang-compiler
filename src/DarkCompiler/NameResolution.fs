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

type ResolutionEnvironment = private {
    OrderedCandidates: Candidate list
    CandidatesByVisibleName: Map<QualifiedName, Candidate list>
    ImportedOrderedCandidates: Candidate list
    ImportedCandidatesByVisibleName: Map<QualifiedName, Candidate list>
}

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

let private importCandidate (candidate: Candidate) : Candidate =
    let provenance =
        match candidate.Provenance with
        | SourceDeclaration name
        | ModuleDeclaration name -> PackageDeclaration name
        | other -> other
    { candidate with Provenance = provenance }

let empty : ResolutionEnvironment = {
    OrderedCandidates = []
    CandidatesByVisibleName = Map.empty
    ImportedOrderedCandidates = []
    ImportedCandidatesByVisibleName = Map.empty
}

let candidates (environment: ResolutionEnvironment) : Candidate list =
    environment.OrderedCandidates

let addCandidate
    (candidate: Candidate)
    (environment: ResolutionEnvironment)
    : ResolutionEnvironment =
    let sameName =
        Map.tryFind candidate.VisibleName environment.CandidatesByVisibleName
        |> Option.defaultValue []
    let importedCandidate = importCandidate candidate
    let importedSameName =
        Map.tryFind
            importedCandidate.VisibleName
            environment.ImportedCandidatesByVisibleName
        |> Option.defaultValue []
    {
        OrderedCandidates = candidate :: environment.OrderedCandidates
        CandidatesByVisibleName =
            Map.add
                candidate.VisibleName
                (candidate :: sameName)
                environment.CandidatesByVisibleName
        ImportedOrderedCandidates =
            importedCandidate :: environment.ImportedOrderedCandidates
        ImportedCandidatesByVisibleName =
            Map.add
                importedCandidate.VisibleName
                (importedCandidate :: importedSameName)
                environment.ImportedCandidatesByVisibleName
    }

let addCandidates
    (newCandidates: Candidate list)
    (environment: ResolutionEnvironment)
    : ResolutionEnvironment =
    List.foldBack addCandidate newCandidates environment

/// Keep only candidates accepted by a compilation boundary.
let filterCandidates
    (predicate: Candidate -> bool)
    (environment: ResolutionEnvironment)
    : ResolutionEnvironment =
    environment.OrderedCandidates
    |> List.filter predicate
    |> fun filtered -> addCandidates filtered empty

let merge
    (baseEnvironment: ResolutionEnvironment)
    (overlayEnvironment: ResolutionEnvironment)
    : ResolutionEnvironment =
    let mergeCandidateMaps baseCandidates overlayCandidates =
        overlayCandidates
        |> Map.fold (fun combined visibleName candidates ->
            let baseCandidates =
                Map.tryFind visibleName combined |> Option.defaultValue []
            Map.add visibleName (candidates @ baseCandidates) combined
        ) baseCandidates
    {
        OrderedCandidates =
            overlayEnvironment.OrderedCandidates
            @ baseEnvironment.ImportedOrderedCandidates
        CandidatesByVisibleName =
            mergeCandidateMaps
                baseEnvironment.ImportedCandidatesByVisibleName
                overlayEnvironment.CandidatesByVisibleName
        ImportedOrderedCandidates =
            overlayEnvironment.ImportedOrderedCandidates
            @ baseEnvironment.ImportedOrderedCandidates
        ImportedCandidatesByVisibleName =
            mergeCandidateMaps
                baseEnvironment.ImportedCandidatesByVisibleName
                overlayEnvironment.ImportedCandidatesByVisibleName
    }

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
    (environment: ResolutionEnvironment)
    : Result<SuccessfulResolution, ResolutionError> =
    let matching =
        Map.tryFind name environment.CandidatesByVisibleName
        |> Option.defaultValue []
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

let private qualifiedNameFromList (segments: string list) : QualifiedName =
    segments
    |> NonEmptyList.tryFromList
    |> Option.map QualifiedName
    |> Option.defaultWith (fun () -> Crash.crash "Name-resolution candidate was unexpectedly empty")

/// Generate the interpreter's ordered relative-name candidates. `Darklang.Stdlib`
/// is canonical; `Stdlib` is the interpreter's explicit source shortcut.
let private namesToTry
    (context: ResolutionContext)
    (currentModule: string list)
    (given: QualifiedName)
    : QualifiedName list =
    let givenSegments = qualifiedNameSegments given
    let rec relative prefixes =
        match prefixes with
        | [] -> [qualifiedNameFromList givenSegments]
        | _ ->
            qualifiedNameFromList (prefixes @ givenSegments)
            :: relative (prefixes |> List.rev |> List.tail |> List.rev)
    let aliases =
        match context, givenSegments with
        | _, "Stdlib" :: rest ->
            [qualifiedNameFromList ("Darklang" :: "Stdlib" :: rest)]
        | ResolutionContext.Type, ["Option"] ->
            [qualifiedNameFromList ["Darklang"; "Stdlib"; "Option"; "Option"]]
        | ResolutionContext.Type, ["Result"] ->
            [qualifiedNameFromList ["Darklang"; "Stdlib"; "Result"; "Result"]]
        | ResolutionContext.Constructor, ["Option"; ("Some" | "None" as caseName)] ->
            [qualifiedNameFromList ["Darklang"; "Stdlib"; "Option"; "Option"; caseName]]
        | ResolutionContext.Constructor, ["Result"; ("Ok" | "Error" as caseName)] ->
            [qualifiedNameFromList ["Darklang"; "Stdlib"; "Result"; "Result"; caseName]]
        | _ -> []
    relative currentModule @ aliases
    |> List.distinct

let internal candidateSpellings
    (context: ResolutionContext)
    (currentModule: string list)
    (spelling: string)
    : string list =
    match tryQualifiedName spelling with
    | None -> []
    | Some name ->
        namesToTry context currentModule name
        |> List.map qualifiedNameToString

let resolveInModule
    (context: ResolutionContext)
    (currentModule: string list)
    (spelling: string)
    (environment: ResolutionEnvironment)
    : Result<SuccessfulResolution, ResolutionError> =
    match tryQualifiedName spelling with
    | None -> Error (InvalidQualifiedName (spelling, context))
    | Some originalName ->
        let rec tryCandidates candidates =
            match candidates with
            | [] -> Error (UnresolvedName (originalName, context))
            | candidate :: rest ->
                match resolveQualified context candidate environment with
                | Ok resolution -> Ok { resolution with OriginalName = originalName }
                | Error (UnresolvedName _) -> tryCandidates rest
                | Error (AmbiguousReference (_, _, identities)) ->
                    Error (AmbiguousReference (originalName, context, identities))
                | Error error -> Error error
        namesToTry context currentModule originalName |> tryCandidates

let resolve
    (context: ResolutionContext)
    (spelling: string)
    (environment: ResolutionEnvironment)
    : Result<SuccessfulResolution, ResolutionError> =
    resolveInModule context [] spelling environment

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
        match context with
        | ResolutionContext.Callable ->
            $"There is no variable named: {qualifiedNameToString name}"
        | _ ->
            $"Unresolved {contextToString context} name: {qualifiedNameToString name}"
    | AmbiguousReference (name, context, identities) ->
        let candidates = identities |> List.map symbolIdentityToString |> String.concat ", "
        $"Ambiguous {contextToString context} reference '{qualifiedNameToString name}': {candidates}"
