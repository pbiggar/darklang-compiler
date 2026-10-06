(*
   NameResolution.fs - Canonical semantic name resolution
   Builds an immutable inventory of compiler-visible symbols and resolves parsed
   qualified names according to the interpreter's context-specific precedence.
   No spelling recovery is performed here: every accepted spelling must be an
   explicit candidate in the inventory.
*)
[@@@warning "-30-4"]
(* NameResolution.ml - Immutable canonical symbol inventory and contextual resolution. *)
type qualifiedName = QualifiedName of string NonEmptyList.t
type resolutionContext = Value | Callable | Constructor | Type
type namespaceIdentity = RootNamespace | ModuleNamespace of string NonEmptyList.t | PackageNamespace of string * string list | BuiltinNamespace
type symbolIdentity =
  | LocalValue of string | ModuleValue of namespaceIdentity * string | PackageValue of namespaceIdentity * string
  | BuiltinValue of string * int | ModuleFunction of namespaceIdentity * string * string
  | PackageFunction of namespaceIdentity * string * string | BuiltinFunction of string * int
  | ConstructorSymbol of string * string | UserType of string | BuiltinType of string
type candidateProvenance = LexicalBinding of string | SourceDeclaration of string | ModuleDeclaration of string | PackageDeclaration of string | BuiltinRegistration of string | CompilerExtension of string
type candidate = {visibleName : qualifiedName; identity : symbolIdentity; provenance : candidateProvenance}
type successfulResolution = {originalName : qualifiedName; context : resolutionContext; identity : symbolIdentity; provenance : candidateProvenance}
type resolutionError = InvalidQualifiedName of string * resolutionContext | UnresolvedName of qualifiedName * resolutionContext | AmbiguousReference of qualifiedName * resolutionContext * symbolIdentity list
let tryQualifiedName spelling =
  Option.bind (NameSyntax.tryParseLegacySpelling spelling) (fun parsed ->
    let segments = List.map NameSyntax.identifierText (NameSyntax.segments parsed) in
    if segments = [] || List.exists ((=) "") segments then None
    else Option.map (fun xs -> QualifiedName xs) (NonEmptyList.tryFromList segments))
let qualifiedNameFromSegments segments = QualifiedName segments
let qualifiedNameSegments (QualifiedName segments) = NonEmptyList.toList segments
let qualifiedNameToString name = String.concat "." (qualifiedNameSegments name)
let namespaceToString = function
  | RootNamespace -> "" | ModuleNamespace path -> String.concat "." (NonEmptyList.toList path)
  | PackageNamespace (owner, modules) -> String.concat "." (owner :: modules) | BuiltinNamespace -> "Builtin"
let symbolIdentityToString = function
  | LocalValue name -> "local value " ^ name
  | ModuleValue (RootNamespace, name) | PackageValue (RootNamespace, name) -> "value " ^ name
  | ModuleValue (ns, name) -> "module value " ^ namespaceToString ns ^ "." ^ name
  | PackageValue (ns, name) -> "package value " ^ namespaceToString ns ^ "." ^ name
  | BuiltinValue (name, version) -> Printf.sprintf "builtin value Builtin.%s_v%d" name version
  | ModuleFunction (RootNamespace, name, _) | PackageFunction (RootNamespace, name, _) -> "function " ^ name
  | ModuleFunction (ns, name, _) | PackageFunction (ns, name, _) -> "function " ^ namespaceToString ns ^ "." ^ name
  | BuiltinFunction (name, version) -> Printf.sprintf "builtin function Builtin.%s_v%d" name version
  | ConstructorSymbol (declaringType, caseName) -> "constructor " ^ declaringType ^ "." ^ caseName
  | UserType name -> "user type " ^ name | BuiltinType name -> "builtin type " ^ name
let canonicalSpelling = function
  | LocalValue name -> name
  | ModuleValue (RootNamespace, name) | PackageValue (RootNamespace, name)
  | ModuleFunction (RootNamespace, name, _) | PackageFunction (RootNamespace, name, _) -> name
  | ModuleValue (ns, name) | PackageValue (ns, name)
  | ModuleFunction (ns, name, _) | PackageFunction (ns, name, _) -> namespaceToString ns ^ "." ^ name
  | BuiltinValue (name, _) | BuiltinFunction (name, _) -> "Builtin." ^ name
  | ConstructorSymbol (declaringType, caseName) -> declaringType ^ "." ^ caseName
  | UserType name | BuiltinType name -> name
let importCandidate (candidate : candidate) =
  let provenance = match candidate.provenance with SourceDeclaration name | ModuleDeclaration name -> PackageDeclaration name | other -> other in
  {candidate with provenance}
module QualifiedOrder = struct
  type t = qualifiedName
  let rec compareSegments left right = match left, right with
    | [], [] -> 0 | [], _ :: _ -> -1 | _ :: _, [] -> 1
    | x :: xs, y :: ys -> let c = StringOrder.compare x y in if c = 0 then compareSegments xs ys else c
  let compare (QualifiedName left) (QualifiedName right) =
    let c = StringOrder.compare left.NonEmptyList.head right.NonEmptyList.head in
    if c = 0 then compareSegments left.NonEmptyList.tail right.NonEmptyList.tail else c
end
module Names = Map.Make(QualifiedOrder)
module NameSet = Set.Make(QualifiedOrder)
type resolutionEnvironment = {
  orderedCandidates : candidate list; candidatesByVisibleName : candidate list Names.t;
  importedOrderedCandidates : candidate list; importedCandidatesByVisibleName : candidate list Names.t}
let empty = {orderedCandidates = []; candidatesByVisibleName = Names.empty; importedOrderedCandidates = []; importedCandidatesByVisibleName = Names.empty}
let candidates environment = environment.orderedCandidates
let indexed name index = Option.value (Names.find_opt name index) ~default:[]
let addCandidate (candidate : candidate) environment =
  let importedCandidate = importCandidate candidate in
  {orderedCandidates = candidate :: environment.orderedCandidates;
   candidatesByVisibleName = Names.add candidate.visibleName (candidate :: indexed candidate.visibleName environment.candidatesByVisibleName) environment.candidatesByVisibleName;
   importedOrderedCandidates = importedCandidate :: environment.importedOrderedCandidates;
   importedCandidatesByVisibleName = Names.add importedCandidate.visibleName (importedCandidate :: indexed importedCandidate.visibleName environment.importedCandidatesByVisibleName) environment.importedCandidatesByVisibleName}
let addCandidates newCandidates environment = List.fold_right addCandidate newCandidates environment
(*
   Keep only candidates accepted by a compilation boundary.
   Imported candidates correspond to the same ordered declarations, with
   import provenance already applied. Preserve those objects and only edit
   index entries whose declarations are actually removed.
*)
let filterCandidates predicate environment =
  let ordered, imported, changedNames =
    List.fold_right2 (fun (candidate : candidate) importedCandidate (ordered, imported, changed) ->
      if predicate candidate then candidate :: ordered, importedCandidate :: imported, changed
      else ordered, imported, NameSet.add candidate.visibleName changed)
      environment.orderedCandidates environment.importedOrderedCandidates ([], [], NameSet.empty) in
  if NameSet.is_empty changedNames then environment else
  let candidatesByVisibleName, importedCandidatesByVisibleName =
    NameSet.fold (fun name (index, importedIndex) ->
      let original = match Names.find_opt name environment.candidatesByVisibleName with Some xs -> xs | None -> Crash.crash "Filtered declaration has no candidate index entry" in
      let retained = List.filter predicate original in
      match retained with
      | [] -> Names.remove name index, Names.remove name importedIndex
      | _ :: _ -> Names.add name retained index, Names.add name (List.map importCandidate retained) importedIndex)
      changedNames (environment.candidatesByVisibleName, environment.importedCandidatesByVisibleName) in
  {orderedCandidates = ordered; candidatesByVisibleName; importedOrderedCandidates = imported; importedCandidatesByVisibleName}
let merge baseEnvironment overlayEnvironment =
  let mergeCandidateMaps base overlay =
    Names.union (fun _ baseCandidates overlayCandidates -> Some (overlayCandidates @ baseCandidates)) base overlay in
  {orderedCandidates = overlayEnvironment.orderedCandidates @ baseEnvironment.importedOrderedCandidates;
   candidatesByVisibleName = mergeCandidateMaps baseEnvironment.importedCandidatesByVisibleName overlayEnvironment.candidatesByVisibleName;
   importedOrderedCandidates = overlayEnvironment.importedOrderedCandidates @ baseEnvironment.importedOrderedCandidates;
   importedCandidatesByVisibleName = mergeCandidateMaps baseEnvironment.importedCandidatesByVisibleName overlayEnvironment.importedCandidatesByVisibleName}
let candidate visibleName identity provenance = Option.map (fun visibleName -> {visibleName; identity; provenance}) (tryQualifiedName visibleName)
let identityCategory = function
  | LocalValue _ -> "local" | ModuleValue _ | PackageValue _ | BuiltinValue _ -> "value"
  | ModuleFunction _ | PackageFunction _ | BuiltinFunction _ -> "function"
  | ConstructorSymbol _ -> "constructor" | UserType _ | BuiltinType _ -> "type"
let precedence context identity = match context, identityCategory identity with
  | Value, "local" | Callable, "local" | Constructor, "constructor" | Type, "type" -> Some 0
  | Value, "value" | Callable, "function" -> Some 1
  | Value, "function" | Callable, "value" -> Some 2 | _ -> None
let provenanceRank = function LexicalBinding _ -> 0 | SourceDeclaration _ -> 1 | ModuleDeclaration _ -> 2 | PackageDeclaration _ -> 3 | BuiltinRegistration _ -> 4 | CompilerExtension _ -> 5
let provenanceText = function LexicalBinding x | SourceDeclaration x | ModuleDeclaration x | PackageDeclaration x | BuiltinRegistration x | CompilerExtension x -> x
let compareProvenance left right = let rank = Int.compare (provenanceRank left) (provenanceRank right) in if rank <> 0 then rank else StringOrder.compare (provenanceText left) (provenanceText right)
let orderedDistinctCandidates candidates =
  let grouped = List.fold_left (fun groups (candidate : candidate) ->
    let rec add = function
      | [] -> [candidate.identity, [candidate]]
      | (identity, members) :: rest when identity = candidate.identity -> (identity, members @ [candidate]) :: rest
      | group :: rest -> group :: add rest in add groups) [] candidates in
  List.map (fun (_, sameIdentity) -> match List.stable_sort (fun (a : candidate) (b : candidate) -> compareProvenance a.provenance b.provenance) sameIdentity with
    | first :: _ -> first | [] -> Crash.crash "Candidate identity group was unexpectedly empty") grouped
  |> List.stable_sort (fun (a : candidate) (b : candidate) -> StringOrder.compare (symbolIdentityToString a.identity) (symbolIdentityToString b.identity))
let resolveQualified context name environment =
  let matching = indexed name environment.candidatesByVisibleName |> List.filter_map (fun (candidate : candidate) -> Option.map (fun rank -> (rank, provenanceRank candidate.provenance), candidate) (precedence context candidate.identity)) in
  match matching with
  | [] -> Error (UnresolvedName (name, context))
  | (firstRank, _) :: rest ->
    let winningRank = List.fold_left (fun best (rank, _) -> min best rank) firstRank rest in
    let winners = List.filter_map (fun (rank, candidate) -> if rank = winningRank then Some candidate else None) matching |> orderedDistinctCandidates in
    match winners with
    | [winner] -> Ok {originalName = name; context; identity = winner.identity; provenance = winner.provenance}
    | _ -> Error (AmbiguousReference (name, context, List.map (fun (c : candidate) -> c.identity) winners))
let qualifiedNameFromList segments = match NonEmptyList.tryFromList segments with Some xs -> QualifiedName xs | None -> Crash.crash "Name-resolution candidate was unexpectedly empty"
(*
   Generate the interpreter's ordered relative-name candidates. `Darklang.Stdlib`
   is canonical; `Stdlib` is the interpreter's explicit source shortcut.
*)
let namesToTry context currentModule given =
  let givenSegments = qualifiedNameSegments given in
  let rec relative = function [] -> [qualifiedNameFromList givenSegments] | prefixes -> qualifiedNameFromList (prefixes @ givenSegments) :: relative (List.rev (List.tl (List.rev prefixes))) in
  let aliases = match context, givenSegments with
    | _, "Stdlib" :: rest -> [qualifiedNameFromList ("Darklang" :: "Stdlib" :: rest)]
    | Type, ["Option"] -> [qualifiedNameFromList ["Darklang"; "Stdlib"; "Option"; "Option"]]
    | Type, ["Result"] -> [qualifiedNameFromList ["Darklang"; "Stdlib"; "Result"; "Result"]]
    | Constructor, ["Option"; ("Some" | "None" as caseName)] -> [qualifiedNameFromList ["Darklang"; "Stdlib"; "Option"; "Option"; caseName]]
    | Constructor, ["Result"; ("Ok" | "Error" as caseName)] -> [qualifiedNameFromList ["Darklang"; "Stdlib"; "Result"; "Result"; caseName]]
    | _ -> [] in
  List.fold_left (fun distinct name -> if List.mem name distinct then distinct else distinct @ [name]) [] (relative currentModule @ aliases)
let candidateSpellings context currentModule spelling = match tryQualifiedName spelling with None -> [] | Some name -> List.map qualifiedNameToString (namesToTry context currentModule name)
let resolveInModule context currentModule spelling environment = match tryQualifiedName spelling with
  | None -> Error (InvalidQualifiedName (spelling, context))
  | Some originalName ->
    let rec tryCandidates = function
      | [] -> Error (UnresolvedName (originalName, context))
      | candidate :: rest -> match resolveQualified context candidate environment with
        | Ok resolution -> Ok {resolution with originalName}
        | Error (UnresolvedName _) -> tryCandidates rest
        | Error (AmbiguousReference (_, _, identities)) -> Error (AmbiguousReference (originalName, context, identities))
        | Error (InvalidQualifiedName _ as error) -> Error error in
    tryCandidates (namesToTry context currentModule originalName)
let resolve context spelling environment = resolveInModule context [] spelling environment
let contextToString = function Value -> "value" | Callable -> "callable" | Constructor -> "constructor" | Type -> "type"
let errorToString = function
  | InvalidQualifiedName (name, context) -> "Invalid " ^ contextToString context ^ " name: " ^ name
  | UnresolvedName (name, Callable) -> "There is no variable named: " ^ qualifiedNameToString name
  | UnresolvedName (name, context) -> "Unresolved " ^ contextToString context ^ " name: " ^ qualifiedNameToString name
  | AmbiguousReference (name, context, identities) -> "Ambiguous " ^ contextToString context ^ " reference '" ^ qualifiedNameToString name ^ "': " ^ String.concat ", " (List.map symbolIdentityToString identities)
