[@@@warning "-30"]

(* NameResolution.mli - Immutable canonical symbol inventory and contextual resolution. *)
type qualifiedName
type resolutionContext = Value | Callable | Constructor | Type

type namespaceIdentity =
  | RootNamespace
  | ModuleNamespace of string NonEmptyList.t
  | PackageNamespace of string * string list
  | BuiltinNamespace

type symbolIdentity =
  | LocalValue of string
  | ModuleValue of namespaceIdentity * string
  | PackageValue of namespaceIdentity * string
  | BuiltinValue of string * int
  | ModuleFunction of namespaceIdentity * string * string
  | PackageFunction of namespaceIdentity * string * string
  | BuiltinFunction of string * int
  | ConstructorSymbol of string * string
  | UserType of string
  | BuiltinType of string

type candidateProvenance =
  | LexicalBinding of string
  | SourceDeclaration of string
  | ModuleDeclaration of string
  | PackageDeclaration of string
  | BuiltinRegistration of string
  | CompilerExtension of string

type candidate = {
  visibleName : qualifiedName;
  identity : symbolIdentity;
  provenance : candidateProvenance;
}

type resolutionEnvironment

type successfulResolution = {
  originalName : qualifiedName;
  context : resolutionContext;
  identity : symbolIdentity;
  provenance : candidateProvenance;
}

type resolutionError =
  | InvalidQualifiedName of string * resolutionContext
  | UnresolvedName of qualifiedName * resolutionContext
  | AmbiguousReference of
      qualifiedName * resolutionContext * symbolIdentity list

val tryQualifiedName : string -> qualifiedName option
val qualifiedNameFromSegments : string NonEmptyList.t -> qualifiedName
val qualifiedNameSegments : qualifiedName -> string list
val qualifiedNameToString : qualifiedName -> string
val symbolIdentityToString : symbolIdentity -> string
val canonicalSpelling : symbolIdentity -> string
val empty : resolutionEnvironment
val candidates : resolutionEnvironment -> candidate list
val addCandidate : candidate -> resolutionEnvironment -> resolutionEnvironment

val addCandidates :
  candidate list -> resolutionEnvironment -> resolutionEnvironment

val filterCandidates :
  (candidate -> bool) -> resolutionEnvironment -> resolutionEnvironment

val merge :
  resolutionEnvironment -> resolutionEnvironment -> resolutionEnvironment

val candidate :
  string -> symbolIdentity -> candidateProvenance -> candidate option

val resolveQualified :
  resolutionContext ->
  qualifiedName ->
  resolutionEnvironment ->
  (successfulResolution, resolutionError) result

val candidateSpellings :
  resolutionContext -> string list -> string -> string list

val resolveInModule :
  resolutionContext ->
  string list ->
  string ->
  resolutionEnvironment ->
  (successfulResolution, resolutionError) result

val resolve :
  resolutionContext ->
  string ->
  resolutionEnvironment ->
  (successfulResolution, resolutionError) result

val contextToString : resolutionContext -> string
val errorToString : resolutionError -> string
