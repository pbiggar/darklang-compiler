(* NameSyntax.mli - Shared lexical and structural contract for source names. *)
type identifier = OrdinaryIdentifier of string | BlankIdentifier
type qualifiedName

module Keyword : sig
  type t =
    | Let
    | Val
    | In
    | If
    | Elif
    | Then
    | Else
    | Type
    | Of
    | Match
    | With
    | Fun
    | When
    | True
    | False
    | Underscore
end

type identifierToken =
  | IdentifierToken of identifier
  | KeywordToken of Keyword.t

module SourceUnitPurpose : sig
  type t = Executable | Library | Package
end

type sourceUnitName

val sourceUnitName : string -> (sourceUnitName, string) result
val sourceUnitNameText : sourceUnitName -> string
val isStartCharacter : int -> bool
val isContinueCharacter : int -> bool
val identifierText : identifier -> string
val identifierFromText : string -> identifier
val classify : string -> identifierToken

module Words : Set.S with type elt = string

val reservedWords : Words.t
val isBareIdentifier : identifier -> bool
val formatIdentifier : identifier -> string
val singleton : identifier -> qualifiedName
val fromNonEmptySegments : identifier NonEmptyList.t -> qualifiedName
val append : identifier -> qualifiedName -> qualifiedName
val concat : qualifiedName -> qualifiedName -> qualifiedName
val segments : qualifiedName -> identifier list
val trySplitLast : qualifiedName -> (qualifiedName * identifier) option
val formatQualifiedName : qualifiedName -> string
val toLegacySpelling : qualifiedName -> string
val tryParseLegacySpelling : string -> qualifiedName option
val scanOrdinary : string -> int -> identifier * int
val scanQuoted : string -> int -> (identifier * int, string) result
val tryExtractModuleHeader : string -> (qualifiedName * string) option
