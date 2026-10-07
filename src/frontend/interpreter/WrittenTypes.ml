(*
   The types that the user writes. Think of this as the Syntax Tree.
   This is the range-complete syntax tree produced by the hand-written
   parser. Every node carries the exact source ranges (whole-node plus the
   fine-grained keyword/symbol ranges) that the editor tooling needs: the
   semantic-token highlighter, the LSP (hover / diagnostics), and the formatter.
   The tree is converted 1:1 into the Dark `LanguageTools.WrittenTypes` (as Dvals) by
   `WrittenTypesToDarkTypes` in `Builtins.Language/Libs/WrittenTypes.ml`.
   Execution lowering (`WrittenTypesToProgramTypes`) consumes the same tree,
   ignoring the ranges and minting fresh node ids as it lowers to ProgramTypes.
   (Node ids are ephemeral — a `gid()` counter, not source-derived — so they are
   created at lowering time rather than stored on every node; the Dark WrittenTypes
   keys on ranges, not ids.)
   Pos, TokenRange
*)
(* WrittenTypes.ml - Retain every syntax node, range, and normalization field. *)
(* Mutually recursive syntax records retain the reference's shared field names. *)
[@@@warning "-30"]
type range = Tokenizer.tokenRange

(*
   Used when a syntactic construct turns into a function, such as some operators.
   Most names are unresolved here and are resolved during WT2PT lowering.
*)
type name =
  | KnownBuiltin of string * int
  | Unresolved of (string) Prelude.neList

(*
   Enum type names are a plain `List<string>`. An empty list is valid, e.g. an
   unqualified `Ok`, where only the case name is written. See the long note in git
   history for why EEnum doesn't reuse `Name`.
*)
type unresolvedEnumTypeName = (string) list

type infix =
  | InfixFnCall of infixFnName
  | BinOp of binaryOperation

and infixFnName =
  | ArithmeticPlus
  | ArithmeticMinus
  | ArithmeticMultiply
  | ArithmeticDivide
  | ArithmeticModulo
  | ArithmeticPower
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ComparisonGreaterThan
  | ComparisonGreaterThanOrEqual
  | ComparisonLessThan
  | ComparisonLessThanOrEqual
  | ComparisonEquals
  | ComparisonNotEquals
  | StringConcat

and binaryOperation =
  | BinOpAnd
  | BinOpOr

(*
   A simple `{ range; name }` identifier. The parent field gives it meaning:
   variable name, function name, type name, etc.
*)
type identifier = { range : range; name : string }

(*
   `Module.Path.fn`; each module segment carries its own range.
   (module ident, trailing-dot range)
*)
type qualifiedFnIdentifier = { range : range; modules : (identifier * range) list; fn : identifier }

(*
   `Module.Path.TypeName<args>`; used by record literals, enum constructors, and
   custom type references.
   `<…>` generic args (e.g. `Option<String>`)
*)
type qualifiedTypeIdentifier = { range : range; modules : (identifier * range) list; typ : identifier; typeArgs : (typeReference) list }

(*
   Type references on parameters and return types. Each primitive/built-in type
   has its own case carrying just its range, so WT2PT and the serializer match
   them exhaustively.
   `'a`
   each item is (`*` range, type)
   each arg: (type, `->` range)
*)
and typeReference =
  | TUnit of range
  | TBool of range
  | TInt of range
  | TInt8 of range
  | TUInt8 of range
  | TInt16 of range
  | TUInt16 of range
  | TInt32 of range
  | TUInt32 of range
  | TInt64 of range
  | TUInt64 of range
  | TInt128 of range
  | TUInt128 of range
  | TFloat of range
  | TChar of range
  | TString of range
  | TDateTime of range
  | TUuid of range
  | TBlob of range
  | TList of range * range * range * typeReference * range
  | TDict of range * range * range * typeReference * range * typeReference * range
  | TCustom of qualifiedTypeIdentifier
  | TVariable of range * range * (range * string)
  | TTuple of range * typeReference * range * typeReference * (range * typeReference) list * range * range
  | TFn of range * (typeReference * range) list * typeReference

(*
   each item is (`,` range, pattern)
*)
type letPattern =
  | LPUnit of range
  | LPVariable of range * string
  | LPWildcard of range
  | LPTuple of range * letPattern * range * letPattern * (range * letPattern) list * range * range

(*
   Match patterns.
   also `_` (as "_")
   arbitrary-precision `Int`
   Recovery hole where a pattern was expected but could not be parsed. The
   parse has a diagnostic at this range, and execution paths reject files with
   diagnostics before lowering.
*)
type matchPattern =
  | MPVariable of range * string
  | MPInt of range * (range * Z.t)
  | MPInt8 of range * (range * int) * range
  | MPUInt8 of range * (range * int) * range
  | MPInt16 of range * (range * int) * range
  | MPUInt16 of range * (range * int) * range
  | MPInt32 of range * (range * int32) * range
  | MPUInt32 of range * (range * int64) * range
  | MPInt64 of range * (range * int64) * range
  | MPUInt64 of range * (range * int64) * range
  | MPInt128 of range * (range * Z.t) * range
  | MPUInt128 of range * (range * Z.t) * range
  | MPFloat of range * bool * string * string
  | MPBool of range * bool
  | MPString of range * (range * string) option * range * range
  | MPChar of range * (range * string) option * range * range
  | MPUnit of range
  | MPEnum of range * (range * string) * (matchPattern) list
  | MPTuple of range * matchPattern * range * matchPattern * (range * matchPattern) list * range * range
  | MPList of range * (matchPattern * (range) option) list * range * range
  | MPListCons of range * matchPattern * matchPattern * range
  | MPOr of range * (matchPattern) list
  | MPError of range

type stringSegment =
  | StringText of range * string
  | StringInterpolation of range * expr * range * range

(*
   bare arbitrary-precision `Int`
   each list element carries its trailing-separator (`;`/`,`) range, if any
   `Dict { k = v; … }`: a dict literal. Syntactically like a record, but `Dict`
   is a keyword (its own range), not a type name, so it's a distinct node.
   (entry, key, `:` range, value)
   (field name, `=` range, value)
   each item is (`|>` range, segment)
   `e1 ⏎ e2` (sequence)
   Recovery hole where an expression was expected but could not be parsed. The
   parse has a diagnostic at this range, and execution paths reject files with
   diagnostics before lowering.
*)
and expr =
  | EUnit of range
  | EBool of range * bool
  | EInt of range * (range * Z.t)
  | EInt64 of range * (range * int64) * range
  | EInt8 of range * (range * int) * range
  | EUInt8 of range * (range * int) * range
  | EInt16 of range * (range * int) * range
  | EUInt16 of range * (range * int) * range
  | EInt32 of range * (range * int32) * range
  | EUInt32 of range * (range * int64) * range
  | EUInt64 of range * (range * int64) * range
  | EInt128 of range * (range * Z.t) * range
  | EUInt128 of range * (range * Z.t) * range
  | EFloat of range * bool * string * string
  | EChar of range * (range * string) option * range * range
  | EString of range * (range) option * (stringSegment) list * range * range
  | EVariable of range * string
  | EFnName of range * qualifiedFnIdentifier
  | EInfix of range * (range * infix) * expr * expr
  | ELet of range * letPattern * expr * expr * range * range
  | EApply of range * expr * (typeReference) list * (expr) list
  | EList of range * (expr * (range) option) list * range * range
  | ETuple of range * expr * range * expr * (range * expr) list * range * range
  | EIf of range * expr * expr * (expr) option * range * range * (range) option
  | ERecordFieldAccess of range * expr * (range * string) * range
  | ELambda of range * (letPattern) list * expr * range * range
  | ERecord of range * qualifiedTypeIdentifier * (range * (range * string) * expr) list * range * range
  | EDict of range * (range * expr * range * expr) list * range * range * range
  | ERecordUpdate of range * expr * ((range * string) * range * expr) list * range * range * range
  | EEnum of range * qualifiedTypeIdentifier * (range * string) * (expr) list * range
  | EMatch of range * expr * (matchCase) list * range * range
  | EPipe of range * expr * (range * pipeExpr) list
  | EStatement of range * expr * expr
  | EError of range

and matchCase = { barRange : range; pat : matchPattern; arrowRange : range; whenCondition : (range * expr) option; rhs : expr }

and pipeExpr =
  | EPipeInfix of range * (range * infix) * expr
  | EPipeLambda of range * (letPattern) list * expr * range * range
  | EPipeEnum of range * qualifiedTypeIdentifier * (range * string) * (expr) list * range
  | EPipeFnCall of range * qualifiedFnIdentifier * (typeReference) list * (expr) list
  | EPipeVariableOrFnCall of range * string

(*
   A function parameter: `(name: Type)` or a `()` unit parameter.
   `description` is the `///` written above it. A parameter's doc is not part of the item's identity
   hash, so it needs an `UpdateDoc` to travel -- but it has to be READ first, and until this field
   existed the lexer's doc comment was attached to the `(` token and then dropped on the floor.
*)
type fnParam =
  | FPUnit of range
  | FPNormal of range * identifier * typeReference * range * range * range * string

(*
   `let name (p: T) … :{Effect, …} Ret = body`
   `<'a, 'b>` (name tick-stripped, with range)
   An optional effect row after the return colon, such as
   `:{Http, Clock} Ret`, sets the function's permission ceiling. It limits
   the effects used by the body and its calls; it never grants access.
   Effect names are resolved, and unknown names reported, by
   `WrittenTypesToProgramTypes`.
   `: Ret` means no ceiling; `:{}` requires a pure body; a non-empty row
   allows only the listed effects.
   `: String`              None       no row, no promise
   `:{} String`            Some []    effect-free: every host effect inside is denied
   `:{Http, Clock} String` Some [...] only these; anything else inside is denied
   preceding `///` doc comments
*)
type fnDecl = { range : range; name : identifier; typeParams : (string * range) list; parameters : (fnParam) list; effects : ((identifier) list) option; returnType : typeReference; body : expr; keywordLet : range; symbolColon : range; symbolEquals : range; description : string }

(*
   `let name = body` (no params)
*)
type valueDecl = { range : range; name : identifier; body : expr; keywordVal : range; symbolEquals : range; description : string }

(*
   --- type declarations ---
   The `///` written above the field. See `FnParam` for why it is kept.
*)
type recordFieldSyntax = { range : range; name : range * string; typ : typeReference; description : string; symbolColon : range }

type enumFieldSyntax = { range : range; typ : typeReference; label : (range * string) option; symbolColon : (range) option }

(*
   The `///` written above the case. See `FnParam` for why it is kept.
*)
type enumCaseSyntax = { range : range; name : range * string; fields : (enumFieldSyntax) list; description : string; keywordOf : (range) option }

(*
   (field, trailing-separator)
   (leading `|` range, case)
*)
type typeDefinition =
  | TDAlias of typeReference
  | TDRecord of (recordFieldSyntax * (range) option) list
  | TDEnum of (range * enumCaseSyntax) list

(*
   `type Name [<'a>] = Definition`
   `<'a, 'b>` (name tick-stripped, with range)
*)
type typeDecl = { range : range; name : identifier; typeParams : (string * range) list; definition : typeDefinition; keywordType : range; symbolEquals : range; description : string }

(*
   A `module Name.Path` header.
*)
type moduleDecl = { range : range; name : range * string; declarations : (declaration) list; keywordModule : range }

(*
   A test assertion's expected side: a value expression, or an expected
   runtime / SQL error message. Validation restricts it to Test source.
*)
and testExpected =
  | TEExpr of expr
  | TEError of string
  | TESqlError of string

(*
   A test assertion `actual = expected`; post-parse validation restricts it to Test source.
*)
and test = { range : range; actual : expr; expected : testExpected }

(*
   A trailing expression inside a module body (`module M = … \n expr`).
   `[<DB>] type Name = AliasedType` — a Test-only user DB.
   `actual = expected` assertion accepted only by Test validation.
*)
and declaration =
  | DFunction of fnDecl
  | DValue of valueDecl
  | DModule of moduleDecl
  | DType of typeDecl
  | DExpr of expr
  | DTypeDB of typeDecl
  | DTest of test

(*
   The whole file: top-level declarations + trailing expressions to eval.
*)
type sourceFile = { range : range; declarations : (declaration) list; exprsToEval : (expr) list }

type parsedFile = SourceFile of sourceFile

module TypeDeclaration = struct
  type recordField = { name : string; typ : typeReference; description : string }
  type enumField = { typ : typeReference; label : string option; description : string }
  type enumCase = { name : string; fields : enumField list; description : string }
  type definition = Alias of typeReference | Record of recordField Prelude.neList | Enum of enumCase Prelude.neList
  type t = { typeParams : string list; definition : definition }
end
module PackageType = struct
  type name = { owner : string; modules : string list; name : string }
  type packageType = { name : name; declaration : TypeDeclaration.t; description : string }
end
module PackageValue = struct
  type name = { owner : string; modules : string list; name : string }
  type packageValue = { name : name; description : string; body : expr }
end
module PackageFn = struct
  type name = { owner : string; modules : string list; name : string }
  type parameter = { name : string; typ : typeReference; description : string }
  type packageFn = {
    name : name; body : expr; typeParams : string list;
    parameters : parameter Prelude.neList; returnType : typeReference;
    effects : string list option; description : string;
  }
end
module DB = struct
  type t = { name : string; version : int; typ : typeReference }
end

let mpRange (p : matchPattern) =
  match p with
  | MPVariable(r, _)
  | MPInt(r, _)
  | MPInt8(r, _, _)
  | MPUInt8(r, _, _)
  | MPInt16(r, _, _)
  | MPUInt16(r, _, _)
  | MPInt32(r, _, _)
  | MPUInt32(r, _, _)
  | MPInt64(r, _, _)
  | MPUInt64(r, _, _)
  | MPInt128(r, _, _)
  | MPUInt128(r, _, _)
  | MPFloat(r, _, _, _)
  | MPBool(r, _)
  | MPString(r, _, _, _)
  | MPChar(r, _, _, _)
  | MPUnit r
  | MPEnum(r, _, _)
  | MPTuple(r, _, _, _, _, _, _)
  | MPList(r, _, _, _)
  | MPListCons(r, _, _, _)
  | MPOr(r, _)
  | MPError r -> r

(*
   Source range covering a whole expression node.
*)
let exprRange (e : expr) =
  match e with
  | EUnit r -> r
  | EBool(r, _)
  | EInt(r, _)
  | EInt64(r, _, _)
  | EInt8(r, _, _)
  | EUInt8(r, _, _)
  | EInt16(r, _, _)
  | EUInt16(r, _, _)
  | EInt32(r, _, _)
  | EUInt32(r, _, _)
  | EUInt64(r, _, _)
  | EInt128(r, _, _)
  | EUInt128(r, _, _)
  | EFloat(r, _, _, _)
  | EChar(r, _, _, _)
  | EString(r, _, _, _, _)
  | EVariable(r, _)
  | EFnName(r, _)
  | EInfix(r, _, _, _)
  | ELet(r, _, _, _, _, _)
  | EApply(r, _, _, _)
  | EList(r, _, _, _)
  | ETuple(r, _, _, _, _, _, _)
  | EIf(r, _, _, _, _, _, _)
  | ERecordFieldAccess(r, _, _, _)
  | ELambda(r, _, _, _, _)
  | ERecord(r, _, _, _, _)
  | EDict(r, _, _, _, _)
  | ERecordUpdate(r, _, _, _, _, _)
  | EEnum(r, _, _, _, _)
  | EMatch(r, _, _, _, _)
  | EPipe(r, _, _)
  | EStatement(r, _, _)
  | EError r -> r

(*
   Normalized package IR + declaration normalization
   The layers below are execution-only (Cli / Package / TestModule -> WT2PT -> PT).
   They are never serialized for highlighting, so synthesized nodes may use
   `synthRange`. They normalize the raw parser tree (rich decls above) into the
   module-qualified package shapes the lowering consumes.
   The declared permission ceiling (effect case names); see `FnDecl`.
*)
let typeReferenceRange (t : typeReference) =
  match t with
  | TUnit r
  | TBool r
  | TInt r
  | TInt8 r
  | TUInt8 r
  | TInt16 r
  | TUInt16 r
  | TInt32 r
  | TUInt32 r
  | TInt64 r
  | TUInt64 r
  | TInt128 r
  | TUInt128 r
  | TFloat r
  | TChar r
  | TString r
  | TDateTime r
  | TUuid r
  | TBlob r
  | TList(r, _, _, _, _)
  | TDict(r, _, _, _, _, _, _)
  | TVariable(r, _, _)
  | TTuple(r, _, _, _, _, _, _)
  | TFn(r, _, _) -> r
  | TCustom q -> q.range

(*
   A synthetic (zero-width) range for nodes the lowering synthesizes with no
   source counterpart, such as an implicit unit parameter. Never serialized for
   highlighting; the package/decl normalization layer is execution-only.
*)
let synthRange = { Tokenizer.start = { Tokenizer.row = 0; column = 0 }; end_ = { Tokenizer.row = 0; column = 0 } }
(*
   Mapping between primitive type names and their `TypeReference` case
   constructors. The parser resolves names through this list; WT2PT and the
   serializer then match the primitive cases exhaustively.
*)
let primTypes = [
  "Unit", (fun range -> TUnit range);
  "Bool", (fun range -> TBool range);
  "Int", (fun range -> TInt range);
  "Int8", (fun range -> TInt8 range);
  "UInt8", (fun range -> TUInt8 range);
  "Int16", (fun range -> TInt16 range);
  "UInt16", (fun range -> TUInt16 range);
  "Int32", (fun range -> TInt32 range);
  "UInt32", (fun range -> TUInt32 range);
  "Int64", (fun range -> TInt64 range);
  "UInt64", (fun range -> TUInt64 range);
  "Int128", (fun range -> TInt128 range);
  "UInt128", (fun range -> TUInt128 range);
  "Float", (fun range -> TFloat range);
  "Char", (fun range -> TChar range);
  "String", (fun range -> TString range);
  "DateTime", (fun range -> TDateTime range);
  "Uuid", (fun range -> TUuid range);
  "Blob", (fun range -> TBlob range);
]
let primTypeFromName name = List.find_map (fun (candidate, constructor) -> if name = candidate then Some constructor else None) primTypes

(*
   --- normalization: raw parser syntax → package IR ---
   The parser produces one range-complete syntax tree. The package form is the
   shape execution wants: names pulled out of `(range, name)` pairs, no ranges.
   Field descriptions default to ""; declaration descriptions keep their `///`
   doc comments.
   A unit parameter is named "_".
*)
let fnParamNorm (parameter : fnParam) : PackageFn.parameter =
  match parameter with
  | FPUnit _ -> { PackageFn.name = "_"; typ = TUnit synthRange; description = "" }
  | FPNormal (_, name, typ, _, _, _, description) -> { PackageFn.name = name.name; typ; description }
let recordFieldNorm (field : recordFieldSyntax) : TypeDeclaration.recordField =
  { TypeDeclaration.name = snd field.name; typ = field.typ; description = field.description }
let enumFieldNorm (field : enumFieldSyntax) : TypeDeclaration.enumField =
  { TypeDeclaration.typ = field.typ; label = Option.map snd field.label; description = "" }
let enumCaseNorm (case : enumCaseSyntax) : TypeDeclaration.enumCase =
  { TypeDeclaration.name = snd case.name; fields = List.map enumFieldNorm case.fields; description = case.description }
let typeDefinitionNorm = function
  | TDAlias typ -> TypeDeclaration.Alias typ
  | TDRecord fields ->
      let normalized = List.map (fun (field, _) -> recordFieldNorm field) fields in
      let fallback : TypeDeclaration.recordField = { TypeDeclaration.name = "_"; typ = TUnit synthRange; description = "" } in
      TypeDeclaration.Record (ParserDependencies.ofListWithDefault fallback normalized)
  | TDEnum cases ->
      let normalized = List.map (fun (_, case) -> enumCaseNorm case) cases in
      let fallback : TypeDeclaration.enumCase = { TypeDeclaration.name = "_"; fields = []; description = "" } in
      TypeDeclaration.Enum (ParserDependencies.ofListWithDefault fallback normalized)
(*
   --- build owner-qualified package items from declarations ---
   A fn `map` inside `module Darklang.Stdlib.List` becomes `Darklang.Stdlib.List.map`:
   the accumulated path's first segment is the owner, the rest the modules.
   The dotted `module A.B.C` header split into its path segments.
*)
let moduleNameParts (moduleDecl : moduleDecl) =
  String.split_on_char '.' (snd moduleDecl.name) |> List.filter (fun segment -> segment <> "")
let packageFn owner modules (fn : fnDecl) : PackageFn.packageFn =
  let fallback : PackageFn.parameter = { PackageFn.name = "_"; typ = TUnit synthRange; description = "" } in
  let parameters = List.map fnParamNorm fn.parameters |> ParserDependencies.ofListWithDefault fallback in
  { PackageFn.name = { PackageFn.owner; modules; name = fn.name.name };
    body = fn.body; typeParams = List.map fst fn.typeParams; parameters;
    returnType = fn.returnType; effects = Option.map (List.map (fun (id : identifier) -> id.name)) fn.effects;
    description = fn.description }
let packageType owner modules (typ : typeDecl) : PackageType.packageType =
  { PackageType.name = { PackageType.owner; modules; name = typ.name.name };
    declaration = { TypeDeclaration.typeParams = List.map fst typ.typeParams; definition = typeDefinitionNorm typ.definition };
    description = typ.description }
let packageValue owner modules (value : valueDecl) : PackageValue.packageValue =
  { PackageValue.name = { PackageValue.owner; modules; name = value.name.name };
    description = value.description; body = value.body }
