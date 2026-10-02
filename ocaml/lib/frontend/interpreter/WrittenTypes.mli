(* WrittenTypes.mli - Complete range-bearing source syntax and normalized packages. *)
(* Mutually recursive syntax records retain the reference's shared field names. *)
[@@@warning "-30"]
type range = Tokenizer.tokenRange

type name =
  | KnownBuiltin of string * int
  | Unresolved of (string) Prelude.neList

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

type identifier = { range : range; name : string }

type qualifiedFnIdentifier = { range : range; modules : (identifier * range) list; fn : identifier }

type qualifiedTypeIdentifier = { range : range; modules : (identifier * range) list; typ : identifier; typeArgs : (typeReference) list }

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

type letPattern =
  | LPUnit of range
  | LPVariable of range * string
  | LPWildcard of range
  | LPTuple of range * letPattern * range * letPattern * (range * letPattern) list * range * range

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

type fnParam =
  | FPUnit of range
  | FPNormal of range * identifier * typeReference * range * range * range * string

type fnDecl = { range : range; name : identifier; typeParams : (string * range) list; parameters : (fnParam) list; effects : ((identifier) list) option; returnType : typeReference; body : expr; keywordLet : range; symbolColon : range; symbolEquals : range; description : string }

type valueDecl = { range : range; name : identifier; body : expr; keywordVal : range; symbolEquals : range; description : string }

type recordFieldSyntax = { range : range; name : range * string; typ : typeReference; description : string; symbolColon : range }

type enumFieldSyntax = { range : range; typ : typeReference; label : (range * string) option; symbolColon : (range) option }

type enumCaseSyntax = { range : range; name : range * string; fields : (enumFieldSyntax) list; description : string; keywordOf : (range) option }

type typeDefinition =
  | TDAlias of typeReference
  | TDRecord of (recordFieldSyntax * (range) option) list
  | TDEnum of (range * enumCaseSyntax) list

type typeDecl = { range : range; name : identifier; typeParams : (string * range) list; definition : typeDefinition; keywordType : range; symbolEquals : range; description : string }

type moduleDecl = { range : range; name : range * string; declarations : (declaration) list; keywordModule : range }

and testExpected =
  | TEExpr of expr
  | TEError of string
  | TESqlError of string

and test = { range : range; actual : expr; expected : testExpected }

and declaration =
  | DFunction of fnDecl
  | DValue of valueDecl
  | DModule of moduleDecl
  | DType of typeDecl
  | DExpr of expr
  | DTypeDB of typeDecl
  | DTest of test

type sourceFile = { range : range; declarations : (declaration) list; exprsToEval : (expr) list }

type parsedFile = SourceFile of sourceFile

module TypeDeclaration : sig
  type recordField = { name : string; typ : typeReference; description : string }
  type enumField = { typ : typeReference; label : string option; description : string }
  type enumCase = { name : string; fields : enumField list; description : string }
  type definition = Alias of typeReference | Record of recordField Prelude.neList | Enum of enumCase Prelude.neList
  type t = { typeParams : string list; definition : definition }
end
module PackageType : sig
  type name = { owner : string; modules : string list; name : string }
  type packageType = { name : name; declaration : TypeDeclaration.t; description : string }
end
module PackageValue : sig
  type name = { owner : string; modules : string list; name : string }
  type packageValue = { name : name; description : string; body : expr }
end
module PackageFn : sig
  type name = { owner : string; modules : string list; name : string }
  type parameter = { name : string; typ : typeReference; description : string }
  type packageFn = {
    name : name; body : expr; typeParams : string list;
    parameters : parameter Prelude.neList; returnType : typeReference;
    effects : string list option; description : string;
  }
end
module DB : sig
  type t = { name : string; version : int; typ : typeReference }
end

val synthRange : range
val primTypes : (string * (range -> typeReference)) list
val primTypeFromName : string -> (range -> typeReference) option
val mpRange : matchPattern -> range
val exprRange : expr -> range
val typeReferenceRange : typeReference -> range
val typeDefinitionNorm : typeDefinition -> TypeDeclaration.definition
val moduleNameParts : moduleDecl -> string list
val packageFn : string -> string list -> fnDecl -> PackageFn.packageFn
val packageType : string -> string list -> typeDecl -> PackageType.packageType
val packageValue : string -> string list -> valueDecl -> PackageValue.packageValue
