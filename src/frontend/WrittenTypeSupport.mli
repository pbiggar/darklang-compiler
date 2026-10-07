(* Shared type inventories and structural-record checking from WrittenTypeSupport.mli. *)
type locals = (AST.semanticType * AST.bindingId) StringOrder.Map.t
type checkedExpression = AST.semanticType * CheckedAST.expr * CheckedAST.symbols
type functionSignature = {id : AST.functionId; typeParams : string list; parameters : AST.semanticType list; return : AST.semanticType}
type typeKind = RecordKind | SumKind | AliasKind
type typeEntry = {kind : typeKind; params : string list; path : string list; definition : WrittenTypes.typeDefinition}
type typeInventory = typeEntry StringOrder.Map.t
type globals = {functions : functionSignature StringOrder.Map.t; values : locals; types : typeInventory; collidingCases : StringOrder.Set.t; allowInternal : bool; typeParams : StringOrder.Set.t; modulePath : string list; currentFunction : (AST.functionId * string * string list) option}
val emptyGlobals : globals
val collidingCaseNames : typeInventory -> StringOrder.Set.t
val caseTag : StringOrder.Set.t -> string -> string -> int -> int
val qualifiedFnName : WrittenTypes.qualifiedFnIdentifier -> string list
val restrictedIdentifier : bool -> string list -> bool
val resolveFunction : globals -> string list -> functionSignature option
val resolveValue : globals -> string list -> (AST.semanticType * AST.bindingId) option
val requireType : AST.semanticType option -> AST.semanticType -> (unit, string) result
val checkedLiteral : AST.semanticType option -> CheckedAST.symbols -> AST.semanticType -> CheckedAST.expr -> (checkedExpression, string) result
val typeReference : (string list -> string -> AST.semanticType list -> (AST.semanticType, string) result) -> StringOrder.Set.t -> WrittenTypes.typeReference -> (AST.semanticType, string) result
val collectWrittenTypeParams : string list -> WrittenTypes.typeReference -> string list
val resolveWrittenType : bool -> typeInventory -> string list -> StringOrder.Set.t -> WrittenTypes.typeReference -> (AST.semanticType, string) result
val findNamedType : globals -> WrittenTypes.qualifiedTypeIdentifier -> (string * typeEntry, string) result
val resolveNamedType : globals -> AST.semanticType option -> WrittenTypes.qualifiedTypeIdentifier -> AST.semanticType list option -> (string * typeEntry * AST.semanticType list, string) result
val recordFields : bool -> typeInventory -> typeEntry -> AST.semanticType list -> ((string * AST.semanticType) list, string) result
val structuralEqualityCompatible : globals -> AST.semanticType -> AST.semanticType -> bool
val convertStructuralRecord : globals -> AST.semanticType -> AST.semanticType -> CheckedAST.expr -> CheckedAST.symbols -> (CheckedAST.expr * CheckedAST.symbols, string) result
