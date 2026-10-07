(* Types.mli - Checking environments, substitutions, and nominal type resolution. *)
type typeEnv = AST.semanticType StringOrder.Map.t
type funcParamNameRegistry = string list StringOrder.Map.t
type typeRegistry = (string * AST.semanticType) list StringOrder.Map.t
type recordTypeInfo = {fields : (string * AST.semanticType) list; fieldTypes : AST.semanticType StringOrder.Map.t; typeParams : string list}
type indexedTypeRegistry = recordTypeInfo StringOrder.Map.t
type sumTypeRegistry = (string * int * AST.semanticType list) list StringOrder.Map.t
type sumVariantInfo = {name : string; tag : int; fields : AST.semanticType list}
type sumTypeInfo = {typeParams : string list; variants : sumVariantInfo list}
type indexedSumTypeRegistry = sumTypeInfo StringOrder.Map.t
type variantLookup = (string * string list * int * AST.semanticType list) StringOrder.Map.t
type genericFuncRegistry = {functions : string list StringOrder.Map.t; requireExplicitTypeArgsForBareCalls : bool}
type aliasRegistry = (string list * AST.semanticType) StringOrder.Map.t
type substitution = AST.semanticType StringOrder.Map.t
type typeCheckEnv = {
 typeCatalog : CheckedAST.typeCatalog; functionCatalog : CheckedAST.functionCatalog;
 typeReg : typeRegistry; indexedTypeReg : indexedTypeRegistry; recordTypeNames : StringOrder.Set.t;
 variantLookup : variantLookup; indexedSumTypeReg : indexedSumTypeRegistry; sumTypeNames : StringOrder.Set.t;
 funcEnv : typeEnv; values : AST.semanticType StringOrder.Map.t; funcParamNames : funcParamNameRegistry;
 genericFuncReg : genericFuncRegistry; genericFuncDefs : AST.functionDef StringOrder.Map.t;
 moduleRegistry : AST.moduleRegistry; aliasReg : aliasRegistry; resolutionEnv : NameResolution.resolutionEnvironment
}
val tryFindVariant : AST.constructorReference -> string -> variantLookup -> (string * string list * int * AST.semanticType list) option
val unqualifiedVariantOwnerCount : string -> variantLookup -> int
val mergeTypeCheckEnv : typeCheckEnv -> typeCheckEnv -> typeCheckEnv
val resolveTypeName : aliasRegistry -> string -> string
val applySubst : substitution -> AST.semanticType -> AST.semanticType
val applyTypeArguments : substitution -> AST.semanticType -> AST.semanticType
val collectTypeVarsInType : AST.semanticType -> string list -> string list
val buildRecordFieldSubstitutionFromParams : string list -> AST.semanticType list -> (substitution, string) result
val resolveAliasTargetType : aliasRegistry -> AST.semanticType -> AST.semanticType
val tryResolveRecordLiteralInfo : aliasRegistry -> indexedTypeRegistry -> AST.recordReference -> (string * AST.semanticType list * recordTypeInfo) option
val buildSubstitution : string list -> AST.semanticType list -> (substitution, string) result
val formatTypeArgumentArityError : string -> int -> int -> string
val formatValueArgumentArityError : string -> int -> int -> string
val applySubstToExpr : substitution -> AST.expr -> AST.expr
val resolveType : aliasRegistry -> AST.semanticType -> AST.semanticType
val resolveAliasesInTypeRegistry : aliasRegistry -> typeRegistry -> typeRegistry
val sumTypeNamesFromVariantLookup : variantLookup -> StringOrder.Set.t
val indexSumTypeRegistry : variantLookup -> indexedSumTypeRegistry
val canonicalizeBareSumTypeRefsWithNames : StringOrder.Set.t -> AST.semanticType -> AST.semanticType
val canonicalizeDeclaredTypeRefsWithSumTypeNames : 'recordInfo StringOrder.Map.t -> StringOrder.Set.t -> AST.semanticType -> AST.semanticType
val indexTypeRegistry : variantLookup -> string list StringOrder.Map.t -> typeRegistry -> indexedTypeRegistry
val typesEqual : aliasRegistry -> AST.semanticType -> AST.semanticType -> bool
val formatLegacyRecordFieldTypeError : aliasRegistry -> string -> AST.semanticType -> AST.semanticType -> AST.expr -> string
