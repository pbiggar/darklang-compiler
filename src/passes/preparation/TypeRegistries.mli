(* Source record, alias, variable, and semantic-identity preparation registries. *)
type recordTypeInfo = {
  typeParams : string list;
  fields : (string * AST.semanticType) list;
}

type typeRegistry = recordTypeInfo StringOrder.Map.t

val recordFieldsRegistry :
  typeRegistry -> (string * AST.semanticType) list StringOrder.Map.t

val recordTypeParamsRegistry : typeRegistry -> string list StringOrder.Map.t

val rcSumShapeRegistryFromVariantLookup :
  Types.variantLookup -> MemoryModel.rcSumShapeRegistry

type functionRegistry = (string * AST.semanticType) FunctionIdMap.t
type functionNameRegistry = string FunctionIdMap.t
type functionIdRegistry = AST.functionId StringOrder.Map.t

val functionIdsFromNames : functionNameRegistry -> functionIdRegistry

type typeNameRegistry = CheckedAST.semanticMetadata

val emptyTypeNames : typeNameRegistry
val typeNamesFromSymbols : CheckedAST.symbols -> typeNameRegistry
val tryFindConstructorTag : AST.constructorId -> typeNameRegistry -> int option
val tryFindFieldIndex : AST.fieldId -> typeNameRegistry -> int option

val listHeadUnsafeExpr :
  functionIdRegistry -> AST.semanticType -> ANF.atom -> ANF.cExpr

type aliasRegistry = (string list * AST.semanticType) StringOrder.Map.t

val canonicalizeBareSumTypeRefsWithNames :
  StringOrder.Set.t -> AST.semanticType -> AST.semanticType

val canonicalizeBareSumTypeRefs :
  Types.variantLookup -> AST.semanticType -> AST.semanticType

val canonicalizeNamedTypeRefs :
  StringOrder.Set.t -> StringOrder.Set.t -> AST.semanticType -> AST.semanticType

val resolveRecordTypeName : aliasRegistry -> string -> string
val expandTypeRegWithAliases : typeRegistry -> aliasRegistry -> typeRegistry

module BindingMap = CheckedAST.BindingIdMap

type varEnv = (ANF.tempId * AST.semanticType) BindingMap.t

val typeEnvFromVarEnv : varEnv -> AST.semanticType BindingMap.t
