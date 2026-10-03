(* Complete migration observations of the semantic AST, including typed evidence. *)
val semanticType : Dark_compiler.AST.semanticType -> Yojson.Basic.t
val expr : Dark_compiler.AST.expr -> Yojson.Basic.t
val typeDef : Dark_compiler.AST.typeDef -> Yojson.Basic.t
val observationSemanticType : Dark_compiler.AST.semanticType -> Yojson.Basic.t
val observationRecordReferenceNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.recordReferenceNode -> Yojson.Basic.t
val observationRecordFieldReference : Dark_compiler.AST.recordFieldReference -> Yojson.Basic.t
val observationConstructorReference : Dark_compiler.AST.constructorReference -> Yojson.Basic.t
val observationBinOp : Dark_compiler.AST.binOp -> Yojson.Basic.t
val observationUnaryOp : Dark_compiler.AST.unaryOp -> Yojson.Basic.t
val observationPattern : Dark_compiler.AST.pattern -> Yojson.Basic.t
val observationLetPattern : Dark_compiler.AST.letPattern -> Yojson.Basic.t
val observationLambdaParameterNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.lambdaParameterNode -> Yojson.Basic.t
val observationBinderStructure : Dark_compiler.AST.binderStructure -> Yojson.Basic.t
val observationRecursiveMemberKind : Dark_compiler.AST.recursiveMemberKind -> Yojson.Basic.t
val observationRecursiveAvailability : Dark_compiler.AST.recursiveAvailability -> Yojson.Basic.t
val observationRecursiveDependencyKind : Dark_compiler.AST.recursiveDependencyKind -> Yojson.Basic.t
val observationRecursiveCandidate : Dark_compiler.AST.recursiveCandidate -> Yojson.Basic.t
val observationParsedRecursiveMember : Dark_compiler.AST.parsedRecursiveMember -> Yojson.Basic.t
val observationResolvedRecursiveMember : Dark_compiler.AST.resolvedRecursiveMember -> Yojson.Basic.t
val observationTypedRecursiveMember : Dark_compiler.AST.typedRecursiveMember -> Yojson.Basic.t
val observationLoweredRecursiveMember : Dark_compiler.AST.loweredRecursiveMember -> Yojson.Basic.t
val observationParsedRecursiveGroup : Dark_compiler.AST.parsedRecursiveGroup -> Yojson.Basic.t
val observationResolvedRecursiveGroup : Dark_compiler.AST.resolvedRecursiveGroup -> Yojson.Basic.t
val observationTypedRecursiveGroup : Dark_compiler.AST.typedRecursiveGroup -> Yojson.Basic.t
val observationLoweredRecursiveGroup : Dark_compiler.AST.loweredRecursiveGroup -> Yojson.Basic.t
val observationRecursiveBindingInfo : Dark_compiler.AST.recursiveBindingInfo -> Yojson.Basic.t
val observationStringPartNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.stringPartNode -> Yojson.Basic.t
val observationExprNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.exprNode -> Yojson.Basic.t
val observationMatchCaseNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.matchCaseNode -> Yojson.Basic.t
val observationFunctionDefNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.functionDefNode -> Yojson.Basic.t
val observationVariantNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.variantNode -> Yojson.Basic.t
val observationTypeDefNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.typeDefNode -> Yojson.Basic.t
val observationValueDefNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.valueDefNode -> Yojson.Basic.t
val observationTopLevelNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.topLevelNode -> Yojson.Basic.t
val observationProgramNode : ('a -> Yojson.Basic.t) -> 'a Dark_compiler.AST.programNode -> Yojson.Basic.t
val observationModuleFunc : Dark_compiler.AST.moduleFunc -> Yojson.Basic.t
val observationModuleDef : Dark_compiler.AST.moduleDef -> Yojson.Basic.t
