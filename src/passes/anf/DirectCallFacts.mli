(* DirectCallFacts.fs - Value, call, and rewrite facts for SSA direct-call specialization. *)
module TempMap = InliningCommon.TempMap
module IntSet : Set.S with type elt = int
module FunctionSet = SpecializationIdentity.FunctionSet
type parameterRewrite = KeepParameter | ReplaceParameterWith of ANF.atom
type programAnalysis = {directCalls : ANF.atom list list FunctionIdMap.t; indirectTargets : FunctionSet.t}
type scalarLiteral = UnitScalar | IntScalar of ANF.sizedInt | BoolScalar of bool | FloatScalar of int64 | StringScalar of string
type knownValue = LiteralValue of scalarLiteral | Int128Value of AST.functionId * int64 * int64 | UInt128Value of AST.functionId * int64 * int64 | TupleValue of scalarLiteral list | RecordValue of ANF.recordDescriptor * scalarLiteral list
type literalPattern = (int * knownValue) list
type literalClone = {originalId : AST.functionId; cloneId : AST.functionId; cloneName : string; pattern : literalPattern}
type valueEnv = knownValue TempMap.t
val compareKnownValue : knownValue -> knownValue -> int
val compareLiteralPattern : literalPattern -> literalPattern -> int
val maxLiteralClonesPerFunction : int
val maxLiteralClonesPerProgram : int
val emptyAnalysis : programAnalysis
val exposeKnownIndirectCExpr : ANF.cExpr -> ANF.cExpr
val addDirectCall : AST.functionId -> ANF.atom list -> programAnalysis -> programAnalysis
val analyzeAtom : ANF.atom -> programAnalysis -> programAnalysis
val analyzeAtoms : ANF.atom list -> programAnalysis -> programAnalysis
val analyzeCExpr : ANF.cExpr -> programAnalysis -> programAnalysis
val scalarLiteralAtom : ANF.atom -> scalarLiteral option
val atomForScalarLiteral : scalarLiteral -> ANF.atom
val isScalarLiteralType : AST.semanticType -> bool
val isConstructionValueType : AST.semanticType -> bool
val isSpecializableValueType : AST.semanticType -> bool
val scalarLiteralMatchesType : AST.semanticType -> scalarLiteral -> bool
val knownValueMatchesType : AST.semanticType -> knownValue -> bool
val rewriteAtom : ANF.atom TempMap.t -> ANF.atom -> ANF.atom
val rewriteCallArgs : parameterRewrite list FunctionIdMap.t -> AST.functionId -> ANF.atom list -> ANF.atom list
val rewriteCExpr : parameterRewrite list FunctionIdMap.t -> ANF.atom TempMap.t -> ANF.cExpr -> ANF.cExpr
val knownValueForAtom : valueEnv -> ANF.atom -> knownValue option
val knownLiteralsForAtoms : valueEnv -> ANF.atom list -> scalarLiteral list option
val knownValueForCExpr : string FunctionIdMap.t -> valueEnv -> ANF.cExpr -> knownValue option
val addKnownBinding : string FunctionIdMap.t -> ANF.tempId -> ANF.cExpr -> valueEnv -> valueEnv
val addKnownCall : AST.functionId -> ANF.atom list -> valueEnv -> knownValue option list list FunctionIdMap.t -> knownValue option list list FunctionIdMap.t
val literalPatternAt : IntSet.t -> knownValue option list -> literalPattern
val boundedCloneGroups : (AST.functionId * string * literalPattern list) list -> (AST.functionId * string * literalPattern list) list
val buildLiteralClones : AST.functionId Seq.t -> StringOrder.Set.t -> (AST.functionId * string * literalPattern list) list -> literalClone list
val removePatternArguments : literalPattern -> ANF.atom list -> ANF.atom list
val routeDirectCall : literalClone list FunctionIdMap.t -> valueEnv -> AST.functionId -> ANF.atom list -> AST.functionId * ANF.atom list
val routeCExpr : literalClone list FunctionIdMap.t -> valueEnv -> ANF.cExpr -> ANF.cExpr
val cexprForKnownValue : knownValue -> ANF.cExpr
val isRematerializedValue : string FunctionIdMap.t -> ANF.cExpr -> bool
