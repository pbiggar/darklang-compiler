(* LiftExpressions.mli - Convert expression-local lambdas into lifted function definitions. *)
val liftLambdasInExpr : CheckedAST.expr -> ClosureAnalysis.liftState -> (CheckedAST.expr * ClosureAnalysis.liftState, string) result
val liftLambdasInArgs : CheckedAST.expr NonEmptyList.t -> ClosureAnalysis.liftState -> (CheckedAST.expr NonEmptyList.t * ClosureAnalysis.liftState, string) result
val liftLambdasInList : CheckedAST.expr list -> ClosureAnalysis.liftState -> (CheckedAST.expr list * ClosureAnalysis.liftState, string) result
val liftLambdasInFields : (AST.fieldId * CheckedAST.expr) list -> ClosureAnalysis.liftState -> ((AST.fieldId * CheckedAST.expr) list * ClosureAnalysis.liftState, string) result
val liftLambdasInDictEntries : (CheckedAST.expr * CheckedAST.expr) list -> ClosureAnalysis.liftState -> ((CheckedAST.expr * CheckedAST.expr) list * ClosureAnalysis.liftState, string) result
val liftLambdasInCases : CheckedAST.matchCase list -> AST.semanticType option -> ClosureAnalysis.liftState -> (CheckedAST.matchCase list * ClosureAnalysis.liftState, string) result
