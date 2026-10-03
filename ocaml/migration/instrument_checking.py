"""Build-only aliases of unchanged checking sources for private typed observations.

The ordinary library keeps opaque catalogs. The migration build renames module
references only, so it can append encoders to CheckedAST without adding a
production accessor or changing the code under test.
"""
import re
import sys
from pathlib import Path

MODULES = ["CallGraphReachability", "InliningCommon", "ANFConstants", "ANFSubstitution", "ANFEffects", "ANFDeadCodeElimination", "AtomLowering", "LoweringAggregates", "LoweringOperators", "LoweringTypeInference", "LoweringCallbacks", "Monomorphization", "PrepareFunctions", "LiftFunctions", "LiftExpressions", "ClosureComparisons", "ClosureAnalysis", "CheckedStructuralFormat", "InlineLambdas", "TypeSubstitution", "LoweringPrimitives", "TypeRegistries", "ANF", "CheckedMaterializeHelpers", "WrittenCallSupport", "WrittenChecking", "WrittenExpressions", "WrittenApplicationSupport", "WrittenCollectionSupport", "WrittenRecordSupport", "WrittenConstructorSupport", "WrittenLetSupport", "WrittenOperatorSupport", "WrittenDeclarations", "WrittenEnvironment", "SpecializationIdentity", "WrittenTypeSupport", "WrittenPatternSupport", "WrittenLambdaSupport", "NameResolution", "CheckingDiagnostics", "CheckedAST", "Types", "Unification", "ExpressionSupport", "ComparisonPlanning", "EqualityHelpers", "OrderingHelpers", "HelperDependencies", "MaterializeHelpers", "ResolveDeclarations", "Declarations", "CheckRecordLiterals", "CheckBinaryOperations", "CheckLambdas", "CheckCalls", "CheckMatches", "ContextInference", "ExplicitCalls", "CheckExpressions", "CheckFunctions", "ResolvedProgram", "TypeChecking"]
pattern = re.compile(r'"(?:[^"\\]|\\.)*"|\b(' + '|'.join(MODULES) + r')\b')
source = Path(sys.argv[1]).read_text()
if not Path(sys.argv[1]).name.startswith("SemanticDiagnostics."):
    print("open! Dark_compiler [@@warning \"-66\"]")
renamed = pattern.sub(lambda m: "Instrumented" + m[1] if m[1] else m[0], source)
print(renamed.replace("Dark_compiler.Instrumented", "Instrumented"), end="")

if Path(sys.argv[1]).name == "NameResolution.ml":
    print("\nlet observationParts env = env.orderedCandidates, Names.bindings env.candidatesByVisibleName, env.importedOrderedCandidates, Names.bindings env.importedCandidatesByVisibleName")
elif Path(sys.argv[1]).name == "NameResolution.mli":
    print("\nval observationParts : resolutionEnvironment -> candidate list * (qualifiedName * candidate list) list * candidate list * (qualifiedName * candidate list) list")

if Path(sys.argv[1]).name == "WrittenChecking.ml":
    print("\nlet observationEnvironment (value : environment) = InstrumentedWrittenDeclarations.observationParts value")
elif Path(sys.argv[1]).name == "WrittenChecking.mli":
    print("\nval observationEnvironment : environment -> InstrumentedWrittenTypeSupport.globals * InstrumentedCheckedAST.symbols")
elif Path(sys.argv[1]).name == "WrittenDeclarations.ml":
    print("\nlet observationParts (Environment (globals, symbols)) = globals, symbols")
elif Path(sys.argv[1]).name == "WrittenDeclarations.mli":
    print("\nval observationParts : environment -> InstrumentedWrittenTypeSupport.globals * InstrumentedCheckedAST.symbols")

if Path(sys.argv[1]).name == "ANF.ml":
    print("\nlet observationParts value = value.firstId, value.types")
elif Path(sys.argv[1]).name == "ANF.mli":
    print("\nval observationParts : typeMap -> int * Dark_compiler.AST.semanticType option array")
