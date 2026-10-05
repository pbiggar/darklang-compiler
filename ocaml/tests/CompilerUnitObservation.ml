(* Compare original compiler unit registration and complete result diagnostics. *)
open Dark_compiler
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let result=function Ok ()->J.union "FSharpResult" "Ok" [`Null]|Error error->J.union "FSharpResult" "Error" [J.string error]
let tests values=`List (List.map (fun (name,run)->let outcome=run () in tuple [J.string name;result outcome]) values)
let stdlib=lazy (Result.bind (Platform.detectHostTarget ()) StdlibCompilation.buildStdlib)
let fixed=lazy (
 let prepared=match Lazy.force stdlib with Error error->J.union "FSharpResult" "Error" [J.string error]|Ok stdlib->J.union "FSharpResult" "Ok" [tuple [tests (ProgramStructureTests.tests stdlib);tests (ValueSearchCatalogTests.tests stdlib);tests (JsonPlanningTests.tests stdlib);tests (StdlibOptimizationTests.tests stdlib);tests (CompilationSessionTests.tests Platform.LinuxX86_64 stdlib)]] in
 [tests RefCountInsertionTests.tests;tests ARM64CodeGenTests.tests;tests StdlibSourceTests.tests;tests ScriptHelperTests.tests;tests ASTToANFTests.tests;tests ListHIRTests.tests;tests SyntaxDSLTests.tests;tests ChordalGraphTests.tests;`List (List.map (fun (name,outcome)->tuple [J.string name;result outcome]) (ChordalGraphTests.runAllTests ()));tests SSALivenessTests.tests;result (SSALivenessTests.runAll ());tests TypeCheckingTests.tests;result (TypeCheckingTests.runAll ());prepared;tests RuntimeDataLayoutTests.tests;tests X86_64ResolveTests.tests;tests LambdaLiftingTests.tests;tests MonomorphizationTests.tests;tests IRPrinterTests.tests;result (IRPrinterTests.runAll ());tests IRSymbolTests.tests;result (IRSymbolTests.runAll ());tests DeadCodeEliminationTests.tests;result (DeadCodeEliminationTests.runAll ())])
let observe source=tuple (J.string source::(Lazy.force fixed @ [`List (List.map (fun instr->J.string (HostStructuralFormat.format (LIRTestFormatting.instr instr))) (Semantic_observation.LIRFixtures.instructions source))]))
