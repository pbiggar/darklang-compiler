[@@@warning "-4-42"]
(* LambdaLiftingTests.fs - Unit tests for lambda lifting in AST_to_ANF
   Ensures lambda return types are preserved when lifting closures with let-bound bodies. *)
open Dark_compiler
type testResult=(unit,string) result
let (let*)=Result.bind
let convertProgramToAnf typedAst=
 let moduleRegistry=DarkStdlib.buildModuleRegistry () in
 let monomorphized=PrepareFunctions.monomorphize typedAst in
 let inlined=InlineLambdas.inlineLambdasInProgram monomorphized in
 let catalog:LiftFunctions.functionCatalog={LiftFunctions.params=FunctionIdMap.empty;returnTypes=FunctionIdMap.empty;genericDefs=FunctionIdMap.empty} in
 let* lifted=LiftFunctions.liftLambdasInProgram StringOrder.Map.empty StringOrder.Map.empty catalog inlined in
 let* typeDefs,functions,expr=AST_to_ANF.splitTopLevels lifted in
 let aliasReg=AST_to_ANF.buildAliasRegistry typeDefs in
 let resolvedFunctions=AST_to_ANF.resolveAliasesInFunctions aliasReg functions in
 let symbols=CheckedAST.programSymbols lifted in
 let registries=AST_to_ANF.buildRegistries symbols moduleRegistry typeDefs aliasReg resolvedFunctions in
 let varGen=ANF.VarGen 0 in
 let* anfFuncs,varGen1=AST_to_ANF.convertFunctions symbols registries varGen resolvedFunctions in
 let* anfExpr,_=AST_to_ANF.convertExprToAnf registries varGen1 expr in Ok (ANF.Program (anfFuncs,anfExpr))
let testLetBoundTupleReturnType ()=
 let source="let apply (f: Int64 -> (Int64 * Int64)) (x: Int64) : (Int64 * Int64) = f x\n"^"apply (fun x -> let t = (x, x + 1L) in t) 1L" in
 match WrittenParsing.parse Validation.Script source with Error error->Error ("Parse error: "^error)|Ok ast->
 match WrittenChecking.checkSourceUnits false true [ast] with Error error->Error ("Type error: "^error)|Ok (_,typedAst)->
 match convertProgramToAnf typedAst with Error error->Error ("ANF conversion error: "^error)|Ok (ANF.Program (functions,_))->
 match List.find_opt (fun (func:ANF.functionDef)->String.starts_with ~prefix:"__closure_" func.ANF.name) functions with
 |None->Error "Expected a lifted lambda function named __closure_*"
 |Some func->let expected=AST.TTuple [AST.TInt64;AST.TInt64] in if func.ANF.returnType=expected then Ok () else Error ("Expected lifted lambda return type "^CheckingDiagnostics.typeToString expected^", got "^CheckingDiagnostics.typeToString func.ANF.returnType)
let tests=["Let-bound tuple return type",testLetBoundTupleReturnType]
