(* LiftExpressions.fs - Convert expression-local lambdas into lifted function definitions. *)
[@@@warning "-4"]
module C = CheckedAST
module A = ClosureAnalysis
module P = ClosureComparisons
module S = SpecializationIdentity
module R = TypeRegistries
module B = C.BindingIdMap
let ( let* ) = Result.bind
let infer expr state = A.simpleInferType expr state.A.typeEnv state.A.funcParams state.A.funcReturnTypes state.A.genericFuncDefs state.A.typeReg state.A.variantLookup (R.typeNamesFromSymbols state.A.symbols)
let merge environment bindings = B.fold B.add bindings environment
(*
   Process args, lifting any lambdas
   Lambda in expression position - lift it to a closure
   Add lambda parameters to type environment before processing body
   Only this lambda is the recursive value. Lambdas nested in
   its body capture the recursive closure like any other local.
   First, lift any lambdas within the body
   Create lifted function
   Recursive references are rewritten while lowering the body,
   so reserve the lifted identity before that traversal.
   Build body that extracts captures from closure tuple
   Restore original TypeEnv (exclude lambda params)
*)
let rec liftLambdasInExpr expr state =
 let one value build = let* value, state = liftLambdasInExpr value state in Ok (build value, state) in
 let two left right build = let* left, state = liftLambdasInExpr left state in let* right, state = liftLambdasInExpr right state in Ok (build left right, state) in
 match expr with
 | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _ | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _
 | C.UInt8Literal _ | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _ | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _
 | C.BlobLiteral _ | C.CharLiteral _ | C.FloatLiteral _ | C.Local _ | C.FuncRef _ | C.Closure _ | C.RuntimeError _ -> Ok (expr, state)
 | C.BoundaryRender (renderer, value) -> one value (fun value -> C.BoundaryRender (renderer, value))
 | C.BinOp (op, left, right) -> two left right (fun left right -> C.BinOp (op, left, right))
 | C.UnaryOp (op, inner) -> one inner (fun value -> C.UnaryOp (op, value))
 | C.Let (pattern, value, body) ->
   let* value', state1 = liftLambdasInExpr value state in
   (* Try to infer the type of the value for capturing in nested lambdas *)
   let child = match infer value state1 with None -> state1 | Some typ -> {state1 with A.typeEnv = List.fold_left (fun env (name, typ) -> B.add name typ env) state1.A.typeEnv (S.letPatternBindingTypes pattern typ)} in
   let* body, next = liftLambdasInExpr body child in
   (* The child scope must restore the complete incoming environment;
      removing by text would lose an outer binding after shadowing. *)
   Ok (C.Let (pattern, value', body), {next with A.typeEnv = state.A.typeEnv})
 | C.RecursiveLet (recursion, value, body) ->
   let self = C.recursiveBindingId recursion in (match C.recursiveBindingAvailability recursion with
   | AST.OrdinaryBinding -> liftLambdasInExpr (C.Let (C.LPVariable self, value, body)) state
   | AST.SelfRecursiveMember ->
     let typ = C.recursiveMemberType recursion in let closure, symbols = C.allocateBinding "__closure" state.A.symbols in
     let value = match value with C.Lambda (parameters, annotation, body) -> C.Lambda (parameters, annotation, P.rewriteRecursiveSelfReferences self closure body) | _ -> Crash.crash "RecursiveLet reached lambda lifting with a non-lambda value" in
     let child = {state with A.symbols; typeEnv = B.add closure typ state.A.typeEnv; recursiveSelf = Some (self, closure, typ, recursion)} in
     let* value, state1 = liftLambdasInExpr value child in
     let child = {state1 with A.typeEnv = B.add self typ state.A.typeEnv; recursiveSelf = state.A.recursiveSelf} in
     let* body, state2 = liftLambdasInExpr body child in
     Ok (C.Let (C.LPVariable self, value, body), {state2 with A.typeEnv = state.A.typeEnv; recursiveSelf = state.A.recursiveSelf})
   | AST.MutualRecursiveMember | AST.CompletedGroupMember | AST.ImportedGroupMember -> Error "Local RecursiveLet has invalid group availability")
 | C.If (condition, yes, no) -> let* condition, state = liftLambdasInExpr condition state in let* yes, state = liftLambdasInExpr yes state in let* no, state = liftLambdasInExpr no state in Ok (C.If (condition, yes, no), state)
 | C.Sequence (first, next) -> two first next (fun first next -> C.Sequence (first, next))
 | C.Call (target, args) -> let* args, state = liftLambdasInArgs args state in Ok (C.Call (target, args), state)
 | C.TypeApp (target, types, args) -> let* args, state = liftLambdasInArgs args state in Ok (C.TypeApp (target, types, args), state)
 | C.TupleLiteral values -> let* values, state = liftLambdasInList (C.tupleElementsToList values) state in Ok (C.TupleLiteral (C.tupleElementsOfList values), state)
 | C.ListLiteral values -> let* values, state = liftLambdasInList values state in Ok (C.ListLiteral values, state)
 | C.TupleAccess (value, index) -> one value (fun value -> C.TupleAccess (value, index))
 | C.DictLiteral (key, value, entries) -> let* entries, state = liftLambdasInDictEntries entries state in Ok (C.DictLiteral (key, value, entries), state)
 | C.RecordLiteral (owner, fields) -> let* fields, state = C.traverseStateRecordFields liftLambdasInExpr state fields in Ok (C.RecordLiteral (owner, fields), state)
 | C.RecordUpdate (record, fields) -> let* record, state = liftLambdasInExpr record state in let* fields, state = liftLambdasInFields fields state in Ok (C.RecordUpdate (record, fields), state)
 | C.RecordAccess (record, field) -> one record (fun record -> C.RecordAccess (record, field))
 | C.Constructor (reference, fields) -> let* fields, state = liftLambdasInList fields state in Ok (C.Constructor (reference, fields), state)
 | C.Match (scrutinee, cases) -> let typ = infer scrutinee state in let* scrutinee, state = liftLambdasInExpr scrutinee state in let* cases, state = liftLambdasInCases (NonEmptyList.toList cases) typ state in Ok (C.Match (scrutinee, NonEmptyList.fromList cases), state)
 | C.Lambda (parameters, _, body) -> liftLambda false parameters body state
 | C.Apply (target, args) -> let* target, state = liftLambdasInExpr target state in let* args, state = liftLambdasInArgs args state in Ok (C.Apply (target, args), state)
 | C.IndirectApply (target, args) -> let* target, state = liftLambdasInExpr target state in let* args, state = liftLambdasInArgs args state in Ok (C.IndirectApply (target, args), state)
 | C.InterpolatedString parts ->
   let rec loop parts state acc = match parts with [] -> Ok (List.rev acc, state) | C.StringText text :: rest -> loop rest state (C.StringText text :: acc) | C.StringExpr value :: rest -> let* value, state = liftLambdasInExpr value state in loop rest state (C.StringExpr value :: acc) in
   let* parts, state = loop parts state [] in Ok (C.InterpolatedString parts, state)
and liftLambda argument parameters body state =
 let bindings = NonEmptyList.toList parameters |> List.concat_map S.lambdaParameterBindings |> List.to_seq |> B.of_seq in
 let withParams = {state with A.typeEnv = merge state.A.typeEnv bindings; recursiveSelf = if argument then state.A.recursiveSelf else None} in
 let* body', state1 = liftLambdasInExpr body withParams in
 let afterBody = if argument then state1 else {state1 with A.recursiveSelf = state.A.recursiveSelf} in
 let* plan, planned = P.planLambdaComparison parameters body' afterBody in
 let name, named = A.freshLiftedName planned "__closure_" in
 let comparison = if A.lambdaNeedsComparison parameters state then let name, add, next = P.comparisonNameForIdentity plan.P.identity plan.P.captureTypes named in Some (name, add, next) else None in
 let comparisonState = Option.fold ~none:named ~some:(fun (_, _, next) -> next) comparison in
 let id, comparisonState = if argument then None, comparisonState else let id, symbols = C.internFunction name comparisonState.A.symbols in Some id, {comparisonState with A.symbols} in
 let metadata = if Option.is_some comparison then [AST.TInternalRawPtr] else [] in
 let tupleTypes = AST.TInt64 :: (metadata @ plan.P.captureTypes) in
 let closure, symbols = if argument then C.allocateBinding "__closure" comparisonState.A.symbols else match state.A.recursiveSelf with Some (_, closure, _, _) -> closure, comparisonState.A.symbols | None -> C.allocateBinding "__closure" comparisonState.A.symbols in
 let loweredParameters, loweredBody, symbols = S.lowerLambdaParameters symbols parameters plan.P.body in
 let loweredBody = if argument then loweredBody else match state.A.recursiveSelf, id with Some _, Some id -> P.rewriteLiftedSelfCalls id closure loweredBody | _ -> loweredBody in
 let offset = if Option.is_some comparison then 2 else 1 in
 let accessors = List.mapi (fun index name -> name, C.TupleAccess (C.Local closure, index + offset)) plan.P.captureNames in
 let bodyWithExtractions = List.fold_right (fun (name, value) body -> C.Let (C.LPVariable name, value, body)) accessors loweredBody in
 let forReturn = {withParams with A.funcParams = state1.A.funcParams; funcReturnTypes = state1.A.funcReturnTypes; genericFuncDefs = state1.A.genericFuncDefs} in
 let* returnType = A.inferLambdaReturnType body forReturn in
 let id, symbols = match id with Some id -> id, symbols | None -> C.internFunction name symbols in
 let func : C.functionDef = {C.id; name; typeParams = []; params = C.checkedParams (S.paramsFromList (if argument then "lifted argument lambda" else "lifted lambda") ((closure, AST.TTuple tupleTypes) :: loweredParameters)); returnType = C.checkedType returnType; body = bodyWithExtractions; recursion = if argument then None else Option.map (fun (_, _, _, typed) -> typed) state.A.recursiveSelf} in
 let comparator, symbols = match comparison with Some (name, true, _) -> let func, symbols = P.makeClosureComparator name plan.P.captureTypes plan.P.compareCaptures state1.A.variantLookup symbols in Some func, symbols | _ -> None, symbols in
 let next : A.liftState = {A.symbols; counter = comparisonState.A.counter; liftedFunctions = (match comparator with Some comparator -> comparator :: func :: state1.A.liftedFunctions | None -> func :: state1.A.liftedFunctions); comparisonFuncs = comparisonState.A.comparisonFuncs; comparableFunctionParams = state.A.comparableFunctionParams; typeEnv = state.A.typeEnv; funcParams = state1.A.funcParams; funcReturnTypes = state1.A.funcReturnTypes; genericFuncDefs = state1.A.genericFuncDefs; typeReg = state1.A.typeReg; variantLookup = state1.A.variantLookup; recursiveSelf = state.A.recursiveSelf} in
 let captures = match comparison with None -> plan.P.captureExprs | Some (name, _, _) -> let id = match C.tryFindFunctionId name symbols with Some id -> id | None -> Crash.crash "Closure comparison is absent from symbols" in C.FuncRef id :: plan.P.captureExprs in
 Ok (C.Closure (id, captures), next)
(*
   Lift lambdas in function arguments, converting all lambdas to Closures
   (even non-capturing lambdas become trivial closures for uniform calling convention)
   Also wraps FuncRef in closures for uniform calling convention
   Add lambda parameters to type environment before processing body
   First, recursively lift any nested lambdas in the body
   All lambdas become closures (even non-capturing ones) for uniform calling convention
   The lifted function takes closure as first param, then original params
   Build body that extracts captures from closure tuple:
   let cap1 = __closure.1 in let cap2 = __closure.2 in ... original_body
   Restore original TypeEnv (exclude lambda params)
   The whole-program wrapper pass deduplicates named function
   values by semantic identity after expression-local lambdas
   have been lifted.
*)
and liftLambdasInArgs args state =
 let rec loop remaining state acc = match remaining with [] -> Ok (S.exprArgsFromList (List.rev acc), state) | arg :: rest ->
 let* arg, state = match arg with C.Lambda (parameters, _, body) -> liftLambda true parameters body state | _ -> liftLambdasInExpr arg state in loop rest state (arg :: acc) in loop (NonEmptyList.toList args) state []
(*
   Helper to lift lambdas in a list of expressions
*)
and liftLambdasInList values state =
 let rec loop remaining state acc = match remaining with [] -> Ok (List.rev acc, state) | value :: rest -> let* value, state = liftLambdasInExpr value state in loop rest state (value :: acc) in loop values state []
(*
   Helper to lift lambdas in record fields
*)
and liftLambdasInFields fields state =
 let rec loop remaining state acc = match remaining with [] -> Ok (List.rev acc, state) | (name, value) :: rest -> let* value, state = liftLambdasInExpr value state in loop rest state ((name, value) :: acc) in loop fields state []
and liftLambdasInDictEntries entries state =
 let rec loop remaining state acc = match remaining with [] -> Ok (List.rev acc, state) | (key, value) :: rest -> let* key, state = liftLambdasInExpr key state in let* value, state = liftLambdasInExpr value state in loop rest state ((key, value) :: acc) in loop entries state []
(*
   Helper to lift lambdas in match cases
   Lift lambdas in guard if present
   Lift lambdas in a function definition
*)
and liftLambdasInCases cases typ state =
 let rec loop cases state acc = match cases with [] -> Ok (List.rev acc, state) | (case : C.matchCase) :: rest ->
 let bindings = match typ with None -> B.empty | Some typ -> NonEmptyList.toList case.C.patterns |> List.map (fun pattern -> A.matchPatternBindingTypes state.A.typeReg state.A.variantLookup (R.typeNamesFromSymbols state.A.symbols) pattern typ) |> List.fold_left merge B.empty in
 let child = {state with A.typeEnv = merge state.A.typeEnv bindings} in
 let* guard, state1 = match case.C.guard with None -> Ok (None, child) | Some guard -> let* guard, next = liftLambdasInExpr guard child in Ok (Some guard, next) in
 let* body, state2 = liftLambdasInExpr case.C.body state1 in
 loop rest {state2 with A.typeEnv = state.A.typeEnv} ({case with C.guard; body} :: acc) in loop cases state []
