(*
   ExpressionSupport.ml - Typed recursive checking interface and call-argument diagnostics.
*)
(* ExpressionSupport.ml - Typed recursive checking interface and call-argument diagnostics. *)
type expressionChecker =
  AST.expr ->
  Types.typeEnv ->
  Types.indexedTypeRegistry ->
  Types.variantLookup ->
  Types.genericFuncRegistry ->
  AST.warningSettings ->
  AST.moduleRegistry ->
  Types.aliasRegistry ->
  AST.semanticType option ->
  (AST.semanticType * AST.expr, CheckingDiagnostics.typeError) result

let paramNameForLegacyError registry name index =
  let zeroBased = Int32.to_int (Int32.sub (Int32.of_int index) 1l) in
  let rec tryGetAtIndex index = function
    | [] -> None
    | item :: rest ->
        if index = 0 then Some item else tryGetAtIndex (index - 1) rest
  in
  let resolved =
    if zeroBased < 0 then None
    else
      Option.bind (Unification.tryLookupResolved name registry)
        (fun (params, _) -> tryGetAtIndex zeroBased params)
  in
  match resolved with Some name -> name | None -> "arg" ^ string_of_int index
