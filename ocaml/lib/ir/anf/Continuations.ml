(* Continuations.fs - Substitute ANF return continuations without changing lexical joins. *)
[@@@warning "-4"]
let isSupportedJoinArgumentType = function
 | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
 | AST.TBool | AST.TDateTime | AST.TUnit | AST.TInternalRawPtr -> true
 | _ -> false
let rec bindReturns expr continuation = match expr with
 | ANF.Jump _ -> expr
 | ANF.Join (parameter, rest, entry) ->
   let rest = bindReturns rest continuation in let entry = bindReturns entry continuation in
   ANF.Join (parameter, rest, entry)
 | ANF.Return atom -> continuation atom
 | ANF.Let (_, ANF.RuntimeError _, _) -> expr
 | ANF.Let (id, expression, rest) -> ANF.Let (id, expression, bindReturns rest continuation)
 | ANF.If (condition, yes, no) ->
   let yes = bindReturns yes continuation in let no = bindReturns no continuation in
   ANF.If (condition, yes, no)
let wrapBindings bindings expression = List.fold_right (fun (id, value) rest -> ANF.Let (id, value, rest)) bindings expression
