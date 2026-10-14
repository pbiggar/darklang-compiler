(* WrittenPipeSupport.mli - Shared pipeline elaboration in written expressions. *)
val lower :
  WrittenTypes.range ->
  WrittenTypes.expr ->
  (WrittenTypes.range * WrittenTypes.pipeExpr) list ->
  WrittenTypes.expr
