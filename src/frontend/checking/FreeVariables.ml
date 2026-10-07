(*
   FreeVariables.ml - Collect expression and pattern binding dependencies.
*)
(* FreeVariables.ml - Preserve lexical closure dependencies and direct-call handling. *)
open! AST
module Names = StringOrder.Set

let unions sets = List.fold_left Names.union Names.empty sets

(*
   Free Variable Analysis for Closures
   When compiling lambdas, we need to identify which variables from the
   enclosing scope are referenced in the lambda body (free variables).
   Only these need to be captured in the closure.
   Collect free variables in an expression.
   Returns the set of variable names that are referenced but not bound locally.
   bound: Set of names that are currently in scope (not free)
   Collect bindings from all patterns (all patterns in a group bind same vars)
   Include guard free vars if present
   Closures capture expressions which may have free variables
*)
let rec collectFreeVars expr bound =
  let collect expr = collectFreeVars expr bound in
  let collectList xs = unions (List.map collect xs) in
  match expr with
  | BoundaryRender (_, value) -> collect value
  | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
  | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
  | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
  | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
  | RuntimeError _ ->
      Names.empty
  | Var name ->
      if
        Names.mem name bound
        || CheckingDiagnostics.isBuiltinTestNanName name
        || CheckingDiagnostics.isBuiltinTestInfinityName name
      then Names.empty
      else Names.singleton name
  | BinOp (_, left, right) -> Names.union (collect left) (collect right)
  | UnaryOp (_, inner) -> collect inner
  | Let (pattern, value, body) ->
      let names = Names.of_list (letPatternBindings pattern) in
      Names.union (collect value)
        (collectFreeVars body (Names.union names bound))
  | RecursiveLet (recursion, value, body) ->
      let recursiveBound = Names.add (recursiveBindingName recursion) bound in
      Names.union
        (collectFreeVars value recursiveBound)
        (collectFreeVars body recursiveBound)
  | If (cond, yes, no) ->
      Names.union (collect cond) (Names.union (collect yes) (collect no))
  | Sequence (first, next) -> Names.union (collect first) (collect next)
  | Apply (Var _, _, args) -> collectList (NonEmptyList.toList args)
  | TupleLiteral elems
  | ListLiteral elems
  | Closure (_, elems)
  | Constructor (_, _, elems) ->
      collectList elems
  | TupleAccess (tuple, _) | RecordAccess (tuple, _) -> collect tuple
  | DictLiteral (_, _, entries) ->
      unions
        (List.concat_map
           (fun (key, value) -> [ collect key; collect value ])
           entries)
  | RecordLiteral (_, fields) ->
      unions (List.map (fun (_, value) -> collect value) fields)
  | RecordUpdate (record, updates) ->
      Names.union (collect record)
        (unions (List.map (fun (_, value) -> collect value) updates))
  | Match (scrutinee, cases) ->
      let casesFree =
        List.map
          (fun case ->
            let patternBindings =
              unions
                (List.map collectPatternBindings
                   (NonEmptyList.toList case.patterns))
            in
            let bodyBound = Names.union bound patternBindings in
            let guardFree =
              Option.fold ~none:Names.empty
                ~some:(fun guard -> collectFreeVars guard bodyBound)
                case.guard
            in
            Names.union guardFree (collectFreeVars case.body bodyBound))
          cases
      in
      Names.union (collect scrutinee) (unions casesFree)
  | Lambda (parameters, _, body) ->
      let paramNames =
        Names.of_list
          (List.concat_map
             (fun parameter -> letPatternBindings parameter.pattern)
             (NonEmptyList.toList parameters))
      in
      collectFreeVars body (Names.union bound paramNames)
  | Apply (func, _, args) | IndirectApply (func, args) ->
      Names.union (collect func) (collectList (NonEmptyList.toList args))
  | InterpolatedString parts ->
      unions
        (List.filter_map
           (function
             | StringText _ -> None | StringExpr expr -> Some (collect expr))
           parts)

(*
   Collect variable names bound by a pattern
*)
and collectPatternBindings = function
  | PUnit | PWildcard | PInt64 _ | PBigInt _ | PInt128Literal _ | PInt8Literal _
  | PInt16Literal _ | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _
  | PUInt32Literal _ | PUInt64Literal _ | PUInt128Literal _ | PBool _
  | PString _ | PChar _ | PFloat _ ->
      Names.empty
  | PVar name -> Names.singleton name
  | PConstructor (_, fields)
  | PResolvedConstructor (_, _, _, fields)
  | PTuple fields
  | PList fields ->
      unions (List.map collectPatternBindings fields)
  | PListCons (heads, tail) ->
      Names.union
        (unions (List.map collectPatternBindings heads))
        (collectPatternBindings tail)
  | POr alternatives -> collectPatternBindings (NonEmptyList.head alternatives)
