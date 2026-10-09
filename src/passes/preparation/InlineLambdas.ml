(*
   InlineLambdas.ml - Inline lexical lambda bindings before closure conversion.
*)
[@@@warning "-4"]

module C = CheckedAST
module M = C.BindingIdMap

type lambdaEnv = C.expr M.t

let same left right = AST.compareBindingId left right = 0
let contains name bindings = List.exists (same name) bindings

(*
   Check if a variable occurs in an expression (for dead code elimination)
   If name is shadowed by a parameter, it doesn't occur
   Function references don't contain variable references
   Check if name occurs in captured expressions
*)
let rec varOccursInExpr name expr =
  let occurs = varOccursInExpr name in
  let arguments args = List.exists occurs (NonEmptyList.toList args) in
  match expr with
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.RuntimeError _ | C.FuncRef _
  | C.GenericFuncRef _ ->
      false
  | C.Local id -> same id name
  | C.BoundaryRender (_, value)
  | C.UnaryOp (_, value)
  | C.TupleAccess (value, _)
  | C.RecordAccess (value, _) ->
      occurs value
  | C.BinOp (_, left, right) | C.Sequence (left, right) ->
      occurs left || occurs right
  | C.Let (pattern, value, body) ->
      occurs value
      || ((not (contains name (C.letPatternBindings pattern))) && occurs body)
  | C.RecursiveLet (recursion, value, body) ->
      if same (C.recursiveBindingId recursion) name then false
      else occurs value || occurs body
  | C.If (condition, yes, no) -> occurs condition || occurs yes || occurs no
  | C.Call (_, args) | C.TypeApp (_, _, args) -> arguments args
  | C.TupleLiteral elements ->
      List.exists occurs (C.tupleElementsToList elements)
  | C.DictLiteral (_, _, entries) ->
      List.exists (fun (key, value) -> occurs key || occurs value) entries
  | C.RecordLiteral (_, fields) ->
      List.exists
        (fun (_, value) -> occurs value)
        (C.recordFieldsInSourceOrder fields)
  | C.RecordUpdate (record, fields) ->
      occurs record || List.exists (fun (_, value) -> occurs value) fields
  | C.Constructor (_, fields) | C.ListLiteral fields | C.Closure (_, fields) ->
      List.exists occurs fields
  | C.Match (value, cases) ->
      occurs value
      || List.exists
           (fun (case : C.matchCase) ->
             Option.fold ~none:false ~some:occurs case.C.guard
             || occurs case.C.body)
           (NonEmptyList.toList cases)
  | C.Lambda (parameters, _, body) ->
      let names =
        NonEmptyList.toList parameters
        |> List.concat_map (fun (parameter : C.lambdaParameter) ->
            C.letPatternBindings parameter.C.pattern)
      in
      if contains name names then false else occurs body
  | C.Apply (target, args) | C.IndirectApply (target, args) ->
      occurs target || arguments args
  | C.InterpolatedString parts ->
      List.exists
        (function
          | C.StringText _ -> false | C.StringExpr value -> occurs value)
        parts

(*
   Inline lambdas at Apply sites
   lambdaEnv: maps variable names to their lambda expressions
   If the value is a lambda, make the name callable only in the body.
   Check if this variable is a known lambda
   Unknown function variable - keep as-is (will error later if not valid)
   Non-variable function (could be lambda or other expr)
   Function references don't need lambda inlining
   Inline lambdas in captured expressions
*)
let rec inlineLambdas expr environment =
  let recurse value = inlineLambdas value environment in
  let remove names environment =
    List.fold_left
      (fun environment name -> M.remove name environment)
      environment names
  in
  match expr with
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.RuntimeError _ | C.Local _
  | C.FuncRef _ | C.GenericFuncRef _ ->
      expr
  | C.BoundaryRender (renderer, value) ->
      C.BoundaryRender (renderer, recurse value)
  | C.BinOp (operation, left, right) ->
      C.BinOp (operation, recurse left, recurse right)
  | C.UnaryOp (operation, value) -> C.UnaryOp (operation, recurse value)
  | C.Let (pattern, value, body) ->
      let value = recurse value in
      let child = remove (C.letPatternBindings pattern) environment in
      let child =
        match (pattern, value) with
        | C.LPVariable name, C.Lambda _ -> M.add name value child
        | _ -> child
      in
      let body = inlineLambdas body child in
      C.Let (pattern, value, body)
  | C.RecursiveLet (recursion, value, body) ->
      let child = M.remove (C.recursiveBindingId recursion) environment in
      C.RecursiveLet
        (recursion, inlineLambdas value child, inlineLambdas body child)
  | C.If (condition, yes, no) ->
      C.If (recurse condition, recurse yes, recurse no)
  | C.Sequence (first, next) -> C.Sequence (recurse first, recurse next)
  | C.Call (target, args) -> C.Call (target, NonEmptyList.map recurse args)
  | C.TypeApp (target, parameters, args) ->
      C.TypeApp (target, parameters, NonEmptyList.map recurse args)
  | C.TupleLiteral elements ->
      C.TupleLiteral (C.mapTupleElements recurse elements)
  | C.TupleAccess (value, index) -> C.TupleAccess (recurse value, index)
  | C.DictLiteral (key, value, entries) ->
      C.DictLiteral
        ( key,
          value,
          List.map (fun (key, value) -> (recurse key, recurse value)) entries )
  | C.RecordLiteral (reference, fields) ->
      C.RecordLiteral (reference, C.mapRecordFields recurse fields)
  | C.RecordUpdate (record, fields) ->
      C.RecordUpdate
        ( recurse record,
          List.map (fun (name, value) -> (name, recurse value)) fields )
  | C.RecordAccess (record, field) -> C.RecordAccess (recurse record, field)
  | C.Constructor (reference, fields) ->
      C.Constructor (reference, List.map recurse fields)
  | C.Match (value, cases) ->
      let cases =
        NonEmptyList.map
          (fun (case : C.matchCase) ->
            let names =
              NonEmptyList.toList case.C.patterns
              |> List.concat_map C.patternBindings
            in
            let child = remove names environment in
            {
              case with
              C.guard =
                Option.map (fun value -> inlineLambdas value child) case.C.guard;
              body = inlineLambdas case.C.body child;
            })
          cases
      in
      C.Match (recurse value, cases)
  | C.ListLiteral values -> C.ListLiteral (List.map recurse values)
  | C.Lambda (parameters, annotation, body) ->
      let names =
        NonEmptyList.toList parameters
        |> List.concat_map (fun (parameter : C.lambdaParameter) ->
            C.letPatternBindings parameter.C.pattern)
      in
      C.Lambda
        (parameters, annotation, inlineLambdas body (remove names environment))
  | C.Apply (target, args) -> (
      let args = NonEmptyList.map recurse args in
      match target with
      | C.Local id -> (
          match M.find_opt id environment with
          | Some _ -> C.Apply (C.Local id, args)
          | None -> C.Apply (C.Local id, args))
      | _ -> C.Apply (recurse target, args))
  | C.IndirectApply (target, args) ->
      C.IndirectApply (recurse target, NonEmptyList.map recurse args)
  | C.Closure (target, captures) -> C.Closure (target, List.map recurse captures)
  | C.InterpolatedString parts ->
      C.InterpolatedString
        (List.map
           (function
             | C.StringText _ as part -> part
             | C.StringExpr value -> C.StringExpr (recurse value))
           parts)

(*
   Inline lambdas in a function definition
*)
let inlineLambdasInFunc (func : C.functionDef) =
  { func with C.body = inlineLambdas func.C.body M.empty }

(*
   Inline lambdas in a program
   Lambda Lifting: Convert Lambdas to Top-Level Functions with Closures
   Lambda lifting transforms nested lambda expressions into top-level functions.
   The process handles both capturing and non-capturing lambdas uniformly.
   Algorithm:
   1. Identify lambdas in argument positions (function calls, let bindings)
   2. Collect free variables (captures) from each lambda body
   3. Generate a lifted function with signature: (closure_tuple, original_params...) -> result
   4. Replace the lambda with a ClosureAlloc expression containing the function and captures
   Closure representation at runtime:
   [func_ptr, cap1, cap2, ...]  -- heap-allocated tuple
   The lifted function extracts captures from the closure tuple:
   let __closure_N(__closure, x, y) =
   let cap1 = __closure.1
   let cap2 = __closure.2
   in <original body with captures replaced>
   All function values use closures for uniform calling convention, even non-capturing
   lambdas and function references. This simplifies higher-order function support.
   See docs/compiler/frontend/closures.md for detailed documentation.
   State for lambda lifting - tracks generated functions and counter
*)
let inlineLambdasInProgram program =
  let tops =
    List.map
      (function
        | C.FunctionDef func -> C.FunctionDef (inlineLambdasInFunc func)
        | C.Expression value -> C.Expression (inlineLambdas value M.empty)
        | C.ValueDef value ->
            C.ValueDef
              {
                value with
                C.body = inlineLambdas (C.valueDefBody value) M.empty;
              }
        | C.TypeDef _ as value -> value)
      (C.programTopLevels program)
  in
  C.withProgramTopLevels tops program
