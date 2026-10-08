(* ExpressionLowering.ml - Lower expressions while delegating recursive children through typed callbacks. *)
[@@@warning "-4"]

module A = ANF
module C = CheckedAST
module R = TypeRegistries
module P = LoweringPrimitives
module T = TypeSubstitution
module O = LoweringOperators
module I = LoweringTypeInference
module G = LoweringAggregates
module K = Continuations
module M = StringOrder.Map
module Indices = Map.Make (Int)

let ( let* ) = Result.bind
let int value = A.IntLiteral (A.Int64 value)
let displayId describe id = StructuralFormat.format (describe id)

let add left right =
  Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))

let _legacyListTreeHelpers () =
  let _listNode = AST.TList (AST.TVar "a") in
  let _listNodeType = Some _listNode in
  let tagRawPtrAsList listNode tag ptr gen bindings =
    let raw, gen = A.freshVar gen in
    let tagExpr = A.Prim (A.BitOr, A.Var ptr, int tag) in
    let typed, gen = A.freshVar gen in
    let typedExpr = A.TypedAtom (A.Var raw, listNode) in
    (A.Var typed, bindings @ [ (raw, tagExpr); (typed, typedExpr) ], gen)
  in
  let _allocLeaf atom typ gen bindings =
    let ptr, gen = A.freshVar gen in
    let set, gen = A.freshVar gen in
    let rc, gen = A.freshVar gen in
    let bindings =
      bindings
      @ [
          (ptr, A.RawAlloc (int 16L));
          (set, A.RawSlotInit (A.Var ptr, int 0L, atom, typ));
          (rc, A.RawWriteWord (A.Var ptr, int 8L, int 1L));
        ]
    in
    tagRawPtrAsList (AST.TList typ) 5L ptr gen bindings
  in
  let allocSingle listNode node gen bindings =
    let ptr, gen = A.freshVar gen in
    let set, gen = A.freshVar gen in
    let rc, gen = A.freshVar gen in
    let bindings =
      bindings
      @ [
          (ptr, A.RawAlloc (int 16L));
          (set, A.RawSlotInit (A.Var ptr, int 0L, node, listNode));
          (rc, A.RawWriteWord (A.Var ptr, int 8L, int 1L));
        ]
    in
    tagRawPtrAsList listNode 1L ptr gen bindings
  in
  let allocDeep listNode measure prefix middle suffix gen bindings =
    let prefixCount = List.length prefix in
    let suffixCount = List.length suffix in
    let ptr, gen = A.freshVar gen in
    let alloc = A.RawAlloc (int 104L) in
    let setAt offset value typ gen bindings =
      let id, gen = A.freshVar gen in
      let expr =
        match typ with
        | Some typ ->
            A.RawSlotInit (A.Var ptr, int (Int64.of_int offset), value, typ)
        | None -> A.RawWriteWord (A.Var ptr, int (Int64.of_int offset), value)
      in
      (gen, bindings @ [ (id, expr) ])
    in
    let gen, bindings =
      setAt 0 (int (Int64.of_int measure)) None gen (bindings @ [ (ptr, alloc) ])
    in
    let gen, bindings =
      setAt 8 (int (Int64.of_int prefixCount)) None gen bindings
    in
    let rec setPrefix nodes offset gen bindings =
      match nodes with
      | [] -> (gen, bindings)
      | node :: rest ->
          let gen, bindings = setAt offset node (Some listNode) gen bindings in
          setPrefix rest (add offset 8) gen bindings
    in
    let gen, bindings = setPrefix prefix 16 gen bindings in
    let gen, bindings = setAt 48 middle (Some listNode) gen bindings in
    let gen, bindings =
      setAt 56 (int (Int64.of_int suffixCount)) None gen bindings
    in
    let gen, bindings = setPrefix suffix 64 gen bindings in
    let gen, bindings = setAt 96 (int 1L) None gen bindings in
    tagRawPtrAsList listNode 2L ptr gen bindings
  in
  let emptyTree = int 0L in
  let nodeAtom = fst in
  let nodeMeasure = snd in
  let allocNode2 listNode left right gen bindings =
    let ptr, gen = A.freshVar gen in
    let alloc = A.RawAlloc (int 32L) in
    let set0, gen = A.freshVar gen in
    let expr0 = A.RawSlotInit (A.Var ptr, int 0L, nodeAtom left, listNode) in
    let set1, gen = A.freshVar gen in
    let expr1 = A.RawSlotInit (A.Var ptr, int 8L, nodeAtom right, listNode) in
    let measure = add (nodeMeasure left) (nodeMeasure right) in
    let set2, gen = A.freshVar gen in
    let expr2 =
      A.RawWriteWord (A.Var ptr, int 16L, int (Int64.of_int measure))
    in
    let rc, gen = A.freshVar gen in
    let rcExpr = A.RawWriteWord (A.Var ptr, int 24L, int 1L) in
    let node, bindings, gen =
      tagRawPtrAsList listNode 3L ptr gen
        (bindings
        @ [
            (ptr, alloc);
            (set0, expr0);
            (set1, expr1);
            (set2, expr2);
            (rc, rcExpr);
          ])
    in
    ((node, measure), bindings, gen)
  in
  let allocNode3 listNode first second third gen bindings =
    let ptr, gen = A.freshVar gen in
    let alloc = A.RawAlloc (int 40L) in
    let set0, gen = A.freshVar gen in
    let expr0 = A.RawSlotInit (A.Var ptr, int 0L, nodeAtom first, listNode) in
    let set1, gen = A.freshVar gen in
    let expr1 = A.RawSlotInit (A.Var ptr, int 8L, nodeAtom second, listNode) in
    let set2, gen = A.freshVar gen in
    let expr2 = A.RawSlotInit (A.Var ptr, int 16L, nodeAtom third, listNode) in
    let measure =
      add (add (nodeMeasure first) (nodeMeasure second)) (nodeMeasure third)
    in
    let set3, gen = A.freshVar gen in
    let expr3 =
      A.RawWriteWord (A.Var ptr, int 24L, int (Int64.of_int measure))
    in
    let rc, gen = A.freshVar gen in
    let rcExpr = A.RawWriteWord (A.Var ptr, int 32L, int 1L) in
    let node, bindings, gen =
      tagRawPtrAsList listNode 4L ptr gen
        (bindings
        @ [
            (ptr, alloc);
            (set0, expr0);
            (set1, expr1);
            (set2, expr2);
            (set3, expr3);
            (rc, rcExpr);
          ])
    in
    ((node, measure), bindings, gen)
  in
  let splitAt count nodes =
    let rec loop remaining acc rest =
      match (remaining, rest) with
      | 0, _ -> Ok (List.rev acc, rest)
      | _, [] -> Error "List literal: not enough nodes for split"
      | n, x :: xs -> loop (add n (-1)) (x :: acc) xs
    in
    loop count [] nodes
  in
  let groupSizes count =
    if count < 2 then Error "List literal: middle spine needs at least 2 nodes"
    else
      match count mod 3 with
      | 0 -> Ok (List.init (count / 3) (fun _ -> 3))
      | 1 ->
          if count < 4 then Error "List literal: invalid middle spine size"
          else Ok (2 :: 2 :: List.init ((count - 4) / 3) (fun _ -> 3))
      | _ -> Ok (2 :: List.init ((count - 2) / 3) (fun _ -> 3))
  in
  let rec buildGroupedNodes listNode sizes nodes gen bindings acc =
    match sizes with
    | [] -> Ok (List.rev acc, bindings, gen)
    | size :: rest -> (
        let* group, remaining = splitAt size nodes in
        match (size, group) with
        | 2, [ a; b ] ->
            let node, bindings, gen = allocNode2 listNode a b gen bindings in
            buildGroupedNodes listNode rest remaining gen bindings (node :: acc)
        | 3, [ a; b; c ] ->
            let node, bindings, gen = allocNode3 listNode a b c gen bindings in
            buildGroupedNodes listNode rest remaining gen bindings (node :: acc)
        | _ ->
            Error ("List literal: unexpected group size " ^ string_of_int size))
  in
  let rec buildTree listNode nodes gen bindings =
    let count = List.length nodes in
    let total () =
      List.fold_left (fun sum node -> add sum (nodeMeasure node)) 0 nodes
    in
    match nodes with
    | [] -> Ok (emptyTree, bindings, gen)
    | [ single ] -> Ok (allocSingle listNode (nodeAtom single) gen bindings)
    | first :: rest when count <= 5 ->
        Ok
          (allocDeep listNode (total ())
             [ nodeAtom first ]
             emptyTree (List.map nodeAtom rest) gen bindings)
    | _ ->
        let* prefix, rest = splitAt 2 nodes in
        let middleCount = List.length rest - 2 in
        let* middle, suffix = splitAt middleCount rest in
        let* sizes = groupSizes (List.length middle) in
        let* grouped, bindings, gen =
          buildGroupedNodes listNode sizes middle gen bindings []
        in
        let* middle, bindings, gen = buildTree listNode grouped gen bindings in
        Ok
          (allocDeep listNode (total ()) (List.map nodeAtom prefix) middle
             (List.map nodeAtom suffix) gen bindings)
  in
  buildTree

(*
   Unit literal becomes return of unit value (represented as 0)
   Integer literal (default Int64)
   Boolean literal becomes return
   String literal becomes return
   Char literal becomes return (stored as string, same runtime representation)
   Float literal becomes return
   Explicit function reference - wrap in closure for uniform calling convention
   Closure: allocate closure tuple with function address and captured values
   Convert each capture expression to an atom
   Generate ClosureAlloc: allocate closure tuple
   Evaluate the RHS in the incoming environment, then prepare every
   projection before exposing any binder to the continuation.
   Infer the type of the value for type-directed field lookup
   Type checking has replaced the continuation with the binding
   mismatch error. Preserve every RHS effect, then transition
   directly to that error without preparing any projection.
   Try toAtom first; if it fails for complex expressions like Match, use toANF
   Complex expression (like Match) - compile with toANF and transform returns
   Unary negation: use operand type to select float vs integer path
   Constant-fold negative float literals at compile time
   The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
   When negated, it should remain INT64_MIN (mathematically correct)
   Boolean not: convert operand to atom and apply Not
   Create unary op and bind to fresh variable
   Build the expression: innerBindings + let tempVar = op
   RuntimeError uses Unit as its unreachable ANF return.
   Concat still needs a representation-valid operand for codegen.
   Infer type of left operand to check if structural comparison is needed
   Generate structural equality
   For Neq, negate the result
   Primitive type or type inference failed - use simple comparison
   Arithmetic, bitwise, and comparison operators - use simple primitive
   Preserve source order and let the final expression carry the value.
   bindReturns also prevents the tail from running after a failing head.
   Function call: convert all arguments to atoms
   If an argument is a function reference, wrap it in a trivial closure for uniform calling convention
   Function reference needs to be wrapped in a closure.
   Wrap function references in closures for uniform calling convention.
   Regular function call (including module functions like Stdlib.Int64.add)
   Bind call result to fresh variable
   Check if funcName is a variable (indirect call) or a defined function (direct call)
   Not a variable - check explicit presentation effects first.
   Check if it's a file intrinsic.
   Check if it's a raw memory intrinsic
   Raw memory intrinsic call
   Check if it's a Float intrinsic
   Float intrinsic call
   Check if it's a random intrinsic
   Random intrinsic call
   Check if it's a DateTime intrinsic.
   DateTime intrinsic call.
   Check if it's a defined function
   Direct call to defined function
   Preserve existing behavior for malformed registry entries.
   Unknown function - could be error or forward reference
   For now, assume it's a valid function (will fail at link time if not)
   Generic function call - not yet implemented
   Convert all elements to bound atoms so tuple elements can include expressions
   that cannot be lowered directly with toAtom (for example Builtin.testRuntimeError).
   Create TupleAlloc and bind to fresh variable
   Convert tuple to atom and create TupleGet
   Evaluate fields in source order, then place their already-computed atoms
   into the record's declaration-order layout.
   Projection is type-directed so the nominal descriptor and keyed slot
   always agree, including after aliases and generic substitution.
   Look up field index in the specific record type
   Check if ANY variant in this type has a payload
   If so, all variants must be heap-allocated for consistency
   Note: We get typeName from variantLookup, not from AST (which may be empty)
   Pure enum type (no payloads anywhere): return tag as an integer
   No payload but type has other variants with payloads
   Heap-allocate as [tag, 0] for uniform 2-element structure
   This enables consistent structural equality comparison
   Variant with payload: allocate [tag, payload] on heap
   Compile list literal as SkewList
   Tags: EMPTY=0, SINGLE=1, DEEP=2, NODE2=3, NODE3=4, LEAF=5
   DEEP layout: [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]
   Tag a raw pointer as a list value without routing through Stdlib wrappers.
   Keep a typed binding so RC/type inference still treats the result as List<a>.
   Helper to create a LEAF node wrapping an element
   Helper to create a SINGLE node containing a TreeNode
   Helper to create a DEEP node
   12 fields * 8 bytes + refcount
   Build all the set operations
   Set prefix nodes (p0-p3 at offsets 16, 24, 32, 40)
   Set middle at offset 48 (type-uniform: another SkewList of nodes)
   Set suffix count at offset 56
   Set suffix nodes (s0-s3 at offsets 64, 72, 80, 88)
   Set refcount at offset 96
   Tag with DEEP (2)
   Build SkewList nodes for middle spines without using pushBack.
   Helper to create a NODE2 (tag 3): [child0:8][child1:8][measure:8]
   Helper to create a NODE3 (tag 4): [child0:8][child1:8][child2:8][measure:8]
   Empty list is EMPTY (represented as 0)
   Evaluate elements in source order before constructing the list.
   Desugar interpolated string to StringConcat chain
   $"Hello {name}!" → "Hello " ++ name ++ "!"
   Empty interpolated string → empty string
   Single part → convert directly
   Multiple parts → fold with StringConcat
   Lambda in expression position - closures not yet fully implemented
   Apply a function expression to arguments
   For now, only support immediate application of lambdas
   Immediate application becomes one non-recursive let per binder.
   Build nested let bindings: let p1 = arg1 in let p2 = arg2 in ... body
   Should not happen due to length check
   Calling a variable that might hold a closure
   Variable exists - treat as closure call
   Generate closure call
   Nested application: (fun x -> fun y -> ...)(a)(b)(c)...
   Flatten all nested applies first, then desugar from innermost out
   allArgLists is a list of arg lists, from innermost to outermost
   e.g., for f(1)(2)(3), we get ([1], [2], [3])
   Desugar all nested lambda applications at once
   Will error later, just wrap in Apply for now
   Desugar: let p1 = a1 in let p2 = a2 in ... body
   Float let out: Apply(let x = v in body, args) → let x = v in Apply(body, args)
   Non-lambda function - wrap remaining in Apply
   Base function is not a lambda - use toAtom which handles nested applies
   Reconstruct the full nested apply, then delegate to toAtom
   Apply(let x = v in body, args) → let x = v in Apply(body, args)
   Float the let binding out
   Closure being called directly - convert to ClosureCall
   First, convert captures to atoms
   Allocate closure
   Convert args
   General function-expression application (for example record field access):
   evaluate function expression to a closure value, then invoke it.
*)
let lowerExpression (toANFCore : LoweringCallbacks.expressionLowerer)
    (toAtomCore : LoweringCallbacks.atomLowerer)
    (toANFBoundAtomCore : LoweringCallbacks.boundAtomLowerer) functionIds sums
    typeNames inert expr gen env registry variants functions names modules =
  let fieldIndex id =
    match R.tryFindFieldIndex id typeNames with
    | Some index -> index
    | None ->
        Crash.crash "Checked field identity is absent from layout metadata"
  in
  let constructorTag id =
    match R.tryFindConstructorTag id typeNames with
    | Some tag -> tag
    | None ->
        Crash.crash
          "Checked constructor identity is absent from layout metadata"
  in
  let functionId name =
    match M.find_opt name functionIds with
    | Some id -> id
    | None ->
        Crash.crash
          ("Expression lowering function '" ^ name
         ^ "' is absent from registries")
  in
  let functionName id =
    match FunctionIdMap.tryFind id names with
    | Some name -> Some name
    | None -> Option.map fst (FunctionIdMap.tryFind id functions)
  in
  let functionNameIs id expected = functionName id = Some expected in
  let anf ?(env = env) value gen =
    toANFCore sums typeNames inert value gen env registry variants functions
      names modules
  in
  let atom value gen =
    toAtomCore sums typeNames inert value gen env registry variants functions
      names modules
  in
  let bound value gen =
    toANFBoundAtomCore sums typeNames inert value gen env registry variants
      functions names modules
  in
  let infer value =
    I.inferTypeCore sums typeNames value (R.typeEnvFromVarEnv env) registry
      variants functions names modules
  in
  let finish gen cexpr =
    let id, gen = A.freshVar gen in
    (A.Let (id, cexpr, A.Return (A.Var id)), gen)
  in
  let sequence setups body =
    List.fold_right
      (fun setup continuation -> K.bindReturns setup (fun _ -> continuation))
      setups body
  in
  let rec convertBound remaining gen acc =
    match remaining with
    | [] -> Ok (List.rev acc, gen)
    | value :: rest ->
        let* setup, value, gen = bound value gen in
        convertBound rest gen ((setup, value) :: acc)
  in
  let rec convertAtoms remaining gen acc =
    match remaining with
    | [] -> Ok (List.rev acc, gen)
    | value :: rest ->
        let* value, bindings, gen = atom value gen in
        convertAtoms rest gen ((value, bindings) :: acc)
  in
  let rec captures remaining gen acc =
    match remaining with
    | [] -> Ok (List.rev acc, gen)
    | C.FuncRef id :: rest -> captures rest gen ((A.FuncRef id, []) :: acc)
    | value :: rest ->
        let* value, bindings, gen = atom value gen in
        captures rest gen ((value, bindings) :: acc)
  in
  match expr with
  | C.RecursiveLet _ ->
      Error "RecursiveLet must be lowered during lambda lifting"
  | C.DictLiteral (_, _, []) -> Ok (A.Return (int 0L), gen)
  | C.DictLiteral _ ->
      Error
        "Non-empty DictLiteral must be lowered during generic specialization"
  | C.BoundaryRender (renderer, value) ->
      let* value, gen = anf value gen in
      let id, gen = A.freshVar gen in
      Ok
        ( K.bindReturns value (fun value ->
              A.Let (id, A.Call (renderer, [ value ]), A.Return (A.Var id))),
          gen )
  | C.RuntimeError message ->
      let id, gen = A.freshVar gen in
      Ok (A.Let (id, A.RuntimeError message, A.Return A.UnitLiteral), gen)
  | C.UnitLiteral -> Ok (A.Return A.UnitLiteral, gen)
  | C.Int64Literal n -> Ok (A.Return (int n), gen)
  | C.Int128Literal n -> Ok (finish gen (P.int128Construction functionId n))
  | C.BigIntLiteral n ->
      let smallMin = Z.neg (Z.shift_left Z.one 62) in
      let smallMax = Z.pred (Z.shift_left Z.one 62) in
      let id, gen = A.freshVar gen in
      let construction =
        if Z.compare n smallMin >= 0 && Z.compare n smallMax <= 0 then
          A.TypedAtom
            (int (Z.to_int64 (Z.succ (Z.mul n (Z.of_int 2)))), AST.TInt)
        else
          A.Call
            ( functionId "Darklang.Stdlib.Int.__value",
              [ A.StringLiteral (Z.to_string n) ] )
      in
      Ok (A.Let (id, construction, A.Return (A.Var id)), gen)
  | C.Int8Literal n -> Ok (A.Return (A.IntLiteral (A.Int8 n)), gen)
  | C.Int16Literal n -> Ok (A.Return (A.IntLiteral (A.Int16 n)), gen)
  | C.Int32Literal n -> Ok (A.Return (A.IntLiteral (A.Int32 n)), gen)
  | C.UInt8Literal n -> Ok (A.Return (A.IntLiteral (A.UInt8 n)), gen)
  | C.UInt16Literal n -> Ok (A.Return (A.IntLiteral (A.UInt16 n)), gen)
  | C.UInt32Literal n -> Ok (A.Return (A.IntLiteral (A.UInt32 n)), gen)
  | C.UInt64Literal n -> Ok (A.Return (A.IntLiteral (A.UInt64 n)), gen)
  | C.UInt128Literal n -> Ok (finish gen (P.uint128Construction functionId n))
  | C.BoolLiteral value -> Ok (A.Return (A.BoolLiteral value), gen)
  | C.StringLiteral value | C.CharLiteral value ->
      Ok (A.Return (A.StringLiteral (Text.normalize value)), gen)
  | C.BlobLiteral value -> Ok (A.Return (A.StringLiteral value), gen)
  | C.FloatLiteral value -> Ok (A.Return (A.FloatLiteral value), gen)
  | C.Local name -> (
      match R.BindingMap.find_opt name env with
      | Some (id, _) -> Ok (A.Return (A.Var id), gen)
      | None -> Error "Undefined local binding identity")
  | C.GenericFuncRef _ -> Crash.crash "Unspecialized function value reached ANF lowering"
  | C.FuncRef id -> Ok (finish gen (A.ClosureAlloc (id, [])))
  | C.Closure (id, values) ->
      let* results, gen = captures values gen [] in
      let values = List.map fst results in
      let bindings = List.concat_map snd results in
      let body, gen = finish gen (A.ClosureAlloc (id, values)) in
      Ok (K.wrapBindings bindings body, gen)
  | C.Let (pattern, value, body) -> (
      let* valueType = infer value in
      if not (G.letPatternAcceptsType pattern valueType) then
        let* value, gen = anf value gen in
        let* failure, gen = anf body gen in
        Ok (K.bindReturns value (fun _ -> failure), gen)
      else
        let compileContinuation value bindings gen =
          match pattern with
          | C.LPVariable name ->
              let id, gen = A.freshVar gen in
              let env = R.BindingMap.add name (id, valueType) env in
              let* body, gen = anf ~env body gen in
              Ok (K.wrapBindings bindings (A.Let (id, A.Atom value, body)), gen)
          | C.LPUnit | C.LPWildcard ->
              let* body, gen = anf body gen in
              Ok (K.wrapBindings bindings body, gen)
          | C.LPTuple _ ->
              let root, gen = A.freshVar gen in
              let* env, projections, gen =
                G.lowerLetPatternBindings pattern (A.Var root) valueType env []
                  gen
              in
              let* body, gen = anf ~env body gen in
              Ok
                ( K.wrapBindings bindings
                    (A.Let
                       ( root,
                         A.Atom value,
                         K.wrapBindings (List.rev projections) body )),
                  gen )
        in
        match atom value gen with
        | Ok (value, bindings, gen) -> compileContinuation value bindings gen
        | Error _ ->
            let* value, gen = anf value gen in
            let root, gen = A.freshVar gen in
            let* env, projections, gen =
              G.lowerLetPatternBindings pattern (A.Var root) valueType env []
                gen
            in
            let* body, gen = anf ~env body gen in
            let continuation = K.wrapBindings (List.rev projections) body in
            let rec transform = function
              | A.Return value -> A.Let (root, A.Atom value, continuation)
              | A.Jump _ as value -> value
              | A.Join (parameter, continuation, entry) ->
                  A.Join (parameter, transform continuation, transform entry)
              | A.Let (id, value, rest) -> A.Let (id, value, transform rest)
              | A.If (condition, yes, no) ->
                  A.If (condition, transform yes, transform no)
            in
            Ok (transform value, gen))
  | C.UnaryOp (AST.Neg, inner) -> (
      let* typ = infer inner in
      match (typ, inner) with
      | AST.TFloat64, C.FloatLiteral value ->
          Ok (A.Return (A.FloatLiteral (-.value)), gen)
      | AST.TFloat64, _ ->
          let* setup, value, gen = bound inner gen in
          let body, gen = finish gen (A.FloatNeg value) in
          Ok (K.bindReturns setup (fun _ -> body), gen)
      | AST.TInt64, C.Int64Literal n when n = Int64.min_int ->
          Ok (A.Return (int Int64.min_int), gen)
      | _, _ -> (
          let zero =
            match typ with
            | AST.TInt64 -> Some (C.Int64Literal 0L)
            | AST.TInt -> Some (C.BigIntLiteral Z.zero)
            | AST.TInt128 -> Some (C.Int128Literal Z.zero)
            | AST.TInt32 -> Some (C.Int32Literal 0l)
            | AST.TInt16 -> Some (C.Int16Literal 0)
            | AST.TInt8 -> Some (C.Int8Literal 0)
            | AST.TUInt64 -> Some (C.UInt64Literal 0L)
            | AST.TUInt32 -> Some (C.UInt32Literal 0L)
            | AST.TUInt16 -> Some (C.UInt16Literal 0)
            | AST.TUInt8 -> Some (C.UInt8Literal 0)
            | AST.TUInt128 -> Some (C.UInt128Literal Z.zero)
            | _ -> None
          in
          match zero with
          | Some zero -> anf (C.BinOp (AST.Sub, zero, inner)) gen
          | None ->
              Error
                ("Negation requires numeric operand, got "
                ^ StructuralFormat.semanticType typ)))
  | C.UnaryOp (AST.Not, inner) ->
      let* setup, value, gen = bound inner gen in
      let body, gen = finish gen (A.UnaryPrim (A.Not, value)) in
      Ok (K.bindReturns setup (fun _ -> body), gen)
  | C.UnaryOp (AST.BitNot, inner) ->
      let* typ = infer inner in
      let* setup, value, gen = bound inner gen in
      let id, gen = A.freshVar gen in
      let value =
        match typ with
        | AST.TInt ->
            A.Call (functionId "Darklang.Stdlib.Int.bitwiseNot", [ value ])
        | AST.TInt128 ->
            A.Call (functionId "Darklang.Stdlib.Int128.bitwiseNot", [ value ])
        | AST.TUInt128 ->
            A.Call (functionId "Darklang.Stdlib.UInt128.bitwiseNot", [ value ])
        | _ -> A.UnaryPrim (A.BitNot, value)
      in
      Ok
        ( K.bindReturns setup (fun _ -> A.Let (id, value, A.Return (A.Var id))),
          gen )
  | C.BinOp (AST.StringConcat, left, right) -> (
      let rec collect value acc =
        match value with
        | C.BinOp (AST.StringConcat, left, right) ->
            collect left (collect right acc)
        | value -> value :: acc
      in
      let rec lower remaining gen setups atoms =
        match remaining with
        | [] -> Ok (List.rev setups, List.rev atoms, gen)
        | value :: rest ->
            let* setup, value, gen = bound value gen in
            let value =
              match value with
              | A.UnitLiteral -> A.StringLiteral ""
              | _ -> value
            in
            lower rest gen (setup :: setups) (value :: atoms)
      in
      let* setups, values, gen =
        lower (collect left (collect right [])) gen [] []
      in
      let nonempty =
        List.filter (function A.StringLiteral "" -> false | _ -> true) values
      in
      match nonempty with
      | [] -> Ok (sequence setups (A.Return (A.StringLiteral "")), gen)
      | [ value ] -> Ok (sequence setups (A.Return value), gen)
      | first :: second :: rest ->
          let raw, gen = A.freshVar gen in
          let id, gen = A.freshVar gen in
          let body =
            A.Let
              ( raw,
                A.StringConcat (first, second, rest),
                A.Let
                  ( id,
                    A.Call
                      ( functionId
                          "Darklang.Stdlib.String.__normalizeAfterConcat",
                        [ A.Var raw ] ),
                    A.Return (A.Var id) ) )
          in
          Ok (sequence setups body, gen))
  | C.BinOp (op, left, right) ->
      let* leftSetup, leftAtom, gen = bound left gen in
      let* rightSetup, rightAtom, gen = bound right gen in
      let equality expression gen =
        let id, gen = A.freshVar gen in
        if op = AST.Neq then
          let neg, gen = A.freshVar gen in
          ( A.Let
              ( id,
                expression,
                A.Let (neg, A.UnaryPrim (A.Not, A.Var id), A.Return (A.Var neg))
              ),
            gen )
        else (A.Let (id, expression, A.Return (A.Var id)), gen)
      in
      let* body, gen =
        match op with
        | AST.Eq | AST.Neq -> (
            match infer left with
            | Ok typ when O.isCompoundType typ ->
                let bindings, value, gen =
                  O.generateStructuralEquality functionId leftAtom rightAtom typ
                    gen registry variants sums.P.cases
                in
                let value, bindings, gen =
                  if op = AST.Neq then
                    let id, gen = A.freshVar gen in
                    ( A.Var id,
                      bindings @ [ (id, A.UnaryPrim (A.Not, value)) ],
                      gen )
                  else (value, bindings, gen)
                in
                Ok (K.wrapBindings bindings (A.Return value), gen)
            | Ok AST.TInt ->
                Ok
                  (equality
                     (A.Call
                        ( functionId "Darklang.Stdlib.Int.__equals",
                          [ leftAtom; rightAtom ] ))
                     gen)
            | Ok typ when Option.is_some (P.canonicalBufferKindForType typ) ->
                let kind =
                  match P.canonicalBufferKindForType typ with
                  | Some kind -> kind
                  | None ->
                      Crash.crash
                        ("Expected canonical buffer type, got "
                        ^ StructuralFormat.semanticType typ)
                in
                Ok
                  (equality
                     (A.CanonicalBufferEq (kind, leftAtom, rightAtom))
                     gen)
            | Ok AST.TInt128 ->
                Ok
                  (equality
                     (A.Call
                        ( functionId "Darklang.Stdlib.Int128.__equals",
                          [ leftAtom; rightAtom ] ))
                     gen)
            | Ok AST.TUInt128 ->
                Ok
                  (equality
                     (A.Call
                        ( functionId "Darklang.Stdlib.UInt128.__equals",
                          [ leftAtom; rightAtom ] ))
                     gen)
            | _ ->
                Ok
                  (finish gen (A.Prim (O.convertBinOp op, leftAtom, rightAtom)))
            )
        | AST.StringConcat ->
            Crash.crash "StringConcat must be lowered as a fused tree"
        | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod | AST.Pow | AST.Shl
        | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor | AST.Lt | AST.Gt
        | AST.Lte | AST.Gte | AST.And | AST.Or ->
            let id, gen = A.freshVar gen in
            let value =
              match infer left with
              | Ok typ -> (
                  match O.integerFunctionForBinOp functionId typ op with
                  | Some id -> A.Call (id, [ leftAtom; rightAtom ])
                  | None -> A.Prim (O.convertBinOp op, leftAtom, rightAtom))
              | Error _ -> A.Prim (O.convertBinOp op, leftAtom, rightAtom)
            in
            Ok (A.Let (id, value, A.Return (A.Var id)), gen)
      in
      Ok
        ( K.bindReturns leftSetup (fun _ ->
              K.bindReturns rightSetup (fun _ -> body)),
          gen )
  | C.If (condition, yes, no) ->
      let* setup, condition, gen = bound condition gen in
      let* yes, gen = anf yes gen in
      let* no, gen = anf no gen in
      Ok (K.bindReturns setup (fun _ -> A.If (condition, yes, no)), gen)
  | C.Sequence (first, next) ->
      let* first, gen = anf first gen in
      let* next, gen = anf next gen in
      Ok (K.bindReturns first (fun _ -> next), gen)
  | C.Call (callee, args) when functionNameIs callee "Builtin.unwrap" -> (
      match NonEmptyList.toList args with
      | [ argument ] -> (
          let* typ = infer argument in
          let lookup expected variant =
            match M.find_opt variant variants with
            | Some (owner, _, tag, fields) when owner = expected ->
                Ok (tag, fields)
            | Some (owner, _, _, _) ->
                Error
                  ("Builtin.unwrap expected variant " ^ variant ^ " in "
                 ^ expected ^ ", got " ^ owner)
            | None ->
                Error
                  ("Builtin.unwrap could not find variant tag for " ^ expected
                 ^ "." ^ variant)
          in
          let payload expected variant expression =
            match (M.find_opt variant variants, expression) with
            | Some (owner, _, tag, _), C.Constructor (reference, [ payload ])
              when owner = expected
                   && C.TypeIdMap.find_opt reference.C.typeId
                        typeNames.C.typeNames
                      = Some expected
                   && R.tryFindConstructorTag reference.C.constructorId
                        typeNames
                      = Some tag ->
                Some payload
            | _ -> None
          in
          let build successTag payloadType unboxed absent failure =
            let* value, bindings, gen = atom argument gen in
            let value, bindings, gen =
              match value with
              | A.Var _ -> (value, bindings, gen)
              | _ ->
                  let id, gen = A.freshVar gen in
                  (A.Var id, bindings @ [ (id, A.Atom value) ], gen)
            in
            let tag, gen = A.freshVar gen in
            let success, gen = A.freshVar gen in
            let tagBindings =
              if unboxed then [ (success, A.Prim (A.Neq, value, int absent)) ]
              else
                [
                  (tag, A.TupleGet (value, 0));
                  ( success,
                    A.Prim (A.Eq, A.Var tag, int (Int64.of_int successTag)) );
                ]
            in
            let payloadType =
              if SpecializationIdentity.containsTypeVar payloadType then
                AST.TUnit
              else payloadType
            in
            let raw, gen = A.freshVar gen in
            let typed, gen = A.freshVar gen in
            let yes =
              A.Let
                ( raw,
                  (if unboxed then A.Atom value else A.TupleGet (value, 1)),
                  A.Let
                    ( typed,
                      A.TypedAtom (A.Var raw, payloadType),
                      A.Return (A.Var typed) ) )
            in
            let print, gen = A.freshVar gen in
            let no =
              A.Let (print, A.RuntimeError failure, A.Return A.UnitLiteral)
            in
            Ok
              ( K.wrapBindings (bindings @ tagBindings)
                  (A.If (A.Var success, yes, no)),
                gen )
          in
          let optionName = "Darklang.Stdlib.Option.Option" in
          let resultName = "Darklang.Stdlib.Result.Result" in
          let errorMessage () =
            match payload resultName "Error" argument with
            | Some expression -> (
                match P.unwrapErrorPayloadToString expression with
                | Some text -> "Cannot unwrap Error: " ^ text
                | None -> "Cannot unwrap Error")
            | None -> "Cannot unwrap Error"
          in
          let inferredPayload expected variant fields =
            match payload expected variant argument with
            | Some value -> infer value
            | None -> Ok (match fields with [ typ ] -> typ | _ -> AST.TUnit)
          in
          match typ with
          | AST.TSum (name, [ value ]) when name = optionName ->
              let* tag, _ = lookup optionName "Some" in
              let nullable =
                Option.is_some
                  (P.nullablePointerSumPayloadType optionName [ value ]
                     sums.P.cases)
              in
              let spare =
                P.spareImmediateSumSentinel optionName [ value ] sums.P.cases
              in
              build tag value
                (nullable || Option.is_some spare)
                (Option.value spare ~default:0L)
                "Cannot unwrap None"
          | AST.TSum (name, []) when name = optionName ->
              let* tag, fields = lookup optionName "Some" in
              let* typ = inferredPayload optionName "Some" fields in
              build tag typ false 0L "Cannot unwrap None"
          | AST.TSum (name, [ ok; _ ]) when name = resultName ->
              let* tag, _ = lookup resultName "Ok" in
              let message = errorMessage () in
              build tag ok false 0L message
          | AST.TSum (name, []) when name = resultName ->
              let* tag, fields = lookup resultName "Ok" in
              let payloadTypeResult = inferredPayload resultName "Ok" fields in
              let message = errorMessage () in
              let* typ = payloadTypeResult in
              build tag typ false 0L message
          | _ ->
              Error
                ("Internal error: Builtin.unwrap should have been typechecked \
                  as Option/Result, got " ^ P.typeToString typ))
      | values ->
          Error
            ("Internal error: Builtin.unwrap should have exactly 1 argument, \
              got "
            ^ string_of_int (List.length values)))
  | C.Call (callee, args)
    when functionNameIs callee "Builtin.testRuntimeError"
         || functionNameIs callee "Builtin.crash" -> (
      match NonEmptyList.toList args with
      | [ message ] -> (
          match P.unwrapErrorPayloadToString message with
          | Some text ->
              let id, gen = A.freshVar gen in
              Ok
                ( A.Let
                    ( id,
                      A.RuntimeError ("Uncaught exception: " ^ text),
                      A.Return A.UnitLiteral ),
                  gen )
          | None ->
              let* value, bindings, gen = atom message gen in
              let full, gen = A.freshVar gen in
              let id, gen = A.freshVar gen in
              let body =
                A.Let
                  ( full,
                    A.StringConcat
                      (A.StringLiteral "Uncaught exception: ", value, []),
                    A.Let
                      ( id,
                        A.RuntimeErrorString (A.Var full),
                        A.Return A.UnitLiteral ) )
              in
              Ok (K.wrapBindings bindings body, gen))
      | values ->
          Error
            ("Internal error: "
            ^ displayId AST.DiagnosticFormatting.func callee
            ^ " should have exactly 1 argument, got "
            ^ string_of_int (List.length values)))
  | C.Call (callee, args) -> (
      let sourceArgs = NonEmptyList.toList args in
      let name =
        match functionName callee with
        | Some name -> name
        | None ->
            Crash.crash
              ("Expression lowering lost function name metadata for FunctionId "
              ^ Z.to_string
                  (let value = AST.functionIdValue callee in
                   if value < 0L then
                     Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)
                   else Z.of_int64 value))
      in
      let rec convert remaining gen setups values =
        match remaining with
        | [] -> Ok (List.rev setups, List.rev values, gen)
        | value :: rest ->
            let* setup, value, gen = bound value gen in
            let setup, value, gen =
              match value with
              | A.FuncRef callee ->
                  let closure, gen = A.freshVar gen in
                  ( K.bindReturns setup (fun _ ->
                        A.Let
                          ( closure,
                            A.ClosureAlloc (callee, []),
                            A.Return (A.Var closure) )),
                    A.Var closure,
                    gen )
              | _ -> (setup, value, gen)
            in
            convert rest gen (setup :: setups) (value :: values)
      in
      let* setups, values, gen = convert sourceArgs gen [] [] in
      let id, gen = A.freshVar gen in
      let finish value =
        Ok (sequence setups (A.Let (id, value, A.Return (A.Var id))), gen)
      in
      match P.tryPresentationIntrinsic name values with
      | Some value -> finish value
      | None -> (
          match
            P.tryCliIntrinsic name (P.normalizeNullaryIntrinsicArgs values)
          with
          | Some value -> finish value
          | None -> (
              match P.tryFileIntrinsic name values with
              | Some value -> finish value
              | None -> (
                  match
                    P.tryRawMemoryIntrinsic functionId sums.P.names name values
                  with
                  | Some value -> finish value
                  | None -> (
                      match P.tryCanonicalPrimitiveIntrinsic name values with
                      | Some value -> finish value
                      | None -> (
                          match P.tryFloatIntrinsic name values with
                          | Some value -> finish value
                          | None -> (
                              match P.tryRandomIntrinsic name values with
                              | Some value -> finish value
                              | None -> (
                                  match P.tryDateTimeIntrinsic name values with
                                  | Some value -> finish value
                                  | None -> (
                                      match
                                        FunctionIdMap.tryFind callee functions
                                      with
                                      | Some (_, AST.TFunction (params, _)) ->
                                          finish
                                            (A.Call
                                               ( callee,
                                                 SpecializationIdentity
                                                 .normalizeSyntheticNullaryArgAtoms
                                                   params sourceArgs values ))
                                      | Some _ | None ->
                                          finish (A.Call (callee, values))))))))
              )))
  | C.TypeApp _ -> Error "Generic function calls not yet implemented"
  | C.TupleLiteral values ->
      let* results, gen = convertBound (C.tupleElementsToList values) gen [] in
      let body, gen = finish gen (A.TupleAlloc (List.map snd results)) in
      Ok (sequence (List.map fst results) body, gen)
  | C.TupleAccess (value, index) ->
      let* setup, value, gen = bound value gen in
      let body, gen = finish gen (A.TupleGet (value, index)) in
      Ok (K.bindReturns setup (fun _ -> body), gen)
  | C.RecordLiteral (reference, fields) ->
      let name =
        match P.tryFindRecordTypeNameById reference.C.typeId typeNames with
        | Some name -> name
        | None ->
            Crash.crash
              "Resolved record type identity is absent from the lowering \
               registry"
      in
      let info =
        match M.find_opt name registry with
        | Some info -> info
        | None -> Crash.crash ("Record type '" ^ name ^ "' not found in typeReg")
      in
      let rec convert remaining gen acc =
        match remaining with
        | [] -> Ok (List.rev acc, gen)
        | (id, value) :: rest ->
            let* setup, value, gen = bound value gen in
            convert rest gen ((id, setup, value) :: acc)
      in
      let* results, gen = convert (C.recordFieldsInSourceOrder fields) gen [] in
      let values =
        List.stable_sort
          (fun (left, _, _) (right, _, _) ->
            Int.compare (fieldIndex left) (fieldIndex right))
          results
        |> List.map (fun (_, _, value) -> value)
      in
      let body, gen =
        finish gen
          (A.RecordAlloc
             ( T.recordDescriptor name
                 (C.semanticTypeArgs reference.C.typeArgs)
                 info,
               values ))
      in
      Ok (sequence (List.map (fun (_, setup, _) -> setup) results) body, gen)
  | C.RecordUpdate (record, updates) -> (
      let* typ = infer record in
      match typ with
      | AST.TRecord (name, args) -> (
          match M.find_opt name registry with
          | None -> Error ("Unknown record type: " ^ name)
          | Some info ->
              let* setup, record, gen = bound record gen in
              let rec convert remaining gen acc =
                match remaining with
                | [] -> Ok (List.rev acc, gen)
                | (id, value) :: rest ->
                    let* setup, value, gen = bound value gen in
                    convert rest gen ((id, setup, value) :: acc)
              in
              let* results, gen = convert updates gen [] in
              let updated =
                List.fold_left
                  (fun map (id, _, value) ->
                    Indices.add (fieldIndex id) value map)
                  Indices.empty results
              in
              let values, projections, gen =
                List.mapi (fun index (name, _) -> (index, name)) info.R.fields
                |> List.fold_left
                     (fun (values, projections, gen) (index, _) ->
                       match Indices.find_opt index updated with
                       | Some value -> (value :: values, projections, gen)
                       | None ->
                           let id, gen = A.freshVar gen in
                           ( A.Var id :: values,
                             ( id,
                               A.RecordGet
                                 ( T.recordDescriptor name args info,
                                   record,
                                   index ) )
                             :: projections,
                             gen ))
                     ([], [], gen)
              in
              let body, gen =
                finish gen
                  (A.RecordClone
                     (T.recordDescriptor name args info, record, List.rev values))
              in
              let body = K.wrapBindings (List.rev projections) body in
              let body =
                sequence (List.map (fun (_, setup, _) -> setup) results) body
              in
              Ok (K.bindReturns setup (fun _ -> body), gen))
      | _ -> Error "Cannot use record update syntax on non-record type")
  | C.RecordAccess (record, field) -> (
      let* typ = infer record in
      match typ with
      | AST.TRecord (name, args) -> (
          match M.find_opt name registry with
          | None -> Error ("Unknown record type: " ^ name)
          | Some info ->
              let index = fieldIndex field in
              if index < 0 || index >= List.length info.R.fields then
                Error
                  ("Record type '" ^ name ^ "' has no field '"
                  ^ displayId AST.DiagnosticFormatting.field field
                  ^ "'")
              else
                let* setup, record, gen = bound record gen in
                let body, gen =
                  finish gen
                    (A.RecordGet
                       ( T.recordDescriptor name args info,
                         record,
                         fieldIndex field ))
                in
                Ok (K.bindReturns setup (fun _ -> body), gen))
      | _ ->
          Error
            ("Cannot access field '"
            ^ displayId AST.DiagnosticFormatting.field field
            ^ "' on non-record type"))
  | C.Constructor (reference, fields) -> (
      match P.tryFindSumTypeNameById reference.C.typeId typeNames with
      | None ->
          Error
            "Resolved constructor type identity is absent from the lowering \
             registry"
      | Some owner -> (
          let tag = constructorTag reference.C.constructorId in
          match
            P.tryFindVariantByConstructorId reference.C.typeId owner
              reference.C.constructorId variants
          with
          | None -> Error ("Unknown constructor tag: " ^ string_of_int tag)
          | Some (name, params, tag, variantFields) -> (
              let args = C.semanticTypeArgs reference.C.typeArgs in
              let nullable =
                Option.is_some
                  (P.nullablePointerSumPayloadType name args sums.P.cases)
              in
              let spare = P.spareImmediateSumSentinel name args sums.P.cases in
              let hasPayload =
                M.exists
                  (fun _ (owner, _, _, fields) -> owner = name && fields <> [])
                  variants
              in
              let descriptor () =
                let* typ = infer expr in
                match typ with
                | AST.TSum (owner, args) when owner = name ->
                    T.boxedSumDescriptor name params args variantFields
                | typ ->
                    Error
                      ("Constructor '" ^ name ^ "' inferred unexpected type '"
                      ^ StructuralFormat.semanticType typ
                      ^ "'")
              in
              match fields with
              | [ field ]
                when Option.is_some
                       (P.transparentSumPayloadType name args sums.P.cases)
                     || nullable || Option.is_some spare ->
                  let* setup, value, gen = bound field gen in
                  let body, gen =
                    finish gen (A.TypedAtom (value, AST.TSum (name, args)))
                  in
                  Ok (K.bindReturns setup (fun _ -> body), gen)
              | [] when nullable || Option.is_some spare ->
                  Ok
                    (finish gen
                       (A.TypedAtom
                          ( int (Option.value spare ~default:0L),
                            AST.TSum (name, args) )))
              | [] when not hasPayload ->
                  Ok (A.Return (int (Int64.of_int tag)), gen)
              | [] ->
                  let* descriptor = descriptor () in
                  Ok
                    (finish gen
                       (A.RecordAlloc
                          (descriptor, [ int (Int64.of_int tag); int 0L ])))
              | _ ->
                  let payload =
                    match fields with
                    | [ field ] -> field
                    | _ -> C.TupleLiteral (C.tupleElementsOfList fields)
                  in
                  let* descriptor = descriptor () in
                  let* setup, value, gen = bound payload gen in
                  let body, gen =
                    finish gen
                      (A.RecordAlloc
                         (descriptor, [ int (Int64.of_int tag); value ]))
                  in
                  Ok (K.bindReturns setup (fun _ -> body), gen))))
  | C.ListLiteral [] -> Ok (A.Return (int 0L), gen)
  | C.ListLiteral values ->
      let rec convert remaining gen acc =
        match remaining with
        | [] -> Ok (List.rev acc, gen)
        | value :: rest ->
            let* typ = infer value in
            let* setup, value, gen = bound value gen in
            convert rest gen ((setup, value, typ) :: acc)
      in
      let* results, gen = convert values gen [] in
      let atoms = List.map (fun (_, value, typ) -> (value, typ)) results in
      let typ =
        match atoms with
        | (_, typ) :: _ -> AST.TList typ
        | [] -> AST.TList (AST.TVar "a")
      in
      let value, bindings, gen = G.buildSkewListLiteral typ atoms gen [] in
      let body, gen = finish gen (A.TypedAtom (value, typ)) in
      let allocation = K.wrapBindings bindings body in
      Ok
        ( sequence (List.map (fun (setup, _, _) -> setup) results) allocation,
          gen )
  | C.Match (value, cases) ->
      PatternLowering.lowerMatch toANFCore toAtomCore toANFBoundAtomCore
        functionIds sums typeNames inert value
        (NonEmptyList.toList cases)
        gen env registry variants functions names modules
  | C.InterpolatedString parts -> (
      let part = function
        | C.StringText text -> C.StringLiteral text
        | C.StringExpr value -> value
      in
      match parts with
      | [] -> Ok (A.Return (A.StringLiteral ""), gen)
      | [ value ] -> anf (part value) gen
      | first :: rest ->
          anf
            (List.fold_left
               (fun acc value -> C.BinOp (AST.StringConcat, acc, part value))
               (part first) rest)
            gen)
  | C.Lambda _ ->
      Error "Lambda expressions (closures) are not yet fully implemented"
  | C.IndirectApply (callee, args) ->
      let* callee, bindings, gen = atom callee gen in
      let* results, gen = convertAtoms (NonEmptyList.toList args) gen [] in
      let values = List.map fst results in
      let bindings = bindings @ List.concat_map snd results in
      let body, gen = finish gen (A.IndirectCall (callee, values)) in
      Ok (K.wrapBindings bindings body, gen)
  | C.Apply (callee, args) -> (
      let argsList = NonEmptyList.toList args in
      let lets body parameters values =
        let rec build parameters values =
          match (parameters, values) with
          | [], [] -> body
          | (parameter : C.lambdaParameter) :: ps, value :: vs ->
              C.Let (parameter.C.pattern, value, build ps vs)
          | _ -> body
        in
        build parameters values
      in
      match callee with
      | C.Lambda (parameters, _, body) ->
          let parameters = NonEmptyList.toList parameters in
          if List.length argsList <> List.length parameters then
            Error
              ("Expected "
              ^ string_of_int (List.length parameters)
              ^ " arguments, got "
              ^ string_of_int (List.length argsList))
          else anf (lets body parameters argsList) gen
      | C.Local name -> (
          match R.BindingMap.find_opt name env with
          | None ->
              Error
                ("Cannot apply variable '"
                ^ displayId AST.DiagnosticFormatting.binding name
                ^ "' as function - variable not in scope")
          | Some (id, _) ->
              let* results, gen = convertAtoms argsList gen [] in
              let values = List.map fst results in
              let bindings = List.concat_map snd results in
              let body, gen = finish gen (A.ClosureCall (A.Var id, values)) in
              Ok (K.wrapBindings bindings body, gen))
      | C.Apply _ -> (
          let rec flatten value args =
            match value with
            | C.Apply (callee, values) ->
                flatten callee (NonEmptyList.toList values :: args)
            | value -> (value, args)
          in
          let base, argLists = flatten callee [ argsList ] in
          match base with
          | C.Lambda _ ->
              let rec desugar value remaining =
                match remaining with
                | [] -> value
                | values :: rest -> (
                    match value with
                    | C.Lambda (parameters, _, body) ->
                        let parameters = NonEmptyList.toList parameters in
                        if List.length values <> List.length parameters then
                          desugar
                            (C.Apply (value, NonEmptyList.fromList values))
                            rest
                        else desugar (lets body parameters values) rest
                    | C.Let (name, binding, body) ->
                        C.Let (name, binding, desugar body (values :: rest))
                    | _ ->
                        desugar
                          (C.Apply (value, NonEmptyList.fromList values))
                          rest)
              in
              anf (desugar base argLists) gen
          | _ ->
              let full =
                List.fold_left
                  (fun value args ->
                    C.Apply (value, NonEmptyList.fromList args))
                  base argLists
              in
              let* value, bindings, gen = atom full gen in
              Ok (K.wrapBindings bindings (A.Return value), gen))
      | C.Let (name, value, body) ->
          anf (C.Let (name, value, C.Apply (body, args))) gen
      | C.Closure (callee, values) ->
          let* results, gen = captures values gen [] in
          let values = List.map fst results in
          let bindings = List.concat_map snd results in
          let closure, gen = A.freshVar gen in
          let alloc = A.ClosureAlloc (callee, values) in
          let* results, gen = convertBound argsList gen [] in
          let body, gen =
            finish gen (A.ClosureCall (A.Var closure, List.map snd results))
          in
          let body = sequence (List.map fst results) body in
          Ok (K.wrapBindings bindings (A.Let (closure, alloc, body)), gen)
      | _ ->
          let* callee, bindings, gen = atom callee gen in
          let* results, gen = convertAtoms argsList gen [] in
          let values = List.map fst results in
          let bindings = bindings @ List.concat_map snd results in
          let body, gen = finish gen (A.ClosureCall (callee, values)) in
          Ok (K.wrapBindings bindings body, gen))
