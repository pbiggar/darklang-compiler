(* AtomLowering.ml - Lower atom-producing expressions and their ordered binding prefixes. *)
[@@@warning "-4"]

module A = ANF
module C = CheckedAST
module R = TypeRegistries
module P = LoweringPrimitives
module S = SpecializationIdentity
module T = TypeSubstitution
module O = LoweringOperators
module I = LoweringTypeInference
module G = LoweringAggregates
module M = StringOrder.Map
module Indices = Map.Make (Int)

let ( let* ) = Result.bind
let int value = A.IntLiteral (A.Int64 value)
let displayId describe id = StructuralFormat.format (describe id)

let add left right =
  Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))

(* Retain the source's local tree builders, superseded at its final call by
   buildSkewListLiteral. They do not participate in the current list path. *)
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
   Char literal uses same representation as string
   Explicit function reference - wrap in closure for uniform calling convention
   Closure in atom position: convert captures and create ClosureAlloc binding
   Create binding for ClosureAlloc
   Let binding in atom position: need to evaluate and return the body as an atom
   Infer the type of the value for type-directed field lookup
   Unary negation: use operand type to select float vs integer path
   Constant-fold negative float literals at compile time
   The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
   When negated, it should remain INT64_MIN (mathematically correct)
   Boolean not: convert operand to atom, create binding
   Create the operation
   Return the temp variable as atom, plus all bindings
   Complex expression: convert operands to atoms, create binding
   Check if this is an equality comparison on compound types
   Generate structural equality
   For Neq, negate the result
   Primitive type - simple comparison
   Arithmetic, bitwise, and comparison operators - use simple primitive
   IfValue selects atoms, but any bindings execute before it. Branches
   with bindings must use full control-flow lowering to remain lazy.
   Create a temporary for the result
   Create an IfValue CExpr
   Return temp as atom with all bindings
   Function call in atom position: convert all arguments to atoms
   Create a temporary for the call result
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
   Assume it's a defined function (direct call)
   Placeholder: Generic instantiation not yet implemented
   Convert all elements to atoms
   Create a temporary for the tuple
   Convert tuple to atom and create TupleGet
   Evaluate field expressions in source order, independently of the
   declaration-order tuple layout used for the record value.
   Evaluate the record once, then updates in source order, before
   projecting untouched fields and allocating the layout tuple.
   Projection is type-directed so the nominal descriptor and keyed slot
   always agree, including after aliases and generic substitution.
   Look up field index in the specific record type
   Check if ANY variant in this type has a payload
   Note: We get typeName from variantLookup, not from AST (which may be empty)
   Pure enum type: return tag as an integer (no bindings needed)
   No payload but type has other variants with payloads
   Heap-allocate as [tag, 0] for uniform 2-element structure
   This enables consistent structural equality comparison
   Variant with payload: allocate [tag, payload] on heap
   Compile list literal as SkewList in atom position
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
   Convert all elements to atoms first
   Flatten all element bindings
   Create LEAF nodes for all elements. Each leaf is independent,
   so collect per-leaf bindings separately to avoid repeatedly
   appending to the growing element-binding prefix for large lists.
   Desugar interpolated string to StringConcat chain
   Empty interpolated string → empty string
   Single part → convert directly
   Multiple parts → desugar to StringConcat and convert
   Match in atom position - compile and extract result
   The match compiles to an if-else chain that returns a value
   We need to extract that value into a temp variable
   For now, just return an error - complex match in atom position needs more work
   Lambda in atom position - closures not yet fully implemented
   Apply in atom position - convert via toANF and extract result
   Immediate application: desugar to let bindings
   Nested application in atom position: (fun x -> fun y -> ...)(a)(b)
   Inner is complex - evaluate inner, then call as closure
   Apply(let x = v in body, args) in atom position
   Float the let out and recurse
   Variable call in atom position - treat as closure call
   Closure call in atom position
   General function-expression application in atom position.
*)
let lowerAtom (toANFCore : LoweringCallbacks.expressionLowerer)
    (toAtomCore : LoweringCallbacks.atomLowerer)
    (_toANFBoundAtomCore : LoweringCallbacks.boundAtomLowerer) functionIds sums
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
          ("Atom lowering function '" ^ name ^ "' is absent from registries")
  in
  let functionName id =
    match FunctionIdMap.tryFind id names with
    | Some name -> Some name
    | None -> Option.map fst (FunctionIdMap.tryFind id functions)
  in
  let functionNameIs id expected = functionName id = Some expected in
  let atom ?(env = env) expr gen =
    toAtomCore sums typeNames inert expr gen env registry variants functions
      names modules
  in
  let infer expr =
    I.inferTypeCore sums typeNames expr (R.typeEnvFromVarEnv env) registry
      variants functions names modules
  in
  let bind result bindings gen expression =
    let id, gen = A.freshVar gen in
    Ok (result id, bindings @ [ (id, expression) ], gen)
  in
  let rec convert remaining gen acc =
    match remaining with
    | [] -> Ok (List.rev acc, gen)
    | expr :: rest ->
        let* value, bindings, gen = atom expr gen in
        convert rest gen ((value, bindings) :: acc)
  in
  let rec captures remaining gen acc =
    match remaining with
    | [] -> Ok (List.rev acc, gen)
    | C.FuncRef id :: rest -> captures rest gen ((A.FuncRef id, []) :: acc)
    | expr :: rest ->
        let* value, bindings, gen = atom expr gen in
        captures rest gen ((value, bindings) :: acc)
  in
  let parts results = (List.map fst results, List.concat_map snd results) in
  let callClosure funcAtoms bindings args gen constructor =
    let* results, gen = convert args gen [] in
    let args, argBindings = parts results in
    bind
      (fun id -> A.Var id)
      (bindings @ argBindings) gen
      (constructor funcAtoms args)
  in
  let equalityResult leftBindings rightBindings gen expression =
    let id, gen = A.freshVar gen in
    if expr |> function C.BinOp (AST.Neq, _, _) -> true | _ -> false then
      let neg, gen = A.freshVar gen in
      Ok
        ( A.Var neg,
          leftBindings @ rightBindings
          @ [ (id, expression); (neg, A.UnaryPrim (A.Not, A.Var id)) ],
          gen )
    else Ok (A.Var id, leftBindings @ rightBindings @ [ (id, expression) ], gen)
  in
  match expr with
  | C.RecursiveLet _ ->
      Error "RecursiveLet must be lowered during lambda lifting"
  | C.DictLiteral (_, _, []) -> Ok (int 0L, [], gen)
  | C.DictLiteral _ ->
      Error
        "Non-empty DictLiteral must be lowered during generic specialization"
  | C.BoundaryRender _ -> Error "BoundaryRender must be lowered through toANF"
  | C.RuntimeError _ ->
      Error "Compiler-generated RuntimeError must be lowered through toANF"
  | C.UnitLiteral -> Ok (A.UnitLiteral, [], gen)
  | C.Int64Literal n -> Ok (A.IntLiteral (A.Int64 n), [], gen)
  | C.Int128Literal n ->
      let id, gen = A.freshVar gen in
      Ok (A.Var id, [ (id, P.int128Construction functionId n) ], gen)
  | C.UInt128Literal n ->
      let id, gen = A.freshVar gen in
      Ok (A.Var id, [ (id, P.uint128Construction functionId n) ], gen)
  | C.BigIntLiteral n ->
      let min = Z.neg (Z.shift_left Z.one 62) in
      let max = Z.pred (Z.shift_left Z.one 62) in
      let id, gen = A.freshVar gen in
      let construction =
        if Z.geq n min && Z.leq n max then
          A.TypedAtom
            (int (Z.to_int64 (Z.succ (Z.mul n (Z.of_int 2)))), AST.TInt)
        else
          A.Call
            ( functionId "Darklang.Stdlib.Int.__value",
              [ A.StringLiteral (Z.to_string n) ] )
      in
      Ok (A.Var id, [ (id, construction) ], gen)
  | C.Int8Literal n -> Ok (A.IntLiteral (A.Int8 n), [], gen)
  | C.Int16Literal n -> Ok (A.IntLiteral (A.Int16 n), [], gen)
  | C.Int32Literal n -> Ok (A.IntLiteral (A.Int32 n), [], gen)
  | C.UInt8Literal n -> Ok (A.IntLiteral (A.UInt8 n), [], gen)
  | C.UInt16Literal n -> Ok (A.IntLiteral (A.UInt16 n), [], gen)
  | C.UInt32Literal n -> Ok (A.IntLiteral (A.UInt32 n), [], gen)
  | C.UInt64Literal n -> Ok (A.IntLiteral (A.UInt64 n), [], gen)
  | C.BoolLiteral value -> Ok (A.BoolLiteral value, [], gen)
  | C.StringLiteral value | C.CharLiteral value ->
      Ok (A.StringLiteral (Text.normalize value), [], gen)
  | C.BlobLiteral value -> Ok (A.StringLiteral value, [], gen)
  | C.FloatLiteral value -> Ok (A.FloatLiteral value, [], gen)
  | C.Local id -> (
      match R.BindingMap.find_opt id env with
      | Some (id, _) -> Ok (A.Var id, [], gen)
      | None -> Error "Undefined local binding identity")
  | C.GenericFuncRef _ ->
      Crash.crash "Unspecialized function value reached ANF lowering"
  | C.FuncRef id -> bind (fun id -> A.Var id) [] gen (A.ClosureAlloc (id, []))
  | C.Closure (id, values) ->
      let* results, gen = captures values gen [] in
      let values, bindings = parts results in
      bind (fun id -> A.Var id) bindings gen (A.ClosureAlloc (id, values))
  | C.Let (pattern, value, body) -> (
      let* typ = infer value in
      if not (G.letPatternAcceptsType pattern typ) then
        Error "Binding mismatch requires control-flow lowering"
      else
        let* value, prefix, gen = atom value gen in
        match pattern with
        | C.LPVariable name ->
            let id, gen = A.freshVar gen in
            let env = R.BindingMap.add name (id, typ) env in
            let* body, bindings, gen = atom ~env body gen in
            Ok (body, prefix @ [ (id, A.Atom value) ] @ bindings, gen)
        | C.LPUnit | C.LPWildcard ->
            let* body, bindings, gen = atom body gen in
            Ok (body, prefix @ bindings, gen)
        | C.LPTuple _ ->
            let root, gen = A.freshVar gen in
            let* env, reversed, gen =
              G.lowerLetPatternBindings pattern (A.Var root) typ env [] gen
            in
            let* body, bindings, gen = atom ~env body gen in
            Ok
              ( body,
                prefix @ [ (root, A.Atom value) ] @ List.rev reversed @ bindings,
                gen ))
  | C.UnaryOp (AST.Neg, inner) -> (
      let* typ = infer inner in
      match typ with
      | AST.TFloat64 -> (
          match inner with
          | C.FloatLiteral value -> Ok (A.FloatLiteral (-.value), [], gen)
          | _ ->
              let* value, bindings, gen = atom inner gen in
              bind (fun id -> A.Var id) bindings gen (A.FloatNeg value))
      | AST.TInt64 -> (
          match inner with
          | C.Int64Literal value when value = Int64.min_int ->
              Ok (int Int64.min_int, [], gen)
          | _ -> atom (C.BinOp (AST.Sub, C.Int64Literal 0L, inner)) gen)
      | AST.TInt -> atom (C.BinOp (AST.Sub, C.BigIntLiteral Z.zero, inner)) gen
      | AST.TInt128 ->
          atom (C.BinOp (AST.Sub, C.Int128Literal Z.zero, inner)) gen
      | AST.TInt32 -> atom (C.BinOp (AST.Sub, C.Int32Literal 0l, inner)) gen
      | AST.TInt16 -> atom (C.BinOp (AST.Sub, C.Int16Literal 0, inner)) gen
      | AST.TInt8 -> atom (C.BinOp (AST.Sub, C.Int8Literal 0, inner)) gen
      | AST.TUInt64 -> atom (C.BinOp (AST.Sub, C.UInt64Literal 0L, inner)) gen
      | AST.TUInt32 -> atom (C.BinOp (AST.Sub, C.UInt32Literal 0L, inner)) gen
      | AST.TUInt16 -> atom (C.BinOp (AST.Sub, C.UInt16Literal 0, inner)) gen
      | AST.TUInt8 -> atom (C.BinOp (AST.Sub, C.UInt8Literal 0, inner)) gen
      | AST.TUInt128 ->
          atom (C.BinOp (AST.Sub, C.UInt128Literal Z.zero, inner)) gen
      | _ ->
          Error
            ("Negation requires numeric operand, got "
            ^ StructuralFormat.semanticType typ))
  | C.UnaryOp (AST.Not, inner) ->
      let* value, bindings, gen = atom inner gen in
      bind (fun id -> A.Var id) bindings gen (A.UnaryPrim (A.Not, value))
  | C.UnaryOp (AST.BitNot, inner) ->
      let* typ = infer inner in
      let* value, bindings, gen = atom inner gen in
      let id, gen = A.freshVar gen in
      let expression =
        match typ with
        | AST.TInt ->
            A.Call (functionId "Darklang.Stdlib.Int.bitwiseNot", [ value ])
        | AST.TInt128 ->
            A.Call (functionId "Darklang.Stdlib.Int128.bitwiseNot", [ value ])
        | AST.TUInt128 ->
            A.Call (functionId "Darklang.Stdlib.UInt128.bitwiseNot", [ value ])
        | _ -> A.UnaryPrim (A.BitNot, value)
      in
      Ok (A.Var id, bindings @ [ (id, expression) ], gen)
  | C.BinOp (AST.StringConcat, left, right) -> (
      let rec collect expr acc =
        match expr with
        | C.BinOp (AST.StringConcat, left, right) ->
            collect left (collect right acc)
        | part -> part :: acc
      in
      let* results, gen = convert (collect left (collect right [])) gen [] in
      let values, bindings = parts results in
      let values =
        List.filter (function A.StringLiteral "" -> false | _ -> true) values
      in
      match values with
      | [] -> Ok (A.StringLiteral "", bindings, gen)
      | [ single ] -> Ok (single, bindings, gen)
      | first :: second :: rest ->
          let raw, gen = A.freshVar gen in
          let result, gen = A.freshVar gen in
          Ok
            ( A.Var result,
              bindings
              @ [
                  (raw, A.StringConcat (first, second, rest));
                  ( result,
                    A.Call
                      ( functionId
                          "Darklang.Stdlib.String.__normalizeAfterConcat",
                        [ A.Var raw ] ) );
                ],
              gen ))
  | C.BinOp (op, left, right) -> (
      let* leftAtom, leftBindings, gen = atom left gen in
      let* rightAtom, rightBindings, gen = atom right gen in
      match op with
      | AST.Eq | AST.Neq -> (
          match infer left with
          | Ok typ when O.isCompoundType typ ->
              let bindings, result, gen =
                O.generateStructuralEquality functionId leftAtom rightAtom typ
                  gen registry variants sums.P.cases
              in
              let result, bindings, gen =
                if op = AST.Neq then
                  let neg, gen = A.freshVar gen in
                  ( A.Var neg,
                    bindings @ [ (neg, A.UnaryPrim (A.Not, result)) ],
                    gen )
                else (result, bindings, gen)
              in
              Ok (result, leftBindings @ rightBindings @ bindings, gen)
          | Ok AST.TInt ->
              equalityResult leftBindings rightBindings gen
                (A.Call
                   ( functionId "Darklang.Stdlib.Int.__equals",
                     [ leftAtom; rightAtom ] ))
          | Ok typ when Option.is_some (P.canonicalBufferKindForType typ) ->
              let kind =
                match P.canonicalBufferKindForType typ with
                | Some kind -> kind
                | None ->
                    Crash.crash
                      ("Expected canonical buffer type, got "
                      ^ StructuralFormat.semanticType typ)
              in
              equalityResult leftBindings rightBindings gen
                (A.CanonicalBufferEq (kind, leftAtom, rightAtom))
          | Ok ((AST.TInt128 | AST.TUInt128) as typ) ->
              equalityResult leftBindings rightBindings gen
                (A.Call
                   ( functionId
                       (if typ = AST.TInt128 then
                          "Darklang.Stdlib.Int128.__equals"
                        else "Darklang.Stdlib.UInt128.__equals"),
                     [ leftAtom; rightAtom ] ))
          | _ ->
              bind
                (fun id -> A.Var id)
                (leftBindings @ rightBindings)
                gen
                (A.Prim (O.convertBinOp op, leftAtom, rightAtom)))
      | AST.StringConcat ->
          Crash.crash "StringConcat must be lowered as a fused tree"
      | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod | AST.Pow | AST.Shl
      | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor | AST.Lt | AST.Gt
      | AST.Lte | AST.Gte | AST.And | AST.Or ->
          let id, gen = A.freshVar gen in
          let expression =
            match infer left with
            | Ok typ -> (
                match O.integerFunctionForBinOp functionId typ op with
                | Some id -> A.Call (id, [ leftAtom; rightAtom ])
                | None -> A.Prim (O.convertBinOp op, leftAtom, rightAtom))
            | _ -> A.Prim (O.convertBinOp op, leftAtom, rightAtom)
          in
          Ok (A.Var id, leftBindings @ rightBindings @ [ (id, expression) ], gen)
      )
  | C.If (cond, yes, no) ->
      let* cond, prefix, gen = atom cond gen in
      let* yes, yesBindings, gen = atom yes gen in
      let* no, noBindings, gen = atom no gen in
      if yesBindings = [] && noBindings = [] then
        bind
          (fun id -> A.Var id)
          (prefix @ yesBindings @ noBindings)
          gen
          (A.IfValue (cond, yes, no))
      else Error "If expression requires lazy branch lowering"
  | C.Sequence _ -> Error "Sequence expression requires ordered lowering"
  | C.Call (id, args) ->
      if functionNameIs id "Builtin.unwrap" then
        Error
          "Internal error: Builtin.unwrap should be lowered via toANF, not \
           toAtom"
      else if functionNameIs id "Builtin.crash" then
        match S.exprArgsToList args with
        | [ message ] -> (
            match P.unwrapErrorPayloadToString message with
            | Some text ->
                let id, gen = A.freshVar gen in
                Ok
                  ( A.UnitLiteral,
                    [ (id, A.RuntimeError ("Uncaught exception: " ^ text)) ],
                    gen )
            | None ->
                let* value, bindings, gen = atom message gen in
                let full, gen = A.freshVar gen in
                let err, gen = A.freshVar gen in
                Ok
                  ( A.UnitLiteral,
                    bindings
                    @ [
                        ( full,
                          A.StringConcat
                            (A.StringLiteral "Uncaught exception: ", value, [])
                        );
                        (err, A.RuntimeErrorString (A.Var full));
                      ],
                    gen ))
        | args ->
            Error
              ("Internal error: "
              ^ displayId AST.DiagnosticFormatting.func id
              ^ " should have exactly 1 argument, got "
              ^ string_of_int (List.length args))
      else
        let argExprs = S.exprArgsToList args in
        let name =
          match functionName id with
          | Some name -> name
          | None -> Crash.crash "Atom lowering lost function name metadata"
        in
        let* results, gen = convert argExprs gen [] in
        let args, bindings = parts results in
        let temp, gen = A.freshVar gen in
        let rec intrinsic factories =
          match factories with
          | [] -> None
          | factory :: rest -> (
              match factory () with
              | Some expr -> Some expr
              | None -> intrinsic rest)
        in
        let expression =
          match
            intrinsic
              [
                (fun () -> P.tryPresentationIntrinsic name args);
                (fun () ->
                  P.tryCliIntrinsic name (P.normalizeNullaryIntrinsicArgs args));
                (fun () -> P.tryFileIntrinsic name args);
                (fun () ->
                  P.tryRawMemoryIntrinsic functionId sums.P.names name args);
                (fun () -> P.tryCanonicalPrimitiveIntrinsic name args);
                (fun () -> P.tryFloatIntrinsic name args);
                (fun () -> P.tryRandomIntrinsic name args);
                (fun () -> P.tryDateTimeIntrinsic name args);
              ]
          with
          | Some expression -> expression
          | None ->
              let args =
                match FunctionIdMap.tryFind id functions with
                | Some (_, AST.TFunction (params, _)) ->
                    S.normalizeSyntheticNullaryArgAtoms params argExprs args
                | _ -> args
              in
              A.Call (id, args)
        in
        Ok (A.Var temp, bindings @ [ (temp, expression) ], gen)
  | C.TypeApp _ ->
      Error "TypeApp (generic instantiation) not yet implemented in toAtom"
  | C.TupleLiteral elements ->
      let* results, gen = convert (C.tupleElementsToList elements) gen [] in
      let args, bindings = parts results in
      bind (fun id -> A.Var id) bindings gen (A.TupleAlloc args)
  | C.TupleAccess (value, index) ->
      let* value, bindings, gen = atom value gen in
      bind (fun id -> A.Var id) bindings gen (A.TupleGet (value, index))
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
      let rec fieldsCore remaining gen acc =
        match remaining with
        | [] -> Ok (List.rev acc, gen)
        | (id, value) :: rest ->
            let* value, bindings, gen = atom value gen in
            fieldsCore rest gen ((id, value, bindings) :: acc)
      in
      let* fields, gen =
        fieldsCore (C.recordFieldsInSourceOrder fields) gen []
      in
      let ordered =
        List.stable_sort
          (fun (a, _, _) (b, _, _) -> Int.compare (fieldIndex a) (fieldIndex b))
          fields
        |> List.map (fun (_, value, _) -> value)
      in
      let bindings =
        List.concat_map (fun (_, _, bindings) -> bindings) fields
      in
      let id, gen = A.freshVar gen in
      Ok
        ( A.Var id,
          bindings
          @ [
              ( id,
                A.RecordAlloc
                  ( T.recordDescriptor name
                      (C.semanticTypeArgs reference.C.typeArgs)
                      info,
                    ordered ) );
            ],
          gen )
  | C.RecordUpdate (record, updates) -> (
      let* typ = infer record in
      match typ with
      | AST.TRecord (name, args) -> (
          match M.find_opt name registry with
          | None -> Error ("Unknown record type: " ^ name)
          | Some info ->
              let* record, prefix, gen = atom record gen in
              let rec updatesCore remaining gen acc =
                match remaining with
                | [] -> Ok (List.rev acc, gen)
                | (id, value) :: rest ->
                    let* value, bindings, gen = atom value gen in
                    updatesCore rest gen ((id, value, bindings) :: acc)
              in
              let* updates, gen = updatesCore updates gen [] in
              let updateAtoms =
                List.fold_left
                  (fun map (id, value, _) ->
                    Indices.add (fieldIndex id) value map)
                  Indices.empty updates
              in
              let updateBindings =
                List.concat_map (fun (_, _, bindings) -> bindings) updates
              in
              let values, projections, gen =
                List.mapi (fun index (name, _) -> (index, name)) info.R.fields
                |> List.fold_left
                     (fun (values, bindings, gen) (index, _) ->
                       match Indices.find_opt index updateAtoms with
                       | Some value -> (value :: values, bindings, gen)
                       | None ->
                           let id, gen = A.freshVar gen in
                           ( A.Var id :: values,
                             bindings
                             @ [
                                 ( id,
                                   A.RecordGet
                                     ( T.recordDescriptor name args info,
                                       record,
                                       index ) );
                               ],
                             gen ))
                     ([], [], gen)
              in
              let id, gen = A.freshVar gen in
              Ok
                ( A.Var id,
                  prefix @ updateBindings @ projections
                  @ [
                      ( id,
                        A.RecordClone
                          ( T.recordDescriptor name args info,
                            record,
                            List.rev values ) );
                    ],
                  gen ))
      | _ -> Error "Cannot use record update syntax on non-record type")
  | C.RecordAccess (record, field) -> (
      let* typ = infer record in
      match typ with
      | AST.TRecord (name, args) -> (
          match M.find_opt name registry with
          | None -> Error ("Unknown record type: " ^ name)
          | Some info ->
              if
                fieldIndex field < 0
                || fieldIndex field >= List.length info.R.fields
              then
                Error
                  ("Record type '" ^ name ^ "' has no field '"
                  ^ displayId AST.DiagnosticFormatting.field field
                  ^ "'")
              else
                let index = fieldIndex field in
                let* record, bindings, gen = atom record gen in
                let id, gen = A.freshVar gen in
                Ok
                  ( A.Var id,
                    bindings
                    @ [
                        ( id,
                          A.RecordGet
                            (T.recordDescriptor name args info, record, index)
                        );
                      ],
                    gen ))
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
      | Some constructorType -> (
          let tag = constructorTag reference.C.constructorId in
          match
            P.tryFindVariantByConstructorId reference.C.typeId constructorType
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
              let payloadVariants =
                M.exists
                  (fun _ (owner, _, _, fields) -> owner = name && fields <> [])
                  variants
              in
              let descriptor () =
                let* typ = infer expr in
                match typ with
                | AST.TSum (inferred, args) when inferred = name ->
                    T.boxedSumDescriptor name params args variantFields
                | typ ->
                    Error
                      ("Constructor '" ^ name ^ "' inferred unexpected type '"
                      ^ StructuralFormat.semanticType typ
                      ^ "'")
              in
              match fields with
              | [ value ]
                when Option.is_some
                       (P.transparentSumPayloadType name args sums.P.cases)
                     || nullable || Option.is_some spare ->
                  let* value, bindings, gen = atom value gen in
                  bind
                    (fun id -> A.Var id)
                    bindings gen
                    (A.TypedAtom (value, AST.TSum (name, args)))
              | [] when nullable || Option.is_some spare ->
                  bind
                    (fun id -> A.Var id)
                    [] gen
                    (A.TypedAtom
                       ( int (Option.value spare ~default:0L),
                         AST.TSum (name, args) ))
              | [] when not payloadVariants ->
                  Ok (int (Int64.of_int tag), [], gen)
              | [] ->
                  let* descriptor = descriptor () in
                  bind
                    (fun id -> A.Var id)
                    [] gen
                    (A.RecordAlloc
                       (descriptor, [ int (Int64.of_int tag); int 0L ]))
              | _ ->
                  let payload =
                    match fields with
                    | [ field ] -> field
                    | _ -> C.TupleLiteral (C.tupleElementsOfList fields)
                  in
                  let* descriptor = descriptor () in
                  let* payload, bindings, gen = atom payload gen in
                  bind
                    (fun id -> A.Var id)
                    bindings gen
                    (A.RecordAlloc
                       (descriptor, [ int (Int64.of_int tag); payload ])))))
  | C.ListLiteral elements ->
      if elements = [] then Ok (int 0L, [], gen)
      else
        let rec elementsCore remaining gen acc =
          match remaining with
          | [] -> Ok (List.rev acc, gen)
          | value :: rest ->
              let* typ = infer value in
              let* value, bindings, gen = atom value gen in
              elementsCore rest gen ((value, typ, bindings) :: acc)
        in
        let* elements, gen = elementsCore elements gen [] in
        let bindings =
          List.concat_map (fun (_, _, bindings) -> bindings) elements
        in
        let values = List.map (fun (value, typ, _) -> (value, typ)) elements in
        let listType =
          match values with
          | (_, typ) :: _ -> AST.TList typ
          | [] -> AST.TList (AST.TVar "a")
        in
        Ok (G.buildSkewListLiteral listType values gen bindings)
  | C.InterpolatedString parts -> (
      let part = function
        | C.StringText value -> C.StringLiteral value
        | C.StringExpr value -> value
      in
      match parts with
      | [] -> Ok (A.StringLiteral "", [], gen)
      | [ single ] -> atom (part single) gen
      | first :: rest ->
          atom
            (List.fold_left
               (fun expr value -> C.BinOp (AST.StringConcat, expr, part value))
               (part first) rest)
            gen)
  | C.Match (scrutinee, cases) ->
      let* _matchExpr, _gen =
        toANFCore sums typeNames inert
          (C.Match (scrutinee, cases))
          gen env registry variants functions names modules
      in
      Error
        "Match expressions in atom position not yet supported (use let binding)"
  | C.Lambda _ ->
      Error "Lambda expressions (closures) are not yet fully implemented"
  | C.IndirectApply (func, args) ->
      let* func, bindings, gen = atom func gen in
      callClosure func bindings (S.exprArgsToList args) gen (fun func args ->
          A.IndirectCall (func, args))
  | C.Apply (func, args) -> (
      let argsList = S.exprArgsToList args in
      let buildLets params args body =
        let rec loop params args =
          match (params, args) with
          | [], [] -> body
          | (param : C.lambdaParameter) :: params, arg :: args ->
              C.Let (param.C.pattern, arg, loop params args)
          | _ -> body
        in
        loop params args
      in
      match func with
      | C.Lambda (params, _, body) ->
          let params = NonEmptyList.toList params in
          if List.length argsList <> List.length params then
            Error
              ("Expected "
              ^ string_of_int (List.length params)
              ^ " arguments, got "
              ^ string_of_int (List.length argsList))
          else atom (buildLets params argsList body) gen
      | C.Apply (inner, innerArgs) -> (
          let innerList = S.exprArgsToList innerArgs in
          match inner with
          | C.Lambda (params, _, body) ->
              let params = NonEmptyList.toList params in
              if List.length innerList <> List.length params then
                Error
                  ("Inner lambda expects "
                  ^ string_of_int (List.length params)
                  ^ " arguments, got "
                  ^ string_of_int (List.length innerList))
              else atom (C.Apply (buildLets params innerList body, args)) gen
          | _ ->
              let* closure, bindings, gen =
                atom (C.Apply (inner, innerArgs)) gen
              in
              callClosure closure bindings argsList gen (fun func args ->
                  A.ClosureCall (func, args)))
      | C.Let (pattern, value, body) ->
          atom (C.Let (pattern, value, C.Apply (body, args))) gen
      | C.Local name -> (
          match R.BindingMap.find_opt name env with
          | Some (id, _) ->
              callClosure (A.Var id) [] argsList gen (fun func args ->
                  A.ClosureCall (func, args))
          | None ->
              Error
                ("Cannot apply variable '"
                ^ displayId AST.DiagnosticFormatting.binding name
                ^ "' as function in atom position - variable not in scope"))
      | C.Closure (id, values) ->
          let* results, gen = captures values gen [] in
          let values, bindings = parts results in
          let closure, gen = A.freshVar gen in
          callClosure (A.Var closure)
            (bindings @ [ (closure, A.ClosureAlloc (id, values)) ])
            argsList gen
            (fun func args -> A.ClosureCall (func, args))
      | _ ->
          let* func, bindings, gen = atom func gen in
          callClosure func bindings argsList gen (fun func args ->
              A.ClosureCall (func, args)))
