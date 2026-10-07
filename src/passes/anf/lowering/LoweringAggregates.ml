(* LoweringAggregates.ml - Build skew-list storage and bind typed deconstruction patterns. *)
[@@@warning "-4"]
module A = ANF
module C = CheckedAST
module R = TypeRegistries
let ( let* ) = Result.bind
let int value = A.IntLiteral (A.Int64 value)
let add left right = Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let buildSkewListLiteral listType elements gen initialBindings =
 let tagRawPtr tag ptr gen reversed =
  let raw, gen1 = A.freshVar gen in let expression = A.Prim (A.BitOr, A.Var ptr, int tag) in
  let typed, gen2 = A.freshVar gen1 in let typedExpr = A.TypedAtom (A.Var raw, listType) in
  A.Var typed, (typed, typedExpr) :: (raw, expression) :: reversed, gen2 in
 let allocLeaf value typ gen reversed =
  let ptr, gen1 = A.freshVar gen in let valueId, gen2 = A.freshVar gen1 in let rc, gen3 = A.freshVar gen2 in
  let reversed = (rc, A.RawWriteWord (A.Var ptr, int 8L, int 1L)) :: (valueId, A.RawSlotInit (A.Var ptr, int 0L, value, typ)) :: (ptr, A.RawAlloc (int 16L)) :: reversed in
  tagRawPtr 2L ptr gen3 reversed in
 let allocNode value typ left right gen reversed =
  let ptr, gen1 = A.freshVar gen in let valueId, gen2 = A.freshVar gen1 in let leftId, gen3 = A.freshVar gen2 in let rightId, gen4 = A.freshVar gen3 in let rc, gen5 = A.freshVar gen4 in
  let reversed = (rc, A.RawWriteWord (A.Var ptr, int 24L, int 1L)) :: (rightId, A.RawSlotInit (A.Var ptr, int 16L, right, listType)) :: (leftId, A.RawSlotInit (A.Var ptr, int 8L, left, listType)) :: (valueId, A.RawSlotInit (A.Var ptr, int 0L, value, typ)) :: (ptr, A.RawAlloc (int 32L)) :: reversed in
  tagRawPtr 3L ptr gen5 reversed in
 let allocDigit weight length tree rest gen reversed =
  let ptr, gen1 = A.freshVar gen in let weightId, gen2 = A.freshVar gen1 in let lengthId, gen3 = A.freshVar gen2 in let treeId, gen4 = A.freshVar gen3 in let restId, gen5 = A.freshVar gen4 in let rc, gen6 = A.freshVar gen5 in
  let reversed = (rc, A.RawWriteWord (A.Var ptr, int 32L, int 1L)) :: (restId, A.RawSlotInit (A.Var ptr, int 24L, rest, listType)) :: (treeId, A.RawSlotInit (A.Var ptr, int 16L, tree, listType)) :: (lengthId, A.RawWriteWord (A.Var ptr, int 8L, int (Int64.of_int length))) :: (weightId, A.RawWriteWord (A.Var ptr, int 0L, int (Int64.of_int weight))) :: (ptr, A.RawAlloc (int 40L)) :: reversed in
  tagRawPtr 1L ptr gen6 reversed in
 let rec buildTrees remaining gen bindings = match remaining with
 | [] -> [], bindings, gen
 | (value, typ) :: rest -> let trees, bindings1, gen1 = buildTrees rest gen bindings in (match trees with
   | (firstWeight, firstTree) :: (secondWeight, secondTree) :: suffix when firstWeight = secondWeight -> let tree, bindings2, gen2 = allocNode value typ firstTree secondTree gen1 bindings1 in (add (add firstWeight secondWeight) 1, tree) :: suffix, bindings2, gen2
   | _ -> let tree, bindings2, gen2 = allocLeaf value typ gen1 bindings1 in (1, tree) :: trees, bindings2, gen2) in
 let rec buildDigits trees gen bindings = match trees with [] -> int 0L, 0, bindings, gen | (weight, tree) :: rest -> let rest, restLength, bindings1, gen1 = buildDigits rest gen bindings in let length = add weight restLength in let digit, bindings2, gen2 = allocDigit weight length tree rest gen1 bindings1 in digit, length, bindings2, gen2 in
 let trees, bindings, gen = buildTrees elements gen (List.rev initialBindings) in
 let root, _, bindings, gen = buildDigits trees gen bindings in root, List.rev bindings, gen
(* Prepare every projection for one binding before its continuation is
   lowered. The type checker has already proved the complete unit/tuple shape. *)
let rec lowerLetPatternBindings pattern source typ environment reversed gen = match pattern with
 | C.LPUnit | C.LPWildcard -> Ok (environment, reversed, gen)
 | C.LPVariable name -> let id, gen = A.freshVar gen in let environment = R.BindingMap.add name (id, typ) environment in Ok (environment, (id, A.TypedAtom (source, typ)) :: reversed, gen)
 | C.LPTuple (first, second, rest) ->
   let patterns = first :: second :: rest in (match typ with
   | AST.TTuple elements when List.length patterns = List.length elements ->
     List.combine patterns elements |> List.mapi (fun index value -> index, value) |> List.fold_left (fun result (index, (pattern, typ)) ->
      let* environment, reversed, gen = result in let raw, gen1 = A.freshVar gen in let typed, gen2 = A.freshVar gen1 in
      let reversed = (typed, A.TypedAtom (A.Var raw, typ)) :: (raw, A.TupleGet (source, index)) :: reversed in
      lowerLetPatternBindings pattern (A.Var typed) typ environment reversed gen2) (Ok (environment, reversed, gen))
   | _ -> Error "Let tuple pattern reached ANF lowering with an incompatible type")
let rec letPatternAcceptsType pattern typ = match pattern, typ with
 | C.LPVariable _, _ | C.LPWildcard, _ -> true
 | C.LPUnit, AST.TUnit -> true
 | C.LPTuple (first, second, rest), AST.TTuple elements -> let patterns = first :: second :: rest in List.length patterns = List.length elements && List.for_all2 letPatternAcceptsType patterns elements
 | _ -> false
