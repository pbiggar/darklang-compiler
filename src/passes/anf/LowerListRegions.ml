(* LowerListRegions.fs - Lower verified owned arrays and scalar joins to native ANF operations. *)
[@@@warning "-4"]
module A = ANF
module H = HIR
module L = ListRegion
module O = OwnedIR
module M = H.ValueMap
module S = H.ValueSet
module R = TypeRegistries
module K = Continuations
let ( let* ) = Result.bind
let word value = A.IntLiteral (A.Int64 (Int64.of_int value))
let bindReturns = K.bindReturns
let metadata length : MemoryModel.rcMetadata option = Some {MemoryModel.releasePlanCacheKey = None; sourceType = None; releasePlan = Some (MemoryModel.RootRelease (L.payloadSize length, MemoryModel.GenericHeap, MemoryModel.NoPayloadRelease))}
(*
   The runtime allocator's small branch reserves the entire 256-byte class;
   its RC word follows capacity, not the runtime logical length.
*)
let releaseRuntimeSmall pointer = A.RefCountDec (pointer, L.payloadSize L.recycledCapacityLimit, MemoryModel.GenericHeap, metadata L.recycledCapacityLimit)
let emit value gen = let id, gen = A.freshVar gen in A.Var id, [id, value], gen
let write pointer offset value gen = emit (A.RawWriteWord (pointer, word offset, value)) gen
let mapFold action initial values = let reversed, state = List.fold_left (fun (reversed, state) value -> let value, state = action state value in value :: reversed, state) ([], initial) values in List.rev reversed, state
let range count = List.init (max 0 count) Fun.id
let wrap = K.wrapBindings
type buffer = {pointer : A.atom; length : A.atom; layout : L.arrayLayout}
type lowerScalar = CheckedAST.expr -> A.varGen -> R.varEnv -> (A.aExpr * A.varGen, string) result
let allocate resolve layout length gen =
 let allocateConstant count primitive = let pointer, allocation, gen = emit (primitive (word (L.allocationSize count))) gen in
  let header = [0,count;8,count;16,0;L.payloadSize count,1] in let writes, gen = mapFold (fun gen (offset, value) -> let _, bindings, gen = write pointer offset (word value) gen in bindings, gen) gen header in pointer, allocation @ List.concat writes, gen in
 match layout with L.RuntimeArray _ -> emit (A.Call (resolve "Darklang.Stdlib.List.__arrayAllocate", [length])) gen | L.RecycledArray count -> allocateConstant count (fun value -> A.RawAlloc value) | L.MappedArray count -> allocateConstant count (fun value -> A.MappedAlloc value)
(*
   Lower verified storage operations to existing raw memory and RC primitives.
   The raw pointer is never tagged as a source List or assigned a fake Blob type.
*)
let lower resolve (lowerScalar : lowerScalar) env gen (L.OwnedRegion (block, layouts) as region) =
 let rec duplicated (block : L.ownedBlock) = List.fold_left (fun ids step -> match step with O.Dup id -> S.add id ids | O.Evaluate (H.Branch (_, _, yes, no)) -> let yes = duplicated yes in let no = duplicated no in S.union ids (S.union yes no) | O.Drop _ | O.Evaluate _ -> ids) S.empty block.O.body.H.operations in
 let shared = duplicated block in
 let lowerValue values gen (value : L.scalar) =
  let sourceEnv = CheckedAST.BindingIdMap.fold (fun name (input : H.value) env -> R.BindingMap.add name (L.lookup "scalar value" input.H.id values) env) value.H.inputs env in
  let* expr, gen = lowerScalar value.H.expression gen sourceEnv in let id, gen = A.freshVar gen in
  Ok (bindReturns expr (fun atom -> A.Let (id, A.TypedAtom (atom, value.H.typ), A.Return (A.Var id))), A.Var id, gen) in
 let release buffers values gen = let bindings, gen = mapFold (fun gen value -> let buffer = L.lookup "release buffer" value buffers in
  let operation = match buffer.layout with L.RecycledArray length -> A.RefCountDec (buffer.pointer, L.payloadSize length, MemoryModel.GenericHeap, metadata length) | L.MappedArray _ when not (S.mem value shared) -> A.MappedFree buffer.pointer | L.MappedArray _ | L.RuntimeArray _ -> A.Call (resolve "Darklang.Stdlib.List.__arrayRelease", [buffer.pointer]) in
  let _, bindings, gen = emit operation gen in bindings, gen) gen values in List.concat bindings, gen in
 let prepareMutation buffer ownership gen = match ownership with
 | L.Consume -> A.Return buffer.pointer, gen
 | L.ConsumeOrCopy when (match buffer.layout with L.RecycledArray _ -> true | _ -> false) ->
   let length = match buffer.layout with L.RecycledArray length -> length | _ -> Crash.crash "List region: expected recycled array" in
   let copy, allocation, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayAllocate", [buffer.length])) gen in
   let _, copied, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayCopy", [buffer.pointer;copy;word 0;buffer.length])) gen in
   let _, released, gen = emit (A.RefCountDec (buffer.pointer, L.payloadSize length, MemoryModel.GenericHeap, metadata length)) gen in wrap (allocation @ copied @ released) (A.Return copy), gen
 | L.ConsumeOrCopy -> let prepared, bindings, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayPrepareMutation", [buffer.pointer])) gen in wrap bindings (A.Return prepared), gen
 | L.BorrowAndCopy -> let copy, allocation, gen = allocate resolve buffer.layout buffer.length gen in
   let copied, gen = match buffer.layout with
   | L.MappedArray _ | L.RuntimeArray _ -> let _, bindings, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayCopy", [buffer.pointer;copy;word 0;buffer.length])) gen in [bindings], gen
   | L.RecycledArray length -> mapFold (fun gen index -> let value, loads, gen = emit (A.RawGet (buffer.pointer, word (L.elementOffset index), Some AST.TInt64)) gen in let _, stores, gen = write copy (L.elementOffset index) value gen in loads @ stores, gen) gen (range length) in
   let _, initialized, gen = write copy 16 buffer.length gen in wrap (allocation @ List.concat copied @ initialized) (A.Return copy), gen in
 let rec lowerBlock values buffers gen (block : L.ownedBlock) = loop block.O.body.H.result values buffers gen block.O.body.H.operations
 and loop (finalValue : H.value) values buffers gen steps = match steps with
 | [] -> let id, _ = L.lookup "block result" finalValue.H.id values in Ok (A.Return (A.Var id), gen)
 | O.Drop value :: rest -> let releases, gen = release buffers [value] gen in let* body, gen = loop finalValue values buffers gen rest in Ok (wrap releases body, gen)
 | O.Dup value :: rest -> let buffer = L.lookup "retain buffer" value buffers in let _, bindings, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayRetain", [buffer.pointer])) gen in let* body, gen = loop finalValue values buffers gen rest in Ok (wrap bindings body, gen)
 | O.Evaluate operation :: rest ->
   let lowerRest values buffers gen = loop finalValue values buffers gen rest in
   (match operation with
   | H.Branch (result, condition, yes, no) ->
     let* evaluation, condition, gen = lowerValue values gen condition in let* yes, gen = lowerBlock values buffers gen yes in let* no, gen = lowerBlock values buffers gen no in
     let joined, gen = A.freshVar gen in let typ = result.H.typ in let* body, gen = lowerRest (M.add result.H.id (joined, typ) values) buffers gen in
     let jump atom = A.Jump (joined, atom) in let yes = bindReturns yes jump in let no = bindReturns no jump in let entry = A.If (condition, yes, no) in
     Ok (bindReturns evaluation (fun _ -> A.Join ({A.id = joined; typ}, body, entry)), gen)
   | H.ScalarBinding (result, value) -> let* expr, atom, gen = lowerValue values gen value in (match atom with A.Var id -> let* body, gen = lowerRest (M.add result.H.id (id, value.H.typ) values) buffers gen in Ok (bindReturns expr (fun _ -> body), gen) | _ -> Crash.crash "List HIR: scalar lowering must bind its result")
   | H.Call _ -> Crash.crash "List HIR: verified list regions cannot contain general calls"
   | H.Leaf (L.Construct (output, L.Repeat (count, value))) ->
     let* countExpr, count, gen = lowerValue values gen count in let* valueExpr, value, gen = lowerValue values gen value in
     let pointer, allocation, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayRepeat", [count;value])) gen in let length, load, gen = emit (A.RawGet (pointer, word 0, Some AST.TInt64)) gen in
     let buffer = {pointer; length; layout = L.lookup "construction layout" output.H.id layouts} in let* body, gen = lowerRest values (M.add output.H.id buffer buffers) gen in
     Ok (bindReturns countExpr (fun _ -> bindReturns valueExpr (fun _ -> wrap (allocation @ load) body)), gen)
   | H.Leaf (L.Construct (output, L.Literal elements)) ->
     let rec evaluate gen expressions evaluated = match expressions with [] -> Ok (A.Return A.UnitLiteral, List.rev evaluated, gen) | value :: rest -> let* expr, atom, gen = lowerValue values gen value in let* body, atoms, gen = evaluate gen rest (atom :: evaluated) in Ok (bindReturns expr (fun _ -> body), atoms, gen) in
     let* evaluation, atoms, gen = evaluate gen elements [] in let layout = L.lookup "construction layout" output.H.id layouts in let length = word (List.length elements) in
     let pointer, allocation, gen = allocate resolve layout length gen in
     let writes, gen = mapFold (fun gen (index, atom) -> let _, bindings, gen = write pointer (L.elementOffset index) atom gen in bindings, gen) gen (List.mapi (fun index atom -> index, atom) atoms) in
     let _, initialized, gen = write pointer 16 length gen in let buffer = {pointer; length; layout} in let* body, gen = lowerRest values (M.add output.H.id buffer buffers) gen in Ok (bindReturns evaluation (fun _ -> wrap (allocation @ List.concat writes @ initialized) body), gen)
   | H.Leaf (L.Transform (output, input, (operation, ownership))) ->
     let buffer = L.lookup "transform buffer" input.H.id buffers in
     let* evaluation, fn, gen = match operation with L.Reverse -> Ok (A.Return A.UnitLiteral, A.UnitLiteral, gen) | L.Map fn -> lowerValue values gen fn in
     let preparation, gen = prepareMutation buffer ownership gen in let destination, gen = A.freshVar gen in let target = A.Var destination in
     let mutations, gen = match operation, buffer.layout with
     | L.Map _, (L.MappedArray _ | L.RuntimeArray _) -> let _, bindings, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayMap", [target;word 0;buffer.length;fn])) gen in [bindings], gen
     | L.Reverse, (L.MappedArray _ | L.RuntimeArray _) -> let last, subtraction, gen = emit (A.Prim (A.Sub, buffer.length, word 1)) gen in let _, bindings, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayReverse", [target;word 0;last])) gen in [subtraction @ bindings], gen
     | L.Map _, L.RecycledArray length -> mapFold (fun gen index -> let value, load, gen = emit (A.RawGet (target, word (L.elementOffset index), Some AST.TInt64)) gen in let mapped, call, gen = emit (A.ClosureCall (fn, [value])) gen in let _, store, gen = write target (L.elementOffset index) mapped gen in load @ call @ store, gen) gen (range length)
     | L.Reverse, L.RecycledArray length -> mapFold (fun gen index -> let other = length - 1 - index in let left, leftLoad, gen = emit (A.RawGet (target, word (L.elementOffset index), Some AST.TInt64)) gen in let right, rightLoad, gen = emit (A.RawGet (target, word (L.elementOffset other), Some AST.TInt64)) gen in let _, leftWrite, gen = write target (L.elementOffset index) right gen in let _, rightWrite, gen = write target (L.elementOffset other) left gen in leftLoad @ rightLoad @ leftWrite @ rightWrite, gen) gen (range (length / 2)) in
     let layout = match ownership with L.ConsumeOrCopy -> L.RuntimeArray output.H.id | L.Consume | L.BorrowAndCopy -> buffer.layout in
     let* body, gen = lowerRest values (M.add output.H.id {buffer with pointer = target; layout} buffers) gen in
     Ok (bindReturns evaluation (fun _ -> bindReturns preparation (fun selected -> A.Let (destination, A.TypedAtom (selected, AST.TInternalRawPtr), wrap (List.concat mutations) body))), gen)
   | H.Leaf (L.Fold (result, input, initial, fn)) ->
     let* initialExpr, accumulator, gen = lowerValue values gen initial in let* callbackExpr, callback, gen = lowerValue values gen fn in let buffer = L.lookup "fold buffer" input.H.id buffers in
     let bindings, (value, gen) = match buffer.layout with
     | L.MappedArray _ | L.RuntimeArray _ -> let result, calls, gen = emit (A.Call (resolve "Darklang.Stdlib.List.__arrayFold", [buffer.pointer;word 0;buffer.length;accumulator;callback])) gen in [calls], (result, gen)
     | L.RecycledArray length -> mapFold (fun (acc, gen) index -> let element, loads, gen = emit (A.RawGet (buffer.pointer, word (L.elementOffset index), Some AST.TInt64)) gen in let result, calls, gen = emit (A.ClosureCall (callback, [acc;element])) gen in loads @ calls, (result, gen)) (accumulator, gen) (range length) in
     let id, gen = A.freshVar gen in let* body, gen = lowerRest (M.add result.H.id (id, AST.TInt64) values) buffers gen in
     Ok (bindReturns initialExpr (fun _ -> bindReturns callbackExpr (fun _ -> wrap (List.concat bindings) (A.Let (id, A.TypedAtom (value, AST.TInt64), body)))), gen)) in
 let initialValues = List.fold_left (fun values (param : H.parameter) ->
  let value = match R.BindingMap.find_opt param.H.binding env with Some value -> value | None -> Crash.crash ("List HIR: missing root parameter for " ^ StructuralFormat.format (AST.DiagnosticFormatting.binding param.H.binding)) in M.add param.H.value.H.id value values) M.empty block.O.body.H.parameters in
 let* () = VerifyListOwnership.verify region in lowerBlock initialValues M.empty gen block
