// LowerListRegions.fs - Lower verified owned arrays and scalar joins to native ANF operations.

module LowerListRegions

open HIR
open OwnedIR

open ListRegion
open VerifyListOwnership

type LowerScalar = CheckedAST.Expr -> ANF.VarGen -> Map<AST.BindingId, ANF.TempId * AST.Type> -> Result<ANF.AExpr * ANF.VarGen, string>

let private word value = ANF.IntLiteral (ANF.Int64 (int64 value))

let rec private bindReturns expression continuation =
    match expression with
    | ANF.Return atom -> continuation atom
    | ANF.Jump _ -> expression
    | ANF.Join (parameter, body, entry) ->
        ANF.Join (parameter, bindReturns body continuation, bindReturns entry continuation)
    | ANF.Let (id, value, body) -> ANF.Let (id, value, bindReturns body continuation)
    | ANF.If (condition, yes, no) -> ANF.If (condition, bindReturns yes continuation, bindReturns no continuation)

let private metadata length : MemoryModel.RcMetadata option =
    Some { ReleasePlanCacheKey = None; SourceType = None
           ReleasePlan = Some (MemoryModel.RootRelease (payloadSize length, MemoryModel.GenericHeap, MemoryModel.NoPayloadRelease)) }

/// The runtime allocator's small branch reserves the entire 256-byte class;
/// its RC word follows capacity, not the runtime logical length.
let releaseRuntimeSmall pointer =
    ANF.RefCountDec (pointer, payloadSize recycledCapacityLimit, MemoryModel.GenericHeap, metadata recycledCapacityLimit)

let private emit value vg =
    let id, next = ANF.freshVar vg
    ANF.Var id, [(id, value)], next

let private write pointer offset value vg = emit (ANF.RawWriteWord (pointer, word offset, value)) vg

type private Buffer = { Pointer: ANF.Atom; Length: ANF.Atom; Layout: ArrayLayout }

let private allocate layout length vg =
    let allocateConstant count primitive =
        let pointer, allocation, next = emit (primitive (word (allocationSize count))) vg
        let header = [0, count; 8, count; 16, 0; payloadSize count, 1]
        let writes, final = header |> List.mapFold (fun state (offset, value) -> let _, bindings, next = write pointer offset (word value) state in bindings, next) next
        pointer, allocation @ List.concat writes, final
    match layout with
    | RuntimeArray _ -> emit (ANF.Call (AST.functionIdForName "Darklang.Stdlib.List.__arrayAllocate", [length])) vg
    | RecycledArray count -> allocateConstant count ANF.RawAlloc
    | MappedArray count -> allocateConstant count ANF.MappedAlloc

let private wrap bindings body = List.foldBack (fun (id, value) tail -> ANF.Let (id, value, tail)) bindings body

/// Lower verified storage operations to existing raw memory and RC primitives.
/// The raw pointer is never tagged as a source List or assigned a fake Blob type.
let lower (lowerScalar: LowerScalar) env vg (OwnedRegion (block, layouts) as region) =
    let lowerValue values vg (value: Scalar) =
        let sourceEnv =
            value.Inputs
            |> Map.fold (fun sourceEnv name input ->
                Map.add name (lookup "scalar value" input.Id values) sourceEnv) env
        lowerScalar value.Expression vg sourceEnv
        |> Result.map (fun (expr, next) ->
            let id, final = ANF.freshVar next
            bindReturns expr (fun atom -> ANF.Let (id, ANF.TypedAtom (atom, value.Type), ANF.Return (ANF.Var id))), ANF.Var id, final)

    let release buffers values vg =
        values |> List.mapFold (fun state value ->
            let buffer = lookup "release buffer" value buffers
            let operation =
                match buffer.Layout with
                | RecycledArray length -> ANF.RefCountDec (buffer.Pointer, payloadSize length, MemoryModel.GenericHeap, metadata length)
                | MappedArray _ -> ANF.MappedFree buffer.Pointer
                | RuntimeArray _ -> ANF.Call (AST.functionIdForName "Darklang.Stdlib.List.__arrayRelease", [buffer.Pointer])
            let _, bindings, next = emit operation state
            bindings, next) vg
        |> fun (bindings, next) -> List.concat bindings, next

    let prepareMutation buffer ownership vg =
        match ownership with
        | Consume -> ANF.Return buffer.Pointer, vg
        | BorrowAndCopy ->
            let copy, allocations, afterAllocation = allocate buffer.Layout buffer.Length vg
            let copied, afterCopy =
                match buffer.Layout with
                | MappedArray _ | RuntimeArray _ ->
                    let _, bindings, next = emit (ANF.Call (AST.functionIdForName "Darklang.Stdlib.List.__arrayCopy", [buffer.Pointer; copy; word 0; buffer.Length])) afterAllocation
                    [bindings], next
                | RecycledArray length ->
                    [0 .. length - 1] |> List.mapFold (fun state index ->
                        let value, loads, next = emit (ANF.RawGet (buffer.Pointer, word (elementOffset index), Some AST.TInt64)) state
                        let _, stores, final = write copy (elementOffset index) value next
                        loads @ stores, final) afterAllocation
            let _, initialized, final = write copy 16 buffer.Length afterCopy
            wrap (allocations @ List.concat copied @ initialized) (ANF.Return copy), final

    let rec lowerBlock values buffers vg block =
        loop block.Body.Result values buffers vg block.Body.Operations
    and loop finalValue values buffers vg (steps: OwnedOperation list) =
        match steps with
        | [] ->
            let id, _ = lookup "block result" finalValue.Id values
            Ok (ANF.Return (ANF.Var id), vg)
        | Drop value :: rest ->
            let releases, next = release buffers [value] vg
            loop finalValue values buffers next rest
            |> Result.map (fun (body, final) -> wrap releases body, final)
        | Dup _ :: _ -> Crash.crash "List HIR: verified unique regions cannot duplicate ownership"
        | Evaluate operation :: rest ->
            let lowerRest values buffers vg = loop finalValue values buffers vg rest
            match operation with
            | Branch (result, condition, yes, no) ->
                lowerValue values vg condition |> Result.bind (fun (evaluation, condition, next) ->
                    lowerBlock values buffers next yes |> Result.bind (fun (yesExpr, afterYes) ->
                        lowerBlock values buffers afterYes no |> Result.bind (fun (noExpr, afterNo) ->
                            let joined, afterJoin = ANF.freshVar afterNo
                            let typ = result.Type
                            lowerRest (Map.add result.Id (joined, typ) values) buffers afterJoin
                            |> Result.map (fun (body, final) ->
                                let jump atom = ANF.Jump (joined, atom)
                                let entry = ANF.If (condition, bindReturns yesExpr jump, bindReturns noExpr jump)
                                bindReturns evaluation (fun _ -> ANF.Join ({ Id = joined; Type = typ }, body, entry)), final))))
            | ScalarBinding (result, value) ->
                lowerValue values vg value
                |> Result.bind (fun (expr, atom, next) ->
                    match atom with
                    | ANF.Var id ->
                        lowerRest (Map.add result.Id (id, value.Type) values) buffers next
                        |> Result.map (fun (body, final) -> bindReturns expr (fun _ -> body), final)
                    | _ -> Crash.crash "List HIR: scalar lowering must bind its result")
            | Call _ -> Crash.crash "List HIR: verified list regions cannot contain general calls"
            | Leaf (Construct (output, Repeat (count, value))) ->
                lowerValue values vg count |> Result.bind (fun (countExpr, countAtom, afterCount) ->
                    lowerValue values afterCount value |> Result.bind (fun (valueExpr, valueAtom, afterValue) ->
                        let pointer, allocation, afterAllocation = emit (ANF.Call (AST.functionIdForName "Darklang.Stdlib.List.__arrayRepeat", [countAtom; valueAtom])) afterValue
                        let length, load, afterLoad = emit (ANF.RawGet (pointer, word 0, Some AST.TInt64)) afterAllocation
                        let buffer = { Pointer = pointer; Length = length; Layout = lookup "construction layout" output.Id layouts }
                        lowerRest values (Map.add output.Id buffer buffers) afterLoad
                        |> Result.map (fun (body, final) ->
                            bindReturns countExpr (fun _ -> bindReturns valueExpr (fun _ -> wrap (allocation @ load) body)), final)))
            | Leaf (Construct (output, Literal elements)) ->
                let rec evaluate vg expressions evaluated =
                    match expressions with
                    | [] -> Ok (ANF.Return ANF.UnitLiteral, List.rev evaluated, vg)
                    | value :: tail ->
                        lowerValue values vg value |> Result.bind (fun (expr, atom, next) ->
                            evaluate next tail (atom :: evaluated)
                            |> Result.map (fun (body, atoms, final) -> bindReturns expr (fun _ -> body), atoms, final))
                evaluate vg elements [] |> Result.bind (fun (evaluation, atoms, next) ->
                    let layout = lookup "construction layout" output.Id layouts
                    let length = word (List.length elements)
                    let pointer, allocation, afterAllocation = allocate layout length next
                    let writes, afterWrites = atoms |> List.mapi (fun index atom -> index, atom) |> List.mapFold (fun state (index, atom) -> let _, bindings, next = write pointer (elementOffset index) atom state in bindings, next) afterAllocation
                    let _, initialized, afterInit = write pointer 16 length afterWrites
                    let buffer = { Pointer = pointer; Length = length; Layout = layout }
                    lowerRest values (Map.add output.Id buffer buffers) afterInit
                    |> Result.map (fun (body, final) -> bindReturns evaluation (fun _ -> wrap (allocation @ List.concat writes @ initialized) body), final))
            | Leaf (Transform (output, input, (operation, ownership))) ->
                let buffer = lookup "transform buffer" input.Id buffers
                let callback =
                    match operation with
                    | Reverse -> Ok (ANF.Return ANF.UnitLiteral, ANF.UnitLiteral, vg)
                    | Map fn -> lowerValue values vg fn
                callback |> Result.bind (fun (evaluation, fn, next) ->
                    let preparation, afterPreparation = prepareMutation buffer ownership next
                    let destination, afterDestination = ANF.freshVar afterPreparation
                    let target = ANF.Var destination
                    let mutations, afterMutation =
                        match operation, buffer.Layout with
                        | Map _, (MappedArray _ | RuntimeArray _) ->
                            let _, bindings, next = emit (ANF.Call (AST.functionIdForName "Darklang.Stdlib.List.__arrayMap", [target; word 0; buffer.Length; fn])) afterDestination
                            [bindings], next
                        | Reverse, (MappedArray _ | RuntimeArray _) ->
                            let last, subtraction, afterLast = emit (ANF.Prim (ANF.Sub, buffer.Length, word 1)) afterDestination
                            let _, bindings, next = emit (ANF.Call (AST.functionIdForName "Darklang.Stdlib.List.__arrayReverse", [target; word 0; last])) afterLast
                            [subtraction @ bindings], next
                        | Map _, RecycledArray length ->
                            [0 .. length - 1] |> List.mapFold (fun state index ->
                                let value, load, next = emit (ANF.RawGet (target, word (elementOffset index), Some AST.TInt64)) state
                                let mapped, call, afterCall = emit (ANF.ClosureCall (fn, [value])) next
                                let _, store, final = write target (elementOffset index) mapped afterCall
                                load @ call @ store, final) afterDestination
                        | Reverse, RecycledArray length ->
                            [0 .. length / 2 - 1] |> List.mapFold (fun state index ->
                                let other = length - 1 - index
                                let left, leftLoad, next = emit (ANF.RawGet (target, word (elementOffset index), Some AST.TInt64)) state
                                let right, rightLoad, afterRight = emit (ANF.RawGet (target, word (elementOffset other), Some AST.TInt64)) next
                                let _, leftWrite, afterLeft = write target (elementOffset index) right afterRight
                                let _, rightWrite, final = write target (elementOffset other) left afterLeft
                                leftLoad @ rightLoad @ leftWrite @ rightWrite, final) afterDestination
                    lowerRest values (Map.add output.Id { buffer with Pointer = target } buffers) afterMutation
                    |> Result.map (fun (body, final) ->
                        bindReturns evaluation (fun _ ->
                            bindReturns preparation (fun selected ->
                                ANF.Let (destination, ANF.TypedAtom (selected, AST.TRawPtr), wrap (List.concat mutations) body))), final))
            | Leaf (Fold (result, input, initial, fn)) ->
                lowerValue values vg initial |> Result.bind (fun (initialExpr, accumulator, next) ->
                    lowerValue values next fn |> Result.bind (fun (callbackExpr, callback, afterCallback) ->
                        let buffer = lookup "fold buffer" input.Id buffers
                        let bindings, (value, afterFold) =
                            match buffer.Layout with
                            | MappedArray _ | RuntimeArray _ ->
                                let result, calls, next = emit (ANF.Call (AST.functionIdForName "Darklang.Stdlib.List.__arrayFold", [buffer.Pointer; word 0; buffer.Length; accumulator; callback])) afterCallback
                                [calls], (result, next)
                            | RecycledArray length ->
                                [0 .. length - 1] |> List.mapFold (fun (acc, state) index ->
                                    let element, loads, next = emit (ANF.RawGet (buffer.Pointer, word (elementOffset index), Some AST.TInt64)) state
                                    let result, calls, final = emit (ANF.ClosureCall (callback, [acc; element])) next
                                    loads @ calls, (result, final)) (accumulator, afterCallback)
                        let id, afterId = ANF.freshVar afterFold
                        lowerRest (Map.add result.Id (id, AST.TInt64) values) buffers afterId
                        |> Result.map (fun (body, final) ->
                            bindReturns initialExpr (fun _ -> bindReturns callbackExpr (fun _ ->
                                wrap (List.concat bindings) (ANF.Let (id, ANF.TypedAtom (value, AST.TInt64), body)))), final)))

    let initialValues =
        block.Body.Parameters
        |> List.fold (fun values parameter ->
            Map.add parameter.Value.Id (lookup "root parameter" parameter.Binding env) values) Map.empty
    verify region |> Result.bind (fun () -> lowerBlock initialValues Map.empty vg block)
