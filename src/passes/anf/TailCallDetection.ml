(*
   Detects tail calls in ANF and transforms them to tail call variants:
   - Call → TailCall
   - IndirectCall → IndirectTailCall
   - ClosureCall → ClosureTailCall
   A call is in tail position if:
   - It's in a Let binding where the body eventually returns the same variable
   - Both branches of an If are in tail position if the If itself is
   This runs AFTER RefCountInsertion, so RefCountDec operations may be inserted
   between the call and the return. We look through any RefCountDec operations
   to find the final Return. This is crucial because without TCO, functions like
   __reverseHelper would use regular calls instead of tail calls, causing the
   intermediate cons cells to be freed prematurely (leading to corrupted results
   when the free list reuses those cells for subsequent allocations).
   CURRENT STATUS: TCO is ENABLED. The DCE bug that caused 197 test failures
   has been fixed (TailCallDetection.ml was not recognizing TailCall as a
   function call, causing stdlib functions called via tail call to be removed).
   See docs/compiler/optimizations/tail-calls.md for detailed documentation.
*)
(* TailCallDetection.ml - Tail Call Detection Pass *)
[@@@warning "-4"]

open ANF
module TempMap = ANFConstants.TempMap
module TempSet = ANFEffects.TempSet
module TM = TempMap
module TS = TempSet

(*
   Check if a CExpr is a RefCountDec operation
*)
let isRefCountDec = function
  | RefCountDec _ | RefCountDecString _ | RefCountDecBlob _ | RefCountDecInt _
    ->
      true
  | _ -> false

(*
   Check if an expression eventually returns a specific TempId
   Looks through any RefCountDec operations to find the final Return
   RefCountDec followed by more expressions - look through it
*)
let rec isReturnOf tempId = function
  | Return (Var tid) when tid = tempId -> true
  | Let (_, cexpr, body) when isRefCountDec cexpr -> isReturnOf tempId body
  | _ -> false

(*
   Transform a Call to TailCall if it's in tail position
*)
let convertToTailCall = function
  | Call (name, args) | BorrowedCall (name, args) -> TailCall (name, args)
  | IndirectCall (func, args) -> IndirectTailCall (func, args)
  | ClosureCall (closure, args) -> ClosureTailCall (closure, args)
  | cexpr -> cexpr

let wrapBindings bindings body =
  List.fold_right (fun (tid, cexpr) acc -> Let (tid, cexpr, acc)) bindings body

(*
   extendAliasRoots canonicalizes the source before insertion, so every map
   value is already a root rather than another link in an alias chain.
*)
let canonicalTempId aliasRoots tempId =
  Option.value ~default:tempId (TM.find_opt tempId aliasRoots)

let extendAliasRoots aliasRoots tempId = function
  | Atom (Var tid) | TypedAtom (Var tid, _) ->
      TM.add tempId (canonicalTempId aliasRoots tid) aliasRoots
  | _ -> aliasRoots

(*
   The temps a projection's result borrows from: a field or element read, a
   backing pointer, or a call that returns a value aliased into its arguments.
   Such a result is only valid while its sources are, so a release of a source
   cannot move ahead of a tail call that passes the result.
*)
let borrowSources cexpr =
  let ofAtom = function Var tid -> [ tid ] | _ -> [] in
  match cexpr with
  | TupleGet (source, _)
  | RecordGet (_, source, _)
  | RawGet (source, _, _)
  | StringToRawPtr source
  | BlobToRawPtr source
  | DictToRawPtr source
  | ListToRawPtr source ->
      ofAtom source
  | BorrowedCall (_, args) -> List.concat_map ofAtom args
  | _ -> []

let extendBorrowRoots aliasRoots borrowRoots tempId cexpr =
  let roots =
    List.fold_left
      (fun acc source ->
        let root = canonicalTempId aliasRoots source in
        let inherited =
          Option.value ~default:TS.empty (TM.find_opt root borrowRoots)
        in
        TS.union inherited (TS.add root acc))
      TS.empty (borrowSources cexpr)
  in
  let inheritedByAlias =
    match cexpr with
    | Atom (Var tid) | TypedAtom (Var tid, _) ->
        Option.value ~default:TS.empty
          (TM.find_opt (canonicalTempId aliasRoots tid) borrowRoots)
    | _ -> TS.empty
  in
  let all = TS.union roots inheritedByAlias in
  if TS.is_empty all then borrowRoots else TM.add tempId all borrowRoots

let tailCallArgTempIds aliasRoots borrowRoots retainedBorrowRoots cexpr =
  let addAtom temps = function
    | Var tid ->
        let root = canonicalTempId aliasRoots tid in
        let borrowed =
          if TS.mem root retainedBorrowRoots then TS.empty
          else
            Option.value ~default:TS.empty
              (match TM.find_opt tid borrowRoots with
              | Some _ as value -> value
              | None -> TM.find_opt root borrowRoots)
        in
        TS.union borrowed (TS.add root temps)
    | _ -> temps
  in
  match cexpr with
  | TailCall (_, args) -> List.fold_left addAtom TS.empty args
  | IndirectTailCall (func, args) | ClosureTailCall (func, args) ->
      List.fold_left addAtom (addAtom TS.empty func) args
  | _ -> TS.empty

let atomOverlapsTailArgs aliasRoots tailArgTemps = function
  | Var tid -> TS.mem (canonicalTempId aliasRoots tid) tailArgTemps
  | _ -> false

let rec collectMovableDecPrefix aliasRoots tailArgTemps = function
  | Let (tid, (RefCountDec (Var released, _, _, _) as cleanup), rest) ->
      let bindings, remaining =
        collectMovableDecPrefix aliasRoots tailArgTemps rest
      in
      if TS.mem (canonicalTempId aliasRoots released) tailArgTemps then
        (bindings, Let (tid, cleanup, remaining))
      else ((tid, cleanup) :: bindings, remaining)
  | Let (tid, (RefCountDecString atom as cleanup), rest)
  | Let (tid, (RefCountDecBlob atom as cleanup), rest)
  | Let (tid, (RefCountDecInt atom as cleanup), rest) ->
      let bindings, remaining =
        collectMovableDecPrefix aliasRoots tailArgTemps rest
      in
      if atomOverlapsTailArgs aliasRoots tailArgTemps atom then
        (bindings, Let (tid, cleanup, remaining))
      else ((tid, cleanup) :: bindings, remaining)
  | expr -> ([], expr)

let isDirectReturnOf tempId = function
  | Return (Var tid) when tid = tempId -> true
  | _ -> false

let rec leadingRetainedParams paramIds = function
  | Let (_, RefCountInc (Var tid, _, _, _), body)
  | Let (_, RefCountIncString (Var tid), body)
  | Let (_, RefCountIncBlob (Var tid), body)
    when TS.mem tid paramIds ->
      TS.add tid (leadingRetainedParams paramIds body)
  | _ -> TS.empty

(*
   Transfer the owned replacement record into the next loop iteration. RC
   insertion marks this shape by retaining the initial parameter and releasing
   the previous parameter before the call. The post-call release belongs to the
   ordinary recursive frame; a loop instead adopts that argument's ownership.
*)
let tryTransferOwnedSelfTailArgument aliasRoots ownedParams releasedTemps
    typedParams callTempId tailCall remainingBody =
  let rec cleanupBindings = function
    | Let (tid, (RefCountDec _ as cleanup), rest) ->
        Option.map
          (fun bindings -> (tid, cleanup) :: bindings)
          (cleanupBindings rest)
    | Return (Var tid) when tid = callTempId -> Some []
    | _ -> None
  in
  match (tailCall, cleanupBindings remainingBody) with
  | TailCall (_, args), Some cleanups -> (
      let cleanupRoots =
        List.map
          (fun (_, cleanup) ->
            match cleanup with
            | RefCountDec (Var tid, _, _, _) -> canonicalTempId aliasRoots tid
            | _ ->
                Crash.crash
                  "tryTransferOwnedSelfTailArgument: unexpected cleanup kind")
          cleanups
      in
      let cleanupRootSet = TS.of_list cleanupRoots in
      let rec matchingOwnedParams paramsRemaining argsRemaining =
        match (paramsRemaining, argsRemaining) with
        | [], [] -> Some []
        | (param : typedParam) :: paramsRest, arg :: argsRest ->
            Option.map
              (fun matches ->
                match arg with
                | Var argTemp
                  when TS.mem
                         (canonicalTempId aliasRoots argTemp)
                         cleanupRootSet
                       && TS.mem param.id ownedParams
                       && TS.mem
                            (canonicalTempId aliasRoots param.id)
                            releasedTemps ->
                    (param.id, canonicalTempId aliasRoots argTemp) :: matches
                | _ -> matches)
              (matchingOwnedParams paramsRest argsRest)
        | _ -> None
      in
      match matchingOwnedParams typedParams args with
      | Some matches ->
          let matchedRoots = List.map snd matches in
          let everyCleanupTransferredExactlyOnce =
            cleanupRoots <> []
            && List.length cleanupRoots = TS.cardinal cleanupRootSet
            && List.length matchedRoots = List.length cleanupRoots
            && TS.equal (TS.of_list matchedRoots) cleanupRootSet
          in
          if everyCleanupTransferredExactlyOnce then
            Some ([], Return (Var callTempId))
          else None
      | _ -> None)
  | _ -> None

(*
   Check if a CExpr is a call (direct, indirect, or closure)
*)
let isCallExpr = function
  | Call _ | BorrowedCall _ | IndirectCall _ | ClosureCall _ -> true
  | _ -> false

(*
   Detect and transform tail calls in an expression.
   The 'inTailPosition' parameter indicates if the current expression
   is in tail position (its result is directly returned).
   Return is always a base case - just return it
   Check if this is a tail call pattern:
   Let (t, Call(...), Return (Var t))
   This is a tail call! Convert the call to tail call variant
   Cleanup remains after the call (typically overlap with a tail argument),
   so keep a normal call to preserve the post-call unwind work.
   Not a tail call - recurse into body
   Body is in tail position if current expression is
   A retain gives a projected managed value ownership independent
   of its source until the matching release consumes that retain.
   If expression: both branches are in tail position if If is
*)
let rec detectTailCalls currentFuncName isCurrentMember typedParams ownedParams
    releasedTemps inTailPosition aliasRoots borrowRoots retainedBorrowRoots expr
    =
  let recurse released aliases borrows retained body =
    detectTailCalls currentFuncName isCurrentMember typedParams ownedParams
      released inTailPosition aliases borrows retained body
  in
  match expr with
  | Return atom -> Return atom
  | Jump _ -> expr
  | Join (parameter, continuation, entry) ->
      let continuation' =
        recurse releasedTemps aliasRoots borrowRoots retainedBorrowRoots
          continuation
      in
      let entry' =
        recurse releasedTemps aliasRoots borrowRoots retainedBorrowRoots entry
      in
      Join (parameter, continuation', entry')
  | Let (tempId, cexpr, body) ->
      if inTailPosition && isCallExpr cexpr && isReturnOf tempId body then
        let tailCall = convertToTailCall cexpr in
        let tailArgTemps =
          tailCallArgTempIds aliasRoots borrowRoots retainedBorrowRoots tailCall
        in
        let movableDecs, remainingBody =
          collectMovableDecPrefix aliasRoots tailArgTemps body
        in
        let transferredBody =
          match tailCall with
          | TailCall (target, _) when isCurrentMember target ->
              tryTransferOwnedSelfTailArgument aliasRoots ownedParams
                releasedTemps typedParams tempId tailCall remainingBody
          | _ -> None
        in
        match transferredBody with
        | Some (transferDecs, bodyAfterTransfer) ->
            wrapBindings
              (movableDecs @ transferDecs)
              (Let (tempId, tailCall, bodyAfterTransfer))
        | None when isDirectReturnOf tempId remainingBody ->
            wrapBindings movableDecs (Let (tempId, tailCall, remainingBody))
        | None ->
            let aliases = extendAliasRoots aliasRoots tempId cexpr in
            let borrows =
              extendBorrowRoots aliasRoots borrowRoots tempId cexpr
            in
            let body' =
              recurse releasedTemps aliases borrows retainedBorrowRoots body
            in
            Let (tempId, cexpr, body')
      else
        let aliases = extendAliasRoots aliasRoots tempId cexpr in
        let borrows = extendBorrowRoots aliasRoots borrowRoots tempId cexpr in
        let retained =
          match cexpr with
          | RefCountInc (Var tid, _, _, _)
          | RefCountIncString (Var tid)
          | RefCountIncBlob (Var tid)
          | RefCountIncInt (Var tid) ->
              TS.add (canonicalTempId aliasRoots tid) retainedBorrowRoots
          | RefCountDec (Var tid, _, _, _)
          | RefCountDecString (Var tid)
          | RefCountDecBlob (Var tid)
          | RefCountDecInt (Var tid) ->
              TS.remove (canonicalTempId aliasRoots tid) retainedBorrowRoots
          | _ -> retainedBorrowRoots
        in
        let released =
          match cexpr with
          | RefCountDec (Var tid, _, _, _)
          | RefCountDecString (Var tid)
          | RefCountDecBlob (Var tid)
          | RefCountDecInt (Var tid) ->
              TS.add (canonicalTempId aliasRoots tid) releasedTemps
          | _ -> releasedTemps
        in
        let body' = recurse released aliases borrows retained body in
        Let (tempId, cexpr, body')
  | If (condition, yes, no) ->
      let yes' =
        recurse releasedTemps aliasRoots borrowRoots retainedBorrowRoots yes
      in
      let no' =
        recurse releasedTemps aliasRoots borrowRoots retainedBorrowRoots no
      in
      If (condition, yes', no')

(*
   The process entrypoint has no caller return address. The listed JSON
   helpers forward projections whose parent is released around the call.
   The process entrypoint has no caller return address. A sibling tail branch
   from _start would make the callee's Ret jump through an invalid address.
   These accessors and generated decoders project managed list/view payloads
   before forwarding them. A sibling tail call would move the parent release
   ahead of that call and invalidate the projected argument. Scanner loops
   remain eligible so large JSON inputs retain bounded stack usage.
*)
let isEligibleFunctionName name =
  let starts prefix = String.starts_with ~prefix name in
  let isJsonOwnershipBoundary =
    starts "Darklang.Stdlib.Json.__view"
    || List.mem name
         [
           "Darklang.Stdlib.Json.__stripLeadingZeroes";
           "Darklang.Stdlib.Json.__shiftIntegerDigits";
           "Darklang.Stdlib.Json.__applyIntegerExponent";
           "Darklang.Stdlib.Json.__unsignedIntegerLexeme";
           "Darklang.Stdlib.Json.__normalizeIntegerMagnitude";
           "Darklang.Stdlib.Json.__integerLexeme";
         ]
  in
  let isGeneratedJsonRootDecoder =
    starts "__dark_json_decode_"
    && (not (starts "__dark_json_decode_list_"))
    && not (starts "__dark_json_decode_dict_")
  in
  name <> "_start"
  && (not isJsonOwnershipBoundary)
  && not isGeneratedJsonRootDecoder

(*
   Detect tail calls in a function
   Function body is always in tail position
*)
let detectTailCallsInFunctionWithRegistry recursiveMembers (func : functionDef)
    =
  if not (isEligibleFunctionName func.name) then func
  else
    let paramIds =
      TS.of_list
        (List.map (fun (param : typedParam) -> param.id) func.typedParams)
    in
    let ownedParams = leadingRetainedParams paramIds func.body in
    let isCurrentMember target =
      match
        ( FunctionIdMap.tryFind func.id recursiveMembers,
          FunctionIdMap.tryFind target recursiveMembers )
      with
      | Some current, Some target ->
          current.AST.typed.AST.resolved.AST.parsed.AST.binding
          = target.AST.typed.AST.resolved.AST.parsed.AST.binding
      | None, None -> target = func.id
      | _ -> false
    in
    let body =
      detectTailCalls func.id isCurrentMember func.typedParams ownedParams
        TS.empty true TM.empty TM.empty TS.empty func.body
    in
    { func with body }

let detectTailCallsInFunction func =
  detectTailCallsInFunctionWithRegistry FunctionIdMap.empty func

(*
   Detect tail calls in a program
   TCO is ENABLED - the DCE bug that caused 197 test failures has been fixed
   (DeadCodeElimination.ml was not recognizing TailCall as a function call)
*)
let detectTailCallsInProgram (Program (functions, main)) =
  Program (List.map detectTailCallsInFunction functions, main)

let detectTailCallsInProgramWithRecursion recursiveMembers
    (Program (functions, main)) =
  Program
    ( List.map (detectTailCallsInFunctionWithRegistry recursiveMembers) functions,
      main )
