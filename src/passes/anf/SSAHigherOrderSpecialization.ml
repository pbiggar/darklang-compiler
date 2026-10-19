(*
   Callable facts are intersected at block parameters. Clones keep SSA value and
   block identities local to each function and replace closure calls with direct
   calls that receive captured values as ordinary parameters.
*)
(* SSAHigherOrderSpecialization.ml - Specialize known callable arguments on typed SSA blocks. *)
[@@@warning "-4-30"]

module A = ANF
module S = SSAANF
module M = InliningCommon.TempMap
module Set = ANFEffects.TempSet
module IntSet = DirectCallFacts.IntSet

type convention = ClosureValue | StaticFunction

type callable = {
  target : AST.functionId;
  captures : A.atom list;
  convention : convention;
}

type knownArgument = { index : int; callable : callable }
type request = { helper : AST.functionId; arguments : knownArgument list }

type shape = {
  values : A.typedParam list;
  captureTypes : AST.semanticType list;
  closureId : A.tempId option;
}

type rewrittenCallable = {
  target : AST.functionId;
  sourceTarget : AST.functionId;
  convention : convention;
  captures : A.typedParam list;
}

type specialization = {
  functions : S.functionDef list;
  cloneOrigins : AST.functionId FunctionIdMap.t;
}

type purpose =
  | Optimization
  | AsyncInterfaces of SpecializationIdentity.FunctionSet.t

let optimization = function Optimization -> true | AsyncInterfaces _ -> false

let helperAllowed purpose helper =
  match purpose with
  | Optimization -> true
  | AsyncInterfaces helpers ->
      SpecializationIdentity.FunctionSet.mem helper helpers

let maxPairs = 16
let maxHelperNodes = 256
let maxTargetNodes = 32

let addInt left right =
  Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))

let operations (func : S.functionDef) =
  S.LabelMap.bindings func.S.blocks
  |> List.concat_map (fun (_, block) -> block.S.operations)

let tryItem index values = if index < 0 then None else List.nth_opt values index

let tryAtom (known : callable M.t) : A.atom -> callable option = function
  | A.Var id -> M.find_opt id known
  | A.FuncRef target ->
      Some { target; captures = []; convention = StaticFunction }
  | _ -> None

let instantiateReturn definitions returns name args =
  match
    (FunctionIdMap.tryFind name definitions, FunctionIdMap.tryFind name returns)
  with
  | Some (func : S.functionDef), Some (callable : callable)
    when List.length func.S.typedParams = List.length args ->
      let replacements =
        M.of_list
          (List.combine
             (List.map
                (fun (param : A.typedParam) -> param.A.id)
                func.S.typedParams)
             args)
      in
      Some
        {
          callable with
          captures =
            List.map
              (function
                | A.Var id as atom ->
                    Option.value ~default:atom (M.find_opt id replacements)
                | atom -> atom)
              callable.captures;
        }
  | _ -> None

let tryOperation definitions returns (known : callable M.t) :
    A.cExpr -> callable option = function
  | A.Atom atom | A.TypedAtom (atom, _) -> tryAtom known atom
  | A.IfValue (_, yes, no) -> (
      match (tryAtom known yes, tryAtom known no) with
      | Some left, Some right when left = right -> Some left
      | _ -> None)
  | A.ClosureAlloc (target, captures) ->
      Some { target; captures; convention = ClosureValue }
  | A.Call (name, args) | A.BorrowedCall (name, args) ->
      instantiateReturn definitions returns name args
  | _ -> None

let evaluateBlock definitions returns known (block : S.block) =
  List.fold_left
    (fun known (id, operation) ->
      match tryOperation definitions returns known operation with
      | Some callable -> M.add id callable known
      | None -> M.remove id known)
    known block.S.operations

let incomingFacts (func : S.functionDef) definitions returns facts =
  let edges =
    S.LabelMap.bindings func.S.blocks
    |> List.concat_map (fun (label, block) ->
        match S.LabelMap.find_opt label facts with
        | None -> []
        | Some known -> (
            let after = evaluateBlock definitions returns known block in
            let edge target args =
              match S.LabelMap.find_opt target func.S.blocks with
              | None ->
                  Crash.crash
                    "Higher-order specialization found a missing SSA block"
              | Some successor ->
                  let params =
                    M.of_list
                      (List.filter_map
                         (fun ((param : A.typedParam), atom) ->
                           Option.map
                             (fun callable -> (param.A.id, callable))
                             (tryAtom after atom))
                         (List.combine successor.S.parameters args))
                  in
                  let inherited =
                    List.fold_left
                      (fun known (param : A.typedParam) ->
                        M.remove param.A.id known)
                      after successor.S.parameters
                  in
                  [
                    ( target,
                      M.fold
                        (fun id callable known -> M.add id callable known)
                        params inherited );
                  ]
            in
            match block.S.terminator with
            | S.Return _ -> []
            | S.Jump (target, args) -> edge target args
            | S.Branch (_, yes, no) -> edge yes [] @ edge no []))
  in
  let groups =
    List.fold_left
      (fun groups (label, input) ->
        S.LabelMap.add label
          (Option.value ~default:[] (S.LabelMap.find_opt label groups)
          @ [ input ])
          groups)
      S.LabelMap.empty edges
  in
  S.LabelMap.map
    (function
      | [] -> M.empty
      | first :: rest ->
          M.filter
            (fun id callable ->
              List.for_all
                (fun input -> M.find_opt id input = Some callable)
                rest)
            first)
    groups

let blockFacts definitions returns (func : S.functionDef) =
  let rec solve remaining current =
    if remaining = 0 then current
    else
      let next =
        S.LabelMap.add func.S.entry M.empty
          (incomingFacts func definitions returns current)
      in
      if S.LabelMap.equal (M.equal ( = )) next current then current
      else solve (remaining - 1) next
  in
  solve
    (addInt
       (Int32.to_int
          (Int32.mul (Int32.of_int (S.LabelMap.cardinal func.S.blocks)) 4l))
       4)
    (S.LabelMap.singleton func.S.entry M.empty)

let knownAtOperations definitions returns (func : S.functionDef) =
  let entries = blockFacts definitions returns func in
  S.LabelMap.bindings func.S.blocks
  |> List.filter_map (fun (label, block) ->
      Option.map
        (fun initial ->
          let _, sites =
            List.fold_left
              (fun (known, sites) (id, operation) ->
                let sites = M.add id known sites in
                let known =
                  match tryOperation definitions returns known operation with
                  | Some callable -> M.add id callable known
                  | None -> M.remove id known
                in
                (known, sites))
              (initial, M.empty) block.S.operations
          in
          (label, sites))
        (S.LabelMap.find_opt label entries))
  |> List.to_seq |> S.LabelMap.of_seq

let returnFact definitions returns (func : S.functionDef) =
  let entries = blockFacts definitions returns func in
  let returned =
    S.LabelMap.bindings func.S.blocks
    |> List.filter_map (fun (label, block) ->
        match (S.LabelMap.find_opt label entries, block.S.terminator) with
        | Some known, S.Return atom ->
            Some (tryAtom (evaluateBlock definitions returns known block) atom)
        | _ -> None)
  in
  let params =
    Set.of_list
      (List.map (fun (param : A.typedParam) -> param.A.id) func.S.typedParams)
  in
  match returned with
  | Some first :: rest
    when List.for_all (( = ) (Some first)) rest
         && List.for_all
              (function A.Var id -> Set.mem id params | _ -> true)
              first.captures ->
      Some first
  | _ -> None

let buildReturns definitions =
  let rec solve remaining current =
    if remaining = 0 then current
    else
      let next =
        FunctionIdMap.fold
          (fun facts name func ->
            match returnFact definitions facts func with
            | Some callable -> FunctionIdMap.add name callable facts
            | None -> facts)
          current definitions
      in
      if FunctionIdMap.toList next = FunctionIdMap.toList current then current
      else solve (remaining - 1) next
  in
  solve (addInt (FunctionIdMap.count definitions) 1) FunctionIdMap.empty

let shape (target : S.functionDef) (callable : callable) =
  match callable.convention with
  | StaticFunction ->
      Some
        { values = target.S.typedParams; captureTypes = []; closureId = None }
  | ClosureValue -> (
      match target.S.typedParams with
      | closure :: values -> (
          match closure.A.typ with
          | AST.TTuple (AST.TInt64 :: captureTypes)
            when List.length captureTypes = List.length callable.captures ->
              Some { values; captureTypes; closureId = Some closure.A.id }
          | _ -> None)
      | [] -> None)

let atomUses = ANFEffects.atomUsesTemp
let operationUses = ANFEffects.cexprUsesTemp

let removeIndexes indexes items =
  List.filter_map
    (fun (index, value) ->
      if IntSet.mem index indexes then None else Some value)
    (List.mapi (fun index value -> (index, value)) items)

let terminatorUses id = function
  | S.Return atom -> atomUses id atom
  | S.Jump (_, args) -> List.exists (atomUses id) args
  | S.Branch (condition, _, _) -> atomUses id condition

let targetUsesOnlyCaptures closure count (target : S.functionDef) =
  S.LabelMap.for_all
    (fun _ block ->
      List.for_all
        (fun (_, operation) ->
          match operation with
          | A.TupleGet (A.Var id, index) when id = closure ->
              index >= 1 && index <= count
          | _ -> not (operationUses closure operation))
        block.S.operations
      && not (terminatorUses closure block.S.terminator))
    target.S.blocks

let helperUsesOnlyCalls (helper : S.functionDef) index parameter =
  let allowed = function
    | (A.ClosureCall (A.Var id, args) | A.ClosureTailCall (A.Var id, args))
      when id = parameter && not (List.exists (atomUses parameter) args) ->
        true
    | A.Call (name, args)
    | A.BorrowedCall (name, args)
    | A.TailCall (name, args)
      when name = helper.S.id -> (
        match tryItem index args with
        | Some (A.Var id) when id = parameter ->
            not
              (List.exists (atomUses parameter)
                 (removeIndexes (IntSet.singleton index) args))
        | _ -> false)
    | operation -> not (operationUses parameter operation)
  in
  S.LabelMap.for_all
    (fun _ block ->
      List.for_all (fun (_, operation) -> allowed operation) block.S.operations
      && not (terminatorUses parameter block.S.terminator))
    helper.S.blocks

let closureCallArity parameter helper =
  List.find_map
    (fun (_, operation) ->
      match operation with
      | (A.ClosureCall (A.Var id, args) | A.ClosureTailCall (A.Var id, args))
        when id = parameter ->
          Some (List.length args)
      | _ -> None)
    (operations helper)

(* A callback may be invoked, ignored, or forwarded to another source helper.
   Visit parameter positions rather than paths so recursive forwarding cycles
   terminate without imposing an optimization budget on interface correctness. *)
let helperUsesOnlyForwarding purpose definitions arity helper index =
  let rec check pending seen =
    match pending with
    | [] -> true
    | ((helper : S.functionDef), index) :: pending -> (
        let visited =
          Option.value ~default:IntSet.empty
            (FunctionIdMap.tryFind helper.S.id seen)
        in
        if IntSet.mem index visited then check pending seen
        else
          match tryItem index helper.S.typedParams with
          | Some { A.typ = AST.TFunction (arguments, _); id = parameter }
            when List.length arguments = arity -> (
              let seen =
                FunctionIdMap.add helper.S.id (IntSet.add index visited) seen
              in
              let forward pending = function
                | A.ClosureCall (A.Var id, args)
                | A.ClosureTailCall (A.Var id, args)
                  when id = parameter ->
                    if
                      List.length args = arity
                      && not (List.exists (atomUses parameter) args)
                    then Some pending
                    else None
                | ( A.Call (callee, args)
                  | A.BorrowedCall (callee, args)
                  | A.TailCall (callee, args) ) as operation
                  when operationUses parameter operation -> (
                    match FunctionIdMap.tryFind callee definitions with
                    | Some target
                      when helperAllowed purpose callee
                           && List.length args
                              = List.length target.S.typedParams ->
                        List.fold_left
                          (fun pending (index, atom) ->
                            Option.bind pending (fun pending ->
                                if not (atomUses parameter atom) then
                                  Some pending
                                else
                                  match atom with
                                  | A.Var id when id = parameter ->
                                      Some ((target, index) :: pending)
                                  | _ -> None))
                          (Some pending)
                          (List.mapi (fun index atom -> (index, atom)) args)
                    | _ -> None)
                | operation ->
                    if operationUses parameter operation then None
                    else Some pending
              in
              let pending =
                S.LabelMap.fold
                  (fun _ block pending ->
                    if terminatorUses parameter block.S.terminator then None
                    else
                      List.fold_left
                        (fun pending (_, operation) ->
                          Option.bind pending (fun pending ->
                              forward pending operation))
                        pending block.S.operations)
                  helper.S.blocks (Some pending)
              in
              match pending with
              | None -> false
              | Some pending -> check pending seen)
          | _ -> false)
  in
  check [ (helper, index) ] FunctionIdMap.empty

let validArgument purpose definitions (helper : S.functionDef) argument =
  match
    ( tryItem argument.index helper.S.typedParams,
      FunctionIdMap.tryFind argument.callable.target definitions )
  with
  | Some parameter, Some target -> (
      match shape target argument.callable with
      | Some shape ->
          let targetOk =
            match shape.closureId with
            | Some id ->
                targetUsesOnlyCaptures id
                  (List.length argument.callable.captures)
                  target
            | None -> true
          in
          ((not (optimization purpose))
          || addInt
               (S.LabelMap.cardinal target.S.blocks)
               (List.length (operations target))
             <= maxTargetNodes)
          && targetOk
          &&
          if optimization purpose then
            helperUsesOnlyCalls helper argument.index parameter.A.id
            && Option.fold ~none:false
                 ~some:(fun arity -> List.length shape.values = arity)
                 (closureCallArity parameter.A.id helper)
          else
            helperUsesOnlyForwarding purpose definitions
              (List.length shape.values) helper argument.index
      | None -> false)
  | _ -> false

let knownArguments purpose definitions known helper args =
  match
    if helperAllowed purpose helper then
      FunctionIdMap.tryFind helper definitions
    else None
  with
  | None -> []
  | Some helper ->
      List.filter
        (validArgument purpose definitions helper)
        (List.filter_map
           (fun (index, atom) ->
             Option.map
               (fun callable -> { index; callable })
               (tryAtom known atom))
           (List.mapi (fun index atom -> (index, atom)) args))

let directCall = function
  | A.Call (name, args) | A.BorrowedCall (name, args) | A.TailCall (name, args)
    ->
      Some (name, args)
  | _ -> None

let key request =
  ( request.helper,
    List.map
      (fun arg -> (arg.index, arg.callable.target, arg.callable.convention))
      request.arguments )

module Keys = Map.Make (struct
  type t = AST.functionId * (int * AST.functionId * convention) list

  let fid left right =
    Int64.unsigned_compare (AST.functionIdValue left)
      (AST.functionIdValue right)

  let rec items left right =
    match (left, right) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | (li, lf, lc) :: ls, (ri, rf, rc) :: rs ->
        let order = Int.compare li ri in
        if order <> 0 then order
        else
          let order = fid lf rf in
          if order <> 0 then order
          else
            let order = Stdlib.compare lc rc in
            if order <> 0 then order else items ls rs

  let compare (lf, ls) (rf, rs) =
    let order = fid lf rf in
    if order <> 0 then order else items ls rs
end)

let knownFor sites label id =
  Option.value ~default:M.empty
    (Option.bind (S.LabelMap.find_opt label sites) (M.find_opt id))

let mergeKnown parameters known =
  M.union (fun _ parameter _ -> Some parameter) parameters known

let callableParameters (helper : S.functionDef) request =
  List.fold_left
    (fun parameters argument ->
      match tryItem argument.index helper.S.typedParams with
      | Some parameter -> M.add parameter.A.id argument.callable parameters
      | None ->
          Crash.crash "Higher-order specialization lost a callback parameter")
    M.empty request.arguments

let requestsInFunction ?(parameters = M.empty) ?(forwardedOnly = false) purpose
    definitions returns (func : S.functionDef) =
  let sites = knownAtOperations definitions returns func in
  S.LabelMap.bindings func.S.blocks
  |> List.concat_map (fun (label, block) ->
      List.filter_map
        (fun (id, operation) ->
          match directCall operation with
          | None -> None
          | Some (helper, args)
            when (not forwardedOnly)
                 || List.exists
                      (function A.Var id -> M.mem id parameters | _ -> false)
                      args -> (
              match
                knownArguments purpose definitions
                  (mergeKnown parameters (knownFor sites label id))
                  helper args
              with
              | [] -> None
              | arguments -> Some { helper; arguments })
          | Some _ -> None)
        block.S.operations)

let forwardedRequests purpose definitions returns request =
  match FunctionIdMap.tryFind request.helper definitions with
  | Some helper ->
      requestsInFunction
        ~parameters:(callableParameters helper request)
        ~forwardedOnly:true purpose definitions returns helper
  | None -> Crash.crash "Higher-order forwarding lost a source helper"

let expandRequests purpose definitions returns requests =
  let add requests request = Keys.add (key request) request requests in
  let rec visit pending discovered =
    match Keys.min_binding_opt pending with
    | None -> Keys.bindings discovered |> List.map snd
    | Some (requestKey, request) ->
        let pending = Keys.remove requestKey pending in
        if Keys.mem requestKey discovered then visit pending discovered
        else
          let dependencies =
            forwardedRequests purpose definitions returns request
          in
          visit
            (List.fold_left add pending dependencies)
            (Keys.add requestKey request discovered)
  in
  visit (List.fold_left add Keys.empty requests) Keys.empty

let retainForwardingDependencies purpose definitions returns requests =
  let rec retain requests =
    let keys =
      Keys.of_list (List.map (fun request -> (key request, ())) requests)
    in
    let next =
      List.filter
        (fun request ->
          List.for_all
            (fun dependency -> Keys.mem (key dependency) keys)
            (forwardedRequests purpose definitions returns request))
        requests
    in
    if List.length next = List.length requests then requests else retain next
  in
  retain requests

let name definitions id =
  match FunctionIdMap.tryFind id definitions with
  | Some func -> func.S.name
  | None ->
      Crash.crash "Higher-order specialization lost function display metadata"

let targetName definitions id = name definitions id ^ "__captures"

let helperName definitions request =
  name definitions request.helper
  ^ "__known_"
  ^ String.concat "__"
      (List.map
         (fun arg ->
           name definitions arg.callable.target ^ "_" ^ string_of_int arg.index)
         request.arguments)

let newParameters types (func : S.functionDef) =
  let greatest =
    List.fold_left
      (fun largest (param : A.typedParam) ->
        let (A.TempId id) = param.A.id in
        max largest id)
      (M.fold
         (fun (A.TempId id) _ largest -> max largest id)
         func.S.freshValueTypes 4000)
      func.S.typedParams
  in
  List.mapi
    (fun index typ ->
      { A.id = A.TempId (addInt (addInt greatest index) 1); typ })
    types

let appendTypes params (func : S.functionDef) =
  {
    func with
    S.freshValueTypes =
      List.fold_left
        (fun types (param : A.typedParam) -> M.add param.A.id param.A.typ types)
        func.S.freshValueTypes params;
  }

let mapOperations transform (func : S.functionDef) =
  {
    func with
    S.blocks =
      S.LabelMap.map
        (fun block ->
          { block with S.operations = List.map transform block.S.operations })
        func.S.blocks;
  }

let required definitions id =
  match FunctionIdMap.tryFind id definitions with
  | Some func -> func
  | None -> Crash.crash "Higher-order specialization lost a validated function"

let generated names name =
  match StringOrder.Map.find_opt name names with
  | Some identity -> identity
  | None ->
      Crash.crash
        ("Higher-order specialization lost generated name '" ^ name ^ "'")

let generatedId names name = fst (generated names name)
let generatedName names name = snd (generated names name)

let cloneTarget definitions names (callable : callable) =
  let original = required definitions callable.target in
  let targetShape =
    match shape original callable with
    | Some value -> value
    | None -> Crash.crash "Higher-order specialization lost target shape"
  in
  let closure =
    match targetShape.closureId with
    | Some id -> id
    | None ->
        Crash.crash "Higher-order specialization expected a closure target"
  in
  let captures = newParameters targetShape.captureTypes original in
  let rewrite (id, operation) =
    ( id,
      match operation with
      | A.TupleGet (A.Var tuple, index) when tuple = closure -> (
          match tryItem (addInt index (-1)) captures with
          | Some param -> A.Atom (A.Var param.A.id)
          | None -> Crash.crash "Higher-order specialization lost a capture")
      | _ -> operation )
  in
  let cloneName = targetName definitions callable.target in
  appendTypes captures
    {
      (mapOperations rewrite original) with
      S.id = generatedId names cloneName;
      name = generatedName names cloneName;
      typedParams = captures @ targetShape.values;
    }

let cloneHelper purpose definitions returns names requests request =
  let original = required definitions request.helper in
  let indexed =
    List.map
      (fun arg ->
        let target = required definitions arg.callable.target in
        let shape =
          match shape target arg.callable with
          | Some value -> value
          | None ->
              Crash.crash "Higher-order specialization lost callable shape"
        in
        (arg, shape.captureTypes))
      request.arguments
  in
  let captures = newParameters (List.concat_map snd indexed) original in
  let _, rewritten =
    List.fold_left
      (fun (remaining, mapped) (arg, types) ->
        let own =
          List.filter_map
            (fun (index, param) ->
              if index < List.length types then Some param else None)
            (List.mapi (fun index param -> (index, param)) remaining)
        in
        let rest =
          List.filter_map
            (fun (index, param) ->
              if index >= List.length types then Some param else None)
            (List.mapi (fun index param -> (index, param)) remaining)
        in
        let param =
          match tryItem arg.index original.S.typedParams with
          | Some value -> value
          | None ->
              Crash.crash "Higher-order specialization lost helper parameter"
        in
        let target =
          match arg.callable.convention with
          | StaticFunction -> arg.callable.target
          | ClosureValue ->
              generatedId names (targetName definitions arg.callable.target)
        in
        let value : rewrittenCallable =
          {
            target;
            sourceTarget = arg.callable.target;
            convention = arg.callable.convention;
            captures = own;
          }
        in
        (rest, M.add param.A.id value mapped))
      (captures, M.empty) indexed
  in
  let cloneName = helperName definitions request in
  let cloneId = generatedId names cloneName in
  let indexes =
    IntSet.of_list (List.map (fun arg -> arg.index) request.arguments)
  in
  let appended =
    List.map (fun (param : A.typedParam) -> A.Var param.A.id) captures
  in
  let parameters =
    M.map
      (fun (value : rewrittenCallable) ->
        ({
           target = value.sourceTarget;
           captures =
             List.map
               (fun (param : A.typedParam) -> A.Var param.A.id)
               value.captures;
           convention = value.convention;
         }
          : callable))
      rewritten
  in
  let sites =
    if optimization purpose then S.LabelMap.empty
    else knownAtOperations definitions returns original
  in
  let specialized =
    Keys.of_list
      (List.map
         (fun request ->
           (key request, generatedId names (helperName definitions request)))
         requests)
  in
  let rewrite label (id, operation) =
    ( id,
      match operation with
      | A.ClosureCall (A.Var param, args) | A.ClosureTailCall (A.Var param, args)
        -> (
          match M.find_opt param rewritten with
          | None -> operation
          | Some (callable : rewrittenCallable) -> (
              let captureArgs =
                match callable.convention with
                | ClosureValue ->
                    List.map
                      (fun (param : A.typedParam) -> A.Var param.A.id)
                      callable.captures
                | StaticFunction -> []
              in
              match operation with
              | A.ClosureTailCall _ ->
                  A.TailCall (callable.target, captureArgs @ args)
              | _ -> A.Call (callable.target, captureArgs @ args)))
      | A.Call (callee, args)
      | A.BorrowedCall (callee, args)
      | A.TailCall (callee, args)
        when not (optimization purpose) -> (
          let known = mergeKnown parameters (knownFor sites label id) in
          let arguments =
            knownArguments purpose definitions known callee args
          in
          match
            Keys.find_opt (key { helper = callee; arguments }) specialized
          with
          | None -> operation
          | Some target -> (
              let indexes =
                IntSet.of_list (List.map (fun arg -> arg.index) arguments)
              in
              let args =
                removeIndexes indexes args
                @ List.concat_map (fun arg -> arg.callable.captures) arguments
              in
              match operation with
              | A.Call _ -> A.Call (target, args)
              | A.BorrowedCall _ -> A.BorrowedCall (target, args)
              | A.TailCall _ -> A.TailCall (target, args)
              | _ -> Crash.crash "Higher-order forwarding lost a direct call"))
      | A.Call (name, args) when name = original.S.id ->
          A.Call (cloneId, removeIndexes indexes args @ appended)
      | A.BorrowedCall (name, args) when name = original.S.id ->
          A.BorrowedCall (cloneId, removeIndexes indexes args @ appended)
      | A.TailCall (name, args) when name = original.S.id ->
          A.TailCall (cloneId, removeIndexes indexes args @ appended)
      | _ -> operation )
  in
  appendTypes captures
    {
      original with
      S.id = cloneId;
      name = generatedName names cloneName;
      typedParams = removeIndexes indexes original.S.typedParams @ captures;
      blocks =
        S.LabelMap.mapi
          (fun label block ->
            {
              block with
              S.operations = List.map (rewrite label) block.S.operations;
            })
          original.S.blocks;
    }

(*
   Closure allocations and aliases made dead by routing are removed by the
   following SSA cleanup. Preserve the ownership-visible operation order.
*)
let rewriteKnownCalls purpose definitions returns names requests
    (func : S.functionDef) =
  let sites = knownAtOperations definitions returns func in
  let specialized =
    Keys.of_list
      (List.map
         (fun request ->
           (key request, generatedId names (helperName definitions request)))
         requests)
  in
  let rewritten =
    {
      func with
      S.blocks =
        S.LabelMap.mapi
          (fun label block ->
            {
              block with
              S.operations =
                List.map
                  (fun (id, operation) ->
                    let known = knownFor sites label id in
                    let operation =
                      match directCall operation with
                      | None -> operation
                      | Some (helper, args) -> (
                          let knownArgs =
                            knownArguments purpose definitions known helper args
                          in
                          let request = { helper; arguments = knownArgs } in
                          match Keys.find_opt (key request) specialized with
                          | None -> operation
                          | Some target -> (
                              let indexes =
                                IntSet.of_list
                                  (List.map (fun arg -> arg.index) knownArgs)
                              in
                              let args =
                                removeIndexes indexes args
                                @ List.concat_map
                                    (fun arg -> arg.callable.captures)
                                    knownArgs
                              in
                              match operation with
                              | A.Call _ -> A.Call (target, args)
                              | A.BorrowedCall _ -> A.BorrowedCall (target, args)
                              | A.TailCall _ -> A.TailCall (target, args)
                              | _ -> operation))
                    in
                    (id, operation))
                  block.S.operations;
            })
          func.S.blocks;
    }
  in
  if optimization purpose then
    SSADirectCallSpecialization.removeUnusedRematerializedValues
      (FunctionIdMap.map (fun _ func -> func.S.name) definitions)
      rewritten
  else rewritten

(*
   Empty request sets still run rewriteKnownCalls for rematerialization cleanup.
   The forward catalog and allocation cursor avoid rebuilding global indexes.
*)
let specializeProgramWithExternalFunctionsAndNames ?(purpose = Optimization)
    reservedIds nextOrdinal externals functions =
  let definitions =
    FunctionIdMap.ofList
      (List.map
         (fun (func : S.functionDef) -> (func.S.id, func))
         (externals @ functions))
  in
  let returns = buildReturns definitions in
  let all =
    List.concat_map (requestsInFunction purpose definitions returns) functions
    |> List.filter (fun request ->
        let helper = required definitions request.helper in
        (not (optimization purpose))
        || addInt
             (S.LabelMap.cardinal helper.S.blocks)
             (List.length (operations helper))
           <= maxHelperNodes)
  in
  let unique =
    List.fold_left
      (fun unique request ->
        let key = key request in
        if Keys.mem key unique then unique else Keys.add key request unique)
      Keys.empty all
    |> Keys.bindings |> List.map snd
  in
  let unique =
    if optimization purpose then unique
    else expandRequests purpose definitions returns unique
  in
  let _, requests =
    List.fold_left
      (fun (remaining, retained) request ->
        let cost = List.length request.arguments in
        if cost <= remaining then (remaining - cost, request :: retained)
        else (remaining, retained))
      (maxPairs, []) unique
  in
  let requests = if optimization purpose then List.rev requests else unique in
  let targetNames =
    StringOrder.Set.of_list
      (List.filter_map
         (fun arg ->
           if arg.callable.convention = ClosureValue then
             Some (targetName definitions arg.callable.target)
           else None)
         (List.concat_map (fun request -> request.arguments) requests))
  in
  let helperNames =
    StringOrder.Set.of_list (List.map (helperName definitions) requests)
  in
  let exists name =
    StringOrder.Map.mem name reservedIds
    || FunctionIdMap.exists (fun _ func -> func.S.name = name) definitions
  in
  let usable =
    List.filter
      (fun request ->
        let helper = helperName definitions request in
        let targets =
          List.filter_map
            (fun arg ->
              if arg.callable.convention = ClosureValue then
                Some (targetName definitions arg.callable.target)
              else None)
            request.arguments
        in
        ((not (optimization purpose)) || not (exists helper))
        && (not (StringOrder.Set.mem helper targetNames))
        && List.for_all
             (fun target ->
               ((not (optimization purpose)) || not (exists target))
               && not (StringOrder.Set.mem target helperNames))
             targets)
      requests
  in
  let usable =
    if optimization purpose then usable
    else retainForwardingDependencies purpose definitions returns usable
  in
  let names =
    if requests = [] then StringOrder.Map.empty
    else
      let next =
        if FunctionIdMap.isEmpty definitions then nextOrdinal
        else
          let id, _ = FunctionIdMap.maxKeyValue definitions in
          let after = AST.nextFunctionIdOrdinal (AST.functionIdValue id) in
          if Int64.unsigned_compare nextOrdinal after >= 0 then nextOrdinal
          else after
      in
      let requested = StringOrder.Set.union targetNames helperNames in
      (* Optional optimization may skip precompiled clones. Async interface
         preparation must instead create a fresh logical version: the existing
         native clone may have a concrete interface from an earlier unit. *)
      let _, displays =
        StringOrder.Set.fold
          (fun key (used, displays) ->
            let rec fresh index =
              let candidate =
                key ^ "__async_" ^ Int64.to_string next ^ "_"
                ^ string_of_int index
              in
              if
                exists candidate
                || StringOrder.Set.mem candidate used
                || StringOrder.Set.mem candidate requested
              then fresh (addInt index 1)
              else candidate
            in
            let display =
              if optimization purpose || not (exists key) then key else fresh 0
            in
            ( StringOrder.Set.add display used,
              StringOrder.Map.add key display displays ))
          requested
          (StringOrder.Set.empty, StringOrder.Map.empty)
      in
      let allocated =
        AST.allocateFunctionIdsFromOrdinal next
          (StringOrder.Map.to_seq displays |> Seq.map snd)
      in
      StringOrder.Map.map
        (fun display ->
          match StringOrder.Map.find_opt display allocated with
          | Some id -> (id, display)
          | None ->
              Crash.crash
                "Higher-order specialization lost an allocated display name")
        displays
  in
  let _, targets =
    List.fold_left
      (fun (seen, retained) arg ->
        let (callable : callable) = arg.callable in
        if
          callable.convention <> ClosureValue
          || DirectCallFacts.FunctionSet.mem callable.target seen
        then (seen, retained)
        else
          ( DirectCallFacts.FunctionSet.add callable.target seen,
            callable :: retained ))
      (DirectCallFacts.FunctionSet.empty, [])
      (List.concat_map (fun request -> request.arguments) usable)
  in
  let targetCallables = List.rev targets in
  let targets = List.map (cloneTarget definitions names) targetCallables in
  let helpers =
    List.map (cloneHelper purpose definitions returns names usable) usable
  in
  let rewritten =
    List.map
      (rewriteKnownCalls purpose definitions returns names usable)
      functions
  in
  let origins =
    FunctionIdMap.ofList
      (List.map
         (fun (callable : callable) ->
           ( generatedId names (targetName definitions callable.target),
             callable.target ))
         targetCallables
      @ List.map
          (fun request ->
            (generatedId names (helperName definitions request), request.helper))
          usable)
  in
  { functions = targets @ rewritten @ helpers; cloneOrigins = origins }
