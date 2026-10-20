(* LowerAsyncDemands.ml - Infer pending call interfaces and demand concrete values before ownership. *)
[@@@warning "-4"]

module A = ANF
module M = InliningCommon.TempMap
module F = FunctionIdMap
module S = SpecializationIdentity.FunctionSet
module T = ANFEffects.TempSet
module Names = StringOrder.Map

let ( let* ) = Result.bind

type pending = { payload : AST.semanticType; force : AST.functionId }
type version = { id : AST.functionId; name : string }

type lowered = {
  functions : A.functionDef list;
  nextFunctionOrdinal : int64;
  sumShapes : MemoryModel.rcSumShapeRegistry;
}

type generation = {
  variables : A.varGen;
  cursor : int64;
  generatedFunctions : A.functionDef list;
  mappers : version Names.t;
}

let freshVar gen =
  let id, variables = A.freshVar gen.variables in
  (id, { gen with variables })

type failure =
  | Diagnostic of string
  | MissingRuntimeSpecializations of
      SpecializationIdentity.SpecSet.t * MemoryModel.rcSumShapeRegistry
  | MissingSourceSpecializations of
      SpecializationIdentity.SpecSet.t * MemoryModel.rcSumShapeRegistry

let carrier payload = AST.TSum ("Darklang.Stdlib.__Async.__Value", [ payload ])
let transportName = AsyncSourceHooks.transportName
let delayName = AsyncSourceHooks.delayName
let specialized base name = String.starts_with ~prefix:(base ^ "_") name
let lookup env = function A.Var id -> M.find_opt id env | _ -> None

(* Stored fields retain independent pending layouts, including outer readiness.
   Nominal records use a structural physical type during transport; native
   adapters rebuild their source descriptor after materializing fields. *)
type storageKind = AsyncTransport.storageKind =
  | TupleStorage
  | RecordStorage of A.recordDescriptor
  | ListStorage of AST.semanticType
  | DictStorage of AST.semanticType * AST.semanticType
  | SumStorage of AsyncSumStorage.layout

type 'a storedSlot = 'a AsyncTransport.storedSlot =
  | PendingSlot of 'a
  | NestedStorage of 'a storage
  | PendingStorage of 'a * 'a storage

and 'a storage = 'a AsyncTransport.storage

type mode = AsyncTransport.mode =
  | Concrete
  | Pending
  | Stored of unit storage
  | PendingStored of unit storage

type dictionaryCall = {
  dictionaryOperation : AsyncDictCalls.operation;
  dictionaryArguments : (AsyncDictCalls.argument * A.atom) list;
  dictionaryKey : AST.semanticType;
  dictionaryValue : AST.semanticType;
  dictionaryValueMode : mode;
  dictionaryResult : mode;
  dictionarySourceResult : AST.semanticType;
}

type dictionaryMappingKind = MapDictionaryValues | FilterDictionaryValues

type dictionaryMapping = {
  dictionaryMappingKind : dictionaryMappingKind;
  mappedDictionary : A.atom;
  dictionaryCallback : A.atom;
  dictionaryCallbackTarget : AST.functionId;
  mapKeyType : AST.semanticType;
  mapInputType : AST.semanticType;
  mapOutputType : AST.semanticType;
  actualDictionaryValue : mode;
}

type listFolding = {
  foldList : A.atom;
  initial : A.atom;
  step : A.atom;
  stepTarget : AST.functionId;
  elementType : AST.semanticType;
  accumulatorType : AST.semanticType;
  actualFoldElement : mode;
  actualInitial : mode;
}

type listPredicate =
  | FilterPredicate
  | AnyPredicate
  | AllPredicate
  | TakeWhilePredicate
  | DropWhilePredicate

type listMappingKind = MapElements | PredicateElements of listPredicate

type listMapping = {
  mappingKind : listMappingKind;
  listValue : A.atom;
  callback : A.atom;
  callbackTarget : AST.functionId;
  inputType : AST.semanticType;
  outputType : AST.semanticType;
  actualElement : mode;
}

type listPrefix = TakePrefix | DropPrefix

type listResult =
  | ListScalar
  | ListElement
  | ListRemainder
  | ListPrefix of listPrefix

let listResult outer storage = function
  | ListScalar -> (None, None)
  | ListRemainder | ListPrefix TakePrefix -> (None, Some storage)
  | ListPrefix DropPrefix -> (outer, Some storage)
  | ListElement -> (
      match AsyncTransport.fields storage with
      | [ None ] -> (None, None)
      | [ Some (PendingSlot value) ] -> (Some value, None)
      | [ Some (NestedStorage fields) ] -> (None, Some fields)
      | [ Some (PendingStorage (value, fields)) ] -> (Some value, Some fields)
      | _ ->
          Crash.crash "Async list selection has an inconsistent element layout")

type summary = {
  parameters : mode list;
  captures : mode list;
  result : mode;
  joins : mode M.t;
  rawLists : mode M.t;
  physicalSumShapes : MemoryModel.rcSumShapeRegistry;
}

let equalSummary left right =
  left == right
  || List.equal AsyncTransport.equal left.parameters right.parameters
     && List.equal AsyncTransport.equal left.captures right.captures
     && AsyncTransport.equal left.result right.result
     && M.equal AsyncTransport.equal left.joins right.joins
     && M.equal AsyncTransport.equal left.rawLists right.rawLists
     && left.physicalSumShapes = right.physicalSumShapes

let changed = AsyncTransport.changed

let modeOf env stores atom =
  match (lookup env atom, lookup stores atom) with
  | Some _, Some storage -> PendingStored (AsyncTransport.interface storage)
  | Some _, None -> Pending
  | None, Some storage -> Stored (AsyncTransport.interface storage)
  | None, None -> Concrete

let modeFacts transform typ = function
  | Concrete -> Ok (None, None)
  | Pending ->
      let* value = transform typ in
      Ok (Some value, None)
  | Stored storage ->
      let* storage = AsyncTransport.instantiateStorage transform storage typ in
      Ok (None, Some storage)
  | PendingStored storage ->
      let* value =
        transform (AsyncTransport.physicalType (Stored storage) typ)
      in
      let* storage = AsyncTransport.instantiateStorage transform storage typ in
      Ok (Some value, Some storage)

let bindMode transform env stores id typ mode =
  let* value, storage = modeFacts transform typ mode in
  let bind map = function
    | None -> M.remove id map
    | Some value -> M.add id value map
  in
  Ok (bind env value, bind stores storage)

let listMode element mode =
  if not (changed mode) then Concrete
  else
    Stored
      (AsyncTransport.storage (ListStorage element)
         [ AsyncTransport.modeSlot mode ])

let listElementMode storage =
  let storage = AsyncTransport.interface storage in
  match (AsyncTransport.kind storage, AsyncTransport.fields storage) with
  | ListStorage _, [ slot ] -> AsyncTransport.slotMode slot
  | _ -> Crash.crash "Async list transport has an inconsistent element layout"

let dictionaryMode key value valueMode =
  if not (changed valueMode) then Concrete
  else
    Stored
      (AsyncTransport.storage
         (DictStorage (key, value))
         [ AsyncTransport.modeSlot valueMode ])

let dictionaryValueMode storage =
  let storage = AsyncTransport.interface storage in
  match (AsyncTransport.kind storage, AsyncTransport.fields storage) with
  | DictStorage _, [ slot ] -> AsyncTransport.slotMode slot
  | _ ->
      Crash.crash "Async dictionary transport has an inconsistent value layout"

let storageField = AsyncTransport.field

let sumField storage tag =
  match AsyncTransport.kind storage with
  | SumStorage layout -> (
      match AsyncSumStorage.payloadIndex layout tag with
      | Some index -> storageField storage index
      | None ->
          Crash.crash "Async variant projection selected an empty constructor")
  | _ -> Crash.crash "Async variant projection has a non-variant field layout"

let pendingField = function
  | Some (PendingSlot value) | Some (PendingStorage (value, _)) -> Some value
  | None | Some (NestedStorage _) -> None

let storageFacts sumShapes env stores operation =
  let slot atom =
    match (lookup env atom, lookup stores atom) with
    | Some value, Some storage -> Some (PendingStorage (value, storage))
    | Some value, None -> Some (PendingSlot value)
    | None, Some storage -> Some (NestedStorage storage)
    | None, None -> None
  in
  let allocation kind values =
    let fields = List.map slot values in
    if List.exists Option.is_some fields then
      Some (AsyncTransport.storage kind fields)
    else None
  in
  match operation with
  | A.TupleAlloc values -> allocation TupleStorage values
  | A.SumAlloc (variant, payload) -> (
      match slot payload with
      | None -> None
      | Some value ->
          let layout =
            match AsyncSumStorage.layout sumShapes variant.A.sum with
            | Ok layout -> layout
            | Error message -> Crash.crash message
          in
          let fields =
            List.map
              (fun (tag, _) ->
                if tag = variant.A.variantTag then Some value else None)
              layout.AsyncSumStorage.payloads
          in
          if not (List.exists Option.is_some fields) then
            Crash.crash
              "Async payload constructor is absent from its sum metadata";
          Some (AsyncTransport.storage (SumStorage layout) fields))
  | A.RecordAlloc (({ A.valueType = AST.TRecord _; _ } as descriptor), values)
  | A.RecordClone (({ A.valueType = AST.TRecord _; _ } as descriptor), _, values)
    ->
      allocation (RecordStorage descriptor) values
  | A.Atom atom | A.TypedAtom (atom, _) -> lookup stores atom
  | A.SumPayload (variant, atom) -> (
      match
        Option.bind (lookup stores atom) (fun storage ->
            sumField storage variant.A.variantTag)
      with
      | Some (NestedStorage fields) | Some (PendingStorage (_, fields)) ->
          Some fields
      | None | Some (PendingSlot _) -> None)
  | A.TupleGet (atom, index) | A.RecordGet (_, atom, index) -> (
      match lookup stores atom with
      | Some fields -> (
          match storageField fields index with
          | Some (NestedStorage fields) | Some (PendingStorage (_, fields)) ->
              Some fields
          | _ -> None)
      | None -> None)
  | _ -> None

(* Closing a nominal cycle can widen a pending field's physical payload. Build
   its typed force facts from the closed interface, not from the old tree node. *)
let normalizeAllocation transform operation storage =
  let source =
    match operation with
    | A.SumAlloc (variant, _) ->
        Some (AST.TSum (variant.A.sum.A.sumName, variant.A.sum.A.sumArgs))
    | A.RecordAlloc (descriptor, _) | A.RecordClone (descriptor, _, _) ->
        Some descriptor.A.valueType
    | _ -> None
  in
  match (storage, source) with
  | Some storage, Some source ->
      Result.map Option.some
        (AsyncTransport.instantiateStorage transform
           (AsyncTransport.interface storage)
           source)
  | _ -> Ok storage

let storageType = AsyncTransport.storageType
let slotRepresentation typ slot = AsyncTransport.slotType slot typ
let physicalSumDescriptor = AsyncTransport.sumDescriptor
let storageDescriptor = AsyncTransport.recordDescriptor

let nativeRuntime name =
  List.exists
    (fun prefix -> String.starts_with ~prefix name)
    [
      "Darklang.Stdlib.__Task.";
      "Darklang.Stdlib.__Async.";
      "Darklang.Stdlib.__Network.";
      "__dark_async_";
    ]

let lower registry names ids sumShapes nextFunctionOrdinal externalDefinitions
    functions =
  let name id = F.tryFind id names in
  let producer id =
    match name id with
    | Some name -> AsyncSourceHooks.isProducerName name
    | None -> false
  in
  let producerStorage stores callee arguments =
    match (name callee, arguments) with
    | Some name, [ _; argument ] when specialized delayName name ->
        lookup stores argument
    | _ -> None
  in
  if not (F.exists (fun id _ -> producer id) names) then
    Ok { functions; nextFunctionOrdinal; sumShapes }
  else
    let functions = List.map AsyncProjectionPruning.prune functions in
    let externalDefinitions =
      Names.map AsyncProjectionPruning.prune externalDefinitions
    in
    let definitions =
      Names.fold
        (fun _ (fn : A.functionDef) map -> F.add fn.A.id fn map)
        externalDefinitions F.empty
      |> fun map ->
      List.fold_left
        (fun map (fn : A.functionDef) -> F.add fn.A.id fn map)
        map functions
    in
    let originalResult id =
      match F.tryFind id definitions with
      | Some fn -> fn.A.returnType
      | None -> (
          match F.tryFind id registry with
          | Some (_, AST.TFunction (_, result)) -> result
          | _ -> Crash.crash "Async source operation has no typed signature")
    in
    let summary state id =
      match F.tryFind id state with
      | Some value -> value
      | None -> Crash.crash "Async inference lost a function summary"
    in
    let target bindings = function
      | A.FuncRef id -> Some id
      | A.Var id -> M.find_opt id bindings
      | _ -> None
    in
    let nativeEntry id =
      match name id with
      | Some name ->
          List.exists
            (fun base -> specialized ("Darklang.Stdlib.__Task." ^ base) name)
            [ "__runMain"; "__startCurrent"; "__startValueCurrent" ]
      | None -> false
    in
    let rec entries bindings found = function
      | A.Return _ | A.Jump _ -> found
      | A.If (_, yes, no) -> entries bindings (entries bindings found yes) no
      | A.Join (_, continuation, entry) ->
          entries bindings (entries bindings found continuation) entry
      | A.Let (id, expression, rest) ->
          let found =
            match expression with
            | A.Call (callee, [ entry ]) when nativeEntry callee -> (
                match target bindings entry with
                | Some entry -> S.add entry found
                | None -> found)
            | _ -> found
          in
          let bindings =
            match expression with
            | A.ClosureAlloc (callee, _) -> M.add id callee bindings
            | A.Atom atom | A.TypedAtom (atom, _) -> (
                match target bindings atom with
                | Some callee -> M.add id callee bindings
                | None -> M.remove id bindings)
            | _ -> M.remove id bindings
          in
          entries bindings found rest
    in
    let nativeEntries =
      F.fold
        (fun found _ fn -> entries M.empty found fn.A.body)
        S.empty definitions
    in
    let listPlans =
      F.map (fun _ fn -> AsyncListStorage.create fn) definitions
    in
    let listPlan (fn : A.functionDef) =
      match F.tryFind fn.A.id listPlans with
      | Some plan -> plan
      | None -> Crash.crash "Async list analysis lost a source plan"
    in
    let rawListMode state (fn : A.functionDef) component =
      match M.find_opt component (summary state fn.A.id).rawLists with
      | Some mode -> mode
      | None -> Concrete
    in
    let dictionaryCall env stores operation =
      match operation with
      | A.Call (callee, arguments)
      | A.BorrowedCall (callee, arguments)
      | A.TailCall (callee, arguments) -> (
          match Option.bind (name callee) AsyncDictCalls.operation with
          | None -> Ok None
          | Some operation ->
              if
                List.length arguments
                <> List.length operation.AsyncDictCalls.arguments
              then
                Error
                  "Async dictionary operation has an inconsistent argument \
                   count"
              else
                let parameters, result =
                  match F.tryFind callee registry with
                  | Some (_, AST.TFunction (parameters, result)) ->
                      (parameters, result)
                  | _ ->
                      Crash.crash
                        "Async dictionary operation lost its source signature"
                in
                let key, value =
                  match
                    List.find_map
                      (function
                        | AST.TDict (key, value) -> Some (key, value)
                        | _ -> None)
                      (result :: parameters)
                  with
                  | Some types -> types
                  | None ->
                      Crash.crash
                        "Async dictionary operation has no dictionary type"
                in
                let arguments =
                  List.combine operation.AsyncDictCalls.arguments arguments
                in
                let* valueMode =
                  List.fold_left
                    (fun result (role, atom) ->
                      let* mode = result in
                      let actual = modeOf env stores atom in
                      let valueMode =
                        match (role, actual) with
                        | AsyncDictCalls.Value, mode -> mode
                        | ( AsyncDictCalls.Dictionary,
                            (Stored storage | PendingStored storage) ) ->
                            dictionaryValueMode storage
                        | AsyncDictCalls.Dictionary, (Concrete | Pending)
                        | AsyncDictCalls.Key, _ ->
                            Concrete
                      in
                      AsyncTransport.merge mode valueMode)
                    (Ok Concrete) arguments
                in
                if
                  not
                    (List.exists
                       (fun (_, atom) -> changed (modeOf env stores atom))
                       arguments)
                then Ok None
                else
                  let* resultMode =
                    match operation.AsyncDictCalls.result with
                    | AsyncDictCalls.DictionaryResult ->
                        Ok (dictionaryMode key value valueMode)
                    | AsyncDictCalls.ScalarResult -> Ok Concrete
                    | AsyncDictCalls.ValuesResult ->
                        Ok (listMode value valueMode)
                    | AsyncDictCalls.EntriesResult ->
                        let entry = AST.TTuple [ key; value ] in
                        let entryMode =
                          if changed valueMode then
                            Stored
                              (AsyncTransport.storage TupleStorage
                                 [ None; AsyncTransport.modeSlot valueMode ])
                          else Concrete
                        in
                        Ok (listMode entry entryMode)
                    | AsyncDictCalls.LookupResult -> (
                        if not (changed valueMode) then Ok Concrete
                        else
                          let source =
                            match result with
                            | AST.TSum (sumName, sumArgs) ->
                                { A.sumName; sumArgs }
                            | _ ->
                                Crash.crash
                                  "Async dictionary lookup has a non-option \
                                   result"
                          in
                          let* layout =
                            AsyncSumStorage.layout sumShapes source
                          in
                          match layout.AsyncSumStorage.payloads with
                          | [ (_, typ) ] when typ = value ->
                              Ok
                                (Stored
                                   (AsyncTransport.storage (SumStorage layout)
                                      [ AsyncTransport.modeSlot valueMode ]))
                          | _ ->
                              Crash.crash
                                "Async dictionary lookup has an inconsistent \
                                 option payload")
                  in
                  Ok
                    (Some
                       {
                         dictionaryOperation = operation;
                         dictionaryArguments = arguments;
                         dictionaryKey = key;
                         dictionaryValue = value;
                         dictionaryValueMode = valueMode;
                         dictionaryResult = resultMode;
                         dictionarySourceResult = result;
                       }))
      | _ -> Ok None
    in
    let listCall stores callee arguments =
      match (name callee, arguments) with
      | Some calleeName, list :: rest -> (
          let operations =
            [
              ("length", ListScalar);
              ("isEmpty", ListScalar);
              ("__length", ListScalar);
              ("__isEmpty", ListScalar);
              ("__isNull", ListScalar);
              ("__getTag", ListScalar);
              ("__headUnsafe", ListElement);
              ("__tail", ListRemainder);
              ("reverse", ListRemainder);
              ("__reverse", ListRemainder);
              ("dropLast", ListRemainder);
              ("__init", ListRemainder);
              ("take", ListPrefix TakePrefix);
              ("drop", ListPrefix DropPrefix);
            ]
          in
          let operation =
            List.find_opt
              (fun (base, _) ->
                specialized ("Darklang.Stdlib.List." ^ base) calleeName)
              operations
          in
          let operation =
            match operation with
            | Some _ as operation -> operation
            | None -> (
                match calleeName with
                | "Darklang.Stdlib.List.__lengthFloat" ->
                    Some ("__length", ListScalar)
                | "Darklang.Stdlib.List.__headUnsafeInt64"
                | "Darklang.Stdlib.List.__headUnsafeFloat" ->
                    Some ("__headUnsafe", ListElement)
                | _ -> None)
          in
          match (operation, lookup stores list) with
          | Some (base, result), Some storage
            when match AsyncTransport.kind storage with
                 | ListStorage _ -> true
                 | _ -> false ->
              Some ("Darklang.Stdlib.List." ^ base, list, rest, storage, result)
          | _ -> None)
      | _ -> None
    in
    let listBuildCall env stores = function
      | A.Call (callee, arguments)
      | A.BorrowedCall (callee, arguments)
      | A.TailCall (callee, arguments) -> (
          let builder = Option.bind (name callee) AsyncListCalls.builder in
          match builder with
          | None -> Ok None
          | Some builder -> (
              if
                List.length arguments
                <> List.length builder.AsyncListCalls.arguments
              then Error "Async list builder has an inconsistent argument count"
              else
                let element =
                  match originalResult callee with
                  | AST.TList element -> element
                  | _ -> Crash.crash "Async list builder has a non-list result"
                in
                let arguments =
                  List.combine builder.AsyncListCalls.arguments arguments
                in
                let* mode =
                  List.fold_left
                    (fun result (role, atom) ->
                      let* mode = result in
                      let actual = modeOf env stores atom in
                      let elementMode =
                        match (role, actual) with
                        | AsyncListCalls.ElementInput, mode -> mode
                        | AsyncListCalls.ListInput, (Concrete | Pending) ->
                            Concrete
                        | ( AsyncListCalls.ListInput,
                            (Stored storage | PendingStored storage) ) ->
                            listElementMode storage
                      in
                      AsyncTransport.merge mode elementMode)
                    (Ok Concrete) arguments
                in
                match listMode element mode with
                | Stored storage ->
                    let result =
                      match (builder.AsyncListCalls.callee, arguments) with
                      | "Darklang.Stdlib.List.__concat", [ _; (_, suffix) ]
                        when Option.is_some (lookup env suffix) ->
                          PendingStored storage
                      | _ -> Stored storage
                    in
                    Ok
                      (Some
                         ( builder.AsyncListCalls.callee,
                           element,
                           mode,
                           storage,
                           arguments,
                           result ))
                | Concrete -> Ok None
                | _ ->
                    Crash.crash "Async list builder inferred outer pendingness")
          )
      | _ -> Ok None
    in
    let listEmptyCheck stores = function
      | A.Prim (((A.Eq | A.Neq) as op), list, A.IntLiteral (A.Int64 0L)) -> (
          match Option.map AsyncTransport.kind (lookup stores list) with
          | Some (ListStorage _ | DictStorage _) -> Some (op, list)
          | _ -> None)
      | _ -> None
    in
    let initial =
      F.map
        (fun _ (fn : A.functionDef) ->
          {
            parameters = List.map (fun _ -> Concrete) fn.A.typedParams;
            captures =
              (match fn.A.typedParams with
              | { A.typ = AST.TTuple (AST.TInt64 :: fields); _ } :: _ ->
                  List.map (fun _ -> Concrete) fields
              | _ -> []);
            result = Concrete;
            joins = M.empty;
            rawLists = M.empty;
            physicalSumShapes = Names.empty;
          })
        definitions
    in
    let eligible id =
      match F.tryFind id definitions with
      | Some fn -> not (nativeRuntime fn.A.name)
      | None -> false
    in
    let called (fn : A.functionDef) =
      ANFDeadCodeElimination.getCalledFunctions fn
    in
    let starts =
      F.fold
        (fun starts id fn ->
          if eligible id && S.exists producer (called fn) then S.add id starts
          else starts)
        S.empty definitions
    in
    let rec reach current =
      let next =
        F.fold
          (fun found id fn ->
            if eligible id && not (S.is_empty (S.inter current (called fn)))
            then S.add id found
            else found)
          current definitions
      in
      if S.equal current next then current else reach next
    in
    let startsAsync = reach starts in
    let sourceCall env stores callee arguments =
      eligible callee
      && (S.mem callee startsAsync
         || List.exists (fun atom -> changed (modeOf env stores atom)) arguments
         )
    in
    let rec closureBindings bindings = function
      | A.Return _ | A.Jump _ -> bindings
      | A.If (_, yes, no) -> closureBindings (closureBindings bindings yes) no
      | A.Join (_, continuation, entry) ->
          closureBindings (closureBindings bindings entry) continuation
      | A.Let (id, operation, rest) ->
          let callee =
            match operation with
            | A.ClosureAlloc (callee, _) -> Some callee
            | A.Atom atom | A.TypedAtom (atom, _) -> target bindings atom
            | _ -> None
          in
          let bindings =
            match callee with
            | Some callee -> M.add id callee bindings
            | None -> bindings
          in
          closureBindings bindings rest
    in
    let callables =
      F.map (fun _ fn -> closureBindings M.empty fn.A.body) definitions
    in
    let callable (fn : A.functionDef) atom =
      match F.tryFind fn.A.id callables with
      | Some bindings -> target bindings atom
      | None -> Crash.crash "Async inference lost callable bindings"
    in
    let dictionaryMappingCall fn env stores state = function
      | A.Call (callee, [ dict; callback ])
      | A.BorrowedCall (callee, [ dict; callback ])
      | A.TailCall (callee, [ dict; callback ]) -> (
          let kind =
            match name callee with
            | Some name
              when specialized "Darklang.Stdlib.Dict.map" name
                   || specialized "Darklang.Stdlib.Dict.__map" name ->
                Some MapDictionaryValues
            | Some name
              when specialized "Darklang.Stdlib.Dict.filter" name
                   || specialized "Darklang.Stdlib.Dict.__filter" name ->
                Some FilterDictionaryValues
            | None -> None
            | Some _ -> None
          in
          match (kind, callable fn callback) with
          | Some kind, Some target
            when eligible target && not (S.mem target nativeEntries) -> (
              match F.tryFind target definitions with
              | Some { A.typedParams = [ _; key; value ]; returnType; _ } ->
                  let actualDictionaryValue =
                    match modeOf env stores dict with
                    | Stored storage | PendingStored storage ->
                        dictionaryValueMode storage
                    | Concrete | Pending -> Concrete
                  in
                  let current = summary state target in
                  if
                    changed actualDictionaryValue
                    || changed current.result
                    || List.exists changed current.parameters
                  then
                    Some
                      {
                        dictionaryMappingKind = kind;
                        mappedDictionary = dict;
                        dictionaryCallback = callback;
                        dictionaryCallbackTarget = target;
                        mapKeyType = key.A.typ;
                        mapInputType = value.A.typ;
                        mapOutputType = returnType;
                        actualDictionaryValue;
                      }
                  else None
              | _ -> None)
          | _ -> None)
      | _ -> None
    in
    let dictionaryMappingModes state mapping =
      let current = summary state mapping.dictionaryCallbackTarget in
      match current.parameters with
      | [ _; key; value ] -> (key, value, current.result)
      | _ ->
          Crash.crash "Async dictionary callback has an inconsistent signature"
    in
    let dictionaryMappingResult state mapping =
      let _, input, result = dictionaryMappingModes state mapping in
      let valueType, valueMode =
        match mapping.dictionaryMappingKind with
        | MapDictionaryValues -> (mapping.mapOutputType, result)
        | FilterDictionaryValues -> (mapping.mapInputType, input)
      in
      ( AST.TDict (mapping.mapKeyType, valueType),
        dictionaryMode mapping.mapKeyType valueType valueMode )
    in
    let mappingCall fn env stores state = function
      | A.Call (callee, [ listValue; callback ])
      | A.BorrowedCall (callee, [ listValue; callback ])
      | A.TailCall (callee, [ listValue; callback ]) -> (
          let kind =
            match name callee with
            | Some name
              when specialized "Darklang.Stdlib.List.map" name
                   || specialized "Darklang.Stdlib.List.__map" name ->
                Some MapElements
            | Some name when specialized "Darklang.Stdlib.List.filter" name ->
                Some (PredicateElements FilterPredicate)
            | Some name when specialized "Darklang.Stdlib.List.any" name ->
                Some (PredicateElements AnyPredicate)
            | Some name when specialized "Darklang.Stdlib.List.all" name ->
                Some (PredicateElements AllPredicate)
            | Some name when specialized "Darklang.Stdlib.List.takeWhile" name
              ->
                Some (PredicateElements TakeWhilePredicate)
            | Some name when specialized "Darklang.Stdlib.List.dropWhile" name
              ->
                Some (PredicateElements DropWhilePredicate)
            | _ -> None
          in
          match (kind, callable fn callback) with
          | Some mappingKind, Some target
            when eligible target && not (S.mem target nativeEntries) -> (
              match F.tryFind target definitions with
              | Some { A.typedParams = [ _; parameter ]; returnType; _ } ->
                  let actualElement =
                    match modeOf env stores listValue with
                    | Stored storage | PendingStored storage ->
                        listElementMode storage
                    | Concrete | Pending -> Concrete
                  in
                  let current = summary state target in
                  let parameterMode =
                    match current.parameters with
                    | [ _; mode ] -> mode
                    | _ ->
                        Crash.crash
                          "Async mapping callback has an inconsistent signature"
                  in
                  if
                    changed actualElement || changed parameterMode
                    || changed current.result
                  then
                    Some
                      {
                        mappingKind;
                        listValue;
                        callback;
                        callbackTarget = target;
                        inputType = parameter.A.typ;
                        outputType = returnType;
                        actualElement;
                      }
                  else None
              | _ -> None)
          | _ -> None)
      | _ -> None
    in
    let mappingModes state mapping =
      let current = summary state mapping.callbackTarget in
      match current.parameters with
      | [ _; input ] -> (input, current.result)
      | _ -> Crash.crash "Async mapping callback has an inconsistent signature"
    in
    let mappingResult state mapping =
      let input, result = mappingModes state mapping in
      match mapping.mappingKind with
      | MapElements ->
          (AST.TList mapping.outputType, listMode mapping.outputType result)
      | PredicateElements
          (FilterPredicate | TakeWhilePredicate | DropWhilePredicate) ->
          (AST.TList mapping.inputType, listMode mapping.inputType input)
      | PredicateElements (AnyPredicate | AllPredicate) -> (AST.TBool, Concrete)
    in
    let foldingCall fn env stores state = function
      | A.Call (callee, [ foldList; initial; step ])
      | A.BorrowedCall (callee, [ foldList; initial; step ])
      | A.TailCall (callee, [ foldList; initial; step ]) -> (
          let isFold =
            match name callee with
            | Some name -> specialized "Darklang.Stdlib.List.fold" name
            | None -> false
          in
          match (isFold, callable fn step) with
          | true, Some target
            when eligible target && not (S.mem target nativeEntries) -> (
              match F.tryFind target definitions with
              | Some { A.typedParams = [ _; accumulator; element ]; _ } ->
                  let actualFoldElement =
                    match modeOf env stores foldList with
                    | Stored storage | PendingStored storage ->
                        listElementMode storage
                    | Concrete | Pending -> Concrete
                  in
                  let actualInitial = modeOf env stores initial in
                  let current = summary state target in
                  if
                    changed actualFoldElement || changed actualInitial
                    || changed current.result
                    || List.exists changed current.parameters
                  then
                    Some
                      {
                        foldList;
                        initial;
                        step;
                        stepTarget = target;
                        elementType = element.A.typ;
                        accumulatorType = accumulator.A.typ;
                        actualFoldElement;
                        actualInitial;
                      }
                  else None
              | _ -> None)
          | _ -> None)
      | _ -> None
    in
    let foldingModes state folding =
      let current = summary state folding.stepTarget in
      match current.parameters with
      | [ _; accumulator; element ] -> (accumulator, element, current.result)
      | _ -> Crash.crash "Async fold callback has an inconsistent signature"
    in
    let seed mapError (fn : A.functionDef) state modes transform =
      let* env, stores =
        List.fold_left2
          (fun result (parameter : A.typedParam) mode ->
            let* env, stores = result in
            bindMode transform env stores parameter.A.id parameter.A.typ mode)
          (Ok (M.empty, M.empty))
          fn.A.typedParams modes
      in
      let captures = (summary state fn.A.id).captures in
      if not (List.exists changed captures) then Ok (env, stores)
      else
        match fn.A.typedParams with
        | ({ A.typ = AST.TTuple (_ :: fields); _ } as parameter) :: _
          when List.length fields = List.length captures ->
            let captureMode =
              Stored
                (AsyncTransport.storage TupleStorage
                   (None :: List.map AsyncTransport.modeSlot captures))
            in
            let* mode =
              Result.map_error mapError
                (AsyncTransport.merge
                   (modeOf env stores (A.Var parameter.A.id))
                   captureMode)
            in
            bindMode transform env stores parameter.A.id parameter.A.typ mode
        | _ ->
            Crash.crash
              "Async captured environment has an inconsistent source layout"
    in
    let rec infer (fn : A.functionDef) env stores body state =
      let update transform =
        let current = summary state fn.A.id in
        let* current = transform current in
        Ok (F.add fn.A.id current state)
      in
      let mergeActual callee actual =
        let current = summary state callee in
        let* parameters = AsyncTransport.mergeList current.parameters actual in
        Ok (F.add callee { current with parameters } state)
      in
      match body with
      | A.Return atom ->
          update (fun current ->
              let* result =
                AsyncTransport.merge current.result (modeOf env stores atom)
              in
              Ok { current with result })
      | A.Jump (target, atom) ->
          update (fun current ->
              let previous =
                match M.find_opt target current.joins with
                | Some mode -> mode
                | None -> Concrete
              in
              let* mode =
                AsyncTransport.merge previous (modeOf env stores atom)
              in
              Ok { current with joins = M.add target mode current.joins })
      | A.If (_, yes, no) ->
          let* state = infer fn env stores yes state in
          infer fn env stores no state
      | A.Join (parameter, continuation, entry) ->
          let* state = infer fn env stores entry state in
          let mode =
            match M.find_opt parameter.A.id (summary state fn.A.id).joins with
            | Some mode -> mode
            | None -> Concrete
          in
          let* env, stores =
            bindMode
              (fun typ -> Ok typ)
              env stores parameter.A.id parameter.A.typ mode
          in
          infer fn env stores continuation state
      | A.Let (id, operation, rest) ->
          let* builder = listBuildCall env stores operation in
          let* dictionary = dictionaryCall env stores operation in
          let dictionaryMapping =
            dictionaryMappingCall fn env stores state operation
          in
          let mapping = mappingCall fn env stores state operation in
          let folding = foldingCall fn env stores state operation in
          let* state =
            match operation with
            | A.RawSlotInit (pointer, offset, value, _) -> (
                match AsyncListStorage.slot (listPlan fn) pointer offset with
                | None -> Ok state
                | Some (component, slot) ->
                    let* actual =
                      match (slot, modeOf env stores value) with
                      | AsyncListStorage.Element _, mode -> Ok mode
                      | AsyncListStorage.Link _, Concrete -> Ok Concrete
                      | AsyncListStorage.Link _, Stored storage ->
                          Ok (listElementMode storage)
                      | AsyncListStorage.Link _, (Pending | PendingStored _) ->
                          Error "Async pending list spines are not implemented"
                    in
                    let* mode =
                      AsyncTransport.merge
                        (rawListMode state fn component)
                        actual
                    in
                    let current = summary state fn.A.id in
                    Ok
                      (F.add fn.A.id
                         {
                           current with
                           rawLists = M.add component mode current.rawLists;
                         }
                         state))
            | _ -> Ok state
          in
          let projected atom index =
            match lookup stores atom with
            | Some storage -> (
                match storageField storage index with
                | Some (PendingSlot typ) | Some (PendingStorage (typ, _)) ->
                    Some typ
                | _ -> None)
            | None -> None
          in
          let* ordinaryStore =
            match operation with
            | A.TypedAtom (_, (AST.TList _ as typ)) -> (
                match AsyncListStorage.root (listPlan fn) (A.Var id) with
                | Some (component, AST.TList element) -> (
                    match listMode element (rawListMode state fn component) with
                    | Stored storage ->
                        Result.map Option.some
                          (AsyncTransport.instantiateStorage
                             (fun typ -> Ok typ)
                             storage typ)
                    | Concrete -> Ok None
                    | _ ->
                        Crash.crash "Async literal list has outer pendingness")
                | _ -> Ok (storageFacts sumShapes env stores operation))
            | _ -> Ok (storageFacts sumShapes env stores operation)
          in
          let* ordinaryStore =
            normalizeAllocation (fun typ -> Ok typ) operation ordinaryStore
          in
          let* () =
            match (operation, ordinaryStore) with
            | A.TypedAtom (_, typ), Some storage -> (
                match (AsyncTransport.kind storage, typ) with
                | TupleStorage, AST.TTuple _
                | RecordStorage _, AST.TRecord _
                | SumStorage _, AST.TSum _
                | ListStorage _, AST.TList _
                | DictStorage _, AST.TDict _ ->
                    Ok ()
                | _ ->
                    Error
                      "Async storage across a collection or variant layout is \
                       not implemented")
            | _ -> Ok ()
          in
          let* state, result, stored =
            match operation with
            | _ when Option.is_some dictionaryMapping ->
                let mapping =
                  match dictionaryMapping with
                  | Some value -> value
                  | None ->
                      Crash.crash
                        "Async dictionary mapping lost its inference plan"
                in
                let* state =
                  mergeActual mapping.dictionaryCallbackTarget
                    [ Concrete; Concrete; mapping.actualDictionaryValue ]
                in
                let typ, mode = dictionaryMappingResult state mapping in
                let* value, storage = modeFacts (fun typ -> Ok typ) typ mode in
                Ok (state, value, storage)
            | _ when Option.is_some dictionary ->
                let dictionary =
                  match dictionary with
                  | Some value -> value
                  | None ->
                      Crash.crash
                        "Async dictionary inference lost its operation"
                in
                let* value, storage =
                  modeFacts
                    (fun typ -> Ok typ)
                    dictionary.dictionarySourceResult
                    dictionary.dictionaryResult
                in
                Ok (state, value, storage)
            | _ when Option.is_some folding ->
                let folding =
                  match folding with
                  | Some folding -> folding
                  | None -> Crash.crash "Async list fold lost its source plan"
                in
                let accumulator, _, result = foldingModes state folding in
                let* accumulator =
                  AsyncTransport.merge accumulator folding.actualInitial
                in
                let* accumulator = AsyncTransport.merge accumulator result in
                let current = summary state folding.stepTarget in
                let* parameters =
                  AsyncTransport.mergeList current.parameters
                    [ Concrete; accumulator; folding.actualFoldElement ]
                in
                let state =
                  F.add folding.stepTarget
                    { current with parameters; result = accumulator }
                    state
                in
                let* value, storage =
                  modeFacts
                    (fun typ -> Ok typ)
                    folding.accumulatorType accumulator
                in
                Ok (state, value, storage)
            | _ when Option.is_some mapping ->
                let mapping =
                  match mapping with
                  | Some mapping -> mapping
                  | None ->
                      Crash.crash "Async list mapping lost its source plan"
                in
                let* state =
                  mergeActual mapping.callbackTarget
                    [ Concrete; mapping.actualElement ]
                in
                let resultType, result = mappingResult state mapping in
                let* storage =
                  match result with
                  | Concrete -> Ok None
                  | Stored storage ->
                      Result.map Option.some
                        (AsyncTransport.instantiateStorage
                           (fun typ -> Ok typ)
                           storage resultType)
                  | _ ->
                      Crash.crash
                        "Async list mapping inferred outer pendingness"
                in
                Ok (state, None, storage)
            | _ when Option.is_some builder ->
                let element, storage, result =
                  match builder with
                  | Some (_, element, _, storage, _, result) ->
                      (element, storage, result)
                  | None ->
                      Crash.crash "Async list builder lost its source plan"
                in
                let* storage =
                  AsyncTransport.instantiateStorage
                    (fun typ -> Ok typ)
                    storage (AST.TList element)
                in
                let value =
                  match result with
                  | PendingStored fields ->
                      Some
                        (AsyncTransport.physicalType (Stored fields)
                           (AST.TList element))
                  | _ -> None
                in
                Ok (state, value, Some storage)
            | A.Call (callee, arguments)
            | A.BorrowedCall (callee, arguments)
            | A.TailCall (callee, arguments)
              when Option.is_some (listCall stores callee arguments) ->
                let list, storage, result =
                  match listCall stores callee arguments with
                  | Some (_, list, _, storage, result) -> (list, storage, result)
                  | None -> Crash.crash "Async list call lost its source plan"
                in
                let value, stored =
                  listResult (lookup env list) storage result
                in
                Ok (state, value, stored)
            | A.Call (callee, arguments) when producer callee ->
                let stored = producerStorage stores callee arguments in
                let payload =
                  match stored with
                  | None -> originalResult callee
                  | Some storage -> storageType storage (originalResult callee)
                in
                Ok (state, Some payload, stored)
            | A.ClosureAlloc (callee, captures) when eligible callee ->
                let current = summary state callee in
                let* captures =
                  AsyncTransport.mergeList current.captures
                    (List.map (modeOf env stores) captures)
                in
                Ok (F.add callee { current with captures } state, None, None)
            | A.TupleGet (atom, index) | A.RecordGet (_, atom, index) ->
                Ok (state, projected atom index, ordinaryStore)
            | A.SumPayload (variant, atom) ->
                let value =
                  Option.bind (lookup stores atom) (fun storage ->
                      pendingField (sumField storage variant.A.variantTag))
                in
                Ok (state, value, ordinaryStore)
            | A.ClosureCall (closure, arguments) -> (
                match callable fn closure with
                | Some callee when eligible callee ->
                    let* state =
                      mergeActual callee
                        (Concrete :: List.map (modeOf env stores) arguments)
                    in
                    let mode =
                      if S.mem callee nativeEntries then Concrete
                      else (summary state callee).result
                    in
                    let* result, stored =
                      match mode with
                      | Concrete -> Ok (None, None)
                      | Pending -> Ok (Some (originalResult callee), None)
                      | PendingStored storage ->
                          let payload =
                            AsyncTransport.physicalType (Stored storage)
                              (originalResult callee)
                          in
                          let* storage =
                            AsyncTransport.instantiateStorage
                              (fun typ -> Ok typ)
                              storage (originalResult callee)
                          in
                          Ok (Some payload, Some storage)
                      | Stored storage ->
                          let* storage =
                            AsyncTransport.instantiateStorage
                              (fun typ -> Ok typ)
                              storage (originalResult callee)
                          in
                          Ok (None, Some storage)
                    in
                    Ok (state, result, stored)
                | _ -> Ok (state, None, ordinaryStore))
            | A.Atom atom -> Ok (state, lookup env atom, ordinaryStore)
            | A.TypedAtom (atom, typ) ->
                Ok
                  ( state,
                    (match lookup env atom with
                    | Some source
                      when source = typ || Option.is_some (lookup stores atom)
                      ->
                        Some source
                    | _ -> None),
                    ordinaryStore )
            | A.IfValue (_, yes, no) -> (
                let* mode =
                  AsyncTransport.merge (modeOf env stores yes)
                    (modeOf env stores no)
                in
                match mode with
                | Concrete -> Ok (state, None, None)
                | Pending ->
                    Ok
                      ( state,
                        (match lookup env yes with
                        | Some _ as value -> value
                        | None -> lookup env no),
                        None )
                | Stored _ | PendingStored _ ->
                    Error
                      "Async storage across an untyped value selection is not \
                       implemented")
            | A.Call (callee, arguments)
            | A.BorrowedCall (callee, arguments)
            | A.TailCall (callee, arguments)
              when sourceCall env stores callee arguments -> (
                let* state =
                  mergeActual callee (List.map (modeOf env stores) arguments)
                in
                match (summary state callee).result with
                | Concrete -> Ok (state, None, None)
                | Pending -> Ok (state, Some (originalResult callee), None)
                | PendingStored storage ->
                    let payload =
                      AsyncTransport.physicalType (Stored storage)
                        (originalResult callee)
                    in
                    let* storage =
                      AsyncTransport.instantiateStorage
                        (fun typ -> Ok typ)
                        storage (originalResult callee)
                    in
                    Ok (state, Some payload, Some storage)
                | Stored storage ->
                    let* storage =
                      AsyncTransport.instantiateStorage
                        (fun typ -> Ok typ)
                        storage (originalResult callee)
                    in
                    Ok (state, None, Some storage))
            | _ -> Ok (state, None, ordinaryStore)
          in
          let stores =
            match stored with
            | Some storage -> M.add id storage stores
            | None -> M.remove id stores
          in
          let env =
            match result with
            | Some typ -> M.add id typ env
            | None -> M.remove id env
          in
          let* state =
            match stored with
            | None -> Ok state
            | Some storage ->
                let current = summary state fn.A.id in
                let* physicalSumShapes =
                  AsyncTransport.collectSumShapes
                    ~into:current.physicalSumShapes storage
                in
                Ok (F.add fn.A.id { current with physicalSumShapes } state)
          in
          infer fn env stores rest state
    in
    let rec converge state =
      let* next =
        F.fold
          (fun result id fn ->
            let* state = result in
            if not (eligible id) then Ok state
            else
              let* env, stores =
                seed Fun.id fn state (summary state id).parameters (fun typ ->
                    Ok typ)
              in
              let* physicalSumShapes =
                M.fold
                  (fun _ storage result ->
                    let* shapes = result in
                    AsyncTransport.collectSumShapes ~into:shapes storage)
                  stores (Ok Names.empty)
              in
              let current = summary state id in
              let state = F.add id { current with physicalSumShapes } state in
              infer fn env stores fn.A.body state)
          (Ok state) definitions
      in
      if F.forall (fun id value -> equalSummary value (summary state id)) next
      then Ok next
      else converge next
    in
    let* summaries =
      Result.map_error (fun message -> Diagnostic message) (converge initial)
    in
    let* sumShapes =
      F.fold
        (fun result _ value ->
          let* shapes = result in
          AsyncSumStorage.includePhysicalShapes ~into:shapes
            value.physicalSumShapes
          |> Result.map_error (fun message -> Diagnostic message))
        (Ok sumShapes) summaries
    in
    let helperTypes base types =
      let name = SpecializationIdentity.specName base types in
      match Names.find_opt name ids with
      | Some id -> Ok id
      | None ->
          Error
            (MissingRuntimeSpecializations
               ( SpecializationIdentity.SpecSet.singleton (base, types),
                 sumShapes ))
    in
    let helper base payload = helperTypes base [ payload ] in
    let info payload =
      let* force = helper "Darklang.Stdlib.__Task.__forceCurrent" payload in
      Ok { payload; force }
    in
    let modeChanges id =
      let value = summary summaries id in
      changed value.result
      || List.exists changed value.parameters
      || List.exists changed value.captures
    in
    let local =
      List.fold_left
        (fun set (fn : A.functionDef) -> S.add fn.A.id set)
        S.empty functions
    in
    let boundary (fn : A.functionDef) =
      S.mem fn.A.id nativeEntries || fn.A.name = "__dark_compiler_program_entry"
    in
    let versions, nativeVersions, nextFunctionOrdinal =
      F.fold
        (fun (versions, nativeVersions, next) id fn ->
          if not (eligible id) then (versions, nativeVersions, next)
          else
            let imported = not (S.mem id local) in
            let needs = modeChanges id || (imported && S.mem id startsAsync) in
            let versions, next =
              if not needs then (versions, next)
              else
                let version =
                  {
                    id = AST.functionId next;
                    name =
                      "__dark_async_values_" ^ Int64.to_string next ^ "."
                      ^ fn.A.name;
                  }
                in
                (F.add id version versions, Int64.succ next)
            in
            if
              boundary fn
              && ((imported && S.mem id startsAsync)
                 || List.exists changed (summary summaries id).captures)
            then
              let version =
                {
                  id = AST.functionId next;
                  name =
                    "__dark_async_native_" ^ Int64.to_string next ^ "."
                    ^ fn.A.name;
                }
              in
              (versions, F.add id version nativeVersions, Int64.succ next)
            else (versions, nativeVersions, next))
        (F.empty, F.empty, nextFunctionOrdinal)
        definitions
    in
    let unsupported functionName where =
      Error
        (Diagnostic
           ("Async transport across " ^ where ^ " is not implemented in "
          ^ functionName))
    in
    let changedCallable fn atom =
      match callable fn atom with
      | Some callee when eligible callee && not (S.mem callee nativeEntries)
        -> (
          let state = summary summaries callee in
          changed state.result
          ||
          match state.parameters with
          | _ :: values -> List.exists changed values
          | [] -> false)
      | _ -> false
    in
    let wraps bindings body =
      List.fold_right
        (fun (id, value) body -> A.Let (id, value, body))
        bindings body
    in
    let ready payload atom gen =
      let raw, gen = freshVar gen in
      let typed, gen = freshVar gen in
      ( A.Var typed,
        [
          (raw, A.TupleAlloc [ A.IntLiteral (A.Int64 0L); atom ]);
          (typed, A.TypedAtom (A.Var raw, carrier payload));
        ],
        gen )
    in
    let mapperBody ?leadingParameter ~closure inputType outputType body gen =
      let key =
        SpecializationIdentity.specName
          (if Option.is_some leadingParameter then
             "__dark_async_dictionary_mapping"
           else if closure then "__dark_async_mapping"
           else "__dark_async_sum_conversion")
          (Option.to_list leadingParameter @ [ inputType; outputType ])
      in
      match Names.find_opt key gen.mappers with
      | Some mapper -> Ok (mapper, gen)
      | None ->
          let mapper =
            {
              id = AST.functionId gen.cursor;
              name =
                (if closure then "__dark_async_map_"
                 else "__dark_async_sum_convert_")
                ^ Int64.to_string gen.cursor;
            }
          in
          let gen =
            {
              gen with
              cursor = Int64.succ gen.cursor;
              mappers = Names.add key mapper gen.mappers;
            }
          in
          let environment, gen =
            if closure then
              let id, gen = freshVar gen in
              (Some { A.id; typ = AST.TTuple [ AST.TInt64 ] }, gen)
            else (None, gen)
          in
          let parameter, gen = freshVar gen in
          let leading, gen =
            match leadingParameter with
            | None -> ([], gen)
            | Some typ ->
                let id, gen = freshVar gen in
                ([ { A.id; typ } ], gen)
          in
          let* body, gen = body (A.Var parameter) gen in
          let definition =
            {
              A.id = mapper.id;
              name = mapper.name;
              typedParams =
                Option.to_list environment @ leading
                @ [ { A.id = parameter; typ = inputType } ];
              returnType = outputType;
              returnOwnership = A.OwnedReturn;
              body;
            }
          in
          Ok
            ( mapper,
              {
                gen with
                generatedFunctions = definition :: gen.generatedFunctions;
              } )
    in
    let mapper ?leadingParameter inputType outputType emit gen =
      mapperBody ?leadingParameter ~closure:true inputType outputType
        (fun atom gen ->
          let* value, before, gen = emit atom gen in
          Ok (wraps before (A.Return value), gen))
        gen
    in
    let sumBranches layout inputSum outputSum inputType outputType convert atom
        gen =
      let branch (tag, payload) gen =
        let result, gen = freshVar gen in
        match payload with
        | None ->
            Ok
              ( A.Let
                  ( result,
                    A.SumConstant (outputSum, tag),
                    A.Return (A.Var result) ),
                gen )
        | Some typ ->
            let projected, gen = freshVar gen in
            let* value, before, gen = convert tag typ (A.Var projected) gen in
            let input =
              {
                A.sum = inputSum;
                variantTag = tag;
                payloadType = inputType tag typ;
              }
            in
            let output =
              {
                A.sum = outputSum;
                variantTag = tag;
                payloadType = outputType tag typ;
              }
            in
            Ok
              ( wraps
                  ((projected, A.SumPayload (input, atom)) :: before)
                  (A.Let
                     ( result,
                       A.SumAlloc (output, value),
                       A.Return (A.Var result) )),
                gen )
      in
      let rec branches gen = function
        | [] -> Crash.crash "Async sum conversion has no constructors"
        | [ variant ] -> branch variant gen
        | ((tag, _) as variant) :: rest ->
            let condition, gen = freshVar gen in
            let* yes, gen = branch variant gen in
            let* no, gen = branches gen rest in
            Ok
              ( A.Let
                  ( condition,
                    A.SumTest (inputSum, tag, atom),
                    A.If (A.Var condition, yes, no) ),
                gen )
      in
      let variants =
        List.map
          (fun (tag, typ) -> (tag, Some typ))
          layout.AsyncSumStorage.payloads
        @ List.map (fun tag -> (tag, None)) layout.AsyncSumStorage.emptyTags
        |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
      in
      branches gen variants
    in
    let rec prepareValue expected source atom actual gen =
      match (expected, actual) with
      | left, right when AsyncTransport.equal left right -> Ok (atom, [], gen)
      | Pending, Concrete -> Ok (ready source atom gen)
      | PendingStored expected, (Concrete | Stored _) ->
          let* value, before, gen =
            prepareValue (Stored expected) source atom actual gen
          in
          let payload = AsyncTransport.physicalType (Stored expected) source in
          let value, added, gen = ready payload value gen in
          Ok (value, before @ added, gen)
      | PendingStored expected, (Pending | PendingStored _) ->
          let inputMode =
            match actual with
            | PendingStored storage -> Stored storage
            | _ -> Concrete
          in
          let inputType = AsyncTransport.physicalType inputMode source in
          let outputType =
            AsyncTransport.physicalType (Stored expected) source
          in
          if inputType = outputType then Ok (atom, [], gen)
          else
            let* mapHelper =
              helperTypes "Darklang.Stdlib.__Task.__mapValueCurrent"
                [ inputType; outputType ]
            in
            let* mapper, gen =
              mapper inputType outputType
                (fun atom gen ->
                  prepareValue (Stored expected) source atom inputMode gen)
                gen
            in
            let closure, gen = freshVar gen in
            let result, gen = freshVar gen in
            Ok
              ( A.Var result,
                [
                  (closure, A.ClosureAlloc (mapper.id, []));
                  (result, A.Call (mapHelper, [ A.Var closure; atom ]));
                ],
                gen )
      | Stored expected, (Concrete | Stored _) -> (
          match AsyncTransport.kind expected with
          | DictStorage (key, element) ->
              let actualMode =
                match actual with
                | Stored storage -> dictionaryValueMode storage
                | _ -> Concrete
              in
              let expectedMode = dictionaryValueMode expected in
              let inputType = AsyncTransport.physicalType actualMode element in
              let outputType =
                AsyncTransport.physicalType expectedMode element
              in
              if inputType = outputType || atom = A.IntLiteral (A.Int64 0L) then
                Ok (atom, [], gen)
              else
                let* mapHelper =
                  helperTypes "Darklang.Stdlib.Dict.__map"
                    [ key; inputType; outputType ]
                in
                let* mapper, gen =
                  mapper ~leadingParameter:key inputType outputType
                    (fun atom gen ->
                      prepareValue expectedMode element atom actualMode gen)
                    gen
                in
                let closure, gen = freshVar gen in
                let result, gen = freshVar gen in
                Ok
                  ( A.Var result,
                    [
                      (closure, A.ClosureAlloc (mapper.id, []));
                      (result, A.Call (mapHelper, [ atom; A.Var closure ]));
                    ],
                    gen )
          | ListStorage element ->
              let actualMode =
                match actual with
                | Stored storage -> listElementMode storage
                | _ -> Concrete
              in
              let expectedMode = listElementMode expected in
              let inputType = AsyncTransport.physicalType actualMode element in
              let outputType =
                AsyncTransport.physicalType expectedMode element
              in
              if inputType = outputType || atom = A.IntLiteral (A.Int64 0L) then
                Ok (atom, [], gen)
              else
                let* mapHelper =
                  helperTypes "Darklang.Stdlib.List.map"
                    [ inputType; outputType ]
                in
                let* mapper, gen =
                  mapper inputType outputType
                    (fun atom gen ->
                      prepareValue expectedMode element atom actualMode gen)
                    gen
                in
                let closure, gen = freshVar gen in
                let result, gen = freshVar gen in
                Ok
                  ( A.Var result,
                    [
                      (closure, A.ClosureAlloc (mapper.id, []));
                      (result, A.Call (mapHelper, [ atom; A.Var closure ]));
                    ],
                    gen )
          | SumStorage layout ->
              let actualStorage =
                match actual with Stored storage -> Some storage | _ -> None
              in
              let actualSlot tag =
                Option.bind actualStorage (fun storage -> sumField storage tag)
              in
              let inputSum =
                match actualStorage with
                | Some storage -> physicalSumDescriptor storage
                | None -> layout.AsyncSumStorage.source
              in
              let outputSum = physicalSumDescriptor expected in
              let inputType = AsyncTransport.physicalType actual source in
              let outputType =
                AsyncTransport.physicalType (Stored expected) source
              in
              let* conversion, gen =
                mapperBody ~closure:false inputType outputType
                  (sumBranches layout inputSum outputSum
                     (fun tag typ -> slotRepresentation typ (actualSlot tag))
                     (fun tag typ ->
                       slotRepresentation typ (sumField expected tag))
                     (fun tag typ atom gen ->
                       prepareValue
                         (AsyncTransport.slotMode (sumField expected tag))
                         typ atom
                         (AsyncTransport.slotMode (actualSlot tag))
                         gen))
                  gen
              in
              let result, gen = freshVar gen in
              Ok
                ( A.Var result,
                  [ (result, A.Call (conversion.id, [ atom ])) ],
                  gen )
          | TupleStorage | RecordStorage _ ->
              let types =
                match (AsyncTransport.kind expected, source) with
                | TupleStorage, AST.TTuple types -> types
                | RecordStorage descriptor, AST.TRecord _ ->
                    List.map snd descriptor.A.fields
                | _ ->
                    Crash.crash
                      "Async promotion has an inconsistent source layout"
              in
              let actualStorage =
                match actual with Stored storage -> Some storage | _ -> None
              in
              let rec fields index types expectedSlots values before gen =
                match (types, expectedSlots) with
                | [], [] -> Ok (List.rev values, before, gen)
                | typ :: types, slot :: expectedSlots ->
                    let projected, gen = freshVar gen in
                    let projection =
                      match actualStorage with
                      | Some storage -> (
                          match AsyncTransport.kind storage with
                          | RecordStorage descriptor ->
                              A.RecordGet
                                ( storageDescriptor storage descriptor,
                                  atom,
                                  index )
                          | _ -> A.TupleGet (atom, index))
                      | None -> (
                          match AsyncTransport.kind expected with
                          | RecordStorage descriptor ->
                              A.RecordGet (descriptor, atom, index)
                          | _ -> A.TupleGet (atom, index))
                    in
                    let actual =
                      match actualStorage with
                      | Some storage ->
                          AsyncTransport.slotMode (storageField storage index)
                      | None -> Concrete
                    in
                    let* value, added, gen =
                      prepareValue
                        (AsyncTransport.slotMode slot)
                        typ (A.Var projected) actual gen
                    in
                    fields (index + 1) types expectedSlots (value :: values)
                      (before @ [ (projected, projection) ] @ added)
                      gen
                | _ ->
                    Crash.crash
                      "Async promotion has an inconsistent field count"
              in
              let* values, before, gen =
                fields 0 types (AsyncTransport.fields expected) [] [] gen
              in
              let result, gen = freshVar gen in
              let allocation =
                match AsyncTransport.kind expected with
                | TupleStorage -> A.TupleAlloc values
                | RecordStorage descriptor ->
                    A.RecordAlloc (storageDescriptor expected descriptor, values)
                | ListStorage _ | DictStorage _ | SumStorage _ ->
                    Crash.crash
                      "Async collection promotion entered fixed-block \
                       allocation"
              in
              Ok (A.Var result, before @ [ (result, allocation) ], gen))
      | _ ->
          Error
            (Diagnostic
               "Async inferred interface lost a pending value or field layout")
    in
    let storedResult mode typ =
      match mode with
      | Stored storage | PendingStored storage ->
          Result.map Option.some
            (AsyncTransport.instantiateStorage info storage typ)
      | Concrete | Pending -> Ok None
    in
    let forceAtom env atom gen =
      match lookup env atom with
      | None -> (atom, [], gen)
      | Some value ->
          let id, gen = freshVar gen in
          (A.Var id, [ (id, A.Call (value.force, [ atom ])) ], gen)
    in
    let appendBridge element mode leftMode gen =
      let physicalElement = AsyncTransport.physicalType mode element in
      let listType = AST.TList element in
      let physicalList = AST.TList physicalElement in
      let leftType = AsyncTransport.physicalType leftMode listType in
      let rightType = carrier physicalList in
      let key =
        SpecializationIdentity.specName "__dark_async_append"
          [ leftType; rightType ]
      in
      match Names.find_opt key gen.mappers with
      | Some bridge -> Ok (bridge, gen)
      | None ->
          let* force =
            helper "Darklang.Stdlib.__Task.__forceCurrent" physicalList
          in
          let* empty =
            helper "Darklang.Stdlib.List.__isEmpty" physicalElement
          in
          let* concat =
            helper "Darklang.Stdlib.List.__concat" physicalElement
          in
          let bridge =
            {
              id = AST.functionId gen.cursor;
              name = "__dark_async_append_" ^ Int64.to_string gen.cursor;
            }
          in
          let gen =
            {
              gen with
              cursor = Int64.succ gen.cursor;
              mappers = Names.add key bridge gen.mappers;
            }
          in
          let left, gen = freshVar gen in
          let right, gen = freshVar gen in
          let concreteLeft, before, gen =
            match leftMode with
            | PendingStored _ ->
                let concrete, gen = freshVar gen in
                ( A.Var concrete,
                  [ (concrete, A.Call (force, [ A.Var left ])) ],
                  gen )
            | Stored _ -> (A.Var left, [], gen)
            | _ -> Crash.crash "Async append bridge has no element layout"
          in
          let condition, gen = freshVar gen in
          let concreteRight, gen = freshVar gen in
          let appended, gen = freshVar gen in
          let readyValue, readyBindings, gen =
            ready physicalList (A.Var appended) gen
          in
          let body =
            wraps
              (before @ [ (condition, A.Call (empty, [ concreteLeft ])) ])
              (A.If
                 ( A.Var condition,
                   A.Return (A.Var right),
                   wraps
                     ([
                        (concreteRight, A.Call (force, [ A.Var right ]));
                        ( appended,
                          A.Call (concat, [ concreteLeft; A.Var concreteRight ])
                        );
                      ]
                     @ readyBindings)
                     (A.Return readyValue) ))
          in
          let definition =
            {
              A.id = bridge.id;
              name = bridge.name;
              typedParams =
                [
                  { A.id = left; typ = leftType };
                  { A.id = right; typ = rightType };
                ];
              returnType = rightType;
              returnOwnership = A.OwnedReturn;
              body;
            }
          in
          Ok
            ( bridge,
              {
                gen with
                generatedFunctions = definition :: gen.generatedFunctions;
              } )
    in
    let prefixBridge prefix physicalElement gen =
      let physicalList = AST.TList physicalElement in
      let inputType = carrier physicalList in
      let operation =
        match prefix with TakePrefix -> "take" | DropPrefix -> "drop"
      in
      let bridgeName = "__dark_async_" ^ operation in
      let key = SpecializationIdentity.specName bridgeName [ inputType ] in
      match Names.find_opt key gen.mappers with
      | Some bridge -> Ok (bridge, gen)
      | None ->
          let* force =
            helper "Darklang.Stdlib.__Task.__forceCurrent" physicalList
          in
          let* transform =
            helper ("Darklang.Stdlib.List." ^ operation) physicalElement
          in
          let* zero = helperTypes "Darklang.Stdlib.Int.__zero" [] in
          let* nonpositive =
            helperTypes "Darklang.Stdlib.Int.__lessThanOrEqualTo" []
          in
          let bridge =
            {
              id = AST.functionId gen.cursor;
              name = bridgeName ^ "_" ^ Int64.to_string gen.cursor;
            }
          in
          let gen =
            {
              gen with
              cursor = Int64.succ gen.cursor;
              mappers = Names.add key bridge gen.mappers;
            }
          in
          let values, gen = freshVar gen in
          let count, gen = freshVar gen in
          let zeroValue, gen = freshVar gen in
          let condition, gen = freshVar gen in
          (* A nonpositive count does not inspect the input spine. Keep the force
             inside the positive branch rather than at the ordinary call boundary. *)
          let* unchanged, gen =
            match prefix with
            | DropPrefix -> Ok (A.Return (A.Var values), gen)
            | TakePrefix ->
                let* empty =
                  helper "Darklang.Stdlib.List.__empty" physicalElement
                in
                let emptyValue, gen = freshVar gen in
                Ok
                  ( wraps
                      [ (emptyValue, A.Call (empty, [])) ]
                      (A.Return (A.Var emptyValue)),
                    gen )
          in
          let concrete, gen = freshVar gen in
          let transformed, gen = freshVar gen in
          let result, after, gen =
            match prefix with
            | TakePrefix -> (A.Var transformed, [], gen)
            | DropPrefix -> ready physicalList (A.Var transformed) gen
          in
          let positive =
            wraps
              ([
                 (concrete, A.Call (force, [ A.Var values ]));
                 ( transformed,
                   A.Call (transform, [ A.Var concrete; A.Var count ]) );
               ]
              @ after)
              (A.Return result)
          in
          let definition =
            {
              A.id = bridge.id;
              name = bridge.name;
              typedParams =
                [
                  { A.id = values; typ = inputType };
                  { A.id = count; typ = AST.TInt };
                ];
              returnType =
                (match prefix with
                | TakePrefix -> physicalList
                | DropPrefix -> inputType);
              returnOwnership = A.OwnedReturn;
              body =
                wraps
                  [
                    (zeroValue, A.Call (zero, []));
                    ( condition,
                      A.Call (nonpositive, [ A.Var count; A.Var zeroValue ]) );
                  ]
                  (A.If (A.Var condition, unchanged, positive));
            }
          in
          Ok
            ( bridge,
              {
                gen with
                generatedFunctions = definition :: gen.generatedFunctions;
              } )
    in
    let predicateAdapter inputType gen =
      let key =
        SpecializationIdentity.specName "__dark_async_predicate" [ inputType ]
      in
      match Names.find_opt key gen.mappers with
      | Some adapter -> Ok (adapter, gen)
      | None ->
          let* force =
            helper "Darklang.Stdlib.__Task.__forceCurrent" AST.TBool
          in
          let adapter =
            {
              id = AST.functionId gen.cursor;
              name = "__dark_async_predicate_" ^ Int64.to_string gen.cursor;
            }
          in
          let gen =
            {
              gen with
              cursor = Int64.succ gen.cursor;
              mappers = Names.add key adapter gen.mappers;
            }
          in
          let environment, gen = freshVar gen in
          let parameter, gen = freshVar gen in
          let callback, gen = freshVar gen in
          let pending, gen = freshVar gen in
          let result, gen = freshVar gen in
          let callbackType = AST.TFunction ([ inputType ], carrier AST.TBool) in
          let definition =
            {
              A.id = adapter.id;
              name = adapter.name;
              typedParams =
                [
                  {
                    A.id = environment;
                    typ = AST.TTuple [ AST.TInt64; callbackType ];
                  };
                  { A.id = parameter; typ = inputType };
                ];
              returnType = AST.TBool;
              returnOwnership = A.OwnedReturn;
              body =
                wraps
                  [
                    (callback, A.TupleGet (A.Var environment, 1));
                    ( pending,
                      A.ClosureCall (A.Var callback, [ A.Var parameter ]) );
                    (result, A.Call (force, [ A.Var pending ]));
                  ]
                  (A.Return (A.Var result));
            }
          in
          Ok
            ( adapter,
              {
                gen with
                generatedFunctions = definition :: gen.generatedFunctions;
              } )
    in
    (* Dictionary keys stay concrete in HAMT storage, even when a shared
       callback accepts a carrier elsewhere. Predicate decisions, unlike map
       results, must be concrete before the library chooses an entry. *)
    let dictionaryCallbackAdapter mapping keyMode inputType outputType gen =
      let keyType = mapping.mapKeyType in
      let callbackKeyType = AsyncTransport.physicalType keyMode keyType in
      let returnType =
        match mapping.dictionaryMappingKind with
        | MapDictionaryValues -> outputType
        | FilterDictionaryValues -> AST.TBool
      in
      let cacheKey =
        SpecializationIdentity.specName "__dark_async_dictionary_callback"
          [ keyType; callbackKeyType; inputType; outputType; returnType ]
      in
      match Names.find_opt cacheKey gen.mappers with
      | Some adapter -> Ok (adapter, gen)
      | None ->
          let adapter =
            {
              id = AST.functionId gen.cursor;
              name =
                "__dark_async_dictionary_callback_" ^ Int64.to_string gen.cursor;
            }
          in
          let gen =
            {
              gen with
              cursor = Int64.succ gen.cursor;
              mappers = Names.add cacheKey adapter gen.mappers;
            }
          in
          let environment, gen = freshVar gen in
          let key, gen = freshVar gen in
          let value, gen = freshVar gen in
          let callback, gen = freshVar gen in
          let* preparedKey, before, gen =
            prepareValue keyMode keyType (A.Var key) Concrete gen
          in
          let called, gen = freshVar gen in
          let* result, after, gen =
            if outputType = returnType then Ok (A.Var called, [], gen)
            else
              let* force =
                helper "Darklang.Stdlib.__Task.__forceCurrent" AST.TBool
              in
              let result, gen = freshVar gen in
              Ok
                ( A.Var result,
                  [ (result, A.Call (force, [ A.Var called ])) ],
                  gen )
          in
          let callbackType =
            AST.TFunction ([ callbackKeyType; inputType ], outputType)
          in
          let definition =
            {
              A.id = adapter.id;
              name = adapter.name;
              typedParams =
                [
                  {
                    A.id = environment;
                    typ = AST.TTuple [ AST.TInt64; callbackType ];
                  };
                  { A.id = key; typ = keyType };
                  { A.id = value; typ = inputType };
                ];
              returnType;
              returnOwnership = A.OwnedReturn;
              body =
                wraps
                  (((callback, A.TupleGet (A.Var environment, 1)) :: before)
                  @ [
                      ( called,
                        A.ClosureCall
                          (A.Var callback, [ preparedKey; A.Var value ]) );
                    ]
                  @ after)
                  (A.Return result);
            }
          in
          Ok
            ( adapter,
              {
                gen with
                generatedFunctions = definition :: gen.generatedFunctions;
              } )
    in
    let rec materializeSlot slot atom gen =
      match slot with
      | None -> Ok (atom, [], gen)
      | Some (PendingSlot value) ->
          let id, gen = freshVar gen in
          Ok (A.Var id, [ (id, A.Call (value.force, [ atom ])) ], gen)
      | Some (NestedStorage storage) -> materializeStorage storage atom gen
      | Some (PendingStorage (value, storage)) ->
          let id, gen = freshVar gen in
          let original = atom in
          let* atom, before, gen = materializeStorage storage (A.Var id) gen in
          Ok (atom, (id, A.Call (value.force, [ original ])) :: before, gen)
    and materializeStorage storage atom gen =
      match (AsyncTransport.kind storage, AsyncTransport.fields storage) with
      | DictStorage (key, element), [ slot ] ->
          let inputType = slotRepresentation element slot in
          let* mapHelper =
            helperTypes "Darklang.Stdlib.Dict.__map" [ key; inputType; element ]
          in
          let* mapper, gen =
            mapper ~leadingParameter:key inputType element
              (fun atom gen -> materializeSlot slot atom gen)
              gen
          in
          let closure, gen = freshVar gen in
          let result, gen = freshVar gen in
          Ok
            ( A.Var result,
              [
                (closure, A.ClosureAlloc (mapper.id, []));
                (result, A.Call (mapHelper, [ atom; A.Var closure ]));
              ],
              gen )
      | DictStorage _, _ ->
          Crash.crash
            "Async dictionary materialization has an inconsistent value layout"
      | ListStorage element, [ slot ] ->
          let mode =
            AsyncTransport.slotMode
              (Option.map
                 (function
                   | PendingSlot _ -> PendingSlot ()
                   | NestedStorage storage ->
                       NestedStorage (AsyncTransport.interface storage)
                   | PendingStorage (_, storage) ->
                       PendingStorage ((), AsyncTransport.interface storage))
                 slot)
          in
          let inputType = AsyncTransport.physicalType mode element in
          if inputType = element then Ok (atom, [], gen)
          else
            let* mapHelper =
              helperTypes "Darklang.Stdlib.List.map" [ inputType; element ]
            in
            let* mapper, gen =
              mapper inputType element
                (fun atom gen -> materializeSlot slot atom gen)
                gen
            in
            let closure, gen = freshVar gen in
            let result, gen = freshVar gen in
            Ok
              ( A.Var result,
                [
                  (closure, A.ClosureAlloc (mapper.id, []));
                  (result, A.Call (mapHelper, [ atom; A.Var closure ]));
                ],
                gen )
      | ListStorage _, _ ->
          Crash.crash "Async list demand has an inconsistent element layout"
      | SumStorage layout, _ ->
          let source = layout.AsyncSumStorage.source in
          let inputType =
            storageType storage (AST.TSum (source.A.sumName, source.A.sumArgs))
          in
          let outputType = AST.TSum (source.A.sumName, source.A.sumArgs) in
          let* conversion, gen =
            mapperBody ~closure:false inputType outputType
              (sumBranches layout
                 (physicalSumDescriptor storage)
                 source
                 (fun tag typ -> slotRepresentation typ (sumField storage tag))
                 (fun _ typ -> typ)
                 (fun tag _ atom gen ->
                   materializeSlot (sumField storage tag) atom gen))
              gen
          in
          let result, gen = freshVar gen in
          Ok (A.Var result, [ (result, A.Call (conversion.id, [ atom ])) ], gen)
      | (TupleStorage | RecordStorage _), _ ->
          let* values, before, gen =
            List.fold_left
              (fun result (index, slot) ->
                let* values, before, gen = result in
                let projected, gen = freshVar gen in
                let projection =
                  match AsyncTransport.kind storage with
                  | TupleStorage -> A.TupleGet (atom, index)
                  | RecordStorage descriptor ->
                      A.RecordGet
                        (storageDescriptor storage descriptor, atom, index)
                  | ListStorage _ | DictStorage _ | SumStorage _ ->
                      Crash.crash
                        "Async collection demand entered fixed-block lowering"
                in
                let* value, added, gen =
                  materializeSlot slot (A.Var projected) gen
                in
                Ok
                  ( value :: values,
                    before @ [ (projected, projection) ] @ added,
                    gen ))
              (Ok ([], [], gen))
              (List.mapi
                 (fun i value -> (i, value))
                 (AsyncTransport.fields storage))
          in
          let id, gen = freshVar gen in
          let allocation =
            match AsyncTransport.kind storage with
            | TupleStorage -> A.TupleAlloc (List.rev values)
            | RecordStorage descriptor ->
                A.RecordAlloc (descriptor, List.rev values)
            | ListStorage _ | DictStorage _ | SumStorage _ ->
                Crash.crash
                  "Async collection demand entered fixed-block allocation"
          in
          Ok (A.Var id, before @ [ (id, allocation) ], gen)
    in
    let forceUses env stores operation gen =
      let* substitutions, before, gen =
        T.fold
          (fun id result ->
            let* substitutions, before, gen = result in
            let* atom, added, gen =
              match M.find_opt id stores with
              | Some storage ->
                  let atom, outer, gen = forceAtom env (A.Var id) gen in
                  let* atom, inner, gen = materializeStorage storage atom gen in
                  Ok (atom, outer @ inner, gen)
              | None -> Ok (forceAtom env (A.Var id) gen)
            in
            match atom with
            | A.Var forced when forced <> id ->
                Ok (M.add id forced substitutions, before @ added, gen)
            | _ -> Ok (substitutions, before, gen))
          (ANFEffects.cexprTempUses operation)
          (Ok (M.empty, [], gen))
      in
      Ok (InliningCommon.renameCExpr substitutions operation, before, gen)
    in
    let rec joinTypes types = function
      | A.Return _ | A.Jump _ -> types
      | A.If (_, yes, no) -> joinTypes (joinTypes types yes) no
      | A.Let (_, _, rest) -> joinTypes types rest
      | A.Join (parameter, continuation, entry) ->
          joinTypes
            (joinTypes
               (M.add parameter.A.id parameter.A.typ types)
               continuation)
            entry
    in
    let rec expression (fn : A.functionDef) nativeReturn env stores body gen =
      let signature = summary summaries fn.A.id in

      let joinPayloads = joinTypes M.empty fn.A.body in
      let prepare payload atom gen =
        match lookup env atom with
        | Some _ -> (atom, [], gen)
        | None -> ready payload atom gen
      in
      match body with
      | A.Return atom ->
          if changedCallable fn atom then
            unsupported fn.A.name "an escaping callable result"
          else if nativeReturn then
            let* atom, before, gen =
              match lookup stores atom with
              | Some storage ->
                  let atom, outer, gen = forceAtom env atom gen in
                  let* atom, inner, gen = materializeStorage storage atom gen in
                  Ok (atom, outer @ inner, gen)
              | None -> Ok (forceAtom env atom gen)
            in
            Ok (wraps before (A.Return atom), gen)
          else
            let* atom, before, gen =
              prepareValue signature.result fn.A.returnType atom
                (modeOf env stores atom) gen
            in
            Ok (wraps before (A.Return atom), gen)
      | A.Jump (target, atom) ->
          if changedCallable fn atom then
            unsupported fn.A.name "a callable join"
          else
            let payload =
              match M.find_opt target joinPayloads with
              | Some typ -> typ
              | None -> Crash.crash "Async join lost its source type"
            in
            let mode =
              match M.find_opt target signature.joins with
              | Some mode -> mode
              | None -> Concrete
            in
            let* atom, before, gen =
              prepareValue mode payload atom (modeOf env stores atom) gen
            in
            Ok (wraps before (A.Jump (target, atom)), gen)
      | A.If (condition, yes, no) ->
          let condition, before, gen = forceAtom env condition gen in
          let* yes, gen = expression fn nativeReturn env stores yes gen in
          let* no, gen = expression fn nativeReturn env stores no gen in
          Ok (wraps before (A.If (condition, yes, no)), gen)
      | A.Join (parameter, continuation, entry) ->
          let mode =
            match M.find_opt parameter.A.id signature.joins with
            | Some mode -> mode
            | None -> Concrete
          in
          let* continuationEnv, continuationStores =
            bindMode info env stores parameter.A.id parameter.A.typ mode
          in
          let* continuation, gen =
            expression fn nativeReturn continuationEnv continuationStores
              continuation gen
          in
          let* entry, gen = expression fn nativeReturn env stores entry gen in
          let parameter =
            {
              parameter with
              A.typ = AsyncTransport.physicalType mode parameter.A.typ;
            }
          in
          Ok (A.Join (parameter, continuation, entry), gen)
      | A.Let (id, operation, rest) ->
          let* dictionary =
            Result.map_error
              (fun message -> Diagnostic message)
              (dictionaryCall env stores operation)
          in
          let dictionaryMapping =
            dictionaryMappingCall fn env stores summaries operation
          in
          let* builder =
            Result.map_error
              (fun message -> Diagnostic message)
              (listBuildCall env stores operation)
          in
          let mapping = mappingCall fn env stores summaries operation in
          let folding = foldingCall fn env stores summaries operation in
          let* () =
            let hasList =
              T.exists
                (fun id ->
                  match
                    Option.map AsyncTransport.kind (M.find_opt id stores)
                  with
                  | Some (ListStorage _) -> true
                  | _ -> false)
                (ANFEffects.cexprTempUses operation)
            in
            let hasDictionary =
              T.exists
                (fun id ->
                  match
                    Option.map AsyncTransport.kind (M.find_opt id stores)
                  with
                  | Some (DictStorage _) -> true
                  | _ -> false)
                (ANFEffects.cexprTempUses operation)
            in
            let supported =
              match operation with
              | A.Atom _ | A.TypedAtom _ | A.Call _ | A.BorrowedCall _
              | A.TailCall _ | A.ClosureAlloc _ | A.ClosureCall _
              | A.TupleAlloc _ | A.RecordAlloc _ | A.RecordClone _
              | A.SumAlloc _ | A.RawSlotInit _ ->
                  true
              | _ -> false
            in
            if
              hasList && (not supported)
              && Option.is_none (listEmptyCheck stores operation)
            then unsupported fn.A.name "this list inspection"
            else if
              hasDictionary && (not supported)
              && Option.is_none (listEmptyCheck stores operation)
            then unsupported fn.A.name "this dictionary inspection"
            else if
              hasDictionary && Option.is_none dictionary
              && Option.is_none dictionaryMapping
              &&
              match operation with
              | A.Call (callee, arguments)
              | A.BorrowedCall (callee, arguments)
              | A.TailCall (callee, arguments)
                when not (sourceCall env stores callee arguments) -> (
                  match name callee with
                  | Some name ->
                      String.starts_with ~prefix:"Darklang.Stdlib.Dict." name
                  | None -> false)
              | _ -> false
            then unsupported fn.A.name "this dictionary operation"
            else Ok ()
          in
          let* () =
            let storageEscape =
              match operation with
              | A.Call (callee, args)
              | A.BorrowedCall (callee, args)
              | A.TailCall (callee, args) ->
                  (not (eligible callee))
                  && (not (producer callee))
                  && Option.is_none builder && Option.is_none mapping
                  && Option.is_none folding && Option.is_none dictionary
                  && Option.is_none dictionaryMapping
                  && Option.is_none (listCall stores callee args)
                  && List.exists
                       (fun atom -> Option.is_some (lookup stores atom))
                       args
              | A.ClosureAlloc (callee, args) ->
                  (not (eligible callee))
                  && List.exists
                       (fun atom -> Option.is_some (lookup stores atom))
                       args
              | A.ClosureCall (closure, args) -> (
                  match callable fn closure with
                  | Some callee when eligible callee -> false
                  | _ ->
                      List.exists
                        (fun atom -> Option.is_some (lookup stores atom))
                        args)
              | A.IndirectCall (_, args) ->
                  List.exists
                    (fun atom -> Option.is_some (lookup stores atom))
                    args
              | A.IfValue (_, yes, no) ->
                  Option.is_some (lookup stores yes)
                  || Option.is_some (lookup stores no)
              | A.TypedAtom (atom, typ) -> (
                  Option.is_some (lookup stores atom)
                  &&
                  match
                    (Option.map AsyncTransport.kind (lookup stores atom), typ)
                  with
                  | Some TupleStorage, AST.TTuple _ -> false
                  | Some (RecordStorage _), AST.TRecord _ -> false
                  | Some (ListStorage _), AST.TList _ -> false
                  | Some (DictStorage _), AST.TDict _ -> false
                  | Some (SumStorage _), AST.TSum _ -> false
                  | _ -> true)
              | _ -> false
            in
            if storageEscape then
              let boundary = "an aggregate storage or call interface" in
              let boundary =
                match operation with
                | A.Call (callee, _)
                | A.BorrowedCall (callee, _)
                | A.TailCall (callee, _) ->
                    Option.fold ~none:boundary
                      ~some:(fun callee -> boundary ^ " to " ^ callee)
                      (name callee)
                | _ -> boundary
              in
              let source =
                match operation with
                | A.Call (callee, _)
                | A.BorrowedCall (callee, _)
                | A.TailCall (callee, _) -> (
                    match
                      Option.bind (name callee) AsyncDictCalls.sourceConstructor
                    with
                    | Some base -> (
                        match F.tryFind callee registry with
                        | Some
                            ( _,
                              AST.TFunction
                                ([ AST.TList (AST.TTuple [ key; value ]) ], _)
                            ) ->
                            Some (base, [ key; value ])
                        | _ ->
                            Crash.crash
                              "Dictionary entry constructor lost its source \
                               signature")
                    | None -> None)
                | _ -> None
              in
              match source with
              | Some spec ->
                  Error
                    (MissingSourceSpecializations
                       (SpecializationIdentity.SpecSet.singleton spec, sumShapes))
              | None -> unsupported fn.A.name boundary
            else Ok ()
          in
          let* storedValue =
            match operation with
            | _ when Option.is_some dictionaryMapping ->
                let mapping =
                  match dictionaryMapping with
                  | Some value -> value
                  | None ->
                      Crash.crash
                        "Async dictionary mapping lost its result plan"
                in
                let typ, mode = dictionaryMappingResult summaries mapping in
                storedResult mode typ
            | _ when Option.is_some dictionary ->
                let dictionary =
                  match dictionary with
                  | Some value -> value
                  | None ->
                      Crash.crash
                        "Async dictionary rewriting lost its operation"
                in
                storedResult dictionary.dictionaryResult
                  dictionary.dictionarySourceResult
            | A.Call (callee, arguments) when producer callee ->
                Ok (producerStorage stores callee arguments)
            | _ when Option.is_some folding ->
                let folding =
                  match folding with
                  | Some folding -> folding
                  | None -> Crash.crash "Async list fold lost its source plan"
                in
                let accumulator, _, _ = foldingModes summaries folding in
                storedResult accumulator folding.accumulatorType
            | _ when Option.is_some mapping ->
                let mapping =
                  match mapping with
                  | Some mapping -> mapping
                  | None ->
                      Crash.crash "Async list mapping lost its source plan"
                in
                let resultType, result = mappingResult summaries mapping in
                storedResult result resultType
            | _ when Option.is_some builder ->
                let element, storage =
                  match builder with
                  | Some (_, element, _, storage, _, _) -> (element, storage)
                  | None ->
                      Crash.crash "Async list builder lost its source plan"
                in
                Result.map Option.some
                  (AsyncTransport.instantiateStorage info storage
                     (AST.TList element))
            | A.Call (callee, args)
            | A.BorrowedCall (callee, args)
            | A.TailCall (callee, args)
              when Option.is_some (listCall stores callee args) ->
                let list, storage, result =
                  match listCall stores callee args with
                  | Some (_, list, _, storage, result) -> (list, storage, result)
                  | None -> Crash.crash "Async list call lost its source plan"
                in
                Ok (snd (listResult (lookup env list) storage result))
            | A.Call (callee, args)
            | A.BorrowedCall (callee, args)
            | A.TailCall (callee, args)
              when sourceCall env stores callee args ->
                storedResult (summary summaries callee).result
                  (originalResult callee)
            | A.ClosureCall (closure, _) -> (
                match callable fn closure with
                | Some callee
                  when eligible callee && not (S.mem callee nativeEntries) ->
                    storedResult (summary summaries callee).result
                      (originalResult callee)
                | _ -> Ok (storageFacts sumShapes env stores operation))
            | A.TypedAtom (_, (AST.TList _ as typ)) -> (
                match AsyncListStorage.root (listPlan fn) (A.Var id) with
                | Some (component, AST.TList element) -> (
                    match
                      listMode element (rawListMode summaries fn component)
                    with
                    | Stored storage ->
                        Result.map Option.some
                          (AsyncTransport.instantiateStorage info storage typ)
                    | Concrete -> Ok None
                    | _ ->
                        Crash.crash "Async literal list has outer pendingness")
                | _ -> Ok (storageFacts sumShapes env stores operation))
            | _ -> Ok (storageFacts sumShapes env stores operation)
          in
          let* storedValue = normalizeAllocation info operation storedValue in
          let* () =
            let escape =
              match operation with
              | _ when Option.is_some dictionaryMapping ->
                  let mapping =
                    match dictionaryMapping with
                    | Some value -> value
                    | None ->
                        Crash.crash
                          "Async dictionary mapping lost its callback plan"
                  in
                  changedCallable fn mapping.mappedDictionary
              | _ when Option.is_some folding ->
                  let folding =
                    match folding with
                    | Some folding -> folding
                    | None -> Crash.crash "Async list fold lost its source plan"
                  in
                  changedCallable fn folding.foldList
                  || changedCallable fn folding.initial
              | _ when Option.is_some mapping ->
                  let mapping =
                    match mapping with
                    | Some mapping -> mapping
                    | None ->
                        Crash.crash "Async list mapping lost its source plan"
                  in
                  changedCallable fn mapping.listValue
              | A.Call (_, args)
              | A.BorrowedCall (_, args)
              | A.TailCall (_, args)
              | A.ClosureAlloc (_, args)
              | A.TupleAlloc args
              | A.RecordAlloc (_, args)
              | A.RecordClone (_, _, args) ->
                  List.exists (changedCallable fn) args
              | A.SumAlloc (_, value) | A.RawSlotInit (_, _, value, _) ->
                  changedCallable fn value
              | A.IfValue (_, yes, no) ->
                  changedCallable fn yes || changedCallable fn no
              | _ -> false
            in
            if escape then
              unsupported fn.A.name "an escaping callable interface"
            else Ok ()
          in
          let* operation, before, value, gen =
            match operation with
            | _ when Option.is_some dictionaryMapping ->
                let mapping =
                  match dictionaryMapping with
                  | Some value -> value
                  | None ->
                      Crash.crash "Async dictionary mapping lost its call plan"
                in
                let keyMode, inputMode, outputMode =
                  dictionaryMappingModes summaries mapping
                in
                let inputType =
                  AsyncTransport.physicalType inputMode mapping.mapInputType
                in
                let outputType =
                  AsyncTransport.physicalType outputMode mapping.mapOutputType
                in
                let* callee =
                  match mapping.dictionaryMappingKind with
                  | MapDictionaryValues ->
                      helperTypes "Darklang.Stdlib.Dict.map"
                        [ mapping.mapKeyType; inputType; outputType ]
                  | FilterDictionaryValues ->
                      helperTypes "Darklang.Stdlib.Dict.filter"
                        [ mapping.mapKeyType; inputType ]
                in
                let actual = modeOf env stores mapping.mappedDictionary in
                let dictionary, before, gen =
                  forceAtom env mapping.mappedDictionary gen
                in
                let actual =
                  match actual with
                  | Pending -> Concrete
                  | PendingStored storage -> Stored storage
                  | mode -> mode
                in
                let* dictionary, added, gen =
                  prepareValue
                    (dictionaryMode mapping.mapKeyType mapping.mapInputType
                       inputMode)
                    (AST.TDict (mapping.mapKeyType, mapping.mapInputType))
                    dictionary actual gen
                in
                let callback, forced, gen =
                  forceAtom env mapping.dictionaryCallback gen
                in
                let* callback, adapterBindings, gen =
                  let needsPredicateForce =
                    match (mapping.dictionaryMappingKind, outputMode) with
                    | FilterDictionaryValues, Pending -> true
                    | FilterDictionaryValues, (Stored _ | PendingStored _) ->
                        Crash.crash
                          "Async dictionary predicate has a non-Boolean layout"
                    | _ -> false
                  in
                  if changed keyMode || needsPredicateForce then
                    let* adapter, gen =
                      dictionaryCallbackAdapter mapping keyMode inputType
                        outputType gen
                    in
                    let closure, gen = freshVar gen in
                    Ok
                      ( A.Var closure,
                        [ (closure, A.ClosureAlloc (adapter.id, [ callback ])) ],
                        gen )
                  else Ok (callback, [], gen)
                in
                Ok
                  ( A.Call (callee, [ dictionary; callback ]),
                    before @ added @ forced @ adapterBindings,
                    None,
                    gen )
            | _ when Option.is_some dictionary ->
                let dictionary =
                  match dictionary with
                  | Some value -> value
                  | None ->
                      Crash.crash "Async dictionary call lost its operation"
                in
                let* arguments, before, gen =
                  List.fold_left
                    (fun result (role, atom) ->
                      let* arguments, before, gen = result in
                      let actual = modeOf env stores atom in
                      let* atom, added, gen =
                        match role with
                        | AsyncDictCalls.Key -> (
                            let stored = lookup stores atom in
                            let atom, outer, gen = forceAtom env atom gen in
                            match stored with
                            | Some storage ->
                                let* atom, inner, gen =
                                  materializeStorage storage atom gen
                                in
                                Ok (atom, outer @ inner, gen)
                            | None -> Ok (atom, outer, gen))
                        | AsyncDictCalls.Value ->
                            prepareValue dictionary.dictionaryValueMode
                              dictionary.dictionaryValue atom actual gen
                        | AsyncDictCalls.Dictionary ->
                            let atom, outer, gen = forceAtom env atom gen in
                            let actual =
                              match actual with
                              | Pending -> Concrete
                              | PendingStored storage -> Stored storage
                              | mode -> mode
                            in
                            let* atom, inner, gen =
                              prepareValue
                                (dictionaryMode dictionary.dictionaryKey
                                   dictionary.dictionaryValue
                                   dictionary.dictionaryValueMode)
                                (AST.TDict
                                   ( dictionary.dictionaryKey,
                                     dictionary.dictionaryValue ))
                                atom actual gen
                            in
                            Ok (atom, outer @ inner, gen)
                      in
                      Ok (atom :: arguments, before @ added, gen))
                    (Ok ([], [], gen))
                    dictionary.dictionaryArguments
                in
                let physicalValue =
                  AsyncTransport.physicalType dictionary.dictionaryValueMode
                    dictionary.dictionaryValue
                in
                let* callee =
                  helperTypes
                    dictionary.dictionaryOperation.AsyncDictCalls.callee
                    [ dictionary.dictionaryKey; physicalValue ]
                in
                let result, gen = freshVar gen in
                let output =
                  AsyncTransport.physicalType dictionary.dictionaryResult
                    dictionary.dictionarySourceResult
                in
                Ok
                  ( A.TypedAtom (A.Var result, output),
                    before @ [ (result, A.Call (callee, List.rev arguments)) ],
                    None,
                    gen )
            | _ when Option.is_some folding ->
                let folding =
                  match folding with
                  | Some folding -> folding
                  | None -> Crash.crash "Async list fold lost its source plan"
                in
                let accumulatorMode, elementMode, resultMode =
                  foldingModes summaries folding
                in
                if accumulatorMode <> resultMode then
                  Crash.crash
                    "Async fold step and accumulator interfaces differ";
                let elementType =
                  AsyncTransport.physicalType elementMode folding.elementType
                in
                let accumulatorType =
                  AsyncTransport.physicalType accumulatorMode
                    folding.accumulatorType
                in
                let* callee =
                  helperTypes "Darklang.Stdlib.List.fold"
                    [ elementType; accumulatorType ]
                in
                let actual = modeOf env stores folding.foldList in
                let values, before, gen = forceAtom env folding.foldList gen in
                let actual =
                  match actual with
                  | Pending -> Concrete
                  | PendingStored storage -> Stored storage
                  | mode -> mode
                in
                let* values, added, gen =
                  prepareValue
                    (listMode folding.elementType elementMode)
                    (AST.TList folding.elementType) values actual gen
                in
                let* initial, promoted, gen =
                  prepareValue accumulatorMode folding.accumulatorType
                    folding.initial
                    (modeOf env stores folding.initial)
                    gen
                in
                let* value, _ =
                  modeFacts info folding.accumulatorType accumulatorMode
                in
                Ok
                  ( A.Call (callee, [ values; initial; folding.step ]),
                    before @ added @ promoted,
                    value,
                    gen )
            | _ when Option.is_some mapping ->
                let mapping =
                  match mapping with
                  | Some mapping -> mapping
                  | None ->
                      Crash.crash "Async list mapping lost its source plan"
                in
                let inputMode, resultMode = mappingModes summaries mapping in
                let inputType =
                  AsyncTransport.physicalType inputMode mapping.inputType
                in
                let outputType =
                  AsyncTransport.physicalType resultMode mapping.outputType
                in
                let* callee, callback, adapterBindings, gen =
                  match mapping.mappingKind with
                  | MapElements ->
                      let* callee =
                        helperTypes "Darklang.Stdlib.List.map"
                          [ inputType; outputType ]
                      in
                      Ok (callee, mapping.callback, [], gen)
                  | PredicateElements predicate -> (
                      let operation =
                        match predicate with
                        | FilterPredicate -> "filter"
                        | AnyPredicate -> "any"
                        | AllPredicate -> "all"
                        | TakeWhilePredicate -> "takeWhile"
                        | DropWhilePredicate -> "dropWhile"
                      in
                      let* callee =
                        helper ("Darklang.Stdlib.List." ^ operation) inputType
                      in
                      match resultMode with
                      | Concrete -> Ok (callee, mapping.callback, [], gen)
                      | Pending ->
                          let* adapter, gen = predicateAdapter inputType gen in
                          let closure, gen = freshVar gen in
                          Ok
                            ( callee,
                              A.Var closure,
                              [
                                ( closure,
                                  A.ClosureAlloc
                                    (adapter.id, [ mapping.callback ]) );
                              ],
                              gen )
                      | Stored _ | PendingStored _ ->
                          Crash.crash
                            "Async predicate result has a non-Boolean layout")
                in
                let actual = modeOf env stores mapping.listValue in
                let listValue, before, gen =
                  forceAtom env mapping.listValue gen
                in
                let actual =
                  match actual with
                  | Pending -> Concrete
                  | PendingStored storage -> Stored storage
                  | mode -> mode
                in
                let* listValue, added, gen =
                  prepareValue
                    (listMode mapping.inputType inputMode)
                    (AST.TList mapping.inputType) listValue actual gen
                in
                Ok
                  ( A.Call (callee, [ listValue; callback ]),
                    before @ added @ adapterBindings,
                    None,
                    gen )
            | _ when Option.is_some builder -> (
                let base, element, mode, storage, arguments, result =
                  match builder with
                  | Some plan -> plan
                  | None ->
                      Crash.crash "Async list builder lost its source plan"
                in
                let* conditional =
                  match (result, arguments) with
                  | PendingStored _, [ (_, left); (_, right) ] ->
                      let leftMode =
                        match modeOf env stores left with
                        | Pending | PendingStored _ -> PendingStored storage
                        | _ -> Stored storage
                      in
                      let leftActual = modeOf env stores left in
                      let rightActual = modeOf env stores right in
                      let* left, before, gen =
                        prepareValue leftMode (AST.TList element) left
                          leftActual gen
                      in
                      let* right, added, gen =
                        prepareValue (PendingStored storage) (AST.TList element)
                          right rightActual gen
                      in
                      let* bridge, gen =
                        appendBridge element mode leftMode gen
                      in
                      let* value =
                        info
                          (AsyncTransport.physicalType (Stored storage)
                             (AST.TList element))
                      in
                      Ok
                        (Some
                           ( A.Call (bridge.id, [ left; right ]),
                             before @ added,
                             Some value,
                             gen ))
                  | Stored _, _ -> Ok None
                  | _ ->
                      Crash.crash
                        "Async list builder has an inconsistent result \
                         interface"
                in
                match conditional with
                | Some lowered -> Ok lowered
                | None ->
                    let physical = AsyncTransport.physicalType mode element in
                    let* callee = helper base physical in
                    let* arguments, before, gen =
                      List.fold_left
                        (fun result (role, atom) ->
                          let* arguments, before, gen = result in
                          let actual = modeOf env stores atom in
                          let source, expected, atom, actual, outer, gen =
                            match role with
                            | AsyncListCalls.ElementInput ->
                                (element, mode, atom, actual, [], gen)
                            | AsyncListCalls.ListInput ->
                                let atom, outer, gen = forceAtom env atom gen in
                                let actual =
                                  match actual with
                                  | Pending -> Concrete
                                  | PendingStored storage -> Stored storage
                                  | mode -> mode
                                in
                                ( AST.TList element,
                                  Stored storage,
                                  atom,
                                  actual,
                                  outer,
                                  gen )
                          in
                          let* atom, added, gen =
                            prepareValue expected source atom actual gen
                          in
                          Ok (atom :: arguments, before @ outer @ added, gen))
                        (Ok ([], [], gen))
                        arguments
                    in
                    Ok (A.Call (callee, List.rev arguments), before, None, gen))
            | operation when Option.is_some (listEmptyCheck stores operation) ->
                let op, list =
                  match listEmptyCheck stores operation with
                  | Some check -> check
                  | None ->
                      Crash.crash "Async list empty check lost its source plan"
                in
                let list, before, gen = forceAtom env list gen in
                Ok
                  ( A.Prim (op, list, A.IntLiteral (A.Int64 0L)),
                    before,
                    None,
                    gen )
            | A.Call (callee, arguments)
            | A.BorrowedCall (callee, arguments)
            | A.TailCall (callee, arguments)
              when Option.is_some (listCall stores callee arguments) -> (
                let base, list, rest, storage, result =
                  match listCall stores callee arguments with
                  | Some value -> value
                  | None ->
                      Crash.crash "Async list inspection lost its source plan"
                in
                let element =
                  match AsyncTransport.kind storage with
                  | ListStorage element -> element
                  | _ ->
                      Crash.crash "Async list inspection has a non-list layout"
                in
                let physical =
                  AsyncTransport.physicalType (listElementMode storage) element
                in
                let* callee = helper base physical in
                let value, _ = listResult (lookup env list) storage result in
                match (result, rest) with
                | ListPrefix prefix, [ count ] -> (
                    let count, before, gen = forceAtom env count gen in
                    match lookup env list with
                    | None ->
                        Ok (A.Call (callee, [ list; count ]), before, value, gen)
                    | Some _ ->
                        let* bridge, gen = prefixBridge prefix physical gen in
                        Ok
                          ( A.Call (bridge.id, [ list; count ]),
                            before,
                            value,
                            gen ))
                | ListPrefix _, _ ->
                    Crash.crash
                      "Async list prefix has an inconsistent argument count"
                | (ListScalar | ListElement | ListRemainder), _ ->
                    let list, before, gen = forceAtom env list gen in
                    Ok (A.Call (callee, list :: rest), before, value, gen))
            | A.RawSlotInit (pointer, offset, value, typ)
              when Option.is_some
                     (AsyncListStorage.slot (listPlan fn) pointer offset) ->
                let component, slot =
                  match AsyncListStorage.slot (listPlan fn) pointer offset with
                  | Some value -> value
                  | None -> Crash.crash "Async list write lost its source plan"
                in
                let mode = rawListMode summaries fn component in
                let expected =
                  match slot with
                  | AsyncListStorage.Element _ -> mode
                  | AsyncListStorage.Link (AST.TList element) ->
                      listMode element mode
                  | AsyncListStorage.Link _ ->
                      Crash.crash "Async list link has a non-list source type"
                in
                let* value, before, gen =
                  prepareValue expected typ value (modeOf env stores value) gen
                in
                let typ = AsyncTransport.physicalType expected typ in
                Ok
                  ( A.RawSlotInit (pointer, offset, value, typ),
                    before,
                    None,
                    gen )
            | A.Call (callee, arguments) when producer callee ->
                let source = originalResult callee in
                let payload =
                  match storedValue with
                  | None -> source
                  | Some storage -> storageType storage source
                in
                let* value = info payload in
                let* operation, before, gen =
                  match (name callee, arguments) with
                  | Some name, [ argument ] when specialized transportName name
                    ->
                      if Option.is_some (lookup env argument) then
                        unsupported fn.A.name "nested source transport"
                      else
                        Ok
                          ( A.TypedAtom (argument, carrier value.payload),
                            [],
                            gen )
                  | Some name, [ delay; argument ]
                    when specialized delayName name ->
                      let delay, before, gen = forceAtom env delay gen in
                      let expected =
                        match storedValue with
                        | None -> Pending
                        | Some storage ->
                            PendingStored (AsyncTransport.interface storage)
                      in
                      let* argument, added, gen =
                        prepareValue expected source argument
                          (modeOf env stores argument)
                          gen
                      in
                      let* delayHelper =
                        helper "Darklang.Stdlib.__Task.__delayValueCurrent"
                          value.payload
                      in
                      Ok
                        ( A.Call (delayHelper, [ delay; argument ]),
                          before @ added,
                          gen )
                  | _ ->
                      Error
                        (Diagnostic "Invalid async source operation arguments")
                in
                Ok (operation, before, Some value, gen)
            | A.Call (callee, arguments)
            | A.BorrowedCall (callee, arguments)
            | A.TailCall (callee, arguments)
              when sourceCall env stores callee arguments ->
                let calleeDefinition =
                  match F.tryFind callee definitions with
                  | Some definition -> definition
                  | None -> Crash.crash "Async direct call lost its body"
                in
                let calleeSummary = summary summaries callee in
                let rec argumentsLoop arguments parameters modes reversed before
                    gen =
                  match (arguments, parameters, modes) with
                  | [], [], [] -> Ok (List.rev reversed, before, gen)
                  | ( argument :: rest,
                      (parameter : A.typedParam) :: parameters,
                      mode :: modes ) ->
                      let* argument, added, gen =
                        prepareValue mode parameter.A.typ argument
                          (modeOf env stores argument)
                          gen
                      in
                      argumentsLoop rest parameters modes (argument :: reversed)
                        (before @ added) gen
                  | _ ->
                      Error
                        (Diagnostic
                           "Async direct call has an invalid argument interface")
                in
                let* arguments, before, gen =
                  argumentsLoop arguments calleeDefinition.A.typedParams
                    calleeSummary.parameters [] [] gen
                in
                let callee =
                  match F.tryFind callee versions with
                  | Some version -> version.id
                  | None -> callee
                in
                let* value =
                  match calleeSummary.result with
                  | Pending ->
                      Result.map Option.some
                        (info calleeDefinition.A.returnType)
                  | PendingStored storage ->
                      Result.map Option.some
                        (info
                           (AsyncTransport.physicalType (Stored storage)
                              calleeDefinition.A.returnType))
                  | Concrete | Stored _ -> Ok None
                in
                Ok (A.Call (callee, arguments), before, value, gen)
            | A.ClosureAlloc (callee, captures) when eligible callee ->
                let definition =
                  match F.tryFind callee definitions with
                  | Some definition -> definition
                  | None -> Crash.crash "Async closure lost its source body"
                in
                let fields =
                  match definition.A.typedParams with
                  | { A.typ = AST.TTuple (_ :: fields); _ } :: _ -> fields
                  | _ ->
                      Crash.crash "Async closure has no environment parameter"
                in
                let modes = (summary summaries callee).captures in
                let* captures, before, gen =
                  List.fold_left2
                    (fun result (atom, typ) mode ->
                      let* values, before, gen = result in
                      let* atom, added, gen =
                        prepareValue mode typ atom (modeOf env stores atom) gen
                      in
                      Ok (atom :: values, before @ added, gen))
                    (Ok ([], [], gen))
                    (List.combine captures fields)
                    modes
                in
                let versions =
                  if S.mem callee nativeEntries then nativeVersions
                  else versions
                in
                let callee =
                  match F.tryFind callee versions with
                  | Some version -> version.id
                  | None -> callee
                in
                Ok
                  (A.ClosureAlloc (callee, List.rev captures), before, None, gen)
            | A.SumAlloc (variant, value) when Option.is_some storedValue ->
                let storage =
                  match storedValue with
                  | Some storage -> storage
                  | None ->
                      Crash.crash
                        "Async variant allocation lost its payload layout"
                in
                let expected =
                  sumField
                    (AsyncTransport.interface storage)
                    variant.A.variantTag
                  |> AsyncTransport.slotMode
                in
                let* value, before, gen =
                  prepareValue expected variant.A.payloadType value
                    (modeOf env stores value) gen
                in
                let payloadType =
                  slotRepresentation variant.A.payloadType
                    (sumField storage variant.A.variantTag)
                in
                Ok
                  ( A.SumAlloc
                      ( {
                          variant with
                          A.sum = physicalSumDescriptor storage;
                          payloadType;
                        },
                        value ),
                    before,
                    None,
                    gen )
            | A.SumTest (sum, tag, atom) ->
                let sum =
                  match lookup stores atom with
                  | Some storage -> physicalSumDescriptor storage
                  | None -> sum
                in
                let atom, before, gen = forceAtom env atom gen in
                Ok (A.SumTest (sum, tag, atom), before, None, gen)
            | A.SumPayload (variant, atom) ->
                let variant, value =
                  match lookup stores atom with
                  | None -> (variant, None)
                  | Some storage ->
                      let slot = sumField storage variant.A.variantTag in
                      ( {
                          variant with
                          A.sum = physicalSumDescriptor storage;
                          payloadType =
                            slotRepresentation variant.A.payloadType slot;
                        },
                        pendingField slot )
                in
                let atom, before, gen = forceAtom env atom gen in
                Ok (A.SumPayload (variant, atom), before, value, gen)
            | A.SumConstant _ | A.SumAlloc _ -> Ok (operation, [], None, gen)
            | A.RecordAlloc (descriptor, values)
            | A.RecordClone (descriptor, _, values)
              when Option.is_some storedValue ->
                let storage =
                  match storedValue with
                  | Some storage -> storage
                  | None ->
                      Crash.crash "Async record allocation lost its layout"
                in
                let expected =
                  AsyncTransport.fields (AsyncTransport.interface storage)
                in
                if
                  List.length expected <> List.length values
                  || List.length descriptor.A.fields <> List.length values
                then
                  Crash.crash
                    "Async record promotion has inconsistent field arities";
                let* values, before, gen =
                  List.fold_left2
                    (fun result ((_, source), value) slot ->
                      let* values, before, gen = result in
                      let* value, added, gen =
                        prepareValue
                          (AsyncTransport.slotMode slot)
                          source value (modeOf env stores value) gen
                      in
                      Ok (value :: values, before @ added, gen))
                    (Ok ([], [], gen))
                    (List.combine descriptor.A.fields values)
                    expected
                in
                Ok
                  ( A.RecordAlloc
                      (storageDescriptor storage descriptor, List.rev values),
                    before,
                    None,
                    gen )
            | A.RecordClone (descriptor, original, values)
              when Option.is_some (lookup stores original) ->
                Ok (A.RecordAlloc (descriptor, values), [], None, gen)
            | A.RecordGet (descriptor, atom, index)
              when Option.is_some (lookup stores atom) ->
                let storage =
                  match lookup stores atom with
                  | Some storage -> storage
                  | None ->
                      Crash.crash "Async record projection lost its layout"
                in
                let value =
                  match storageField storage index with
                  | Some (PendingSlot value) | Some (PendingStorage (value, _))
                    ->
                      Some value
                  | _ -> None
                in
                let atom, before, gen = forceAtom env atom gen in
                Ok
                  ( A.RecordGet
                      (storageDescriptor storage descriptor, atom, index),
                    before,
                    value,
                    gen )
            | A.TupleAlloc _ -> Ok (operation, [], None, gen)
            | A.TupleGet (atom, index) -> (
                let value =
                  match lookup stores atom with
                  | Some fields -> (
                      match storageField fields index with
                      | Some (PendingSlot value)
                      | Some (PendingStorage (value, _)) ->
                          Some value
                      | _ -> None)
                  | None -> None
                in
                match (value, lookup stores atom) with
                | Some value, _ ->
                    let atom, before, gen = forceAtom env atom gen in
                    Ok (A.TupleGet (atom, index), before, Some value, gen)
                | None, Some _ ->
                    let atom, before, gen = forceAtom env atom gen in
                    Ok (A.TupleGet (atom, index), before, None, gen)
                | None, None ->
                    let* operation, before, gen =
                      forceUses env stores operation gen
                    in
                    Ok (operation, before, None, gen))
            | A.Atom atom -> Ok (operation, [], lookup env atom, gen)
            | A.TypedAtom (atom, typ) when Option.is_some storedValue ->
                let mode =
                  match (modeOf env stores atom, storedValue) with
                  | Concrete, Some storage ->
                      Stored (AsyncTransport.interface storage)
                  | mode, _ -> mode
                in
                let typ = AsyncTransport.physicalType mode typ in
                Ok (A.TypedAtom (atom, typ), [], lookup env atom, gen)
            | A.TypedAtom (atom, typ) -> (
                match lookup env atom with
                | Some value when typ = value.payload ->
                    Ok (A.TypedAtom (atom, carrier typ), [], Some value, gen)
                | _ ->
                    let* operation, before, gen =
                      forceUses env stores operation gen
                    in
                    Ok (operation, before, None, gen))
            | A.IfValue (condition, yes, no) -> (
                let condition, before, gen = forceAtom env condition gen in
                let value =
                  match lookup env yes with
                  | Some _ as value -> value
                  | None -> lookup env no
                in
                match value with
                | None -> Ok (A.IfValue (condition, yes, no), before, None, gen)
                | Some value ->
                    let yes, left, gen = prepare value.payload yes gen in
                    let no, right, gen = prepare value.payload no gen in
                    Ok
                      ( A.IfValue (condition, yes, no),
                        before @ left @ right,
                        Some value,
                        gen ))
            | A.ClosureCall (closure, arguments)
              when match callable fn closure with
                   | Some callee -> eligible callee
                   | None -> false ->
                let callee =
                  match callable fn closure with
                  | Some callee -> callee
                  | None -> Crash.crash "Async closure call lost its target"
                in
                let state = summary summaries callee in
                let definition =
                  match F.tryFind callee definitions with
                  | Some definition -> definition
                  | None ->
                      Crash.crash "Async closure call lost its source body"
                in
                let parameters, modes =
                  match (definition.A.typedParams, state.parameters) with
                  | _ :: parameters, _ :: modes -> (parameters, modes)
                  | _ ->
                      Crash.crash
                        "Async closure call has no environment parameter"
                in
                let* arguments, before, gen =
                  List.fold_left2
                    (fun result (atom, (parameter : A.typedParam)) mode ->
                      let* values, before, gen = result in
                      let* atom, added, gen =
                        prepareValue mode parameter.A.typ atom
                          (modeOf env stores atom) gen
                      in
                      Ok (atom :: values, before @ added, gen))
                    (Ok ([], [], gen))
                    (List.combine arguments parameters)
                    modes
                in
                let* value =
                  if S.mem callee nativeEntries then Ok None
                  else
                    match state.result with
                    | Pending ->
                        Result.map Option.some (info definition.A.returnType)
                    | PendingStored storage ->
                        Result.map Option.some
                          (info
                             (AsyncTransport.physicalType (Stored storage)
                                definition.A.returnType))
                    | Concrete | Stored _ -> Ok None
                in
                Ok
                  ( A.ClosureCall (closure, List.rev arguments),
                    before,
                    value,
                    gen )
            | A.ClosureCall (callee, arguments)
            | A.IndirectCall (callee, arguments) ->
                if
                  List.exists
                    (fun atom -> Option.is_some (lookup env atom))
                    arguments
                then unsupported fn.A.name "an indirect argument interface"
                else
                  let callee, before, gen = forceAtom env callee gen in
                  let operation =
                    match operation with
                    | A.ClosureCall _ -> A.ClosureCall (callee, arguments)
                    | _ -> A.IndirectCall (callee, arguments)
                  in
                  Ok (operation, before, None, gen)
            | _ ->
                let hasPending =
                  T.exists
                    (fun id -> M.mem id env || M.mem id stores)
                    (ANFEffects.cexprTempUses operation)
                in
                if not hasPending then Ok (operation, [], None, gen)
                else
                  let* () =
                    match operation with
                    | A.Prim _ | A.UnaryPrim _ | A.TupleGet _ | A.RecordGet _
                    | A.StringConcat _ | A.CanonicalBufferEq _ | A.Print _
                    | A.StdoutWrite _ | A.RuntimeErrorString _ | A.FloatSqrt _
                    | A.FloatAbs _ | A.FloatNeg _ | A.Int64ToFloat _
                    | A.FloatToInt64 _ | A.FloatToBits _ | A.FloatToString _
                    | A.RawAlloc _ | A.MappedAlloc _ | A.RawFree _
                    | A.MappedFree _ | A.RawGet _ | A.RawTake _ | A.RawGetByte _
                    | A.RawWriteWord _ | A.RawWriteByte _ | A.StringToRawPtr _
                    | A.RawPtrToString _ | A.BlobToRawPtr _ | A.RawPtrToBlob _
                    | A.RawPtrToInt128 _ | A.RawPtrToUInt128 _
                    | A.DictToRawPtr _ | A.ListToRawPtr _
                    | A.FixedBlockToRawPtr _ | A.RawPtrToDict _
                    | A.RawPtrToList _ | A.Sleep _ | A.CliNative _
                    | A.FileReadBlob _ | A.FileExists _ | A.FileWriteBlob _
                    | A.FileAppendText _ | A.FileDelete _
                    | A.FileCreateDirectory _ | A.FileSetExecutable _
                    | A.FileWriteFromPtr _ ->
                        Ok ()
                    | _ -> unsupported fn.A.name "this call or storage boundary"
                  in
                  let* operation, before, gen =
                    forceUses env stores operation gen
                  in
                  Ok (operation, before, None, gen)
          in
          let stores =
            match storedValue with
            | Some fields -> M.add id fields stores
            | None -> M.remove id stores
          in
          let env =
            match value with
            | Some value -> M.add id value env
            | None -> M.remove id env
          in
          let* rest, gen = expression fn nativeReturn env stores rest gen in
          Ok (wraps before (A.Let (id, operation, rest)), gen)
    in
    let initialTemp =
      1
      + F.fold
          (fun maximum _ fn -> max maximum (ANF_to_MIR.maxTempIdInFunction fn))
          0 definitions
    in
    let rewrite (fn : A.functionDef) nativeReturn modes gen =
      let* env, stores =
        seed (fun message -> Diagnostic message) fn summaries modes info
      in
      expression fn nativeReturn env stores fn.A.body gen
    in
    let* originals, gen =
      List.fold_left
        (fun result (fn : A.functionDef) ->
          let* values, gen = result in
          if
            nativeRuntime fn.A.name
            || modeChanges fn.A.id
               && ((not (boundary fn))
                  || List.exists changed (summary summaries fn.A.id).captures)
          then Ok (fn :: values, gen)
          else
            let* body, gen =
              rewrite fn (boundary fn)
                (List.map (fun _ -> Concrete) fn.A.typedParams)
                gen
            in
            Ok ({ fn with A.body } :: values, gen))
        (Ok
           ( [],
             {
               variables = A.VarGen initialTemp;
               cursor = nextFunctionOrdinal;
               generatedFunctions = [];
               mappers = Names.empty;
             } ))
        functions
    in
    let clone nativeReturn (fn : A.functionDef) (version : version) gen =
      let state = summary summaries fn.A.id in
      let modes =
        if nativeReturn then List.map (fun _ -> Concrete) state.parameters
        else state.parameters
      in
      let* body, gen = rewrite fn nativeReturn modes gen in
      let* parameterEnv, parameterStores =
        seed
          (fun message -> Diagnostic message)
          fn summaries modes
          (fun typ -> Ok typ)
      in
      let parameters, substitutions, gen =
        List.fold_left
          (fun (parameters, substitutions, gen) (parameter : A.typedParam) ->
            let id, gen = freshVar gen in
            let mode =
              modeOf parameterEnv parameterStores (A.Var parameter.A.id)
            in
            let typ = AsyncTransport.physicalType mode parameter.A.typ in
            ( { A.id; typ } :: parameters,
              M.add parameter.A.id id substitutions,
              gen ))
          ([], M.empty, gen) fn.A.typedParams
      in
      let body, variables =
        InliningCommon.renameExpr substitutions gen.variables body
      in
      let gen = { gen with variables } in
      let returnType =
        AsyncTransport.physicalType
          (if nativeReturn then Concrete else state.result)
          fn.A.returnType
      in
      Ok
        ( {
            A.id = version.id;
            name = version.name;
            typedParams = List.rev parameters;
            returnType;
            returnOwnership = A.OwnedReturn;
            body;
          },
          gen )
    in
    let addVersions nativeReturn versions result =
      F.fold
        (fun result source version ->
          let* values, gen = result in
          let definition =
            match F.tryFind source definitions with
            | Some definition -> definition
            | None -> Crash.crash "Async version lost its source definition"
          in
          let* value, gen = clone nativeReturn definition version gen in
          Ok (value :: values, gen))
        result versions
    in
    let* cloned, gen = addVersions false versions (Ok ([], gen)) in
    let* cloned, gen = addVersions true nativeVersions (Ok (cloned, gen)) in
    let functions =
      ANFDeadCodeElimination.filterReachableFunctions local
        (List.rev originals @ cloned @ List.rev gen.generatedFunctions)
    in
    Ok { functions; nextFunctionOrdinal = gen.cursor; sumShapes }
