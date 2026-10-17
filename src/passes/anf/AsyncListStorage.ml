(* AsyncListStorage.ml - Preserve one element representation across connected skew-list nodes. *)
[@@@warning "-4"]

module A = ANF
module M = InliningCommon.TempMap

type slot = Element of AST.semanticType | Link of AST.semanticType
type node = { tag : int64; listType : AST.semanticType }
type cluster = Root of int * A.tempId | Parent of A.tempId

type t = {
  bindings : A.cExpr M.t;
  nodes : node M.t;
  roots : A.tempId M.t;
  components : A.tempId M.t;
}

let rec bindings result = function
  | A.Return _ | A.Jump _ -> result
  | A.Let (id, operation, rest) -> bindings (M.add id operation result) rest
  | A.If (_, yes, no) -> bindings (bindings result yes) no
  | A.Join (_, continuation, entry) ->
      bindings (bindings result entry) continuation

let integer = function A.IntLiteral (A.Int64 value) -> Some value | _ -> None

let rec pointer source roots = function
  | A.Var id -> (
      match M.find_opt id roots with
      | Some value -> Some value
      | None -> (
          match M.find_opt id source with
          | Some (A.Atom value | A.TypedAtom (value, _)) ->
              pointer source roots value
          | _ -> None))
  | _ -> None

let rec representative clusters id =
  match M.find_opt id clusters with
  | Some (Root (size, minimum)) -> (id, size, minimum)
  | Some (Parent parent) -> representative clusters parent
  | None -> Crash.crash "Async list component has an unknown allocation"

let minimum (A.TempId left as a) (A.TempId right as b) =
  if left <= right then a else b

let union clusters left right =
  let left, leftSize, leftMinimum = representative clusters left in
  let right, rightSize, rightMinimum = representative clusters right in
  if left = right then clusters
  else
    let root, child =
      if leftSize >= rightSize then (left, right) else (right, left)
    in
    M.add child (Parent root)
      (M.add root
         (Root (leftSize + rightSize, minimum leftMinimum rightMinimum))
         clusters)

let nodeSlot node offset =
  match node.listType with
  | AST.TList element ->
      if (node.tag = 2L || node.tag = 3L) && offset = 0L then
        Some (Element element)
      else if
        (node.tag = 3L && (offset = 8L || offset = 16L))
        || (node.tag = 1L && (offset = 16L || offset = 24L))
      then Some (Link node.listType)
      else None
  | _ -> Crash.crash "Async list allocation has a non-list type"

let create (fn : A.functionDef) =
  let source = bindings M.empty fn.A.body in
  let nodes, roots =
    M.fold
      (fun id operation (nodes, roots) ->
        match operation with
        | A.TypedAtom (A.Var tagged, (AST.TList _ as listType)) -> (
            match M.find_opt tagged source with
            | Some (A.Prim (A.BitOr, A.Var pointer, tag)) -> (
                match (integer tag, M.find_opt pointer source) with
                | Some tag, Some (A.RawAlloc size) ->
                    let valid =
                      match (tag, integer size) with
                      | 1L, Some 40L | 2L, Some 16L | 3L, Some 32L -> true
                      | _ -> false
                    in
                    if not valid then (nodes, roots)
                    else
                      ( M.add pointer { tag; listType } nodes,
                        M.add tagged pointer (M.add id pointer roots) )
                | _ -> (nodes, roots))
            | _ -> (nodes, roots))
        | _ -> (nodes, roots))
      source (M.empty, M.empty)
  in
  let clusters = M.mapi (fun id _ -> Root (1, id)) nodes in
  let clusters =
    M.fold
      (fun _ operation clusters ->
        match operation with
        | A.RawSlotInit (A.Var parent, offset, value, _) -> (
            match (M.find_opt parent nodes, integer offset) with
            | Some node, Some offset -> (
                match nodeSlot node offset with
                | Some (Link _) -> (
                    match pointer source roots value with
                    | Some child -> union clusters parent child
                    | None -> clusters)
                | _ -> clusters)
            | _ -> clusters)
        | _ -> clusters)
      source clusters
  in
  let components =
    M.mapi
      (fun id _ ->
        let _, _, minimum = representative clusters id in
        minimum)
      nodes
  in
  { bindings = source; nodes; roots; components }

let root plan atom =
  Option.bind (pointer plan.bindings plan.roots atom) (fun pointer ->
      match
        (M.find_opt pointer plan.nodes, M.find_opt pointer plan.components)
      with
      | Some node, Some component -> Some (component, node.listType)
      | _ -> Crash.crash "Async list root lost its allocation component")

let slot plan pointer offset =
  match (pointer, integer offset) with
  | A.Var pointer, Some offset -> (
      match M.find_opt pointer plan.nodes with
      | None -> None
      | Some node ->
          Option.map
            (fun slot ->
              match M.find_opt pointer plan.components with
              | Some component -> (component, slot)
              | None ->
                  Crash.crash "Async list slot lost its allocation component")
            (nodeSlot node offset))
  | _ -> None
