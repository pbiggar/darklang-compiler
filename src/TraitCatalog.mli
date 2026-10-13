(* TraitCatalog.mli - Canonical, immutable trait metadata for static specialization. *)
type obligation = string * AST.semanticType list
type methodTarget = { name : string; typeParams : string list }
type dispatch = { traitName : string; methodName : string; arity : int }

type implementation = {
  identity : string;
  traitName : string;
  typeParams : string list;
  typeArgs : AST.semanticType list;
  bounds : obligation list;
  methods : methodTarget StringOrder.Map.t;
}

type t = {
  dispatches : dispatch StringOrder.Map.t;
  implementations : implementation StringOrder.Map.t;
  functionBounds : (string list * obligation list) StringOrder.Map.t;
  typeBounds : (string list * obligation list) StringOrder.Map.t;
}

val empty : t
val merge : t -> t -> t
