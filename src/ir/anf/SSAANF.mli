(* SSAANF.mli - Typed control-flow form for optimized ANF operations. *)
type label = Label of int

module LabelMap : Map.S with type key = label

type terminator =
  | Return of ANF.atom
  | Jump of label * ANF.atom list
  | Branch of ANF.atom * label * label

type block = {
  label : label;
  parameters : ANF.typedParam list;
  operations : (ANF.tempId * ANF.cExpr) list;
  terminator : terminator;
}

type functionDef = {
  id : AST.functionId;
  name : string;
  typedParams : ANF.typedParam list;
  returnType : AST.semanticType;
  returnOwnership : ANF.returnOwnership;
  entry : label;
  blocks : block LabelMap.t;
  freshValueTypes : AST.semanticType RcTypeFacts.TempMap.t;
}

val convertFunction :
  int -> ANF.typeMap -> ANF.functionDef -> (functionDef, string) result

val convertFunctionBeforeRC :
  int ->
  RcTypeFacts.typeContext ->
  ANF.functionDef ->
  (functionDef, string) result
