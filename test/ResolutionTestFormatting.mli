(* Typed descriptions for original resolution and recursive-group assertion failures. *)
open Dark_compiler

val successfulResolution :
  NameResolution.successfulResolution -> StructuralValue.value

val resolutionError : NameResolution.resolutionError -> StructuralValue.value

val resolvedRecursiveMember :
  AST.resolvedRecursiveMember -> StructuralValue.value
