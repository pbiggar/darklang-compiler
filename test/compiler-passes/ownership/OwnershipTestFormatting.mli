(* Typed structural descriptions for the original ownership test diagnostics. *)
val value : Dark_compiler.HIR.value -> Dark_compiler.StructuralValue.value
val operand : Dark_compiler.HIR.operand -> Dark_compiler.StructuralValue.value
val signature : ('id -> Dark_compiler.StructuralValue.value) -> 'id Dark_compiler.OwnedIR.functionSignature -> Dark_compiler.StructuralValue.value
val block : ('leaf -> Dark_compiler.StructuralValue.value) -> ('id -> Dark_compiler.StructuralValue.value) -> ('leaf, 'id) Dark_compiler.OwnedIR.block -> Dark_compiler.StructuralValue.value
val steps : ('leaf -> Dark_compiler.StructuralValue.value) -> ('id -> Dark_compiler.StructuralValue.value) -> ('leaf, 'id) Dark_compiler.OwnedIR.step list -> Dark_compiler.StructuralValue.value
val functionDef : ('leaf -> Dark_compiler.StructuralValue.value) -> ('id -> Dark_compiler.StructuralValue.value) -> ('leaf, 'id) Dark_compiler.OwnedIR.functionDef -> Dark_compiler.StructuralValue.value
val option : ('value -> Dark_compiler.StructuralValue.value) -> 'value option -> Dark_compiler.StructuralValue.value
val callSignature : Dark_compiler.OwnedIR.callSignature -> Dark_compiler.StructuralValue.value
