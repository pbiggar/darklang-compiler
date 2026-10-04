type fixup = {patchOffset:int;nextInstrOffset:int;targetLabel:string}
type resolveResult = {machineCode:bytes;labelPositions:int StringOrder.Map.t;deferredFixups:fixup list}
val requireLabelPosition : string -> int StringOrder.Map.t -> (int,string) result
val patchRel32 : bytes -> int -> int -> unit
val collectStringPool : X86_64.instr list -> LiteralPool.stringPool
val dataLabelOffsets : int -> int -> LiteralPool.stringPool -> int StringOrder.Map.t
val resolveAndEncode : X86_64.instr list -> (resolveResult,string) result
val patchDataLabels : resolveResult -> int StringOrder.Map.t -> int -> (resolveResult,string) result
