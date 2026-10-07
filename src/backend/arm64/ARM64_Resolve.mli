val collectPoolsFromLabelRefs :
  Symbolic.labelRef Seq.t -> LiteralPool.stringPool * LiteralPool.floatPool

val collectPools :
  Symbolic.instr list -> LiteralPool.stringPool * LiteralPool.floatPool
