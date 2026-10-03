(* LiteralPool.fs - Dense literal storage for late constant resolution. *)
module FloatBitsMap : Map.S with type key = int64
type stringPool = {strings : (string * int) array; stringToId : int StringOrder.Map.t}
type floatPool = {floats : float array; floatBitsToId : int FloatBitsMap.t}
val emptyStringPool : stringPool
val emptyFloatPool : floatPool
val createStringPool : string Seq.t -> stringPool
val createFloatPool : float Seq.t -> floatPool
