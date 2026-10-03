(* DeadCode.fs - Eliminate MIR definitions unreachable from observable roots. *)
val eliminateDeadCodeWithTickTrace : (string -> int64 -> unit) option -> MIR.cfg -> MIR.cfg * bool
val eliminateDeadCode : MIR.cfg -> MIR.cfg * bool
