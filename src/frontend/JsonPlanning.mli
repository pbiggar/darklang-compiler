(* JsonPlanning.fs - monomorphic, type-directed JSON conversion plans. *)
class planningSession : object
  method tryFind : string -> CheckedAST.functionDef list option
  method store : string -> CheckedAST.functionDef list -> unit
  method count : int
  method hitCount : int
  method missCount : int
  method dispose : unit
end
val rewriteProgramWithSession : planningSession option -> Types.typeCheckEnv -> CheckedAST.program -> CheckedAST.program
val rewriteProgram : Types.typeCheckEnv -> CheckedAST.program -> CheckedAST.program
