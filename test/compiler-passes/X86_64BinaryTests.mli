val testGenerateElf : unit -> (unit, string) result
val testElfIdentHelper : unit -> (unit, string) result
val testCombinedInstructionEncodings : unit -> (unit, string) result
val runElfBinary : bytes -> (int, string) result
val testExecuteElf : unit -> (unit, string) result
val tests : (string * (unit -> (unit, string) result)) list
