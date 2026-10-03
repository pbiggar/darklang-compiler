val verifyJoin : Dark_compiler.ANF.aExpr -> (unit, string) result
val rejectsJoin : string -> Dark_compiler.ANF.aExpr -> unit -> (unit, string) result
val joinParameter : Dark_compiler.ANF.typedParam
val joinValue : Dark_compiler.ANF.atom
val testJoinCleanupPaths : unit -> (unit, string) result
