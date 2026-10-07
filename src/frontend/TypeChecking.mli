(* TypeChecking.mli - Orchestrate name resolution and checked-program construction. *)
val checkProgram : AST.program -> (AST.semanticType * CheckedAST.program, CheckingDiagnostics.typeError) result
val checkPublicProgram : AST.program -> (AST.semanticType * CheckedAST.program, CheckingDiagnostics.typeError) result
val checkProgramWithEnv : AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
val checkDeclarationProgramWithEnv : AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
val checkProgramWithBaseEnv : Types.typeCheckEnv -> AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
val checkDeclarationProgramWithBaseEnv : Types.typeCheckEnv -> AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
val checkProgramWithBaseEnvAndSettings : Types.typeCheckEnv -> bool -> AST.warningSettings -> AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
val checkProgramWithBaseEnvAndSettingsWithTrace : (string -> float -> unit) -> Types.typeCheckEnv -> bool -> AST.warningSettings -> AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
val checkDeclarationProgramWithBaseEnvAndSettings : Types.typeCheckEnv -> bool -> AST.warningSettings -> AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
val checkSyntheticPreambleWithBaseEnvAndSettings : Types.typeCheckEnv -> bool -> AST.warningSettings -> AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
val checkPublicProgramWithBaseEnvAndSettings : Types.typeCheckEnv -> bool -> AST.warningSettings -> AST.program -> (AST.semanticType * CheckedAST.program * Types.typeCheckEnv, CheckingDiagnostics.typeError) result
