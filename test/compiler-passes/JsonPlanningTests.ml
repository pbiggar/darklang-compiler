[@@@warning "-4-42"]

(* JsonPlanningTests.ml - Check that JSON planning preserves unrelated programs. *)
open Dark_compiler

type testResult = (unit, string) result

let ( let* ) = Result.bind

let testNonJsonProgramIsUnchanged (stdlib : CompilationContexts.stdlibResult) ()
    =
  let* program = WrittenParsing.parse Validation.Script "1L + 2L" in
  let* _, typedProgram, _ =
    WrittenChecking.checkSourceUnitsWithBase
      stdlib.CompilationContexts.context.CompilationContexts.writtenEnvironment
      false true [ program ]
  in
  let env =
    Types.mergeTypeCheckEnv
      stdlib.CompilationContexts.context.CompilationContexts.typeCheckEnv
      (WrittenChecking.typeCheckEnvironment typedProgram)
  in
  let planned = JsonPlanning.rewriteProgram env typedProgram in
  if planned = typedProgram then Ok ()
  else
    Error
      "Expected JSON planning to leave a program without JSON intrinsics \
       unchanged"

let tests stdlib =
  [
    ( "JSON planning leaves non-JSON programs unchanged",
      testNonJsonProgramIsUnchanged stdlib );
  ]
