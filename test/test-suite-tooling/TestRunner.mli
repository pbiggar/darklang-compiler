(* Complete test entrypoint and original timing/profile JSON schemas. *)
[@@@warning "-30"]

type timingJsonSummary = {
  passed : int;
  failed : int;
  total : int;
  total_ms : float;
  unaccounted_ms : float;
  runtime_unaccounted_ms : float;
  overhead_unaccounted_ms : float;
  e2e_batch_size : int;
  e2e_logical_tests : int;
  e2e_batch_eligible_tests : int;
  e2e_physical_executions : int;
  e2e_batch_executions : int;
  e2e_batched_logical_tests : int;
  e2e_largest_batch : int;
}

type timingJsonTest = {
  name : string;
  total_ms : float;
  compile_ms : float option;
  runtime_ms : float option;
}

type timingJsonPass = { name : string; elapsed_ms : float; invocations : int }

type timingJsonPayload = {
  summary : timingJsonSummary;
  tests : timingJsonTest array;
  passes : timingJsonPass array;
}

type codegenProfileFunction = {
  name : string;
  category : string;
  generations : int;
  elapsed_ms : float;
  lir_instructions : int;
  symbolic_instructions : int;
}

type codegenProfileCategory = {
  name : string;
  elapsed_ms : float;
  percentage_of_codegen : float;
  functions : int;
  generations : int;
}

type codegenProfilePhase = {
  name : string;
  elapsed_ms : float;
  percentage_of_codegen : float;
}

type codegenProfileLirOp = {
  name : string;
  occurrences : int;
  elapsed_ms : float;
  symbolic_instructions_before_peephole : int;
  average_symbolic_instructions_before_peephole : float;
}

type codegenProfileLirOpFunction = {
  function_name : string;
  category : string;
  opcode : string;
  detail : string;
  occurrences : int;
  elapsed_ms : float;
  symbolic_instructions_before_peephole : int;
}

type codegenProfileSummary = {
  codegen_ms : float;
  attributed_function_ms : float;
  program_overhead_ms : float;
  cache_hits : int;
  cache_misses : int;
  release_plan_summary_cache_hits : int;
  release_plan_summary_cache_misses : int;
  json_plan_cache_hits : int;
  json_plan_cache_misses : int;
  anf_dependency_cache_hits : int;
  anf_dependency_cache_misses : int;
  compiled_dependency_cache_hits : int;
  compiled_dependency_cache_misses : int;
  mir_optimization_cache_hits : int;
  mir_optimization_cache_misses : int;
  allocated_lir_function_cache_hits : int;
  allocated_lir_function_cache_misses : int;
  stdlib_reachability_cache_hits : int;
  stdlib_reachability_cache_misses : int;
  metadata_group_cache_hits : int;
  metadata_group_cache_misses : int;
  helper_cache_hits : int;
  helper_cache_misses : int;
  start_codegen_cache_hits : int;
}

type codegenProfilePayload = {
  schema_version : int;
  summary : codegenProfileSummary;
  phases : codegenProfilePhase array;
  categories : codegenProfileCategory array;
  functions : codegenProfileFunction array;
  lir_ops : codegenProfileLirOp array;
  lir_op_functions : codegenProfileLirOpFunction array;
}

val printHelp : unit -> unit
val main : string array -> int
