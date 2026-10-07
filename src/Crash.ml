(*
   Crash.ml - Dependency-free crash helper
   Provides a single crash function for internal invariant violations.
   Crash to mark incomplete work that should never be hit in production.
   Use this when a developer or AI needs to flag missing logic.
*)
(* Crash.ml - Dependency-free failure boundary for impossible compiler states. *)
(*
   Crash the program with an error message.
   Used for internal invariant violations (unreachable code).
   When the compiler is migrated to Darklang (self-hosting), this will
   be replaced with Darklang's error handling (Result types or similar).
*)
let crash message = failwith message
let todo message = crash ("TODO: " ^ message)
