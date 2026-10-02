(* Crash.ml - Dependency-free failure boundary for impossible compiler states. *)
let crash message = failwith message
let todo message = crash ("TODO: " ^ message)
