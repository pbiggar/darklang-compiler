(* Run the native compiler command line. *)
let () =
  exit
    (Dark_compiler.Program.main
       (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)))
