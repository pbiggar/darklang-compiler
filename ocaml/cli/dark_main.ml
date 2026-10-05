(* dark_main.ml - Launch the complete compiler CLI without the reference runtime. *)
let () =
  let arguments = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  exit (Dark_compiler.Program.main arguments)
