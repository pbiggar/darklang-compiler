(* Run the translated suite independently of the packaged executable name. *)
let () =
  exit (TestRunner.main (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)))
