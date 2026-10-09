(* executable_path_compile.ml - Compile executable identity checks for every native target. *)
open Dark_compiler

let () =
  if Array.length Sys.argv <> 3 then
    failwith "Usage: executable_path_compile SOURCE OUTPUT_DIRECTORY";
  List.iter
    (fun (name, target) ->
      let options =
        {
          Program.defaultOptions with
          target = Program.ExplicitTarget target;
          emitResult = true;
          leakCheck = true;
          allowInternal = true;
        }
      in
      let output = Filename.concat Sys.argv.(2) name in
      if Program.compile Sys.argv.(1) output Program.Quiet options <> 0 then
        failwith ("Executable path compilation failed for " ^ name))
    [
      ("linux-x86_64", Platform.LinuxX86_64);
      ("linux-arm64", Platform.ARM64Backend Platform.LinuxARM64);
      ("macos-arm64", Platform.ARM64Backend Platform.MacOSARM64);
    ]
