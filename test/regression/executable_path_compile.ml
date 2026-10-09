(* executable_path_compile.ml - Compile executable identity checks for every native target. *)
open Dark_compiler
module X = CompilationContexts

let require = function Ok value -> value | Error message -> failwith message

let () =
  if Array.length Sys.argv <> 3 then
    failwith "Usage: executable_path_compile SOURCE OUTPUT_DIRECTORY";
  let source = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  List.iter
    (fun (name, target) ->
      let stdlib = require (StdlibCompilation.buildStdlib target) in
      let request : X.compileRequest =
        {
          X.context = X.StdlibOnly stdlib;
          X.mode = CompilerOptions.TestExpression;
          X.sources =
            NonEmptyList.singleton
              {
                X.name = Sys.argv.(1);
                X.purpose = NameSyntax.SourceUnitPurpose.Executable;
                X.source;
              };
          X.allowInternal = true;
          X.verbosity = 0;
          X.options =
            {
              CompilerOptions.defaultOptions with
              CompilerOptions.enableLeakCheck = true;
            };
          X.packageValues = X.emptyPackageValueCatalog;
          X.packageManager = None;
          X.passTimingRecorder = None;
          X.session = None;
        }
      in
      let output = Filename.concat Sys.argv.(2) name in
      let report = CompilerLibrary.compile request in
      let binary = require report.CompilerOptions.result in
      (* Signing is required for execution on macOS, but unavailable when
         cross-compiling its Mach-O image from a Linux test host. *)
      if
        target = Platform.ARM64Backend Platform.MacOSARM64
        && Platform.detectHostTarget () = Ok target
      then require (Binary_Generation_MachO.writeToFile output binary)
      else (
        Out_channel.with_open_bin output (fun channel ->
            Out_channel.output_bytes channel binary);
        Unix.chmod output 0o700))
    [
      ("linux-x86_64", Platform.LinuxX86_64);
      ("linux-arm64", Platform.ARM64Backend Platform.LinuxARM64);
      ("macos-arm64", Platform.ARM64Backend Platform.MacOSARM64);
    ]
