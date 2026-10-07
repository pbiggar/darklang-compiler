(* main.ml - Audit original interpreter package syntax without a package server. *)
open Dark_compiler

let readSource path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let audit path =
  let result =
    try WrittenParsing.parse Validation.Package (readSource path)
    with Sys_error message -> Error message
  in
  let status, diagnostics =
    match result with
    | Ok _ -> ("passed", `Null)
    | Error message -> ("failed", `String message)
  in
  Yojson.Safe.to_channel stdout
    (`Assoc
       [
         ("source", `String path);
         ("phase", `String "parse");
         ("status", `String status);
         ("diagnostics", diagnostics);
       ]);
  output_char stdout '\n';
  flush stdout;
  status = "passed"

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: package-audit PACKAGE.dark [PACKAGE.dark ...]";
    exit 2);
  let passed = ref true in
  Array.iteri
    (fun index path -> if index > 0 && not (audit path) then passed := false)
    Sys.argv;
  exit (if !passed then 0 else 1)
