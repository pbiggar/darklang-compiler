(* Oracle.ml - The interpreter defines semantics; the compiler supplies native code. *)
open Dark_compiler

type outcome = Passed | Unsupported of string | OracleFailed of string
  | CompilerRejected of string * string | CompilerCrashed of string * string
  | NativeFailed of string * int * string * string
  | ResultMismatch of string * string

let describe = function
  | Passed -> "interpreter and compiler agree"
  | Unsupported message -> "interpreter rejected source: " ^ message
  | OracleFailed message -> "interpreter execution failed: " ^ message
  | CompilerRejected (_, message) -> "compiler rejected accepted source: " ^ message
  | CompilerCrashed (_, message) -> "compiler crashed on accepted source: " ^ message
  | NativeFailed (_, code, stdout, stderr) ->
      Printf.sprintf "native execution failed (exit %d): %s%s" code stdout stderr
  | ResultMismatch (expected, actual) ->
      Printf.sprintf "result mismatch: expected %S, got %S" expected actual

let observation output =
  let output = String.trim output in
  if output = "true" || output = "false" then Some `Bool
  else match Int64.of_string_opt output with Some _ -> Some `Int64 | None -> None

let check interpreter timeout stdlib source =
  match ProcessCapture.capture interpreter ["eval"; source] timeout with
  | Error message -> OracleFailed message
  | Ok (code, _, stderr) when code <> 0 -> Unsupported stderr
  | Ok (_, expected, _) ->
      if observation expected = None then Unsupported "top-level result is outside Int64/Bool observations"
      else
        let request = CompilationContexts.{
          context=CompilationContexts.StdlibOnly stdlib; mode=CompilerOptions.TestExpression;
          sources=NonEmptyList.singleton {CompilationContexts.name="fuzz.dark";
            purpose=NameSyntax.SourceUnitPurpose.Executable; source};
          allowInternal=false; verbosity=0;
          options=CompilerOptions.{defaultOptions with enableLeakCheck=true};
          packageValues=CompilationContexts.emptyPackageValueCatalog; packageManager=None;
          passTimingRecorder=None; session=None} in
        let compiled = try Ok (CompilerLibrary.compile request) with exception_ -> Error (Printexc.to_string exception_) in
        match compiled with
        | Error message -> CompilerCrashed (expected, message)
        | Ok report -> match report.CompilerOptions.result with
        | Error message -> CompilerRejected (expected, message)
        | Ok binary ->
            let path = Filename.temp_file "dark-fuzz-" "" in
            Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
              Out_channel.with_open_bin path (fun channel -> Out_channel.output_bytes channel binary);
              Unix.chmod path 0o700;
              let signing = if Platform.requiresCodeSigning (Platform.osFor report.CompilerOptions.target)
                then ProcessCapture.capture "codesign" ["-s"; "-"; path] timeout else Ok (0, "", "") in
              match signing with
              | Error message -> NativeFailed (expected, -1, "", message)
              | Ok (code, stdout, stderr) when code <> 0 -> NativeFailed (expected, code, stdout, stderr)
              | Ok _ ->
                  match ProcessCapture.capture path [] timeout with
                  | Error message -> NativeFailed (expected, -1, "", message)
                  | Ok (code, stdout, stderr) when code <> 0 || String.trim stderr <> "" ->
                      NativeFailed (expected, code, stdout, stderr)
                  | Ok (_, actual, _) ->
                      let expected = String.trim expected and actual = String.trim actual in
                      if expected = actual then Passed else ResultMismatch (expected, actual))

let sameFailure original candidate = match original, candidate with
  | CompilerRejected (expected, _), CompilerRejected (other, _) -> observation expected = observation other
  | CompilerCrashed (expected, message), CompilerCrashed (other, candidate) ->
      observation expected = observation other && message = candidate
  | NativeFailed (expected, code, _, _), NativeFailed (otherExpected, other, _, _) ->
      code = other && observation expected = observation otherExpected
  | ResultMismatch (expected, _), ResultMismatch (other, _) -> observation expected = observation other
  | _ -> false
