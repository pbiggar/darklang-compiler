(* Protect process capture, UTF-8 output and descendant timeout behavior. *)
let tests = [
  "Runner reports the OS signal exit code", (fun () ->
    match ProcessCapture.capture "/bin/sh" ["-c"; "kill -TERM $$"] 10000 with
    | Ok (143,"","") -> Ok ()
    | Ok _ -> Error "Expected SIGTERM exit code 143"
    | Error error -> Error error);
  "Runner capture drains both large streams", (fun () ->
    match ProcessCapture.capture "/bin/sh"
      ["-c"; "printf '%100000s' a; printf '%100000s' b >&2; exit 17"] 10000 with
    | Ok (17, stdout, stderr)
      when stdout = String.make 99999 ' ' ^ "a" &&
           stderr = String.make 99999 ' ' ^ "b" -> Ok ()
    | Ok _ -> Error "Captured process output or exit code differs"
    | Error error -> Error error);
  "Runner capture preserves UTF-8 and CRLF", (fun () ->
    match ProcessCapture.capture "/bin/sh"
      ["-c"; "printf 'é😀\\r\\n'; printf 'err\\r\\n' >&2"] 10000 with
    | Ok (0, "é😀\r\n", "err\r\n") -> Ok ()
    | Ok _ -> Error "Captured text differs"
    | Error error -> Error error);
  "Runner capture times out descendants", (fun () ->
    match ProcessCapture.capture "/bin/sh" ["-c"; "sleep 30 & wait"] 100 with
    | Error "Execution timed out after 100ms" -> Ok ()
    | Error error -> Error error
    | Ok _ -> Error "Expected a process timeout")
]
