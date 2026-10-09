(* ProgressBar.ml - Progress bar utilities for the test runner
   Provides a thread-safe progress bar for long-running test suites. *)
open Dark_compiler
module Colors = TestRunnerColors

type state = {
  total : int;
  mutable completed : int;
  mutable failed : int;
  label : string;
}

let barWidth = 20
let lockObj = Mutex.create ()

let locked action =
  Mutex.lock lockObj;
  Fun.protect ~finally:(fun () -> Mutex.unlock lockObj) action

let create label total = { total; completed = 0; failed = 0; label }

let update (state : state) =
  locked (fun () ->
      let displayCompleted =
        if state.total <= 0 then 0 else max 0 (min state.completed state.total)
      in
      let rawPct =
        if state.total = 0 then 0.
        else float displayCompleted /. float state.total
      in
      let pct = max 0. (min 1. rawPct) in
      let filled = int_of_float (pct *. float barWidth) in
      let bar = String.make filled '=' ^ String.make (barWidth - filled) ' ' in
      let failStr =
        if state.failed > 0 then
          Printf.sprintf " (%s%d failed%s)" Colors.red state.failed Colors.reset
        else ""
      in
      (* Use \r to return to start of line, \x1b[K to clear to end of line *)
      Output.eprint
        (Printf.sprintf "\r\027[K  %s: [%s] %d/%d%s" state.label bar
           displayCompleted state.total failStr);
      (* Flush stderr so progress updates are visible in buffered environments. *)
      flush stderr)

let increment state success =
  locked (fun () ->
      state.completed <- state.completed + 1;
      if not success then state.failed <- state.failed + 1);
  update state

let finish (_state : state) =
  locked (fun () ->
      (* Clear the progress line and print final summary *)
      Output.eprint "\r\027[K";
      flush stderr)
