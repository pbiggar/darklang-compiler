// ProgressBar.fs - Progress bar utilities for the test runner
//
// Provides a thread-safe progress bar for long-running test suites.

module ProgressBar

open Output

let private barWidth = 20
let private lockObj = obj()
type State = {
    Total: int
    mutable Completed: int
    mutable Failed: int
    Label: string
}

let create label total = { Total = total; Completed = 0; Failed = 0; Label = label }

let update (state: State) =
    lock lockObj (fun () ->
        let rawPct =
            if state.Total = 0 then 0.0
            else float state.Completed / float state.Total
        let pct = max 0.0 (min 1.0 rawPct)
        let filled = int (pct * float barWidth)
        let bar = String.replicate filled "=" + String.replicate (barWidth - filled) " "
        let failStr = if state.Failed > 0 then $" ({Colors.red}{state.Failed} failed{Colors.reset})" else ""
        // Use \r to return to start of line, \x1b[K to clear to end of line
        eprint $"\r\x1b[K  {state.Label}: [{bar}] {state.Completed}/{state.Total}{failStr}"
        // Flush stderr so progress updates are visible in buffered environments.
        System.Console.Error.Flush()
    )

let increment (state: State) (success: bool) =
    lock lockObj (fun () ->
        state.Completed <- state.Completed + 1
        if not success then state.Failed <- state.Failed + 1
    )
    update state

let finish (state: State) =
    lock lockObj (fun () ->
        // Clear the progress line and print final summary
        eprint "\r\x1b[K"
        System.Console.Error.Flush()
    )
