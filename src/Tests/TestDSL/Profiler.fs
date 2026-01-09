// Profiler.fs - Lightweight test runner profiling utilities
//
// Records timestamped events with thread IDs for offline analysis.

module TestDSL.Profiler

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO

type ProfileEvent = {
    Name: string
    ThreadId: int
    StartMs: float
    EndMs: float
    Meta: (string * string) list
}

let private clock = Stopwatch.StartNew()
let private events = ConcurrentQueue<ProfileEvent>()

let private nowMs () : float =
    clock.Elapsed.TotalMilliseconds

let isEnabled () : bool =
    Environment.GetEnvironmentVariable("TEST_PROFILE") = "1"

let record (name: string) (startMs: float) (endMs: float) (meta: (string * string) list) : unit =
    if isEnabled() then
        events.Enqueue({
            Name = name
            ThreadId = Threading.Thread.CurrentThread.ManagedThreadId
            StartMs = startMs
            EndMs = endMs
            Meta = meta
        })

let time (name: string) (meta: (string * string) list) (f: unit -> 'a) : 'a =
    let startMs = nowMs()
    let result = f()
    let endMs = nowMs()
    record name startMs endMs meta
    result

let rec clear () : unit =
    match events.TryDequeue() with
    | true, _ -> clear ()
    | false, _ -> ()

let snapshot () : ProfileEvent list =
    events.ToArray() |> Array.toList

let writeReport (path: string) : unit =
    if isEnabled() then
        let dir = Path.GetDirectoryName(path)
        let finalPath =
            if String.IsNullOrEmpty(dir) then
                path
            elif File.Exists(dir) then
                "test-profile.csv"
            else
                Directory.CreateDirectory(dir) |> ignore
                path

        let escape (value: string) =
            "\"" + value.Replace("\"", "\"\"") + "\""

        let formatMeta (meta: (string * string) list) =
            meta
            |> List.map (fun (k, v) -> $"{k}={v}")
            |> String.concat ";"

        use writer = new StreamWriter(finalPath, false)
        writer.WriteLine("name,thread,start_ms,end_ms,duration_ms,meta")
        for ev in snapshot() do
            let durationMs = ev.EndMs - ev.StartMs
            let metaStr = formatMeta ev.Meta
            let line =
                String.concat ","
                    [ escape ev.Name
                      ev.ThreadId.ToString()
                      ev.StartMs.ToString("F3")
                      ev.EndMs.ToString("F3")
                      durationMs.ToString("F3")
                      escape metaStr ]
            writer.WriteLine(line)
