// BenchmarkHost.fs - Optional benchmark orchestration through the native CLI.
module BenchmarkHost

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices

type CompileReport = { CompileTime: TimeSpan; Target: string }
type ExecutionOutput = {
    ExitCode: int; Stdout: string; Stderr: string; RuntimeTime: TimeSpan
}

let detectHostTarget () =
    match OperatingSystem.IsLinux(), OperatingSystem.IsMacOS(), RuntimeInformation.OSArchitecture with
    | true, _, Architecture.X64 -> Ok "LinuxX86_64"
    | true, _, Architecture.Arm64 -> Ok "ARM64Backend LinuxARM64"
    | _, true, Architecture.Arm64 -> Ok "ARM64Backend MacOSARM64"
    | _ -> Error "Unsupported benchmark host"

let private capture command arguments =
    let start = ProcessStartInfo(command, RedirectStandardInput = true,
                                RedirectStandardOutput = true,
                                RedirectStandardError = true, UseShellExecute = false)
    arguments |> List.iter start.ArgumentList.Add
    let timer = Stopwatch.StartNew()
    use child = Process.Start(start)
    child.StandardInput.Close()
    let stdout = child.StandardOutput.ReadToEndAsync()
    let stderr = child.StandardError.ReadToEndAsync()
    child.WaitForExit()
    timer.Stop()
    { ExitCode = child.ExitCode; Stdout = stdout.Result; Stderr = stderr.Result;
      RuntimeTime = timer.Elapsed }

let private withDirectory action =
    let path = Path.Combine(Path.GetTempPath(), "dark-benchmark-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(path) |> ignore
    try action path
    finally Directory.Delete(path, true)

let compile enableLeakCheck (name: string) (source: string) =
    detectHostTarget () |> Result.bind (fun target ->
        withDirectory (fun directory ->
            let input = Path.Combine(directory, name + ".dark")
            let output = Path.Combine(directory, "program")
            File.WriteAllText(input, source)
            let arguments = ["-q"; "--emit-result"; "--allow-internal"; input; "-o"; output]
            let arguments = if enableLeakCheck then "--leak-check" :: arguments else arguments
            let execution = capture "./ocaml/_build/default/bin/dark.exe" arguments
            if execution.ExitCode <> 0 then Error (execution.Stdout + execution.Stderr)
            else Ok ({ CompileTime = execution.RuntimeTime; Target = target }, File.ReadAllBytes(output))))

let executeCaptured (_target: string) (binary: byte array) =
    withDirectory (fun directory ->
        let output = Path.Combine(directory, "program")
        File.WriteAllBytes(output, binary)
        File.SetUnixFileMode(output, UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute)
        capture output [])
