(* ScriptHelperTests.fs - Repository policy tests for compiler and shell tooling
   Enforces selected source and script invariants that are cheap to check structurally. *)
open Dark_compiler
module R=RepositoryTestFiles
type testResult=(unit,string) result
let (let*)=Result.bind
let compilerSourceFiles ()=R.filesUnder "src/DarkCompiler" ".fs"
let testToolingSourceFiles ()=R.filesUnder "src/Tests/test-suite-tooling" ".fs"
let findTextUsesIn needle sourceFiles=
 let* reversed=Array.fold_left (fun result path->let* accumulated=result in let* text=R.readFile path in Ok (if HostText.contains text needle then path::accumulated else accumulated)) (Ok []) (sourceFiles ()) in Ok (List.rev reversed)
let testCompilerAvoidsFailwith ()=
 let* paths=findTextUsesIn "failwith" compilerSourceFiles in
 let offenders=List.filter (fun path->Filename.basename path<>"Crash.fs") paths |> List.map R.relativePath in if offenders=[] then Ok () else Error ("Unexpected failwith usage in compiler: "^String.concat ", " offenders)
let testTestToolingAvoidsFailwith ()=
 let* paths=findTextUsesIn "failwith" testToolingSourceFiles in let offenders=List.map R.relativePath paths in if offenders=[] then Ok () else Error ("Unexpected failwith usage in test tooling: "^String.concat ", " offenders)
let testCompilerAvoidsOptionGet ()=
 let* paths=findTextUsesIn "Option.get" compilerSourceFiles in let offenders=List.map R.relativePath paths in if offenders=[] then Ok () else Error ("Unexpected Option.get usage in compiler: "^String.concat ", " offenders)
let testInstallerFormatsAssetListWithStableDelimiter ()=
 let* text=R.readFile (R.scriptPath "scripts/install-darklang-interpreter.sh") in
 if HostText.contains text "paste -sd ', ' -" then Error "install-darklang-interpreter.sh uses paste with multiple delimiters, which alternates comma and space instead of joining every asset with ', '" else Ok ()
let testShellcheckScansAllTrackedBashScripts ()=
 let* text=R.readFile (R.scriptPath "scripts/check-shell.sh") in
 if HostText.contains text "git ls-files -z -- run-tests scripts" then Error "check-shell.sh only scans run-tests and scripts/, omitting other tracked bash scripts"
 else if not (HostText.contains text "git ls-files -z)") then Error "check-shell.sh does not enumerate all tracked files before filtering bash scripts"
 else if not (HostText.contains text "shellcheck --severity=error") then Error "check-shell.sh should scan all tracked bash scripts for shellcheck errors without requiring existing warnings to be fixed in the same pass" else Ok ()
let testDumpLirFuncDoesNotSuppressCompilerFailures ()=
 let* text=R.readFile (R.scriptPath "scripts/dump-lir-func.sh") in
 if HostText.contains text "|| true" then Error "dump-lir-func.sh suppresses ./dark --dump-lir failures with `|| true`, so failed dumps can look successful" else Ok ()
let tests=["compiler avoids failwith",testCompilerAvoidsFailwith;"test tooling avoids failwith",testTestToolingAvoidsFailwith;"compiler avoids Option.get",testCompilerAvoidsOptionGet;"installer formats asset list with stable delimiter",testInstallerFormatsAssetListWithStableDelimiter;"shellcheck scans all tracked bash scripts",testShellcheckScansAllTrackedBashScripts;"dump-lir-func does not suppress compiler failures",testDumpLirFuncDoesNotSuppressCompilerFailures]
