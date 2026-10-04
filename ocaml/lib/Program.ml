(*
   Program.fs - Compiler CLI Entry Point
   The main entry point for the Darklang compiler CLI.
   This module:
   - Parses command-line arguments using POSIX-style flags
   - Orchestrates the compilation pipeline through all passes
   - Handles errors and provides user feedback
   Compilation pipeline:
   1. Parser and checking: Source → parsed AST → CheckedAST
   2. AST_to_ANF: CheckedAST → ANF
   3. ANF_to_MIR: ANF → MIR
   4. MIR_to_LIR: MIR → LIR
   5. RegisterAllocation: LIR (virtual) → LIR (physical)
   6. CodeGen: LIR → target ISA instructions
   7. Encoding: target ISA instructions → machine code
   8. Binary generation: machine code → platform executable
*)
(* Program.fs - Command-line entry, batch compilation and native execution. *)
(*
   Output verbosity level
   0 = Quiet (no output)
   1 = Normal (standard output)
   2 = Verbose (show pass names)
   3 = VeryVerbose (show pass names + timing)
   4 = DumpIR (show all intermediate representations)
*)
type verbosityLevel=Quiet|Normal|Verbose|VeryVerbose|DumpIR
(*
   Select the compiler backend independently from the process architecture.
   Explicit targets are compile-only because the CLI does not emulate them.
*)
type targetSelection=HostTarget|ExplicitTarget of Platform.target
(*
   One independently linked executable in a batch compiler invocation.
*)
type batchCompileItem={kind:string;name:string;sourceFile:string;outputFile:string}
type batchInput=CommandLineItems of batchCompileItem * batchCompileItem list|ManifestFile of string
(*
   Batch mode shares immutable stdlib preparation while preserving a separate
   compilation request and executable for every source.
*)
type batchCliOptions={target:targetSelection;verbosity:verbosityLevel;leakCheck:bool;allowInternal:bool;packageServer:string option;input:batchInput;keepGoing:bool;reportPath:string option}
type batchManifestItem={kind:string;name:string;source:string;output:string}
type batchReportItem={kind:string;name:string;source:string;output:string;status:string;error:string;milliseconds:float}
(*
   Parsed CLI options
   True = run, False = compile (default)
   True = expression, False = file (default)
   Opt-in hosted package resolution. Ordinary compilation stays offline.
   Compiler-owned sources may use private runtime and HAMT helpers.
   Optimization flags
   IR dump flags
*)
type cliOptions={run:bool;isExpression:bool;outputFile:string option;verbosity:verbosityLevel;help:bool;version:bool;argument:string option;leakCheck:bool;target:targetSelection;emitResult:bool;packageServer:string option;allowInternal:bool;disableFreeList:bool;disableANFOpt:bool;disableANFConstFolding:bool;disableANFConstProp:bool;disableANFCopyProp:bool;disableANFDCE:bool;disableANFStrengthReduction:bool;disableInlining:bool;disableTCO:bool;disableMIROpt:bool;disableMIRSCCP:bool;disableMIRCSE:bool;disableMIRDCE:bool;disableMIRLICM:bool;disableLIROpt:bool;disableLIRPeephole:bool;disableFunctionTreeShaking:bool;dumpANF:bool;dumpMIR:bool;dumpLIR:bool;dumpFunction:string option;dumpIRSummary:bool;dumpIROutput:string option}
type cliCommand=SingleCommand of cliOptions|BatchCommand of batchCliOptions
module X=CompilationContexts
let (let*)=Result.bind
(*
   Convert VerbosityLevel to integer for library
   Library verbosity: 0=silent, 1=pass names, 2=pass names + timing, 3=dump all IRs
   No output
   CLI handles output, library silent
   Library shows pass names
   Library shows pass names + timing
   Library dumps all IRs
*)
let verbosityToInt=function Quiet|Normal->0|Verbose->1|VeryVerbose->2|DumpIR->3
(*
   Determine whether the CLI should emit normal output for a verbosity level
*)
let shouldShowNormal=function Quiet->false|Normal|Verbose|VeryVerbose|DumpIR->true
let increment=function Quiet->Normal|Normal->Verbose|Verbose->VeryVerbose|VeryVerbose|DumpIR->DumpIR
(*
   Default empty options
*)
let defaultOptions:cliOptions={run=false;isExpression=false;outputFile=None;verbosity=Normal;help=false;version=false;argument=None;leakCheck=false;target=HostTarget;emitResult=false;packageServer=None;allowInternal=false;disableFreeList=false;disableANFOpt=false;disableANFConstFolding=false;disableANFConstProp=false;disableANFCopyProp=false;disableANFDCE=false;disableANFStrengthReduction=false;disableInlining=false;disableTCO=false;disableMIROpt=false;disableMIRSCCP=false;disableMIRCSE=false;disableMIRDCE=false;disableMIRLICM=false;disableLIROpt=false;disableLIRPeephole=false;disableFunctionTreeShaking=false;dumpANF=false;dumpMIR=false;dumpLIR=false;dumpFunction=None;dumpIRSummary=false;dumpIROutput=None}
let parseTargetValue value=if HostText.lowerInvariant (HostText.trim value)="linux-x86_64" then Ok (ExplicitTarget Platform.LinuxX86_64) else Error ("Invalid target '"^value^"' (expected 'linux-x86_64')")
let parsePackageServerValue value=match HostUri.absoluteHttp value with Some server->Ok server|None->Error ("Invalid package server '"^value^"' (expected an absolute HTTP(S) URL)")
(*
   Build compiler options from CLI options
*)
let buildCompilerOptions (options:cliOptions):CompilerOptions.compilerOptions={CompilerOptions.disableFreeList=options.disableFreeList;disableANFOpt=options.disableANFOpt;disableANFConstFolding=options.disableANFConstFolding;disableANFConstProp=options.disableANFConstProp;disableANFCopyProp=options.disableANFCopyProp;disableANFDCE=options.disableANFDCE;disableANFStrengthReduction=options.disableANFStrengthReduction;disableInlining=options.disableInlining;disableTCO=options.disableTCO;disableMIROpt=options.disableMIROpt;disableMIRSCCP=options.disableMIRSCCP;disableMIRCSE=options.disableMIRCSE;disableMIRDCE=options.disableMIRDCE;disableMIRLICM=options.disableMIRLICM;disableLIROpt=options.disableLIROpt;disableLIRPeephole=options.disableLIRPeephole;disableFunctionTreeShaking=options.disableFunctionTreeShaking;enableCoverage=false;enableLeakCheck=options.leakCheck;nativeLayoutProbe=CompilerOptions.NoNativeLayoutProbe;warnings=CompilerOptions.defaultWarningSettings;dumpANF=options.dumpANF;dumpMIR=options.dumpMIR;dumpLIR=options.dumpLIR;dumpFunction=options.dumpFunction;dumpIRSummary=options.dumpIRSummary}
(*
   Parse command-line flags into options
   Apply last verbosity setting (last one wins)
   Handle -ofile format
   Handle --output=file format
   Stack -v flags: -v = Verbose, -vv = VeryVerbose, -vvv = DumpIR
   Deliberately omitted from user-facing help. This mode exists for
   compiler-owned stdlib, regression, and benchmark sources only.
   Special case: "-" means stdin
   Handle combined short flags like -qr, -re, etc.
   -ovalue format
   Invalid flag character
   Non-flag argument - this is the filename or expression
*)
let parseArgs argv=
 let rec flags args (options:cliOptions) verbosity=
  let resume rest options=flags rest options verbosity in
  let output value rest=if Option.is_some options.outputFile then Error "Output file specified multiple times" else resume rest {options with outputFile=Some value} in
  let target value rest=match options.target with ExplicitTarget _->Error "Target specified multiple times"|HostTarget->let* target=parseTargetValue value in resume rest {options with target} in
  let server value rest=if Option.is_some options.packageServer then Error "Package server specified multiple times" else let* server=parsePackageServerValue value in resume rest {options with packageServer=Some server} in
  let dump field value rest=
   let existing,duplicate,empty=match field with `Function->options.dumpFunction,"Dump function filter specified multiple times","--dump-function requires a non-empty value"|`Output->options.dumpIROutput,"IR dump output specified multiple times","--dump-ir-output requires a non-empty value" in
   if Option.is_some existing then Error duplicate else if HostText.trim value="" then Error empty else resume rest (match field with `Function->{options with dumpFunction=Some value}|`Output->{options with dumpIROutput=Some value}) in
  match args with
  |[]->Ok {options with verbosity}
  |("-r"|"--run")::rest->if options.run then Error "Run flag specified multiple times" else resume rest {options with run=true}
  |("-e"|"--expression")::rest->if options.isExpression then Error "Expression flag specified multiple times" else resume rest {options with isExpression=true}
  |"--target"::value::rest->target value rest
  |["--target"]->Error "Missing value for --target (expected 'linux-x86_64')"
  |"--package-server"::value::rest->server value rest
  |["--package-server"]->Error "Missing value for --package-server"
  |flag::rest when String.starts_with ~prefix:"--package-server=" flag->server (String.sub flag 17 (String.length flag-17)) rest
  |flag::rest when String.starts_with ~prefix:"--target=" flag->target (String.sub flag 9 (String.length flag-9)) rest
  |("-o"|"--output")::value::rest->output value rest
  |flag::rest when String.starts_with ~prefix:"-o" flag && String.length flag>2->output (String.sub flag 2 (String.length flag-2)) rest
  |flag::rest when String.starts_with ~prefix:"--output=" flag->output (String.sub flag 9 (String.length flag-9)) rest
  |("-q"|"--quiet")::rest->flags rest options Quiet
  |("-v"|"--verbose")::rest->flags rest options (increment verbosity)
  |"--dump-function"::value::rest->dump `Function value rest
  |["--dump-function"]->Error "Missing value for --dump-function"
  |flag::rest when String.starts_with ~prefix:"--dump-function=" flag->dump `Function (String.sub flag 16 (String.length flag-16)) rest
  |"--dump-ir-output"::value::rest->dump `Output value rest
  |["--dump-ir-output"]->Error "Missing value for --dump-ir-output"
  |flag::rest when String.starts_with ~prefix:"--dump-ir-output=" flag->dump `Output (String.sub flag 17 (String.length flag-17)) rest
  |("--dump-anf")::rest->resume rest {options with dumpANF=true}
  |("--dump-mir")::rest->resume rest {options with dumpMIR=true}
  |("--dump-lir")::rest->resume rest {options with dumpLIR=true}
  |("--dump-ir-summary")::rest->resume rest {options with dumpIRSummary=true}
  |("--leak-check")::rest->resume rest {options with leakCheck=true}
  |("--allow-internal")::rest->resume rest {options with allowInternal=true}
  |("--emit-result")::rest->resume rest {options with emitResult=true}
  |("-h"|"--help")::rest->resume rest {options with help=true}
  |("--version")::rest->resume rest {options with version=true}
  |("--no-free-list"|"--disable-opt-freelist")::rest->resume rest {options with disableFreeList=true}
  |("--disable-opt-anf")::rest->resume rest {options with disableANFOpt=true}
  |("--disable-opt-anf-const-folding")::rest->resume rest {options with disableANFConstFolding=true}
  |("--disable-opt-anf-const-prop")::rest->resume rest {options with disableANFConstProp=true}
  |("--disable-opt-anf-copy-prop")::rest->resume rest {options with disableANFCopyProp=true}
  |("--disable-opt-anf-dce")::rest->resume rest {options with disableANFDCE=true}
  |("--disable-opt-anf-strength-reduction")::rest->resume rest {options with disableANFStrengthReduction=true}
  |("--disable-opt-inline")::rest->resume rest {options with disableInlining=true}
  |("--disable-opt-tco")::rest->resume rest {options with disableTCO=true}
  |("--disable-opt-mir")::rest->resume rest {options with disableMIROpt=true}
  |("--disable-opt-mir-sccp")::rest->resume rest {options with disableMIRSCCP=true}
  |("--disable-opt-mir-cse")::rest->resume rest {options with disableMIRCSE=true}
  |("--disable-opt-mir-dce")::rest->resume rest {options with disableMIRDCE=true}
  |("--disable-opt-mir-licm")::rest->resume rest {options with disableMIRLICM=true}
  |("--disable-opt-lir")::rest->resume rest {options with disableLIROpt=true}
  |("--disable-opt-lir-peephole")::rest->resume rest {options with disableLIRPeephole=true}
  |("--disable-opt-function-tree-shaking")::rest->resume rest {options with disableFunctionTreeShaking=true}
  |("--disable-opt-dce")::rest->resume rest {options with disableFunctionTreeShaking=true}
  |"-"::rest->if Option.is_some options.argument then Error "Cannot specify multiple input sources" else resume rest {options with argument=Some "-"}
  |flag::rest when String.starts_with ~prefix:"-" flag && not (String.starts_with ~prefix:"--" flag) && String.length flag>2->
   let units=HostText.utf16Units (String.sub flag 1 (String.length flag-1)) |> Array.to_list in
   let rec expand units reversed=match units with
    |[]->List.rev reversed
    |(114|101|113|118|104 as code)::rest->expand rest (("-"^HostText.ofUtf16Units [|code|])::reversed)
    |111::(_::_ as value)->List.rev (("-o"^HostText.ofUtf16Units (Array.of_list value))::reversed)
    |code::_->List.rev (("-"^HostText.ofUtf16Units [|code|])::reversed) in
   flags (expand units []@rest) options verbosity
  |argument::rest when not (String.starts_with ~prefix:"-" argument)->if Option.is_some options.argument then Error ("Unexpected argument: "^argument) else resume rest {options with argument=Some argument}
  |flag::_->Error ("Unknown flag: "^flag)
 in flags (Array.to_list argv) defaultOptions Normal
(*
   Validate parsed options
   Help and version override everything else
   Check for required argument
   Check for conflicting options
*)
let validateOptions (options:cliOptions)=
 if options.help || options.version then Ok options else if options.argument=None then Error "Missing input (filename or expression with -e)"
 else if options.run && Option.is_some options.outputFile then Error "Cannot specify output file with run mode (-r)"
 else if options.run && options.target<>HostTarget then Error "Explicit compiler targets are compile-only; remove --run"
 else if (Option.is_some options.dumpFunction || options.dumpIRSummary || Option.is_some options.dumpIROutput) && not (options.dumpANF || options.dumpMIR || options.dumpLIR || options.verbosity=DumpIR) then Error "--dump-function, --dump-ir-summary, and --dump-ir-output require an IR dump selection"
 else Ok options
let parseBatchArgs argv=
 let rec extract seen reversed=function []->seen,List.rev reversed|"--"::_ as rest->seen,List.rev reversed@rest|"--leak-check"::rest->extract true reversed rest|arg::rest->extract seen (arg::reversed) rest in
 let leakCheck,args=extract false [] (Array.to_list argv) in
 let items args=
  let rec loop reversed=function
   |[]->(match List.rev reversed with []->Error "Batch compilation requires at least one SOURCE OUTPUT pair"|first::rest->Ok (first,rest))
   |[_]->Error "Batch compilation requires an output path after every source path"
   |source::output::rest->if HostText.trim source="" || HostText.trim output="" then Error "Batch source and output paths must be non-empty" else loop ({kind="source";name=source;sourceFile=source;outputFile=output}::reversed) rest in loop [] args in
 let rec flags target verbosity allowInternal packageServer manifestPath keepGoing reportPath args=
  let resume target packageServer manifestPath keepGoing reportPath rest=flags target verbosity allowInternal packageServer manifestPath keepGoing reportPath rest in
  let targetValue value rest=match target with ExplicitTarget _->Error "Target specified multiple times"|HostTarget->let* target=parseTargetValue value in resume target packageServer manifestPath keepGoing reportPath rest in
  let serverValue value rest=if Option.is_some packageServer then Error "Package server specified multiple times" else let* server=parsePackageServerValue value in resume target (Some server) manifestPath keepGoing reportPath rest in
  let pathValue manifest value rest=
   let previous,duplicate,empty=if manifest then manifestPath,"Batch manifest specified multiple times","--manifest requires a non-empty path" else reportPath,"Batch report specified multiple times","--report requires a non-empty path" in
   if Option.is_some previous then Error duplicate else if HostText.trim value="" then Error empty else resume target packageServer (if manifest then Some value else manifestPath) keepGoing (if manifest then reportPath else Some value) rest in
  let finish input=Ok {target;verbosity;leakCheck;allowInternal;packageServer;input;keepGoing;reportPath} in
  match args with
  |"--"::rest->if Option.is_some manifestPath then Error "Batch compilation cannot combine --manifest with SOURCE OUTPUT pairs" else let* first,rest=items rest in finish (CommandLineItems (first,rest))
  |("-q"|"--quiet")::rest->flags target Quiet allowInternal packageServer manifestPath keepGoing reportPath rest
  |"--allow-internal"::rest->flags target verbosity true packageServer manifestPath keepGoing reportPath rest
  |"--keep-going"::rest->if keepGoing then Error "Keep-going specified multiple times" else resume target packageServer manifestPath true reportPath rest
  |"--manifest"::value::rest->pathValue true value rest
  |["--manifest"]->Error "Missing value for --manifest"
  |flag::rest when String.starts_with ~prefix:"--manifest=" flag->pathValue true (String.sub flag 11 (String.length flag-11)) rest
  |"--report"::value::rest->pathValue false value rest
  |["--report"]->Error "Missing value for --report"
  |flag::rest when String.starts_with ~prefix:"--report=" flag->pathValue false (String.sub flag 9 (String.length flag-9)) rest
  |"--package-server"::value::rest->serverValue value rest
  |["--package-server"]->Error "Missing value for --package-server"
  |flag::rest when String.starts_with ~prefix:"--package-server=" flag->serverValue (String.sub flag 17 (String.length flag-17)) rest
  |"--target"::value::rest->targetValue value rest
  |["--target"]->Error "Missing value for --target (expected 'linux-x86_64')"
  |flag::rest when String.starts_with ~prefix:"--target=" flag->targetValue (String.sub flag 9 (String.length flag-9)) rest
  |[]->(match manifestPath with None->Error "Batch compilation requires --manifest or '--' before SOURCE OUTPUT pairs"|Some path->finish (ManifestFile path))
  |flag::_->Error ("Unknown batch flag: "^flag)
 in flags HostTarget Normal false None None false None args
let parseCommand argv=match Array.to_list argv with "--batch"::rest->Result.map (fun value->BatchCommand value) (parseBatchArgs (Array.of_list rest))|_->let* options=parseArgs argv in Result.map (fun value->SingleCommand value) (validateOptions options)
let sourceFileForDiagnostics (options:cliOptions)=if options.isExpression then "<expression>" else match options.argument with Some value->value|None->Crash.crash "sourceFileForDiagnostics: compile/run called without a validated input source"
let sourceDescription (options:cliOptions)=if options.isExpression then "<expression>" else match options.argument with Some value->value|None->Crash.crash "sourceDescription: compile/run called without a validated input source"
(*
   Redirect compiler diagnostics and IR dumps at the CLI boundary. Explicit
   dump flags do not otherwise enable pass chatter, so their files contain
   only the requested representations.
*)
let withIRDumpOutput (options:cliOptions) compile=
 match options.dumpIROutput with None->Ok (compile ())|Some path->
 let writer=try Ok (Unix.openfile path [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o666) with exn->Error ("Failed to open IR dump '"^path^"': "^HostFile.errorMessage path exn) in
 let* writer=writer in
 Fun.protect ~finally:(fun ()->Unix.close writer) (fun ()->
 flush stdout;let original=Unix.dup Unix.stdout in
 let result=Fun.protect ~finally:(fun ()->flush stdout;Unix.dup2 original Unix.stdout;Unix.close original) (fun ()->Unix.dup2 writer Unix.stdout;compile ()) in
 try flush stdout;Ok result with exn->Error ("Failed to write IR dump to '"^path^"': "^HostFile.errorMessage path exn))
let selectedTarget=function HostTarget->Platform.detectHostTarget ()|ExplicitTarget target->Ok target
let request stdlib source verbosity (options:cliOptions):X.compileRequest={X.context=X.StdlibOnly stdlib;mode=(if options.isExpression || options.emitResult then CompilerOptions.TestExpression else CompilerOptions.FullProgram);sources=NonEmptyList.singleton {X.name=sourceFileForDiagnostics options;purpose=NameSyntax.SourceUnitPurpose.Executable;source};allowInternal=options.allowInternal;verbosity=verbosityToInt verbosity;options=buildCompilerOptions options;packageValues=X.emptyPackageValueCatalog;packageManager=Option.map (fun server->{(PackageManager.defaultConfig ()) with PackageManager.server}) options.packageServer;passTimingRecorder=None;session=None}
let compileWithStdlib stdlib source outputPath verbosity (options:cliOptions)=
 if shouldShowNormal verbosity then print_endline ("Compiling: "^sourceDescription options);
 let* report=withIRDumpOutput options (fun ()->CompilerLibrary.compile (request stdlib source verbosity options)) in
 let* binary=Result.map_error (fun message->"Compilation failed: "^message) report.CompilerOptions.result in
 let output=match report.CompilerOptions.target with Platform.ARM64Backend Platform.MacOSARM64->Binary_Generation_MachO.writeToFile outputPath binary|Platform.ARM64Backend Platform.LinuxARM64|Platform.LinuxX86_64->Backend_Arm64_Binary_Generation_ELF.writeToFile outputPath binary in
 let* ()=Result.map_error (fun message->"Failed to write binary: "^message) output in
 if shouldShowNormal verbosity then Printf.printf "Successfully wrote %d bytes to %s\n" (Bytes.length binary) outputPath;Ok ()
(*
   Compile source expression to executable
*)
let compile source outputPath verbosity (options:cliOptions)=
 let result=let* target=Result.map_error (fun message->"Target detection failed: "^message) (selectedTarget options.target) in
 let* stdlib=Result.map_error (fun message->"Compilation failed: "^message) (StdlibCompilation.buildStdlib target) in compileWithStdlib stdlib source outputPath verbosity options in
 match result with Ok ()->0|Error message->prerr_endline message;1
let fileExists=HostFile.exists
let readText=HostFile.readText
let readSourceFile path=if not (fileExists path) then Error ("File not found: "^path) else try Ok (readText path) with exn->Error ("Failed to read file '"^path^"': "^HostFile.errorMessage path exn)
let readBatchManifest path=
 let* text=readSourceFile path in
 try match HostBatchJson.parse text with
 |None->Error ("Batch manifest '"^path^"' must contain a JSON array")
 |Some entries->
 let* items=ResultList.traverse (function None->Error ("Failed to parse batch manifest '"^path^"': Object reference not set to an instance of an object.")|Some (entry:HostBatchJson.item)->
 if List.exists (fun value->HostText.trim value="") [entry.HostBatchJson.kind;entry.HostBatchJson.name;entry.HostBatchJson.source;entry.HostBatchJson.output] then Error ("Batch manifest '"^path^"' contains an empty kind, name, source, or output") else Ok {kind=entry.HostBatchJson.kind;name=entry.HostBatchJson.name;sourceFile=entry.HostBatchJson.source;outputFile=entry.HostBatchJson.output}) entries in
 (match items with []->Error ("Batch manifest '"^path^"' must contain at least one item")|first::rest->Ok (first,rest))
 with exn->let message=match exn with Failure message|Invalid_argument message->message|_->Printexc.to_string exn in Error ("Failed to parse batch manifest '"^path^"': "^message)
let jsonString value=
 let buffer=Buffer.create (String.length value+2) in Buffer.add_char buffer '"';
 Array.iter (fun code->match code with
 |8->Buffer.add_string buffer "\\b"|9->Buffer.add_string buffer "\\t"|10->Buffer.add_string buffer "\\n"|12->Buffer.add_string buffer "\\f"|13->Buffer.add_string buffer "\\r"|92->Buffer.add_string buffer "\\\\"
 |code when code<32 || code>126 || List.mem code [34;38;39;43;60;62;96]->Buffer.add_string buffer (Printf.sprintf "\\u%04X" code)
 |code->Buffer.add_char buffer (Char.chr code)) (HostText.utf16Units value);
 Buffer.add_char buffer '"';Buffer.contents buffer
let roundMilliseconds value=
 let scaled=value*.1000. in let lower=Float.floor scaled in let fraction=scaled-.lower in
 (if fraction>0.5 || (fraction=0.5 && mod_float lower 2.<>0.) then lower+.1. else lower) /.1000.
let reportJson (value:batchReportItem)=
 let fields=["kind",jsonString value.kind;"name",jsonString value.name;"source",jsonString value.source;"output",jsonString value.output;"status",jsonString value.status;"error",jsonString value.error;"milliseconds",HostFloat.roundTrip value.milliseconds] in
 "{"^String.concat "," (List.map (fun (key,value)->jsonString key^":"^value) fields)^"}"
let rec createDirectory path=if path<>"" && path<>"." && not (Sys.file_exists path) then (createDirectory (Filename.dirname path);Unix.mkdir path 0o777)
let writeBatchReport path reports=try createDirectory (Filename.dirname path);Out_channel.with_open_bin path (fun channel->List.iter (fun value->output_string channel (reportJson value);output_char channel '\n') reports);Ok () with exn->Error ("Failed to write batch report '"^path^"': "^HostFile.errorMessage path exn)
let compileBatch (options:batchCliOptions)=
 let sources=let* first,rest=match options.input with CommandLineItems (first,rest)->Ok (first,rest)|ManifestFile path->readBatchManifest path in
 ResultList.traverse (fun (item:batchCompileItem)->Result.map (fun source->item,source) (readSourceFile item.sourceFile)) (first::rest) in
 match selectedTarget options.target,sources with
 |Error message,_->prerr_endline ("Target detection failed: "^message);1
 |_,Error message->prerr_endline message;1
 |Ok target,Ok sources->match StdlibCompilation.buildStdlib target with Error message->prerr_endline ("Compilation failed: "^message);1|Ok stdlib->
 let rec loop failed reports=function
 |[]->failed,List.rev reports
 |((item:batchCompileItem),source)::rest->
  let start=HostClock.milliseconds () in
  let cli={defaultOptions with argument=Some item.sourceFile;outputFile=Some item.outputFile;verbosity=options.verbosity;leakCheck=options.leakCheck;target=options.target;allowInternal=options.allowInternal;packageServer=options.packageServer} in
  let compiled=compileWithStdlib stdlib source item.outputFile options.verbosity cli in
  let milliseconds=roundMilliseconds (HostClock.milliseconds ()-.start) in
  let report status error={kind=item.kind;name=item.name;source=item.sourceFile;output=item.outputFile;status;error;milliseconds} in
  match compiled with Ok ()->loop failed (report "compiled" ""::reports) rest|Error error->
  prerr_endline ("Compilation failed for "^item.kind^" "^item.name^": "^error);
  let reports=report "failed" error::reports in if options.keepGoing then loop true reports rest else true,List.rev reports in
 let failed,reports=loop false [] sources in
 let written=match options.reportPath with None->Ok ()|Some path->writeBatchReport path reports in
 match written with Error message->prerr_endline message;1|Ok ()->if failed then 1 else 0
(*
   Run an expression (compile to temp and execute)
   Use library for compile and run
*)
let run source verbosity (options:cliOptions)=
 let show=shouldShowNormal verbosity in if show then (print_endline ("Compiling and running: "^sourceDescription options);print_endline "---");
 let result=let* target=Result.map_error (fun message->"Target detection failed: "^message) (Platform.detectHostTarget ()) in
 let* stdlib=StdlibCompilation.buildStdlib target in
 let* report=withIRDumpOutput options (fun ()->CompilerLibrary.compile (request stdlib source verbosity options)) in
 let* binary=report.CompilerOptions.result in Ok (CompilerExecution.executeAttached report.CompilerOptions.target (verbosityToInt verbosity) binary) in
 let output=match result with Ok output->output|Error stderr->{CompilerOptions.exitCode=1;stdout="";stderr;runtimeTime=0L} in
 if show then (print_endline "---";Printf.printf "Exit code: %d\n" output.CompilerOptions.exitCode);
 if output.CompilerOptions.stderr<>"" then prerr_endline output.CompilerOptions.stderr;output.CompilerOptions.exitCode
(*
   Print version information
*)
let versionLines ()=["Dark Compiler v0.1.0";"Darklang native compiler for macOS and Linux"]
let printVersion ()=List.iter print_endline (versionLines ())
(*
   Print usage information
*)
let printUsage ()=
 print_endline "Dark Compiler v0.1.0";
 print_endline "";
 print_endline "Usage:";
 print_endline "  dark <file> [-o <output>]           Compile file to executable (default)";
 print_endline "  dark -r <file>                      Compile and run file";
 print_endline "  dark -e <expression> [-o <output>]  Compile expression to executable";
 print_endline "  dark -r -e <expression>             Run expression";
 print_endline "  dark -r -e -                        Read expression from stdin and run";
 print_endline "  dark --batch [OPTIONS] -- SOURCE OUTPUT [SOURCE OUTPUT ...]";
 print_endline "  dark --batch [OPTIONS] --manifest FILE";
 print_endline "";
 print_endline "Flags:";
 print_endline "  -r, --run            Run instead of compile (shows exit code)";
 print_endline "  -e, --expression     Treat argument as expression (not filename)";
 print_endline "  --target TARGET      Compile for linux-x86_64 instead of the host";
 print_endline "  --manifest FILE      Read labeled batch inputs from a JSON manifest";
 print_endline "  --keep-going         Continue batch compilation after individual failures";
 print_endline "  --report FILE        Write one JSON object per batch result";
 print_endline "  --package-server URL Resolve referenced hosted packages from URL";
 print_endline "  --manifest FILE      Read labeled batch inputs from a JSON manifest";
 print_endline "  --keep-going         Continue batch compilation after individual failures";
 print_endline "  --report FILE        Write one JSON object per batch result";
 print_endline "  --emit-result        Print a file's final expression result when executed";
 print_endline "  -o, --output FILE    Output file (default: dark.out)";
 print_endline "  -q, --quiet          Suppress compilation output";
 print_endline "  -v, --verbose        Show compilation pass names";
 print_endline "  -vv                  Show pass names + timing details";
 print_endline "  -vvv                 Dump all intermediate representations";
 print_endline "  --dump-anf           Dump ANF (all ANF stages)";
 print_endline "  --dump-mir           Dump MIR (control-flow graph)";
 print_endline "  --dump-lir           Dump LIR (before and after register allocation)";
 print_endline "  --dump-function TEXT Restrict IR dumps to matching function names";
 print_endline "  --dump-ir-summary    Emit function/block/instruction counts instead of full IR";
 print_endline "  --dump-ir-output FILE  Write compiler IR output to FILE instead of stdout";
 print_endline "  --leak-check         Enable leak checking, including in batch mode (debug builds only)";
 print_endline "  -h, --help           Show this help message";
 print_endline "  --version            Show version information";
 print_endline "";
 print_endline "Optimization flags (for debugging):";
 print_endline "  --disable-opt-anf       Disable ANF-level optimizations";
 print_endline "  --disable-opt-inline    Disable function inlining";
 print_endline "  --disable-opt-tco       Disable tail call optimization";
 print_endline "  --disable-opt-mir       Disable MIR-level optimizations";
 print_endline "  --disable-opt-lir       Disable LIR-level optimizations";
 print_endline "  --disable-opt-function-tree-shaking  Disable function tree shaking";
 print_endline "  --disable-opt-dce       Alias for --disable-opt-function-tree-shaking";
 print_endline "  --disable-opt-freelist  Disable free list memory reuse";
 print_endline "";
 print_endline "Flags can appear in any order and can be combined (e.g., -qr, -re, -vvre)";
 print_endline "Verbosity levels: (none)=normal, -v=passes, -vv=passes+timing, -vvv=dump IRs";
 print_endline "";
 print_endline "Examples:";
 print_endline "  dark prog.dark                     Compile file to 'dark.out'";
 print_endline "  dark prog.dark -o output           Compile file to 'output'";
 print_endline "  dark -r prog.dark                  Compile and run file";
 print_string "  dark -e \"2 + 3\"                    Compile expression to 'dark.out'\n";
 print_string "  dark -e \"2 + 3\" -o output          Compile expression to 'output'\n";
 print_string "  dark -r -e \"2 + 3\"                 Run and show exit code (5)\n";
 print_string "  dark -qr -e \"6 * 7\"                Run quietly (exit code: 42)\n";
 print_endline "  dark --target=linux-x86_64 prog.dark -o prog-x86_64";
 print_endline "  dark -v prog.dark -o output        Compile with verbose output";
 print_endline "  dark --batch -q -- a.dark a.out b.dark b.out";
 print_endline "  dark -r -e - < input.txt           Run expression from stdin";
 print_endline "";
 print_endline "Note: Generated executables may require code signing to run on macOS";
 ()
(*
   Get source code (from stdin, file, or inline expression)
   Read from stdin
   Inline expression
   Read from file
   Run mode
   Compile mode (default)
*)
let main argv=
 try match parseCommand argv with
 |Error message->print_endline ("Error: "^message);print_endline "";printUsage ();1
 |Ok (SingleCommand options) when options.help->printUsage ();0
 |Ok (SingleCommand options) when options.version->printVersion ();0
 |Ok (BatchCommand options)->compileBatch options
 |Ok (SingleCommand options)->
  let source=match options.argument with
  |Some "-"->(try let source=HostEncoding.utf8 (In_channel.input_all stdin) in if HostText.trim source="" then Error "No input provided on stdin" else Ok source with exn->Error ("Failed to read from stdin: "^Printexc.to_string exn))
  |Some value when options.isExpression->Ok value
  |Some path->if not (fileExists path) then Error ("File not found: "^path) else (try Ok (readText path) with exn->Error ("Failed to read file: "^HostFile.errorMessage path exn))
  |None->Error "No source provided" in
  match source with Error message->print_endline ("Error: "^message);1|Ok source->if options.run then run source options.verbosity options else compile source (Option.value ~default:"dark.out" options.outputFile) options.verbosity options
 with exn->print_endline ("Error: "^Printexc.to_string exn);print_endline (Printexc.get_backtrace ());1
