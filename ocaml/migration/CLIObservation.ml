(* Compare CLI parsing, native byte output, scoped dumps and host I/O. *)
module P=InstrumentedProgram
let tuple=SemanticJson.tuple
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error value->SemanticJson.union "FSharpResult" "Error" [str value]
let verbosity value=SemanticJson.union "VerbosityLevel" (match value with P.Quiet->"Quiet"|P.Normal->"Normal"|P.Verbose->"Verbose"|P.VeryVerbose->"VeryVerbose"|P.DumpIR->"DumpIR") []
let target=function P.HostTarget->SemanticJson.union "TargetSelection" "HostTarget" []|P.ExplicitTarget value->SemanticJson.union "TargetSelection" "ExplicitTarget" [CacheIdentityObservation.target value]
let options (value:P.cliOptions)=SemanticJson.record "CliOptions" [
 "Run",((fun value->`Bool value)) value.P.run;
 "IsExpression",((fun value->`Bool value)) value.P.isExpression;
 "OutputFile",(SemanticJson.option SemanticJson.string) value.P.outputFile;
 "Verbosity",(verbosity) value.P.verbosity;
 "Help",((fun value->`Bool value)) value.P.help;
 "Version",((fun value->`Bool value)) value.P.version;
 "Argument",(SemanticJson.option SemanticJson.string) value.P.argument;
 "LeakCheck",((fun value->`Bool value)) value.P.leakCheck;
 "Target",(target) value.P.target;
 "EmitResult",((fun value->`Bool value)) value.P.emitResult;
 "PackageServer",(SemanticJson.option SemanticJson.string) value.P.packageServer;
 "AllowInternal",((fun value->`Bool value)) value.P.allowInternal;
 "DisableFreeList",((fun value->`Bool value)) value.P.disableFreeList;
 "DisableANFOpt",((fun value->`Bool value)) value.P.disableANFOpt;
 "DisableANFConstFolding",((fun value->`Bool value)) value.P.disableANFConstFolding;
 "DisableANFConstProp",((fun value->`Bool value)) value.P.disableANFConstProp;
 "DisableANFCopyProp",((fun value->`Bool value)) value.P.disableANFCopyProp;
 "DisableANFDCE",((fun value->`Bool value)) value.P.disableANFDCE;
 "DisableANFStrengthReduction",((fun value->`Bool value)) value.P.disableANFStrengthReduction;
 "DisableInlining",((fun value->`Bool value)) value.P.disableInlining;
 "DisableTCO",((fun value->`Bool value)) value.P.disableTCO;
 "DisableMIROpt",((fun value->`Bool value)) value.P.disableMIROpt;
 "DisableMIRSCCP",((fun value->`Bool value)) value.P.disableMIRSCCP;
 "DisableMIRCSE",((fun value->`Bool value)) value.P.disableMIRCSE;
 "DisableMIRDCE",((fun value->`Bool value)) value.P.disableMIRDCE;
 "DisableMIRLICM",((fun value->`Bool value)) value.P.disableMIRLICM;
 "DisableLIROpt",((fun value->`Bool value)) value.P.disableLIROpt;
 "DisableLIRPeephole",((fun value->`Bool value)) value.P.disableLIRPeephole;
 "DisableFunctionTreeShaking",((fun value->`Bool value)) value.P.disableFunctionTreeShaking;
 "DumpANF",((fun value->`Bool value)) value.P.dumpANF;
 "DumpMIR",((fun value->`Bool value)) value.P.dumpMIR;
 "DumpLIR",((fun value->`Bool value)) value.P.dumpLIR;
 "DumpFunction",(SemanticJson.option SemanticJson.string) value.P.dumpFunction;
 "DumpIRSummary",((fun value->`Bool value)) value.P.dumpIRSummary;
 "DumpIROutput",(SemanticJson.option SemanticJson.string) value.P.dumpIROutput;
 ]
let item (value:P.batchCompileItem)=SemanticJson.record "BatchCompileItem" ["Kind",str value.P.kind;"Name",str value.P.name;"SourceFile",str value.P.sourceFile;"OutputFile",str value.P.outputFile]
let input=function P.ManifestFile path->SemanticJson.union "BatchInput" "ManifestFile" [str path]|P.CommandLineItems (first,rest)->SemanticJson.union "BatchInput" "CommandLineItems" [tuple [item first;list item rest]]
let batch (value:P.batchCliOptions)=SemanticJson.record "BatchCliOptions" ["Target",target value.P.target;"Verbosity",verbosity value.P.verbosity;"LeakCheck",`Bool value.P.leakCheck;"AllowInternal",`Bool value.P.allowInternal;"PackageServer",SemanticJson.option str value.P.packageServer;"Input",input value.P.input;"KeepGoing",`Bool value.P.keepGoing;"ReportPath",SemanticJson.option str value.P.reportPath]
let command=function P.SingleCommand value->SemanticJson.union "CliCommand" "SingleCommand" [options value]|P.BatchCommand value->SemanticJson.union "CliCommand" "BatchCommand" [batch value]
let capture action=
 let out=Filename.temp_file "cli-output" ".txt" in let err=Filename.temp_file "cli-error" ".txt" in
 let savedOut=Unix.dup ~cloexec:true Unix.stdout in let savedErr=Unix.dup ~cloexec:true Unix.stderr in
 Fun.protect ~finally:(fun ()->flush stdout;flush stderr;Unix.dup2 savedOut Unix.stdout;Unix.dup2 savedErr Unix.stderr;Unix.close savedOut;Unix.close savedErr;Sys.remove out;Sys.remove err) (fun ()->
 let attach path target=let fd=Unix.openfile path [Unix.O_WRONLY;Unix.O_CLOEXEC;Unix.O_TRUNC] 0 in Unix.dup2 fd target;Unix.close fd in
 flush stdout;flush stderr;attach out Unix.stdout;attach err Unix.stderr;
 let value=action () in flush stdout;flush stderr;
 value,In_channel.with_open_bin out In_channel.input_all,In_channel.with_open_bin err In_channel.input_all)
let normalize value=
 let timing=Str.regexp "^        [0-9]+\\(\\.[0-9]+\\)?ms$" in
 let complete=Str.regexp "^  ✓ Compilation complete ([0-9]+\\(\\.[0-9]+\\)?ms)$" in
 String.split_on_char '\n' value |> List.map (fun line->if Str.string_match timing line 0 then "        <duration>ms" else if Str.string_match complete line 0 then "  ✓ Compilation complete (<duration>ms)" else line) |> String.concat "\n"
let read path=if Sys.file_exists path && not (Sys.is_directory path) then Some (In_channel.with_open_bin path In_channel.input_all) else None
let observe input=
 let open Yojson.Basic.Util in let request=Yojson.Basic.from_string input in let bucket=request |> member "bucket" |> to_int in
 let selected f xs=List.mapi (fun index value->index,value) xs |> List.filter (fun (index,_)->index mod 16=bucket) |> list (fun (index,value)->tuple [SemanticJson.int32 index;f value]) in
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/cli_fixtures.json" in
 let root="TestResults/ocaml-migration/cli-observation" in
 if not (Sys.file_exists root) then Unix.mkdir root 0o755;
 let parsed=selected (fun args->let args=to_list args |> List.map to_string |> Array.of_list in
 let parsed=P.parseArgs args in tuple [result options parsed;result options (Result.bind parsed P.validateOptions);result batch (P.parseBatchArgs args);result command (P.parseCommand args);result DriverDiagnosticsObservation.options (Result.map P.buildCompilerOptions parsed)]) (fixtures |> member "arguments" |> to_list) in
 let manifests=selected (fun (index,text)->let path=Filename.concat root ("manifest-"^string_of_int index^".json") in Out_channel.with_open_bin path (fun channel->output_string channel text);
 result (fun (first,rest)->tuple [item first;list item rest]) (P.readBatchManifest path)) (fixtures |> member "manifests" |> to_list |> List.mapi (fun index value->index,to_string value)) in
 let fast=selected (fun args->let code,out,err=capture (fun ()->P.main (Array.of_list args)) in tuple [SemanticJson.int32 code;str out;str err]) [["--help"];["--version"];[];["--unknown"];["missing-file.dark"];["--dump-function=map";"file"]] in
 let stdlib,_=Lazy.force StdlibCompilationObservation.base in
 let texts=["()";"42L";"true";"let id (x: 'a) : 'a = x\nid 3L";"Darklang.Stdlib.List.length [1L,2L]";"\"12.3ms é😀\"";"unknown 1L";"let broken ="] in
 let cases=List.concat_map (fun source->List.concat_map (fun expression->List.map (fun profile->source,expression,profile) [0;1;2;3]) [false;true]) texts in
 let compiled=result (fun stdlib->selected (fun (source,expression,profile)->
 let privateRoot=Filename.concat root "native" in if not (Sys.file_exists privateRoot) then Unix.mkdir privateRoot 0o755;
 let output=Filename.concat privateRoot ("output-"^string_of_int bucket^".out") in let dump=Filename.concat privateRoot ("dump-"^string_of_int bucket^".ir") in
 List.iter (fun path->if Sys.file_exists path then Sys.remove path) [output;dump];
 let verbosity=if profile=0 then P.Normal else if profile=1 then P.VeryVerbose else P.Quiet in
 let cli={P.defaultOptions with P.argument=Some "fixture.dark";isExpression=expression;verbosity;outputFile=Some output;dumpANF=profile=1;dumpMIR=profile=2;dumpLIR=profile=3;dumpIRSummary=profile<>0;dumpIROutput=(if profile>=2 then Some dump else None)} in
 let report,out,err=capture (fun ()->P.compileWithStdlib stdlib source output verbosity cli) in
 let normalized value=normalize value |> fun value->Str.global_replace (Str.regexp_string privateRoot) "<output>" value in
 tuple [result (fun ()->`Null) report;str (normalized out);str (normalized err);SemanticJson.option X64EncodingObservation.bytes (Option.map Bytes.of_string (read output));SemanticJson.option str (Option.map normalized (read dump))]) cases) stdlib in
 let reports=selected (fun (index,milliseconds)->let report:P.batchReportItem={P.kind="source";name="é😀 <&>\"+";source="a.dark";output="a.out";status="failed";error="message\nline";milliseconds} in
 let path=Filename.concat root ("report-native-"^string_of_int index^".jsonl") in
 let json=P.reportJson report in let written=P.writeBatchReport path [report] in let contents=read path in
 tuple [str json;result (fun ()->`Null) written;SemanticJson.option str contents]) (List.mapi (fun index value->index,value) [0.;-0.;0.001;33.125;15000.]) in
 tuple [parsed;manifests;fast;compiled;reports]
