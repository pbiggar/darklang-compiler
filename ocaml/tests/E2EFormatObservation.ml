(* Complete unchanged E2E files, private grammar boundaries and encoded records. *)
[@@@warning "-42"]
open Dark_compiler
module J=Semantic_observation.SemanticJson
module P=ObservedE2EFormat
let list f xs=`List (List.map f xs)
let option=J.option
let boolean v=`Bool v
let result f=function Ok v->J.union "FSharpResult" "Ok" [f v]|Error e->J.union "FSharpResult" "Error" [J.string e]
let guarded f=try f () with Invalid_argument _ | Failure _ -> `Assoc ["internalException",`Bool true]
let test (v:E2EFormat.e2eTest)=J.record "E2ETest" [
  "Name",J.string v.E2EFormat.name;
  "SourceLine",J.int32 v.E2EFormat.sourceLine;
  "Source",J.string v.E2EFormat.source;
  "ExpectedValueExpr",option J.string v.E2EFormat.expectedValueExpr;
  "Preamble",J.string v.E2EFormat.preamble;
  "ExpectedStdout",option J.string v.E2EFormat.expectedStdout;
  "ExpectedStderr",option J.string v.E2EFormat.expectedStderr;
  "Arguments",list J.string v.E2EFormat.arguments;
  "Environment",list (fun (k,v)->J.tuple [J.string k;J.string v]) v.E2EFormat.environment;
  "Stdin",(match v.E2EFormat.stdin with E2EFormat.Closed->J.union "TestStdin" "Closed" []|E2EFormat.Bytes s->J.union "TestStdin" "Bytes" [J.string s]);
  "OutputMatch",(match v.E2EFormat.outputMatch with E2EFormat.NormalizedText->J.union "OutputMatch" "NormalizedText" []|E2EFormat.ExactBytes->J.union "OutputMatch" "ExactBytes" []);
  "Isolated",boolean v.E2EFormat.isolated;
  "ExpectedExitCode",J.int32 v.E2EFormat.expectedExitCode;
  "ErrorExpectation",option (fun k->J.union "ErrorExpectation" (match k with E2EFormat.AnyError->"AnyError"|E2EFormat.CompileError->"CompileError") []) v.E2EFormat.errorExpectation;
  "ExpectedErrorMessage",option J.string v.E2EFormat.expectedErrorMessage;
  "SkipReason",option J.string v.E2EFormat.skipReason;
  "DisableFreeList",boolean v.E2EFormat.disableFreeList;
  "DisableANFOpt",boolean v.E2EFormat.disableANFOpt;
  "DisableANFConstFolding",boolean v.E2EFormat.disableANFConstFolding;
  "DisableANFConstProp",boolean v.E2EFormat.disableANFConstProp;
  "DisableANFCopyProp",boolean v.E2EFormat.disableANFCopyProp;
  "DisableANFDCE",boolean v.E2EFormat.disableANFDCE;
  "DisableANFStrengthReduction",boolean v.E2EFormat.disableANFStrengthReduction;
  "DisableInlining",boolean v.E2EFormat.disableInlining;
  "DisableTCO",boolean v.E2EFormat.disableTCO;
  "DisableMIROpt",boolean v.E2EFormat.disableMIROpt;
  "DisableMIRSCCP",boolean v.E2EFormat.disableMIRSCCP;
  "DisableMIRCSE",boolean v.E2EFormat.disableMIRCSE;
  "DisableMIRDCE",boolean v.E2EFormat.disableMIRDCE;
  "DisableMIRLICM",boolean v.E2EFormat.disableMIRLICM;
  "DisableLIROpt",boolean v.E2EFormat.disableLIROpt;
  "DisableLIRPeephole",boolean v.E2EFormat.disableLIRPeephole;
  "DisableFunctionTreeShaking",boolean v.E2EFormat.disableFunctionTreeShaking;
  "DisableLeakCheck",boolean v.E2EFormat.disableLeakCheck;
  "SourceFile",J.string v.E2EFormat.sourceFile;
  "FunctionLineMap",(`Assoc ["map",list (fun (k,v)->J.tuple [J.string k;J.int32 v]) (E2EFormat.StringMap.bindings v.E2EFormat.functionLineMap)]);
]
let privateTest (v:P.e2eTest)=J.record "E2ETest" [
  "Name",J.string v.P.name;
  "SourceLine",J.int32 v.P.sourceLine;
  "Source",J.string v.P.source;
  "ExpectedValueExpr",option J.string v.P.expectedValueExpr;
  "Preamble",J.string v.P.preamble;
  "ExpectedStdout",option J.string v.P.expectedStdout;
  "ExpectedStderr",option J.string v.P.expectedStderr;
  "Arguments",list J.string v.P.arguments;
  "Environment",list (fun (k,v)->J.tuple [J.string k;J.string v]) v.P.environment;
  "Stdin",(match v.P.stdin with P.Closed->J.union "TestStdin" "Closed" []|P.Bytes s->J.union "TestStdin" "Bytes" [J.string s]);
  "OutputMatch",(match v.P.outputMatch with P.NormalizedText->J.union "OutputMatch" "NormalizedText" []|P.ExactBytes->J.union "OutputMatch" "ExactBytes" []);
  "Isolated",boolean v.P.isolated;
  "ExpectedExitCode",J.int32 v.P.expectedExitCode;
  "ErrorExpectation",option (fun k->J.union "ErrorExpectation" (match k with P.AnyError->"AnyError"|P.CompileError->"CompileError") []) v.P.errorExpectation;
  "ExpectedErrorMessage",option J.string v.P.expectedErrorMessage;
  "SkipReason",option J.string v.P.skipReason;
  "DisableFreeList",boolean v.P.disableFreeList;
  "DisableANFOpt",boolean v.P.disableANFOpt;
  "DisableANFConstFolding",boolean v.P.disableANFConstFolding;
  "DisableANFConstProp",boolean v.P.disableANFConstProp;
  "DisableANFCopyProp",boolean v.P.disableANFCopyProp;
  "DisableANFDCE",boolean v.P.disableANFDCE;
  "DisableANFStrengthReduction",boolean v.P.disableANFStrengthReduction;
  "DisableInlining",boolean v.P.disableInlining;
  "DisableTCO",boolean v.P.disableTCO;
  "DisableMIROpt",boolean v.P.disableMIROpt;
  "DisableMIRSCCP",boolean v.P.disableMIRSCCP;
  "DisableMIRCSE",boolean v.P.disableMIRCSE;
  "DisableMIRDCE",boolean v.P.disableMIRDCE;
  "DisableMIRLICM",boolean v.P.disableMIRLICM;
  "DisableLIROpt",boolean v.P.disableLIROpt;
  "DisableLIRPeephole",boolean v.P.disableLIRPeephole;
  "DisableFunctionTreeShaking",boolean v.P.disableFunctionTreeShaking;
  "DisableLeakCheck",boolean v.P.disableLeakCheck;
  "SourceFile",J.string v.P.sourceFile;
  "FunctionLineMap",(`Assoc ["map",list (fun (k,v)->J.tuple [J.string k;J.int32 v]) (P.StringMap.bindings v.P.functionLineMap)]);
]
let observe source=
 let open Yojson.Basic.Util in
 let data=Yojson.Basic.from_file "scripts/ocaml/e2e_format_fixtures.json" in
 let files=data |> member "files" |> to_list |> List.map to_string in
 let fixtures=data |> member "fixtures" |> to_list |> List.map (fun f->f |> member "path" |> to_string) in
 let inputs=data |> member "inputs" |> to_list |> List.map (fun row -> row |> to_list |> List.map to_int |> Array.of_list |> HostText.ofScalars) in
 let bucket=int_of_string source in
 let selected=List.filteri (fun i _->i mod 16=bucket) in
 let row s=J.tuple [J.string s;`List [
  guarded (fun ()->option J.string (P.extractFuncName s));
  guarded (fun ()->result J.string (P.parseStringLiteral s));
  guarded (fun ()->result J.string (P.parseTripleQuotedStringLiteral s));
  guarded (fun ()->result J.string (P.parseAnyStringLiteral s));
  guarded (fun ()->J.string (P.stripOuterParens s));
  guarded (fun ()->option (result (option J.string)) (P.tryParseBuiltinErrorExpectation s));
  guarded (fun ()->result (fun (k,v)->J.tuple [J.string k;J.string v]) (P.parseAttribute s));
  guarded (fun ()->list J.string (P.splitBySpacesRespectingQuotes s));
  guarded (fun ()->option J.int32 (P.findCommentStartOutsideQuotes s));
  guarded (fun ()->boolean (P.isExpectationStart s));
  guarded (fun ()->J.string (P.stripQuotedContent s));
  guarded (fun ()->boolean (P.isExpectationCandidate s));
  guarded (fun ()->boolean (P.isAttributeKey s));
  guarded (fun ()->result J.string (P.parseSimpleStdout s));
  guarded (fun ()->boolean (P.hasUnclosedDelimiters s));
  guarded (fun ()->boolean (P.isIncompleteExpectationHead s));
  guarded (fun ()->boolean (P.isIdentifierPathHead s));
  guarded (fun ()->boolean (P.hasClosingParenTest s));
  guarded (fun ()->(fun (idx,n)->J.tuple [option J.int32 idx;J.int32 n]) (P.findSeparatorIndexAndCount s));
  guarded (fun ()->J.int32 (P.countPotentialSeparators s));
  guarded (fun ()->option J.int32 (P.findSeparatorIndex s));
  guarded (fun ()->boolean (P.isTestLine s));
  guarded (fun ()->J.string (P.stripComment s));
  guarded (fun ()->result (option J.string) (P.parseCompileErrorDirective 7 s));
  guarded (fun ()->result privateTest (P.parseMultilineTest s 7 "boundary.e2e" "def f(x) = x" P.StringMap.empty))]] in
 let file path=J.tuple [J.string path;result (list test) (E2EFormat.parseE2ETestFile path);result test (E2EFormat.parseE2ETest path)] in
 J.tuple [list file (selected (files@fixtures)@["missing.e2e";"src/Tests"]);list row (selected inputs)]
