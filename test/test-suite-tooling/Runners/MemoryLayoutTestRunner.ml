(*
   MemoryLayoutTestRunner.ml - Execute source fixtures and observe final native value words.
   The x64 integer printer emits a newline before the separator.
*)
[@@@warning "-4-42"]
open Dark_compiler
module C=CompilationContexts
module O=CompilerOptions
type case={name:string;source:string;expected:string}
let (let*)=Result.bind
let indexFrom source needle start=
 let rec search i=if i+String.length needle>String.length source then None else if String.sub source i (String.length needle)=needle then Some i else search (i+1) in search start
let section heading body=
 let marker="---"^heading^"---" in
 Option.map (fun start->let first=start+String.length marker in let finish=Option.value ~default:(String.length body) (indexFrom body "---" first) in Text.trim (String.sub body first (finish-first))) (indexFrom body marker 0)
let parseCase body=match section "NAME" body,section "INPUT" body,section "EXPECTED" body with
 |Some name,Some source,Some expected when name<>"" && source<>"" && ((String.starts_with ~prefix:"root = word(" expected && String.ends_with ~suffix:")" expected) || expected="root = tuple(shared_word, shared_word)")->Ok {name;source;expected}
 |_->Error "Memory layout fixture needs NAME, INPUT, and EXPECTED root = word(...) or root = tuple(shared_word, shared_word)"
let parseFile path=
 let contents=FileIO.readText path in
 let n=String.length contents in
 let boundaries=List.init n Fun.id |> List.filter (fun i->(i=0 || contents.[i-1]='\n') && i+10<=n && String.sub contents i 10="---NAME---" && (i+10=n || contents.[i+10]='\n')) in
 let boundaries=List.sort_uniq Int.compare (0::n::boundaries) in
 let rec chunks=function first::(last::_ as rest)->String.sub contents first (last-first)::chunks rest|_->[] in
 let chunks=chunks boundaries |> List.filter (fun s->Text.trim s<>"") in
 Result.map List.rev (List.fold_left (fun result chunk->let* cases=result in Result.map (fun case->case::cases) (parseCase chunk)) (Ok []) chunks)
let numberAt text start signed=
 let first=if signed && start<String.length text && text.[start]='-' then start+1 else start in
 let rec endAt i=if i<String.length text && text.[i]>='0' && text.[i]<='9' then endAt (i+1) else i in
 let last=endAt first in if last=first then None else Some (String.sub text start (last-start),last)
let runCase stdlib path test=
 let options={O.defaultOptions with O.nativeLayoutProbe=(if test.expected="root = tuple(shared_word, shared_word)" then O.TupleWords else O.RootWord);enableLeakCheck=false} in
 let report=CompilerLibrary.compile {C.context=C.StdlibOnly stdlib;mode=O.TestExpression;sources=AST.NonEmptyList.singleton {C.name=path;purpose=NameSyntax.SourceUnitPurpose.Executable;source=test.source};allowInternal=false;verbosity=0;options;packageValues=C.emptyPackageValueCatalog;packageManager=None;passTimingRecorder=None;session=None} in
 let* binary=report.O.result in let* output=E2ETestRunner.executeBinaryForTarget report.O.target binary in
 if output.O.exitCode<>0 then Error (Printf.sprintf "Native process exited %d: %s" output.O.exitCode output.O.stderr)
 else if test.expected="root = tuple(shared_word, shared_word)" then
  let observed=Option.bind (numberAt output.O.stdout 0 false) (fun (left,next)->
    let next=if next<String.length output.O.stdout && output.O.stdout.[next]='\r' then next+1 else next in
    let next=if next<String.length output.O.stdout && output.O.stdout.[next]='\n' then next+1 else next in
    if next>=String.length output.O.stdout || output.O.stdout.[next]<>'|' then None else Option.map (fun (right,_)->left,right) (numberAt output.O.stdout (next+1) false)) in
  (match observed with None->Error "Native process did not print two tuple words"|Some ("0",_)->Error "Expected a nonzero managed pointer"|Some (left,right)->if left=right then Ok () else Error ("Expected shared pointer words, got "^left^" and "^right))
 else match numberAt output.O.stdout 0 true with
  |None->Error "Native process did not print a root word"
  |Some (value,_)->let actual="root = word("^value^")" in if actual=test.expected then Ok () else Error ("Expected "^test.expected^", got "^actual)
let tests stdlib files=Array.to_list files |> List.sort StringOrder.compare |> List.concat_map (fun path->match parseFile path with
 |Error error->["parse "^Filename.basename path,(fun ()->Error error)]
 |Ok cases->List.map (fun test->test.name,(fun ()->runCase stdlib path test)) cases)
