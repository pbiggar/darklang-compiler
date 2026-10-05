(* Original session cache-contract tests and platform-specific registration. *)
open Dark_compiler
module J=Semantic_observation.SemanticJson
let list f xs=`List (List.map f xs)
let result=function Ok ()->J.union "FSharpResult" "Ok" [`Null]|Error error->J.union "FSharpResult" "Error" [J.string error]
let tests values=list (fun (name,run)->prerr_endline ("Session unit: "^name);let outcome=run () in J.tuple [J.string name;result outcome]) values
let prepared=lazy (
 let row target=match StdlibCompilation.buildStdlib target with Error error->J.union "FSharpResult" "Error" [J.string error]|Ok stdlib->J.union "FSharpResult" "Ok" [J.tuple [tests (CompilationSessionTests.tests target stdlib);list (fun target->list (fun (name,_)->J.string name) (CompilationSessionTests.tests target stdlib)) [Platform.ARM64Backend Platform.MacOSARM64;Platform.ARM64Backend Platform.LinuxARM64;Platform.LinuxX86_64]]] in
 list row [Platform.ARM64Backend Platform.LinuxARM64;Platform.LinuxX86_64])
let observe source=J.tuple [J.string source;Lazy.force prepared]
