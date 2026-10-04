(* Compare package rendering, dependencies, content decoding and local SQLite. *)
open Dark_compiler
module P=PackageManager
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let option f=function None->SemanticJson.union "FSharpOption" "None" []|Some value->SemanticJson.union "FSharpOption" "Some" [f value]
let kindName=function P.PackageType->"PackageType"|P.PackageValue->"PackageValue"|P.PackageFunction->"PackageFunction"
let located (value:P.locatedEntity)=SemanticJson.record "LocatedEntity" ["Kind",SemanticJson.union "ItemKind" (kindName value.P.kind) [];"Hash",str value.P.hash;"Location",str value.P.location;"Json",str value.P.json]
let resolved (value:P.resolvedSource)=SemanticJson.record "ResolvedSource" ["Name",str value.P.name;"Source",str value.P.source]
let fetched=function P.Missing->SemanticJson.union "FetchResult" "Missing" []|P.Found body->SemanticJson.union "FetchResult" "Found" [str body]
let attempt action=try action () with Failure message|Invalid_argument message|Yojson.Json_error message->result (fun _->assert false) (Error message)
let cached source=
 let path=Filename.temp_file ~temp_dir:"TestResults/ocaml-migration" "package-native" ".sqlite3" in
 let config={P.server=P.defaultServer;cachePath=path} in
 Fun.protect ~finally:(fun ()->Sys.remove path) (fun ()->
  let events=ref [] in let emit value=events:=value:: !events in
  let read key=emit (result (option fetched) (P.cacheRead config key)) in
  let write key value=emit (result (fun ()->`Null) (P.cacheWrite config key value)) in
  read "key";write "key" (P.Found source);read "key";write "key" P.Missing;read "key";write source (P.Found "body\000λ");read source;
  HostPackageIO.cacheWrite path "bad" 500 "body";read "bad";
  write ("https://matter.darklang.com/cached") (P.Found source);write ("https://matter.darklang.com/missing") P.Missing;
  let client=HostPackageIO.create () in Fun.protect ~finally:(fun ()->HostPackageIO.dispose client) (fun ()->emit (result fetched (P.fetchByHash client config "/cached"));emit (result fetched (P.fetchByHash client config "/missing")));
  list Fun.id (List.rev !events))
let observe source=
 let open Yojson.Basic.Util in
 let cases=Yojson.Basic.from_string source |> to_list in
 list (fun case->attempt (fun ()->
  let text name=case |> member name |> to_string in
  let json ()=text "json" in let element ()=HostJson.parse (json ()) in
  let strings name=case |> member name |> to_list |> List.map to_string in
  let kind ()=match text "kind" with "PackageType"->P.PackageType|"PackageValue"->P.PackageValue|"PackageFunction"->P.PackageFunction|_->invalid_arg "Unknown fixture kind" in
  match text "op" with
  |"renderType"->result str (P.renderType (element ()))|"renderLetPattern"->result str (P.renderLetPattern (element ()))|"renderMatchPattern"->result str (P.renderMatchPattern (element ()))|"infixText"->result str (P.infixText (element ()))|"locationName"->result str (P.locationName (element ()))|"resolvedName"->result str (P.resolvedName (element ()))|"parseHashJson"->result str (P.parseHashJson (json ()))
  |"renderExpr"->result str (P.renderExpr (strings "params") (text "self") (element ()))
  |"parseLocatedEntity"->result located (P.parseLocatedEntity (kind ()) (text "hash") (json ()))
  |"dependencyRefs"->result (list (fun (hash,name)->tuple [str hash;str name])) (P.dependencyRefs (json ()))
  |"renderEntity"->let entity={P.kind=kind ();hash=text "hash";location=text "location";json=json ()} in result resolved (P.renderEntity entity)
  |"candidatePrefixes"->let known=strings "known" in list str (P.candidatePrefixes (fun name->List.mem name known) (strings "names"))
  |"defaults"->let config=P.defaultConfig () in tuple [str P.defaultServer;str (P.defaultCachePath ());str config.P.server;str config.P.cachePath]
  |"cache"->cached (text "text")
  |"content"->let charset=match case |> member "charset" with `Null->None|value->Some ("application/json; charset="^to_string value) in let hex=text "bytes" in let bytes=String.init (String.length hex/2) (fun index->Char.chr (int_of_string ("0x"^String.sub hex (2*index) 2))) in str (HostPackageIO.decodeContent charset bytes)
  |_->invalid_arg "Unknown package observation")) cases
