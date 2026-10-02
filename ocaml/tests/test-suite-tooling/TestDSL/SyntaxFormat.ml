(* SyntaxFormat.ml - Preserve section validation and syntax-case grouping. *)
open Dark_compiler
open Common
type syntaxTest = {name : string; source : string; expectedError : string option; expectedFormat : string option; roundtrip : bool; sourceFile : string}
let caseFromSections path sections =
  let values = List.fold_left (fun values (name, value) -> StringOrder.Map.add name value values) StringOrder.Map.empty sections in
  let allowed = ["NAME"; "SOURCE"; "EXPECT-ERROR"; "EXPECTED"; "ROUNDTRIP"] in
  let unknown = List.find_opt (fun (name, _) -> not (List.mem name allowed)) sections in
  let required name = match StringOrder.Map.find_opt name values with Some value -> Ok (HostText.trim value) | None -> Error ("Missing required syntax section: " ^ name) in
  match unknown, required "NAME", required "SOURCE" with
  | Some (name, _), _, _ -> Error ("Unknown syntax section: " ^ name)
  | None, Ok name, Ok source ->
      let expectedError = Option.map HostText.trim (StringOrder.Map.find_opt "EXPECT-ERROR" values) in
      let expectedFormat = Option.map normalizeLineEndings (StringOrder.Map.find_opt "EXPECTED" values) in
      let roundtrip = StringOrder.Map.mem "ROUNDTRIP" values in
      if Option.is_some expectedError && (Option.is_some expectedFormat || roundtrip) then Error "EXPECT-ERROR cannot be combined with formatting or roundtrip"
      else Ok {name; source = normalizeLineEndings source; expectedError; expectedFormat; roundtrip; sourceFile = path}
  | None, Error error, _ | None, _, Error error -> Error error
let parseSyntaxFileContent path content =
  let rec groups completed current = function
    | [] -> Ok (List.rev (if current = [] then completed else List.rev current :: completed))
    | (("NAME", _) as section) :: rest -> if current = [] then groups completed [section] rest else groups (List.rev current :: completed) [section] rest
    | ((name, _) as section) :: rest -> if current = [] then Error ("Syntax case must start with NAME, found " ^ name) else groups completed (section :: current) rest in
  let sections = parseSections (normalizeLineEndings content) in
  if sections = [] then Error "Syntax fixture contains no sections" else
  Result.bind (groups [] [] sections) (fun cases ->
    List.fold_left (fun state sections -> Result.bind state (fun parsed -> Result.map (fun test -> parsed @ [test]) (caseFromSections path sections))) (Ok []) cases)
