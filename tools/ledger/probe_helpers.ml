(* probe_helpers.ml - Probe blocked imported declarations against the selected compiler library. *)
open Dark_compiler
module W = WrittenTypes
module S = SourceSelection
module I = SourceInventory
module J = Yojson.Basic.Util

let require = function Ok value -> value | Error message -> failwith message

let unitFor (node : S.node) =
  let declaration =
    if node.S.path = [] then node.S.declaration
    else
      W.DModule
        {
          W.range = W.synthRange;
          keywordModule = W.synthRange;
          name = (W.synthRange, String.concat "." node.S.path);
          declarations = [ node.S.declaration ];
        }
  in
  Validation.validate Validation.Script
    { W.range = W.synthRange; declarations = [ declaration ]; exprsToEval = [] }
  |> Result.map_error (fun _ -> "Invalid helper declaration")

let main includePackages root manifest output =
  let reasons =
    CompilerAnnotations.readReasons
      (Filename.concat root "porting/compiler-reasons.json")
    |> require
  in
  let paths =
    Yojson.Basic.from_file manifest |> J.to_list |> List.map J.to_string
  in
  let candidates =
    paths
    |> List.filter (fun file -> includePackages || Text.contains file "/tests/")
    |> List.concat_map (fun file ->
        let text =
          In_channel.with_open_bin
            (Filename.concat root file)
            In_channel.input_all
        in
        let source = I.parse ~reasons text |> require in
        S.nodes file source
        |> List.filter (fun node ->
            match node.S.declaration with
            | W.DExpr _ when not includePackages -> false
            | W.DFunction _ | W.DType _ | W.DValue _ | W.DExpr _ -> (
                match node.S.disposition with
                | CompilerAnnotations.Blocked { reason; _ } ->
                    List.mem reason
                      (if includePackages then
                         [ "missing-implementation"; "upstream-dependency" ]
                       else
                         [
                           "missing-implementation";
                           "upstream-dependency";
                           "compiler-gap";
                           "missing-intrinsic";
                           "test-counter";
                         ])
                | CompilerAnnotations.Run | CompilerAnnotations.Excluded _
                | CompilerAnnotations.Replace _ | CompilerAnnotations.Xfail _ ->
                    false)
            | W.DTrait _ | W.DImpl _ | W.DModule _ | W.DTest _ | W.DAssertion _
              ->
                false))
  in
  let readLines path =
    In_channel.with_open_bin (Filename.concat root path) In_channel.input_all
    |> String.split_on_char '\n'
    |> List.filter (fun line ->
        line <> "" && not (String.starts_with ~prefix:"#" line))
  in
  let roots =
    readLines "upstream/sources.list"
    |> List.filter (String.starts_with ~prefix:"packages/")
    |> List.map (( ^ ) "upstream/")
  in
  let supplements = readLines "library-sources.list" in
  let sources =
    roots @ supplements
    |> List.map (fun file ->
        let text =
          In_channel.with_open_bin
            (Filename.concat root file)
            In_channel.input_all
        in
        (file, I.parse ~reasons text |> require))
  in
  let selected =
    S.select ~context:SourceContracts.emptyContext ~roots ~supplements ~sources
    |> require
  in
  let units = List.map snd selected.S.units in
  let base =
    let _, _, environment =
      WrittenChecking.checkSourceUnitsWithBase None true false units |> require
    in
    Some environment
  in
  let rec pass environment accepted candidates =
    let environment, accepted, remaining, changed =
      List.fold_left
        (fun (environment, accepted, remaining, changed) (node, _) ->
          let result =
            Result.bind (unitFor node) (fun unit_ ->
                WrittenChecking.checkSourceUnitsWithBase environment true false
                  [ unit_ ])
          in
          match result with
          | Ok (_, _, next) -> (Some next, node :: accepted, remaining, true)
          | Error message ->
              (environment, accepted, (node, message) :: remaining, changed))
        (environment, accepted, [], false)
        candidates
    in
    Printf.eprintf "Probe: %d checked, %d remaining\n%!" (List.length accepted)
      (List.length remaining);
    if changed then pass environment accepted (List.rev remaining)
    else (List.rev accepted, List.rev remaining)
  in
  let sameNode (left : S.node) (right : S.node) =
    left.S.file = right.S.file && left.S.name = right.S.name
    && S.kind left.S.declaration = S.kind right.S.declaration
  in
  let rec combined rejected =
    let available =
      candidates
      |> List.filter (fun node ->
          not (List.exists (fun (other, _) -> sameNode node other) rejected))
    in
    let accepted, blocked =
      pass base [] (List.map (fun node -> (node, "")) available)
    in
    let initialRejections = List.length rejected in
    let rec validate accepted rejected =
      let combinedSources =
        List.map
          (fun (file, (source : I.source)) ->
            let entries =
              List.map
                (fun (entry : I.entry) ->
                  if
                    List.exists
                      (fun (node : S.node) ->
                        node.S.file = file && node.S.name = entry.I.name
                        && S.kind node.S.declaration = I.kindName entry.I.kind)
                      accepted
                  then { entry with I.disposition = CompilerAnnotations.Run }
                  else entry)
                source.I.entries
            in
            (file, { source with I.entries }))
          sources
      in
      let result =
        Result.bind
          (S.select ~context:SourceContracts.emptyContext ~roots ~supplements
             ~sources:combinedSources) (fun selected ->
            WrittenChecking.checkSourceUnitsWithBase None true false
              (List.map snd selected.S.units))
      in
      match result with
      | Ok _ -> (accepted, rejected @ blocked)
      | Error diagnostic -> (
          let responsible =
            if
              String.starts_with ~prefix:"Constructor identity collision "
                diagnostic
            then
              let direct =
                List.find_opt
                  (fun node ->
                    S.kind node.S.declaration = "type"
                    && Text.contains diagnostic (node.S.name ^ "."))
                  accepted
              in
              match direct with
              | Some _ -> direct
              | None ->
                  (* A newly duplicated case name can change the identity of
                    an existing constructor, including two cases in one base type. *)
                  let identities = String.split_on_char ' ' diagnostic in
                  let names =
                    List.filter_map
                      (fun identity ->
                        match List.rev (String.split_on_char '.' identity) with
                        | name :: _ :: _ ->
                            Some
                              (String.trim
                                 (String.concat ""
                                    (String.split_on_char ',' name)))
                        | _ -> None)
                      identities
                  in
                  List.find_opt
                    (fun (node : S.node) ->
                      match node.S.declaration with
                      | W.DType { W.definition = W.TDEnum cases; _ } ->
                          List.exists
                            (fun (_, (case : W.enumCaseSyntax)) ->
                              List.mem (snd case.W.name) names)
                            cases
                      | W.DType { W.definition = W.TDAlias _ | W.TDRecord _; _ }
                      | W.DFunction _ | W.DValue _ | W.DTrait _ | W.DImpl _
                      | W.DModule _ | W.DTest _ | W.DAssertion _ | W.DExpr _ ->
                          false)
                    accepted
            else
              List.find_opt
                (fun (node : S.node) ->
                  String.starts_with ~prefix:(node.S.name ^ ":") diagnostic)
                accepted
          in
          match responsible with
          | Some node ->
              Printf.eprintf "Combined rejection: %s: %s\n%!" node.S.name
                diagnostic;
              validate
                (List.filter (fun other -> not (sameNode node other)) accepted)
                ((node, diagnostic) :: rejected)
          | None ->
              if List.length rejected > initialRejections then combined rejected
              else
                failwith
                  ("Cannot attribute combined source failure: " ^ diagnostic))
    in
    validate accepted rejected
  in
  let accepted, blocked =
    if includePackages then combined []
    else pass base [] (List.map (fun node -> (node, "")) candidates)
  in
  let row (node : S.node) status diagnostic =
    let line =
      match node.S.declaration with
      | W.DFunction fn -> fn.W.keywordLet.Tokenizer.start.Tokenizer.row + 1
      | W.DType typ -> typ.W.keywordType.Tokenizer.start.Tokenizer.row + 1
      | W.DValue value -> value.W.keywordVal.Tokenizer.start.Tokenizer.row + 1
      | W.DExpr expression ->
          (W.exprRange expression).Tokenizer.start.Tokenizer.row + 1
      | W.DTrait _ | W.DImpl _ | W.DModule _ | W.DTest _ | W.DAssertion _ ->
          Crash.crash "Non-helper candidate"
    in
    `Assoc
      [
        ("source", `String node.S.file);
        ("name", `String node.S.name);
        ("kind", `String (S.kind node.S.declaration));
        ("line", `Int line);
        ("status", `String status);
        ("diagnostic", `String diagnostic);
      ]
  in
  Yojson.Basic.to_file output
    (`List
       (List.map (fun node -> row node "checked" "") accepted
       @ List.map (fun (node, message) -> row node "blocked" message) blocked));
  Printf.printf "Imported declarations: %d checked, %d blocked\n"
    (List.length accepted) (List.length blocked)

let () =
  match Array.to_list Sys.argv with
  | [ _; root; manifest; output ] -> main false root manifest output
  | [ _; "--packages"; root; manifest; output ] ->
      main true root manifest output
  | _ ->
      prerr_endline "Usage: probe_helpers [--packages] ROOT MANIFEST REPORT";
      exit 1
