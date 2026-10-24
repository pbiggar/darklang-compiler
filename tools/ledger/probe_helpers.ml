(* probe_helpers.ml - Check imported test helper bodies against the selected compiler library. *)
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

let main root manifest output =
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
    |> List.filter (fun file -> Text.contains file "/tests/")
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
            | W.DFunction _ | W.DType _ | W.DValue _ -> (
                match node.S.disposition with
                | CompilerAnnotations.Blocked reason ->
                    List.mem reason
                      [
                        "missing-implementation";
                        "upstream-dependency";
                        "compiler-gap";
                        "missing-intrinsic";
                        "test-counter";
                      ]
                | CompilerAnnotations.Run | CompilerAnnotations.ErrorMessage _
                | CompilerAnnotations.Excluded _ | CompilerAnnotations.Replace _
                | CompilerAnnotations.Xfail _ ->
                    false)
            | W.DTrait _ | W.DImpl _ | W.DModule _ | W.DTest _ | W.DAssertion _
            | W.DExpr _ ->
                false))
  in
  let target = Platform.detectHostTarget () |> require in
  let stdlib =
    StdlibCompilation.buildImportedTestStdlibWithTrace target None |> require
  in
  let base =
    stdlib.CompilationContexts.context.CompilationContexts.writtenEnvironment
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
    if changed then pass environment accepted (List.rev remaining)
    else (List.rev accepted, List.rev remaining)
  in
  let accepted, blocked =
    pass base [] (List.map (fun node -> (node, "")) candidates)
  in
  let row (node : S.node) status diagnostic =
    let line =
      match node.S.declaration with
      | W.DFunction fn -> fn.W.keywordLet.Tokenizer.start.Tokenizer.row + 1
      | W.DType typ -> typ.W.keywordType.Tokenizer.start.Tokenizer.row + 1
      | W.DValue value -> value.W.keywordVal.Tokenizer.start.Tokenizer.row + 1
      | W.DTrait _ | W.DImpl _ | W.DModule _ | W.DTest _ | W.DAssertion _
      | W.DExpr _ ->
          Crash.crash "Non-helper candidate"
    in
    `Assoc
      [
        ("source", `String node.S.file);
        ("name", `String node.S.name);
        ("line", `Int line);
        ("status", `String status);
        ("diagnostic", `String diagnostic);
      ]
  in
  Yojson.Basic.to_file output
    (`List
       (List.map (fun node -> row node "checked" "") accepted
       @ List.map (fun (node, message) -> row node "blocked" message) blocked));
  Printf.printf "Imported test helpers: %d checked, %d blocked\n"
    (List.length accepted) (List.length blocked)

let () =
  match Array.to_list Sys.argv with
  | [ _; root; manifest; output ] -> main root manifest output
  | _ ->
      prerr_endline "Usage: probe_helpers ROOT MANIFEST REPORT";
      exit 1
