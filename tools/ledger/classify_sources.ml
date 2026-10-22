(* classify_sources.ml - Bootstrap source annotations by checking existing compiler contracts. *)
open Dark_compiler
module W = WrittenTypes
module I = SourceInventory
module S = SourceSelection
module J = Yojson.Basic.Util

let require = function Ok value -> value | Error message -> failwith message

let paths file =
  Yojson.Basic.from_file file |> J.to_list |> List.map J.to_string

let read file = In_channel.with_open_bin file In_channel.input_all

let main root upstream compiler output =
  let reasons =
    CompilerAnnotations.readReasons
      (Filename.concat root "porting/compiler-reasons.json")
    |> require
  in
  let load path =
    (path, I.parse ~reasons (read (Filename.concat root path)) |> require)
  in
  let compilerSources = List.map load (paths compiler) in
  let units =
    List.map
      (fun (_, source) ->
        Validation.validate Validation.Script source.I.tree
        |> Result.map_error (fun _ -> "Invalid compiler library syntax")
        |> require)
      compilerSources
  in
  let items =
    ResultList.traverse WrittenSource.items units |> require |> List.concat
  in
  let types = WrittenDeclarations.predeclareTypes items |> require in
  let _, _, environment =
    WrittenChecking.checkSourceUnitsWithBase None true false units |> require
  in
  let traits =
    List.fold_left
      (fun found -> function
        | WrittenSource.Trait (path, declaration) ->
            StringOrder.Set.add
              (String.concat "." (path @ [ declaration.W.name.W.name ]))
              found
        | WrittenSource.Function _ | WrittenSource.Value _
        | WrittenSource.Type _ | WrittenSource.Impl _
        | WrittenSource.Expression _ ->
            found)
      StringOrder.Set.empty items
  in
  let context : SourceContracts.context =
    { SourceContracts.types; traits; environment = Some environment }
  in
  let candidates =
    List.concat_map (fun (file, source) -> S.nodes file source) compilerSources
  in
  let decision (node : S.node) =
    let matches =
      List.filter
        (fun (candidate : S.node) ->
          candidate.S.name = node.S.name
          && S.kind candidate.S.declaration = S.kind node.S.declaration)
        candidates
    in
    let blocked reason diagnostic =
      `Assoc
        [
          ("status", `String "blocked");
          ("reason", `String reason);
          ("diagnostic", `String diagnostic);
        ]
    in
    match node.S.declaration with
    | W.DTest _ | W.DAssertion _ ->
        blocked "pending-validation"
          "Not executed during declaration classification"
    | W.DTrait _ | W.DImpl _ ->
        blocked "container-adaptation"
          "Container replacement is not yet supported"
    | W.DFunction _ | W.DType _ | W.DValue _ -> (
        match matches with
        | [ replacement ] -> (
            match
              SourceContracts.check ~context ~sources:[]
                [ (node.S.path, node.S.declaration, replacement.S.declaration) ]
            with
            | Ok () ->
                `Assoc
                  [
                    ("status", `String "replace");
                    ("reason", `String "compiler-implementation");
                    ( "target",
                      `String (replacement.S.file ^ "#" ^ replacement.S.name) );
                  ]
            | Error message -> blocked "compiler-contract" message)
        | [] ->
            blocked "missing-implementation"
              "No compiler implementation is registered"
        | _ :: _ :: _ -> blocked "compiler-contract" "Ambiguous counterpart")
    | W.DExpr _ | W.DModule _ ->
        blocked "missing-implementation"
          "No compiler implementation is registered"
  in
  let rows =
    List.map
      (fun file ->
        let _, source = load file in
        let nodes = S.nodes file source in
        let entries =
          List.map
            (fun entry ->
              let compiler =
                match
                  List.find_opt
                    (fun (node : S.node) -> node.S.name = entry.I.name)
                    nodes
                with
                | Some node -> decision node
                | None ->
                    `Assoc
                      [
                        ("status", `String "blocked");
                        ("reason", `String "container-adaptation");
                      ]
              in
              `Assoc
                [
                  ("name", `String entry.I.name);
                  ("kind", `String (I.kindName entry.I.kind));
                  ("line", `Int (entry.I.position.Tokenizer.row + 1));
                  ("column", `Int entry.I.position.Tokenizer.column);
                  ("compiler", compiler);
                ])
            source.I.entries
        in
        `Assoc [ ("source", `String file); ("items", `List entries) ])
      (paths upstream)
  in
  Yojson.Basic.to_file output (`Assoc [ ("sources", `List rows) ])

let () =
  match Array.to_list Sys.argv with
  | [ _; root; upstream; compiler; output ] ->
      main root upstream compiler output
  | _ ->
      prerr_endline
        "Usage: classify_sources ROOT UPSTREAM_MANIFEST COMPILER_MANIFEST \
         OUTPUT";
      exit 1
