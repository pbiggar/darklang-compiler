(* main.ml - Seeded fuzz campaigns, replay, and deterministic reduction. *)
open Dark_compiler

let write path source =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel source)

let read path = In_channel.with_open_bin path In_channel.input_all

let main () =
  let seed = ref None
  and depth = ref 6
  and timeout = ref 2000
  and limit = ref None in
  let artifacts = ref "fuzz-results"
  and interpreter = ref "darklang-interpreter" in
  let replay = ref None and minimize = ref None and generate = ref None in
  let positive flag assign value =
    if value < 1 then raise (Arg.Bad (flag ^ " must be positive"))
    else assign value
  in
  let setAction target value =
    if !replay <> None || !minimize <> None || !generate <> None then
      raise (Arg.Bad "Choose one action");
    target := Some value
  in
  Arg.parse
    [
      ( "--seed",
        Arg.Int (fun value -> seed := Some value),
        "N Reproducible random seed" );
      ( "--depth",
        Arg.Int (positive "depth" (fun value -> depth := value)),
        "N Maximum AST depth" );
      ( "--max-depth",
        Arg.Int (positive "depth" (fun value -> depth := value)),
        "N Maximum AST depth" );
      ( "--timeout-ms",
        Arg.Int (positive "timeout" (fun value -> timeout := value)),
        "N External process timeout" );
      ( "--limit",
        Arg.Int (positive "limit" (fun value -> limit := Some value)),
        "N Stop after N generated cases" );
      ( "--artifacts",
        Arg.Set_string artifacts,
        "PATH Saved sources and findings" );
      ( "--interpreter",
        Arg.Set_string interpreter,
        "PATH Semantic oracle executable" );
      ("--replay", Arg.String (setAction replay), "FILE Replay a source file");
      ( "--minimize",
        Arg.String (setAction minimize),
        "FILE Reduce a failing source file" );
      ( "--generate",
        Arg.Int (positive "generate" (setAction generate)),
        "N Write N generated sources without executing" );
    ]
    (fun argument -> raise (Arg.Bad ("Unexpected argument: " ^ argument)))
    "Native Darklang differential fuzzer";
  let seed =
    match !seed with
    | Some seed -> seed
    | None ->
        Random.self_init ();
        Random.bits ()
  in
  let random = Random.State.make [| seed |] in
  let artifactPath = Fpath.v !artifacts in
  (match Bos.OS.Dir.create artifactPath with
  | Ok _ -> ()
  | Error (`Msg message) -> failwith message);
  let source () =
    Generator.generate random !depth |> ASTPrettyPrinter.formatProgram
  in
  match !generate with
  | Some count ->
      for index = 0 to count - 1 do
        write
          (Filename.concat !artifacts
             (Printf.sprintf "seed-%d-case-%d.dark" seed index))
          (source () ^ "\n")
      done;
      Printf.printf "Generated %d typed AST programs (seed %d).\n%!" count seed;
      0
  | None -> (
      if String.contains !interpreter '/' && not (Sys.file_exists !interpreter)
      then
        failwith
          "Oracle executable is unavailable; darklang-interpreter is not on \
           PATH";
      let available = Bos.OS.Cmd.find_tool (Bos.Cmd.v !interpreter) in
      (match available with
      | Ok (Some _) -> ()
      | Ok None ->
          failwith "darklang-interpreter is not on PATH; pass --interpreter"
      | Error (`Msg message) -> failwith message);
      let target =
        match Platform.detectHostTarget () with
        | Ok target -> target
        | Error message -> failwith message
      in
      let stdlib =
        match StdlibCompilation.buildStdlib target with
        | Ok stdlib -> stdlib
        | Error message -> failwith message
      in
      let check = Oracle.check !interpreter !timeout stdlib in
      match (!replay, !minimize) with
      | Some path, _ -> (
          let outcome = check (read path) in
          match outcome with
          | Oracle.Passed ->
              Printf.printf "Replay passed; interpreter and compiler agree.\n%!";
              0
          | Oracle.OracleFailed message ->
              Printf.eprintf "Oracle failed: %s\n%!" message;
              2
          | _ ->
              Printf.eprintf "Replay reproduced: %s\n%!"
                (Oracle.describe outcome);
              1)
      | _, Some path -> (
          match Reducer.minimize check (read path) with
          | Error message ->
              Printf.eprintf "Minimization failed: %s\n%!" message;
              1
          | Ok (source, outcome, attempts, reductions) ->
              let output = Filename.remove_extension path ^ ".min.dark" in
              write output (source ^ "\n");
              Printf.printf
                "Minimized in %d oracle attempts and %d reductions.\n\
                 Result: %s\n\
                 Preserved failure: %s\n\
                 %!"
                attempts reductions output (Oracle.describe outcome);
              0)
      | None, None ->
          Printf.printf "seed: %d\n%!" seed;
          let rec loop index accepted skipped =
            match !limit with
            | Some count when index >= count ->
                Printf.printf
                  "checked %d; accepted %d; interpreter rejected %d\n%!" index
                  accepted skipped;
                if accepted = 0 then (
                  Printf.eprintf "No oracle-accepted cases were checked.\n%!";
                  2)
                else 0
            | _ -> (
                let source = source () in
                write (Filename.concat !artifacts "current.dark") (source ^ "\n");
                match check source with
                | Oracle.Passed ->
                    if (index + 1) mod 100 = 0 then
                      Printf.printf "checked %d; accepted %d; skipped %d\n%!"
                        (index + 1) (accepted + 1) skipped;
                    loop (index + 1) (accepted + 1) skipped
                | Oracle.Unsupported _ -> loop (index + 1) accepted (skipped + 1)
                | Oracle.OracleFailed message ->
                    Printf.eprintf "Oracle failed: %s\n%!" message;
                    2
                | failure ->
                    let prefix =
                      Filename.concat !artifacts
                        (Printf.sprintf "seed-%d-case-%d" seed index)
                    in
                    write (prefix ^ ".dark") (source ^ "\n");
                    write (prefix ^ ".txt")
                      (Printf.sprintf "seed: %d\ncase: %d\nmax-depth: %d\n%s\n"
                         seed index !depth (Oracle.describe failure));
                    Printf.eprintf
                      "Discrepancy found: %s\nArtifacts: %s.dark and %s.txt\n%!"
                      (Oracle.describe failure) prefix prefix;
                    1)
          in
          loop 0 0 0)

let () =
  Sys.catch_break true;
  try exit (main ()) with
  | Sys.Break -> exit 130
  | exception_ ->
      Printf.eprintf "Fuzzer failed: %s\n%!" (Printexc.to_string exception_);
      exit 2
