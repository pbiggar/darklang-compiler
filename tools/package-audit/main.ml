(* main.ml - Audit original interpreter package syntax without a package server. *)
open Dark_compiler

let readSource path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let report ?(metadata = []) phase path result =
  let status, diagnostics =
    match result with
    | Ok _ -> ("passed", `Null)
    | Error message -> ("failed", `String message)
  in
  Yojson.Safe.to_channel stdout
    (`Assoc
       ([
         ("source", `String path);
         ("phase", `String phase);
         ("status", `String status);
         ("diagnostics", diagnostics);
       ] @ metadata));
  output_char stdout '\n';
  flush stdout;
  Result.is_ok result

let compile stdlib dependencies path source =
  let package : CompilationContexts.sourceUnit =
    { name = path; purpose = NameSyntax.SourceUnitPurpose.Package; source }
  and entry : CompilationContexts.sourceUnit =
    {
      name = "package-audit-entry.dark";
      purpose = NameSyntax.SourceUnitPurpose.Executable;
      source = "0L\n";
    }
  in
  let request : CompilationContexts.compileRequest =
    {
      context = CompilationContexts.StdlibOnly stdlib;
      mode = CompilerOptions.FullProgram;
      sources = NonEmptyList.fromList (dependencies @ [ package; entry ]);
      allowInternal = false;
      verbosity = 0;
      options = CompilerOptions.defaultOptions;
      packageValues = CompilationContexts.emptyPackageValueCatalog;
      packageManager = None;
      passTimingRecorder = None;
      session = None;
    }
  in
  (CompilerLibrary.compile request).CompilerOptions.result

let metadata parsed =
  let names =
    Result.bind (WrittenSource.items parsed) (fun items ->
        let exports =
          List.filter_map
            (function
              | WrittenSource.Function (path, fn) ->
                  Some (String.concat "." (path @ [ fn.WrittenTypes.name.name ]))
              | WrittenSource.Value (path, value) ->
                  Some (String.concat "." (path @ [ value.WrittenTypes.name.name ]))
              | WrittenSource.Type (path, typ) ->
                  Some (String.concat "." (path @ [ typ.WrittenTypes.name.name ]))
              | WrittenSource.Expression _ -> None)
            items
        in
        Result.map
          (fun references -> (exports, references))
          (WrittenSource.qualifiedNames [ parsed ]))
  in
  let strings values = `List (List.map (fun value -> `String value) values) in
  match names with
  | Ok (exports, references) ->
      [ ("exports", strings exports); ("references", strings references) ]
  | Error message -> [ ("inventory_error", `String message) ]

let audit stdlib dependencies path =
  let result =
    try
      let source = readSource path in
      Result.map
        (fun parsed -> (source, parsed))
        (WrittenParsing.parse Validation.Package source)
    with Sys_error message -> Error message
  in
  let metadata = match result with Ok (_, parsed) -> metadata parsed | Error _ -> [] in
  let parsed = report ~metadata "parse" path result in
  match (stdlib, result) with
  | Some stdlib, Ok (source, _) ->
      let dependencies =
        List.filter (fun dependency -> dependency.CompilationContexts.name <> path) dependencies
      in
      report "compile" path (compile stdlib dependencies path source)
  | None, _ | Some _, Error _ -> parsed

let prepareStdlib () =
  let result =
    Result.bind (Platform.detectHostTarget ()) StdlibCompilation.buildStdlib
  in
  match result with
  | Ok stdlib -> stdlib
  | Error message ->
      prerr_endline ("Could not prepare compiler standard library: " ^ message);
      exit 2

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: package-audit [--compile] [--dependency PATH] PACKAGE.dark ...";
    exit 2);
  let compile = Sys.argv.(1) = "--compile" in
  let first = if compile then 2 else 1 in
  let rec arguments dependencies paths = function
    | "--dependency" :: path :: rest -> arguments (path :: dependencies) paths rest
    | "--dependency" :: [] ->
        prerr_endline "--dependency requires a package file";
        exit 2
    | path :: rest -> arguments dependencies (path :: paths) rest
    | [] -> (List.rev dependencies, List.rev paths)
  in
  let dependencies, paths =
    arguments [] [] (Array.to_list (Array.sub Sys.argv first (Array.length Sys.argv - first)))
  in
  if paths = [] then (
    prerr_endline "At least one package file is required";
    exit 2);
  let stdlib = if compile then Some (prepareStdlib ()) else None in
  let dependencies =
    List.map
      (fun path ->
        { CompilationContexts.name = path;
          purpose = NameSyntax.SourceUnitPurpose.Package;
          source = readSource path })
      dependencies
  in
  let passed = ref true in
  List.iter
    (fun path -> if not (audit stdlib dependencies path) then passed := false)
    paths;
  exit (if !passed then 0 else 1)
