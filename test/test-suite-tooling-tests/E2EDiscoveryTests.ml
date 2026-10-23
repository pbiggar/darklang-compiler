(* E2EDiscoveryTests.ml - Preserve discovered tests and validate source-owned dispositions. *)
open E2EFormat

let require condition message = if condition then Ok () else Error message

let withFixture contents operation =
  let path = Filename.temp_file "dark-discovery-" ".dark" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun channel ->
          Out_channel.output_string channel contents);
      operation path)

let testPerTestMetadata () =
  withFixture
    "// @compiler blocked: test-counter\nUnknown.fn () = 1L\n2L = 2L\n"
    (fun path ->
      let tests, errors = E2EDiscovery.loadE2ETests [| path |] in
      require
        (errors = []
        && Array.length tests = 2
        && tests.(0).skipReason = Some "blocked: test-counter"
        && tests.(1).skipReason = None)
        "Discovery lost a test or leaked a disposition")

let testLiteralMetadata () =
  withFixture
    "\"// @compiler blocked: test-counter\" = \"// @compiler blocked: \
     test-counter\"\n" (fun path ->
      let tests, errors = E2EDiscovery.loadE2ETests [| path |] in
      require
        (errors = [] && Array.length tests = 1 && tests.(0).skipReason = None)
        "Literal contents suppressed a fixture")

let testUnknownTag () =
  withFixture "// @compiler blocked: unknown-tag\n1L = 1L\n" (fun path ->
      let tests, errors = E2EDiscovery.loadE2ETests [| path |] in
      require
        (Array.length tests = 0 && List.length errors = 1)
        "Unknown metadata was silently ignored")

let testIndependentFiles () =
  withFixture "// @compiler blocked: unknown-tag\n1L = 1L\n" (fun invalid ->
      withFixture "2L = 2L\n" (fun valid ->
          let tests, errors = E2EDiscovery.loadE2ETests [| invalid; valid |] in
          require
            (Array.length tests = 1
            && List.length errors = 1
            && tests.(0).sourceFile = valid)
            "A bad fixture hid tests from another file"))

let tests =
  [
    ("per-test metadata preserves all cases", testPerTestMetadata);
    ("literal metadata is not a disposition", testLiteralMetadata);
    ("unknown tags are discovery errors", testUnknownTag);
    ("invalid files do not hide valid files", testIndependentFiles);
  ]
