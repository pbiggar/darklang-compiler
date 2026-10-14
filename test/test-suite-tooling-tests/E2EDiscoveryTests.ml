(* E2EDiscoveryTests.ml - Verify effective skips and deterministic real-parser exports. *)
[@@@warning "-4-42"]
open Dark_compiler
open E2EFormat

let require condition message = if condition then Ok () else Error message
let ( let* ) = Result.bind

let withFixture contents operation =
  let path = Filename.temp_file "dark-discovery-" ".dark" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    Out_channel.with_open_bin path (fun channel -> Out_channel.output_string channel contents);
    operation path)

let withTest operation =
  withFixture "1L = 1L\n" (fun path ->
    match E2EFormat.parseE2ETestFile path with
    | Ok [test] -> operation test
    | Ok _ -> Error "Expected exactly one fixture assertion"
    | Error message -> Error message)

let testMetadataHasPrecedence () =
  withTest (fun test ->
    let test = { test with E2EFormat.sourceFile = "test/fixtures/e2e/upstream/stdlib/sqlite.dark";
                          skipReason = Some "fixture reason" } in
    let selected = E2EDiscovery.applyUpstreamEnablementGate test in
    let* () = require (selected.skipReason = Some "fixture reason") "Gate overwrote fixture metadata" in
    require (E2EDiscovery.exclusionOrigin test = Some "fixture-metadata") "Wrong metadata provenance")

let testWholeFileGateAndSpecialReason () =
  withTest (fun test ->
    let test = { test with E2EFormat.sourceFile = "/work/tree/test/fixtures/e2e/upstream/stdlib/sqlite.dark";
                          preamble = "Builtin.testRuntimeError" } in
    let selected = E2EDiscovery.applyUpstreamEnablementGate test in
    let* () = require (E2EDiscovery.exclusionOrigin test = Some "whole-file-gate") "Missing file gate" in
    require (selected.skipReason = Some "unsupported interpreter test infrastructure: Builtin.testRuntimeError")
      "Wrong upstream infrastructure reason")

let testLineGateUsesRunnerName () =
  withTest (fun test ->
    let test = { test with E2EFormat.sourceFile = "test/fixtures/e2e/upstream/stdlib/string.dark";
                          name = "L179: multiline assertion"; sourceLine = 180 } in
    let selected = E2EDiscovery.applyUpstreamEnablementGate test in
    let* () = require (selected.skipReason = Some "pending upstream support") "Name-based line gate did not apply" in
    let* () = require (E2EDiscovery.exclusionOrigin test = Some "line-gate") "Wrong line provenance" in
    let ungated = { test with E2EFormat.name = "L178: different assertion" } in
    require (Option.is_none (E2EDiscovery.applyUpstreamEnablementGate ungated).skipReason)
      "Source coordinates incorrectly gated a different assertion")

let testPathNormalizationAndUngatedCases () =
  withTest (fun test ->
    let windows = { test with E2EFormat.sourceFile = "C:\\tree\\test\\fixtures\\e2e\\upstream\\stdlib\\sqlite.dark" } in
    let* () = require (E2EDiscovery.exclusionOrigin windows = Some "whole-file-gate") "Path separators changed gating" in
    require (E2EDiscovery.exclusionOrigin test = None &&
             (E2EDiscovery.applyUpstreamEnablementGate test).skipReason = None)
      "Ungated fixture was suppressed")

let testYamlScalarsPreserveNames () =
  require (E2EDiscovery.yamlString "emoji 😀\n\"\\\001" = "\"emoji \\U0001F600\\n\\\"\\\\\\u0001\"")
    "YAML escaping lost a Unicode name or control byte"

let testInventoryUsesRealParserAndPreservesMetadata () =
  withFixture "(1L +\n  1L) = 2L\n2L = skip=\"fixture reason\"\n3L = compileerror=\"rejected\"\n" (fun path ->
    let inventory, complete = E2EDiscovery.renderInventory (fun _ -> "fixture.dark") [|path|] in
    let* () = require complete "Valid multiline fixture failed discovery" in
    let* () = require (Text.contains inventory "name: \"L1:") "Multiline assertion identity was not parsed" in
    let* () = require (Text.contains inventory "exclusion_origin: \"fixture-metadata\"" &&
                      Text.contains inventory "effective_skip: \"fixture reason\"" &&
                      Text.contains inventory "eligible: false") "Effective metadata skip was lost" in
    require (Text.contains inventory "error_expectation: \"compile-error\"") "AOT expectation was lost")

let testInventoryReportsErrorsAndSortsFiles () =
  withFixture "1L = 1L\n" (fun path ->
    let missing = path ^ ".missing" in
    let first, complete = E2EDiscovery.renderInventory Fun.id [|missing; path|] in
    let second, _ = E2EDiscovery.renderInventory Fun.id [|path; missing|] in
    let* () = require (first = second) "Filesystem order changed the inventory" in
    let normalized, _ = E2EDiscovery.renderInventory (fun _ -> "fixture.dark") [|missing|] in
    let* () = require (not (Text.contains normalized missing) &&
                      Text.contains normalized "Test file not found: fixture.dark")
      "A missing-file error retained its worktree location" in
    require (not complete && Text.contains first "parse_errors:\n  - source_file:")
      "A failed fixture disappeared or discovery was claimed complete")

let tests = [
  "fixture metadata takes precedence", testMetadataHasPrecedence;
  "whole-file gates retain infrastructure reasons", testWholeFileGateAndSpecialReason;
  "line gates match actual runner names", testLineGateUsesRunnerName;
  "paths normalize and ungated cases remain eligible", testPathNormalizationAndUngatedCases;
  "YAML scalars preserve Unicode and controls", testYamlScalarsPreserveNames;
  "inventory uses real multiline parser and metadata", testInventoryUsesRealParserAndPreservesMetadata;
  "inventory reports errors and sorts files", testInventoryReportsErrorsAndSortsFiles;
]
