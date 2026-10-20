(* E2EDiscoveryTests.ml - Verify fixture metadata and upstream enablement gates. *)
[@@@warning "-4-42"]
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
    require (selected.skipReason = Some "fixture reason") "Gate overwrote fixture metadata")

let testWholeFileGateAndSpecialReason () =
  withTest (fun test ->
    let test = { test with E2EFormat.sourceFile = "/work/tree/test/fixtures/e2e/upstream/stdlib/sqlite.dark";
                          preamble = "Builtin.testRuntimeError" } in
    let selected = E2EDiscovery.applyUpstreamEnablementGate test in
    require (selected.skipReason = Some "unsupported interpreter test infrastructure: Builtin.testRuntimeError")
      "Wrong upstream infrastructure reason")

let testLineGateUsesRunnerName () =
  withTest (fun test ->
    let test = { test with E2EFormat.sourceFile = "test/fixtures/e2e/upstream/stdlib/string.dark";
                          name = "L179: multiline assertion"; sourceLine = 180 } in
    let selected = E2EDiscovery.applyUpstreamEnablementGate test in
    let* () = require (selected.skipReason = Some "pending upstream support") "Name-based line gate did not apply" in
    let ungated = { test with E2EFormat.name = "L178: different assertion" } in
    require (Option.is_none (E2EDiscovery.applyUpstreamEnablementGate ungated).skipReason)
      "Source coordinates incorrectly gated a different assertion")

let testPathNormalizationAndUngatedCases () =
  withTest (fun test ->
    let windows = { test with E2EFormat.sourceFile = "C:\\tree\\test\\fixtures\\e2e\\upstream\\stdlib\\sqlite.dark" } in
    let* () = require ((E2EDiscovery.applyUpstreamEnablementGate windows).skipReason = Some "pending upstream support")
      "Path separators changed gating" in
    require ((E2EDiscovery.applyUpstreamEnablementGate test).skipReason = None)
      "Ungated fixture was suppressed")

let tests = [
  "fixture metadata takes precedence", testMetadataHasPrecedence;
  "whole-file gates retain infrastructure reasons", testWholeFileGateAndSpecialReason;
  "line gates match actual runner names", testLineGateUsesRunnerName;
  "paths normalize and ungated cases remain eligible", testPathNormalizationAndUngatedCases;
]
