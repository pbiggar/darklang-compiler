(* E2EDiscovery.ml - Load fixtures with the suite's upstream enablement gates. *)
[@@@warning "-4-42"]
open Dark_compiler
module E = E2EFormat

(* Exclusions match the exhaustive 2026-10-08 compatibility test audit. *)
let disabledUpstreamFiles =
  [
    "test/fixtures/e2e/upstream/cli/app-service-safety.dark";
    "test/fixtures/e2e/upstream/cli/command-completions.dark";
    "test/fixtures/e2e/upstream/cli/deprecation-kinds.dark";
    "test/fixtures/e2e/upstream/cli/include-parsing.dark";
    "test/fixtures/e2e/upstream/cli/outliner.dark";
    "test/fixtures/e2e/upstream/cli/permissions-display.dark";
    "test/fixtures/e2e/upstream/cli/permissions-grammar.dark";
    "test/fixtures/e2e/upstream/cli/tailscale.dark";
    "test/fixtures/e2e/upstream/cli/workbench-repl.dark";
    "test/fixtures/e2e/upstream/language/builtin-introspection.dark";
    "test/fixtures/e2e/upstream/language/effect-ceiling.dark";
    "test/fixtures/e2e/upstream/language/runtime-to-programtypes.dark";
    "test/fixtures/e2e/upstream/scm/commit-hash.dark";
    "test/fixtures/e2e/upstream/scm/constraint-kinds.dark";
    "test/fixtures/e2e/upstream/scm/lww.dark";
    "test/fixtures/e2e/upstream/scm/matter-routes.dark";
    "test/fixtures/e2e/upstream/scm/propagation-policy.dark";
    "test/fixtures/e2e/upstream/scm/removal-conflicts.dark";
    "test/fixtures/e2e/upstream/scm/sync-seen-everything.dark";
    "test/fixtures/e2e/upstream/scm/sync-wire.dark";
    "test/fixtures/e2e/upstream/stachu/darklangParser.dark";
    "test/fixtures/e2e/upstream/stachu/parser.dark";
    "test/fixtures/e2e/upstream/stachu/tinyLang.dark";
    "test/fixtures/e2e/upstream/stdlib/earg.dark";
    "test/fixtures/e2e/upstream/stdlib/eself.dark";
    "test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark";
    "test/fixtures/e2e/upstream/stdlib/language-tools/pickLocation.dark";
    "test/fixtures/e2e/upstream/stdlib/language-tools/semanticTokenization.dark";
    "test/fixtures/e2e/upstream/stdlib/prettyPrinter.dark";
    "test/fixtures/e2e/upstream/stdlib/sqlite.dark";
  ]
let disabledUpstreamLines =
  [
    ("test/fixtures/e2e/upstream/stdlib/string.dark", [ 179 ]);
    ("test/fixtures/e2e/upstream/language/apply/eapply.dark", [ 118 ]);
    ("test/fixtures/e2e/upstream/language/apply/einfix.dark", [ 57; 58; 59 ]);
    ("test/fixtures/e2e/upstream/language/basic/eand.dark", [ 5; 13 ]);
    ( "test/fixtures/e2e/upstream/language/basic/elet.dark",
      [ 1; 2; 19; 20; 45; 54; 60; 66; 71; 82 ] );
    ("test/fixtures/e2e/upstream/language/basic/eor.dark", [ 18; 19 ]);
    ("test/fixtures/e2e/upstream/language/collections/dlist.dark", [ 9; 10 ]);
    ( "test/fixtures/e2e/upstream/language/collections/dtuple.dark",
      [ 9; 11 ] );
    ( "test/fixtures/e2e/upstream/language/custom-data/enums.dark",
      [ 7; 9; 11; 74 ] );
    ( "test/fixtures/e2e/upstream/language/custom-data/record-field-acess.dark",
      [ 13 ] );
    ("test/fixtures/e2e/upstream/language/custom-data/records.dark", [ 8 ]);
    ( "test/fixtures/e2e/upstream/language/custom-data/values.dark",
      [
        5;
        9;
        13;
        17;
        21;
        25;
        29;
        33;
        37;
        41;
        45;
        49;
        53;
        57;
        61;
        65;
        76;
        77;
        78;
        80;
        81;
        83;
        84;
        86;
        87;
        89;
        90;
        92;
        93;
        95;
        96;
        98;
        99;
        101;
        102;
        104;
        105;
        107;
        108;
        110;
        111;
        113;
        114;
        116;
        117;
        119;
        120;
        122;
        123;
        125;
        126;
        128;
        129;
      ] );
    ( "test/fixtures/e2e/upstream/language/derror.dark",
      [ 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 25; 32 ] );
    ( "test/fixtures/e2e/upstream/language/elambda.dark",
      [ 13; 18; 21; 33; 35 ] );
    ("test/fixtures/e2e/upstream/language/error-syntax.dark", [ 1 ]);
    ( "test/fixtures/e2e/upstream/language/flow-control/eif.dark",
      [ 3; 4; 5; 6; 10; 23; 26 ] );
    ( "test/fixtures/e2e/upstream/language/flow-control/ematch.dark",
      [ 656; 660; 663; 667; 672; 677; 681 ] );
    ("test/fixtures/e2e/upstream/language/flow-control/epipe.dark", [ 11 ]);
    ("test/fixtures/e2e/upstream/language/nested-fns.dark", [ 55; 60 ]);
    ( "test/fixtures/e2e/upstream/scm/branch-identity.dark",
      [ 9; 20; 23; 27; 31; 32 ] );
    ( "test/fixtures/e2e/upstream/scm/conflicts.dark",
      [
        51;
        58;
        64;
        70;
        79;
        86;
        92;
        99;
        105;
        111;
        129;
        136;
        144;
        155;
        161;
        169;
        175;
        184;
        194;
        204;
        220;
        224;
        228;
        232;
        233;
        234;
        236;
        242;
        243;
        246;
        251;
        257;
        258;
        259;
        263;
        265;
        266;
      ] );
    ( "test/fixtures/e2e/upstream/stdlib/base64.dark",
      [
        7;
        9;
        10;
        11;
        12;
        20;
        21;
        22;
        23;
        24;
        25;
        26;
        27;
        28;
        29;
        33;
        34;
        35;
        36;
        39;
        40;
        43;
        44;
        45;
        46;
        47;
      ] );
    ("test/fixtures/e2e/upstream/stdlib/dict.dark", [ 76; 93 ]);
    ( "test/fixtures/e2e/upstream/stdlib/float.dark",
      [
        47;
        55;
        58;
        59;
        64;
        65;
        73;
        76;
        81;
        89;
        107;
        110;
        111;
        125;
        127;
        134;
        136;
        160;
        173;
        176;
        244;
      ] );
    ( "test/fixtures/e2e/upstream/stdlib/html.dark",
      [ 42; 44; 66; 69; 72; 75; 83 ] );
    ( "test/fixtures/e2e/upstream/stdlib/http.dark",
      [
        21;
        27;
        33;
        48;
        51;
        57;
        63;
        102;
        108;
        110;
        111;
        114;
        117;
        122;
        123;
        124;
        128;
        129;
        130;
        131;
        133;
        137;
        144;
        149;
        155;
        161;
      ] );
    ( "test/fixtures/e2e/upstream/stdlib/httpclient.dark",
      [ 71; 108; 111; 131; 132; 133; 134 ] );
    ("test/fixtures/e2e/upstream/stdlib/httpserver.dark", [ 29; 33; 37 ]);
    ("test/fixtures/e2e/upstream/stdlib/ints/int64.dark", [ 368 ]);
    ("test/fixtures/e2e/upstream/stdlib/ints/int8.dark", [ 47 ]);
    ( "test/fixtures/e2e/upstream/stdlib/json.dark",
      [
        722;
        726;
        728;
        750;
        751;
        755;
        932;
        1071;
        1076;
        1178;
        1180;
        1236;
        1239;
        1243;
        1248;
        1268;
        1330;
        1388;
        1392;
        1402;
        1406;
        1408;
        1411;
      ] );
    ( "test/fixtures/e2e/upstream/stdlib/list.dark",
      [ 61; 65; 71; 75; 81; 157; 184; 238; 346 ] );
    ("test/fixtures/e2e/upstream/stdlib/math.dark", [ 27; 30 ]);
    ("test/fixtures/e2e/upstream/stdlib/nomodule.dark", [ 308 ]);
  ]
let normalizePath path =
  String.map (fun c -> if c = '\\' then '/' else c) path
let pathMatchesSourceFile source suffix =
  Text.endsWith (normalizePath source) suffix
let isDisabledUpstreamFile source =
  List.exists (pathMatchesSourceFile source) disabledUpstreamFiles
let disabledLinesForSourceFile source =
  List.find_map
    (fun (suffix, lines) ->
      if pathMatchesSourceFile source suffix then Some lines else None)
    disabledUpstreamLines
let tryParseTestLineNumber name =
  if Text.startsWith name "L" then
    match String.index_opt name ':' with
    | Some colon when colon > 1 ->
        Option.map Int32.to_int
          (Text.tryParseInt32 (String.sub name 1 (colon - 1)))
    | _ -> None
  else None
let upstreamSkipReason (test : E.e2eTest) =
  if
    Text.contains test.E.source "Builtin.testRuntimeError"
    || Text.contains test.E.preamble "Builtin.testRuntimeError"
  then
    "unsupported interpreter test infrastructure: Builtin.testRuntimeError"
  else "pending upstream support"
let applyUpstreamEnablementGate (test : E.e2eTest) =
  if Option.is_some test.E.skipReason then test
  else if isDisabledUpstreamFile test.E.sourceFile then
    { test with E.skipReason = Some (upstreamSkipReason test) }
  else
    match
      ( disabledLinesForSourceFile test.E.sourceFile,
        tryParseTestLineNumber test.E.name )
    with
    | Some lines, Some line when List.mem line lines ->
        { test with E.skipReason = Some (upstreamSkipReason test) }
    | _ -> test
let loadE2ETests files =
  let tests, errors =
    Array.fold_left
      (fun (tests, errors) file ->
        match E.parseE2ETestFile file with
        | Ok parsed ->
            ( List.rev_append
                (List.map applyUpstreamEnablementGate parsed)
                tests,
              errors )
        | Error msg -> (tests, (file, msg) :: errors))
      ([], []) files
  in
  (Array.of_list (List.rev tests), List.rev errors)

