(* Literal regression checks for exact source formatting and package numbers. *)
open Dark_compiler

let checkFloat value =
  let source =
    String.trim
      (ASTPrettyPrinter.formatProgram
         (AST.Program [ AST.Expression ([], AST.FloatLiteral value) ]))
  in
  match WrittenParsing.parse Validation.Script source with
  | Error error ->
      Crash.crash
        ("Formatted float is invalid Dark source: " ^ source ^ ": " ^ error)
  | Ok _ ->
      if
        Int64.bits_of_float (float_of_string source)
        <> Int64.bits_of_float value
      then Crash.crash ("Float source formatting lost binary64 bits: " ^ source)

let () =
  List.iter checkFloat
    [
      0.;
      -0.;
      0.1;
      -0.1;
      1e-5;
      1e17;
      1.2345678901234567;
      Float.max_float;
      Float.min_float;
      Int64.float_of_bits 1L;
      Int64.float_of_bits 0x000fffffffffffffL;
    ];
  let state = Random.State.make [| 42 |] in
  for _ = 1 to 2000 do
    let value = Int64.float_of_bits (Random.State.bits64 state) in
    if Float.is_finite value then checkFloat value
  done;
  let integer = "170141183460469231731687303715884105727" in
  let package = `Assoc [ ("EInt128", `List [ `Null; `Intlit integer ]) ] in
  (match PackageManager.renderExpr [] "literal" package with
  | Ok source when source = integer ^ "Q" -> ()
  | Ok source -> Crash.crash ("Package integer spelling changed: " ^ source)
  | Error error -> Crash.crash ("Package integer decoding failed: " ^ error));
  print_endline "Exact float source and package integer regressions passed"
