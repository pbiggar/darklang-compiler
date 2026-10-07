(*
   EncodingDSLTests.ml - Unit tests for ARM64 and x64 encoding fixture extensions.
   Tests parser and runner behavior that cannot safely be asserted by their own DSL files.
*)
[@@@warning "-42"]

open Dark_compiler

type testResult = (unit, string) result

let testParsesAndRunsARM64ExpectedEncodingError () =
  match
    ARM64EncodingFormat.parseARM64EncodingTest
      {fixture|---NAME---
invalid add immediate
---INPUT-ARM64---
ADD_imm(X0, X1, 4096)
---EXPECT-ERROR---
immediate
|fixture}
  with
  | Error msg -> Error ("Expected ARM64 error fixture to parse, got: " ^ msg)
  | Ok test ->
      let result = ARM64EncodingTestRunner.runARM64EncodingTest test in
      if result.TestOutcome.success then Ok ()
      else
        Error
          ("Expected ARM64 encoding error fixture to pass, got: "
         ^ result.TestOutcome.message)

let displayCase (test : X86_64EncodingFormat.x64EncodingTest) =
  let open StructuralValue in
  let byteArray value =
    Array
      (Bytes.to_seq value |> List.of_seq
      |> List.map (fun value -> Scalar (string_of_int (Char.code value) ^ "uy"))
      )
  in
  let expectation =
    match test.X86_64EncodingFormat.expectation with
    | X86_64EncodingFormat.ResolutionErrorContaining value ->
        Union ("ResolutionErrorContaining", [ Text value ])
    | X86_64EncodingFormat.ResolvesTo (bytes, fixups) ->
        Union
          ( "ResolvesTo",
            [
              Tuple
                [
                  (match bytes with
                  | None -> Union ("None", [])
                  | Some value -> Union ("Some", [ byteArray value ]));
                  Sequence (List.map (fun value -> Text value) fixups);
                ];
            ] )
  in
  StructuralFormat.format
    (Record
       [
         ("Name", Text test.X86_64EncodingFormat.name);
         ( "Instructions",
           Sequence
             (List.map MachineDiagnostic.x64Instr
                test.X86_64EncodingFormat.instructions) );
         ("Expectation", expectation);
         ("SourceFile", Text test.X86_64EncodingFormat.sourceFile);
       ])

let displayCases cases =
  let rec first count = function
    | [] -> []
    | _ when count = 0 -> [ "... " ]
    | value :: rest -> displayCase value :: first (count - 1) rest
  in
  "[" ^ String.concat "; " (first 3 cases) ^ "]"

let testParsesMultipleX64EncodingCases () =
  match
    X86_64EncodingFormat.parseX64EncodingFileContent "encoding.x64enc"
      {fixture|---NAME---
register move
---INPUT-X64---
MOV_reg(RAX, RBX)
---OUTPUT-HEX---
48 89 D8

---NAME---
forward jump
---INPUT-X64---
JMP(skip)
MOV_reg(RAX, RAX)
Label(skip)
RET
---OUTPUT-HEX---
E9 03 00 00 00 48 89 C0 C3
|fixture}
  with
  | Ok [ first; second ]
    when first.X86_64EncodingFormat.name = "register move"
         && second.X86_64EncodingFormat.name = "forward jump" ->
      Ok ()
  | Ok cases ->
      Error ("Expected two x64 encoding cases, got " ^ displayCases cases)
  | Error msg -> Error ("Expected x64 encoding cases to parse, got: " ^ msg)

let testRunsX64DeferredFixupExpectation () =
  match
    X86_64EncodingFormat.parseX64EncodingFileContent "fixup.x64enc"
      {fixture|---NAME---
deferred data label
---INPUT-X64---
JMP(data)
---EXPECT-FIXUPS---
data
|fixture}
  with
  | Error msg -> Error ("Expected x64 fixup fixture to parse, got: " ^ msg)
  | Ok [ test ] ->
      let result = X86_64EncodingTestRunner.runX64EncodingTest test in
      if result.TestOutcome.success then Ok ()
      else
        Error
          ("Expected x64 fixup fixture to pass, got: "
         ^ result.TestOutcome.message)
  | Ok cases ->
      Error
        (Printf.sprintf "Expected one x64 fixup case, got %d"
           (List.length cases))

let tests =
  [
    ( "ARM64 encoding DSL supports expected errors",
      testParsesAndRunsARM64ExpectedEncodingError );
    ( "x64 encoding DSL parses multiple cases",
      testParsesMultipleX64EncodingCases );
    ( "x64 encoding DSL checks deferred fixups",
      testRunsX64DeferredFixupExpectation );
  ]
