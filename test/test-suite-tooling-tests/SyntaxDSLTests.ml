(*
   SyntaxDSLTests.ml - Unit tests for the syntax fixture parser and runner.
   Keeps the DSL implementation honest without expressing its own behavior in the DSL.
*)
[@@@warning "-4-42"]
open Dark_compiler
open SyntaxFormat
type testResult=(unit,string) result
let display cases=
 let option=function None->StructuralValue.Union ("None",[])|Some s->StructuralValue.Union ("Some",[StructuralValue.Text s]) in
 let format (test:syntaxTest)=StructuralFormat.format (StructuralValue.Record ["Name",StructuralValue.Text test.name;"Source",StructuralValue.Text test.source;"ExpectedError",option test.expectedError;"ExpectedFormat",option test.expectedFormat;"Roundtrip",StructuralValue.Scalar (string_of_bool test.roundtrip);"SourceFile",StructuralValue.Text test.sourceFile]) in
 let first=List.filteri (fun index _->index<3) cases |> List.map format in "["^String.concat "; " first^(if List.length cases>3 then "; ... " else "")^"]"
let testParsesMultipleSyntaxCases ()=
 let content="---NAME---\ncanonical formatting\n---SOURCE---\nlet x = 5 in x\n---EXPECTED---\nlet x = 5 in x\n---ROUNDTRIP---\n\n---NAME---\nreject fat-arrow lambda\n---SOURCE---\nlet inc = (x: Int64) => x + 1\n---EXPECT-ERROR---\ndoes not use\n" in
 match parseSyntaxFileContent "syntax.syntax" content with
 |Ok [first;second] when first.name="canonical formatting" && first.expectedFormat=Some "\nlet x = 5 in x\n" && first.roundtrip && second.expectedError=Some "does not use"->Ok ()
 |Ok cases->Error ("Expected two fully parsed syntax cases, got "^display cases)
 |Error msg->Error ("Expected syntax cases to parse, got: "^msg)
let testRejectsLegacySyntaxSelector ()=
 let content="---NAME---\nlegacy selector\n---PARSE-AS---\ncompiler\n---SOURCE---\n1\n" in
 match parseSyntaxFileContent "invalid.syntax" content with
 |Error msg when Text.contains msg "Unknown syntax section: PARSE-AS"->Ok ()
 |Error msg->Error ("Expected legacy selector validation error, got: "^msg)
 |Ok _->Error "Expected the legacy parser selector to be rejected"
let testRunsFormattingAndRoundtripChecks ()=
 let testCase={name="canonical formatting";source="let x = 5 in Stdlib.Int64.add x 1";expectedError=None;expectedFormat=Some "let x = 5 in Stdlib.Int64.add x 1";roundtrip=true;sourceFile="syntax.syntax"} in
 let result=SyntaxTestRunner.runSyntaxTest testCase in if result.TestOutcome.success then Ok () else Error ("Expected syntax runner success, got: "^result.TestOutcome.message)
let tests=["syntax DSL parses multiple cases",testParsesMultipleSyntaxCases;"syntax DSL rejects legacy parser selectors",testRejectsLegacySyntaxSelector;"syntax DSL runs format and roundtrip checks",testRunsFormattingAndRoundtripChecks]
