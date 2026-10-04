(*
   ARM64EncodingFormat.fs - Parser for ARM64 encoding test DSL
   Parses .arm64enc test files that specify ARM64 instructions and their
   expected machine code encodings.
   Example format:
   ---NAME---
   Encode MOVZ instruction
   ---INPUT-ARM64---
   MOVZ(X0, 42, 0)
   ---OUTPUT-HEX---
   0xD2800540
   ARM64 encoding test case
   If true, all hex values should be different
*)
[@@@warning "-4"]
open Dark_compiler
type arm64EncodingExpectation = EncodesTo of int32 list | EncodingErrorContaining of string
type arm64EncodingTest = {name:string;instructions:ARM64.instr list;expectation:arm64EncodingExpectation;assertDifferent:bool}
(*
   Parse a hex value from string (e.g., "0xD2800540" -> 0xD2800540u)
*)
let parseHexValue text =
 let text=HostText.trim text in
 if String.starts_with ~prefix:"0x" text || String.starts_with ~prefix:"0X" text then
 let hexStr=String.sub text 2 (String.length text-2) in
 let isHexDigit=function '0'..'9' | 'a'..'f' | 'A'..'F' -> true | _ -> false in
 let hasOnlyHexDigits=String.for_all isHexDigit hexStr in
 let value=if hasOnlyHexDigits && hexStr<>"" then try Some (Z.of_string_base 16 hexStr) with Invalid_argument _ -> None else None in
 (match value with Some value when Z.sign value>=0 && Z.compare value (Z.of_string "4294967295")<=0 -> Ok (Int64.to_int32 (Z.to_int64 value))
 | _ when hasOnlyHexDigits && String.length hexStr>8 -> Error ("Hex value too large: '"^text^"'")
 | _ -> Error ("Invalid hex format: '"^text^"'"))
 else Error ("Hex value must start with '0x': '"^text^"'")
let parseAssertDifferent text=match HostText.lowerInvariant (HostText.trim text) with
 | "true" -> Ok true | "false" -> Ok false | value -> Error ("Invalid ASSERT-DIFFERENT value '"^value^"' (expected 'true' or 'false')")
(*
   Parse ARM64 encoding test from file content
   Parse name (optional, default to "ARM64 encoding test")
   Parse INPUT-ARM64 section
   Parse each hex value
   Verify counts match
   Parse ASSERT-DIFFERENT (optional)
*)
let parseARM64EncodingTest content =
 let testFile=Common.parseTestFile content in
 let name=match Common.getOptionalSection "NAME" testFile with Some text -> HostText.trim text | None -> "ARM64 encoding test" in
 match Common.getRequiredSection "INPUT-ARM64" testFile with
 | Error e -> Error e
 | Ok inputText ->
 let parser=match Common.getOptionalSection "EXPECT-ERROR" testFile with Some _ -> ARM64Parser.parseARM64ForEncodingError | None -> ARM64Parser.parseARM64 in
 (match parser inputText with
 | Error e -> Error ("Failed to parse INPUT-ARM64: "^e)
 | Ok instructions ->
 let outputText=Common.getOptionalSection "OUTPUT-HEX" testFile in let expectedError=Common.getOptionalSection "EXPECT-ERROR" testFile in
 match outputText,expectedError with
 | None,None -> Error "ARM64 encoding test requires OUTPUT-HEX or EXPECT-ERROR"
 | Some _,Some _ -> Error "ARM64 encoding test cannot combine OUTPUT-HEX and EXPECT-ERROR"
 | None,Some errorText when HostText.trim errorText="" -> Error "EXPECT-ERROR cannot be empty"
 | None,Some errorText -> (match Common.getOptionalSection "ASSERT-DIFFERENT" testFile with Some _ -> Error "ASSERT-DIFFERENT requires OUTPUT-HEX" | None -> Ok {name;instructions;expectation=EncodingErrorContaining errorText;assertDifferent=false})
 | Some outputText,None ->
 let hexLines=String.split_on_char '\n' outputText |> List.map HostText.trim |> List.filter (fun line -> line<>"" && not (String.starts_with ~prefix:"//" line)) in
 let rec parseHexValues acc=function [] -> Ok (List.rev acc) | line::rest -> match parseHexValue line with Error e -> Error e | Ok value -> parseHexValues (value::acc) rest in
 (match parseHexValues [] hexLines with
 | Error e -> Error ("Failed to parse OUTPUT-HEX: "^e)
 | Ok hexValues ->
 if List.length instructions<>List.length hexValues then Error (Printf.sprintf "Instruction count (%d) does not match hex value count (%d)" (List.length instructions) (List.length hexValues)) else
 match Common.getOptionalSection "ASSERT-DIFFERENT" testFile with
 | Some text -> Result.map (fun assertDifferent -> {name;instructions;expectation=EncodesTo hexValues;assertDifferent}) (parseAssertDifferent text)
 | None -> Ok {name;instructions;expectation=EncodesTo hexValues;assertDifferent=false}))
