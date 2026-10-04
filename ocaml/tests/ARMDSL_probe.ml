(* Exact observations for ARM64 fixture parsing, including the unchanged corpus. *)
[@@@warning "-4"]
open Dark_compiler
module J=Semantic_observation.MachineISAObservation
let tuple values=`Assoc ["tuple",`List values]
let list f xs=`List (List.map f xs)
let str=Semantic_observation.SemanticJson.string
let word value=`Assoc ["kind",`String "uint32";"value",`String (Printf.sprintf "%lu" value)]
let result f=function Ok value -> Semantic_observation.SemanticJson.union "FSharpResult" "Ok" [f value] | Error msg -> Semantic_observation.SemanticJson.union "FSharpResult" "Error" [str msg]
let fixture (t:ARM64EncodingFormat.arm64EncodingTest)=Semantic_observation.SemanticJson.record "ARM64EncodingTest" ["Name",str t.ARM64EncodingFormat.name;"Instructions",list J.armInstr t.ARM64EncodingFormat.instructions;"Expectation",(match t.ARM64EncodingFormat.expectation with ARM64EncodingFormat.EncodesTo words -> Semantic_observation.SemanticJson.union "ARM64EncodingExpectation" "EncodesTo" [list word words] | ARM64EncodingFormat.EncodingErrorContaining error -> Semantic_observation.SemanticJson.union "ARM64EncodingExpectation" "EncodingErrorContaining" [str error]);"AssertDifferent",`Bool t.ARM64EncodingFormat.assertDifferent]
let observe source =
 let registers=["";"X0";"X18";"X31";"SP";"x1";"\u{2000}X19\u{00a0}";source]@List.init 32 (fun n -> "X"^string_of_int n) in
 let conditions=["EQ";"NE";"LT";"GT";"LE";"GE";"LO";"HI";"LS";"HS";"";source] in
 let numbers=["";"0";"-0";"1";"-1";"+1";"4095";"4096";"65535";"65536";"-32768";"32767";"32768";"2147483647";"2147483648";"999999999999999999999999";"٠";"१२";"1_0";"0x10"] in
 let ops=["MOVZ";"MOVN";"MOVK";"ADD_imm";"SUB_imm";"SVC";"STP";"STP_pre";"LDP";"LDP_post";"STR";"STUR";"LDR";"LDUR"] in
 let instruction name reg value = match name with
 | "MOVZ" | "MOVN" | "MOVK" -> name^"("^reg^", "^value^", "^value^")"
 | "ADD_imm" | "SUB_imm" -> name^"("^reg^", X1, "^value^")"
 | "SVC" -> name^"("^value^")"
 | "STP" | "STP_pre" | "LDP" | "LDP_post" -> name^"("^reg^", X1, SP, "^value^")"
 | _ -> name^"("^reg^", SP, "^value^")" in
 let cases=List.concat_map (fun op -> List.concat_map (fun reg -> List.map (instruction op reg) numbers) ["X0";"X18";"SP";"bad"]) ops @
 List.concat_map (fun reg -> ["ADD_reg("^reg^", X1, X2)";"SUB_reg(X1, "^reg^", X2)";"MUL(X1, X2, "^reg^")";"SDIV("^reg^", X1, X2)";"UDIV(X1, "^reg^", X2)";"MOV_reg("^reg^", X1)";"B_cond_label("^reg^", target)"]) registers @
 [source;"RET";"ret";" RET ";"RET\n";"B_label(a,b)";"BL(a\nb)";"BL(a\rb)";"MOVZ(X0,\u{2000}1,\u{00a0}2)";"ADD_reg(X0,, X1)";"ADD_reg(X0, X1,X2,X3)";"MOVZ(X0, 0, 0)tail"] in
 let parses=list (fun line -> tuple [str line;list (fun n -> result J.armInstr (ARM64Parser.parseInstruction n line)) [0;1;-1;Int32.to_int Int32.max_int];result (list J.armInstr) (ARM64Parser.parseARM64 line);result (list J.armInstr) (ARM64Parser.parseARM64ForEncodingError line)]) cases in
 let formats=[source;"";"---INPUT-ARM64---\nRET\n";"---INPUT-ARM64---\nRET\n---OUTPUT-HEX---\n0xD65F03C0\n";"---INPUT-ARM64---\nADD_imm(X0, X1, 4096)\n---EXPECT-ERROR---\nimm12\n";"---INPUT-ARM64---\nRET\n---OUTPUT-HEX---\n0xD65F03C0\n---EXPECT-ERROR---\nerror\n"]@List.map (fun value -> "---INPUT-ARM64---\nRET\n---OUTPUT-HEX---\n0xD65F03C0\n---ASSERT-DIFFERENT---\n"^value) ["true";"FALSE";"maybe";""] in
 let paths=Sys.readdir "src/Tests/passes/arm64enc" |> Array.to_list |> List.sort StringOrder.compare |> List.filter (fun p -> Filename.check_suffix p ".arm64enc") in
 let fixtures=list (fun path -> let content=TestFileIO.readAllText (Filename.concat "src/Tests/passes/arm64enc" path) in tuple [str path;result fixture (ARM64EncodingFormat.parseARM64EncodingTest content)]) paths in
 let hexes=list (fun value -> result word (ARM64EncodingFormat.parseHexValue value)) ["";"0x";"0X0";"0xFFFFFFFF";"0x100000000";"0x00000000000000000001";"0x-1";"0x 1";" 0xd65F03c0 ";"0xé";source] in
 tuple [list (fun value -> result J.armReg (ARM64Parser.parseReg value)) registers;list (fun value -> result (fun cond -> let instr=ARM64.B_cond_label (cond,"") in J.armInstr instr) (ARM64Parser.parseCond value)) conditions;parses;list (fun content -> result fixture (ARM64EncodingFormat.parseARM64EncodingTest content)) formats;fixtures;hexes]
let run () =
 let rec loop ()=match input_line stdin with
 | line -> let request=Yojson.Basic.from_string line in let source=Yojson.Basic.Util.(request |> member "source" |> to_string) in print_endline (Yojson.Basic.to_string (`Assoc ["schema",`Int 1;"stage",`String "arm64-dsl";"value",observe source]));loop ()
 | exception End_of_file -> () in loop ()
