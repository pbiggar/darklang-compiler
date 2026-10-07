[@@@warning "-4-42"]
(* IRPrinterTests.ml - Unit tests for shared IR formatting
   Ensures IRPrinter outputs match pinned formatting for MIR/LIR programs. *)
open Dark_compiler
module M=MIR
type testResult=(unit,string) result
let expectFormatted label expected actual=if actual=expected then Ok () else Error (label^" did not match.\nExpected:\n"^expected^"\nActual:\n"^actual)
let testFormatMIR ()=
 let entry=M.Label "entry" in let exit=M.Label "exit" in
 let entryBlock:M.basicBlock={M.label=entry;instrs=[M.Mov (M.VReg 0,M.Int64Const 1L,Some AST.TInt64)];terminator=M.Jump exit} in
 let exitBlock:M.basicBlock={M.label=exit;instrs=[M.Mov (M.VReg 1,M.Register (M.VReg 0),Some AST.TInt64)];terminator=M.Ret (M.Register (M.VReg 1))} in
 let cfg:M.cfg={M.entry;blocks=M.LabelMap.of_list [entry,entryBlock;exit,exitBlock]} in
 let func:M.functionDef={M.id=TestIds.functionIdForName "cfg_pretty";name="cfg_pretty";typedParams=[];returnType=AST.TInt64;cfg;floatRegs=M.IntSet.empty} in
 let program=M.Program ([func],StringOrder.Map.empty,StringOrder.Map.empty) in
 let expected=String.concat "\n" ["Function cfg_pretty:";"  entry:";"    v0 <- 1 : TInt64";"    jump exit";"  exit:";"    v1 <- v0 : TInt64";"    ret v1"] in
 expectFormatted "formatMIR" expected (MIRPrinter.formatMIR program)
let emptyMIRFunction name=
 let entry=M.Label (name^"_entry") in
 {M.id=TestIds.functionIdForName name;name;typedParams=[];returnType=AST.TUnit;cfg={M.entry;blocks=M.LabelMap.singleton entry {M.label=entry;instrs=[];terminator=M.Ret (M.Int64Const 0L)}};floatRegs=M.IntSet.empty}
let testFormatMIRDumpFiltersBeforeFormatting ()=
 let program=M.Program ([emptyMIRFunction "Darklang.Stdlib.List.map";emptyMIRFunction "Darklang.Stdlib.List.filter"],StringOrder.Map.empty,StringOrder.Map.empty) in
 let actual=MIRPrinter.formatMIRDump (Some "MAP") false program in
 if Text.contains actual "Darklang.Stdlib.List.map" && not (Text.contains actual "Darklang.Stdlib.List.filter") then Ok () else Error ("Expected case-insensitive function-scoped MIR output, got:\n"^actual)
let testFormatMIRDumpSummary ()=
 let program=M.Program ([emptyMIRFunction "Darklang.Stdlib.List.map";emptyMIRFunction "Darklang.Stdlib.List.filter"],StringOrder.Map.empty,StringOrder.Map.empty) in
 expectFormatted "formatMIRDump summary" "Functions: 1\nDarklang.Stdlib.List.map: 1 blocks, 0 instructions" (MIRPrinter.formatMIRDump (Some "map") true program)
let testFormatMIRDumpReportsNoMatches ()=
 let program=M.Program ([emptyMIRFunction "Darklang.Stdlib.List.map"],StringOrder.Map.empty,StringOrder.Map.empty) in
 expectFormatted "formatMIRDump no matches" "No functions matched 'missing'." (MIRPrinter.formatMIRDump (Some "missing") false program)
let tests=["format MIR",testFormatMIR;"filter MIR dump functions before formatting",testFormatMIRDumpFiltersBeforeFormatting;"summarize scoped MIR dumps",testFormatMIRDumpSummary;"report empty MIR dump scopes",testFormatMIRDumpReportsNoMatches]
let runAll ()=
 let rec run=function []->Ok ()|(name,test)::rest->match test () with Ok ()->run rest|Error message->Error (name^" test failed: "^message) in run tests
