(* LIRLayoutTests.fs - Tests deterministic CFG block layout for backend fallthrough. *)
[@@@warning "-4-42"]
open Dark_compiler
open LIR
type testResult = (unit, string) result
let labelsText labels = HostStructuralFormat.format (HostStructuralFormat.Sequence (List.map (fun (LIR.Label name) -> HostStructuralFormat.Union ("Label",[HostStructuralFormat.Text name])) labels))
let branchFixture () : LIR.cfg =
 let entry = LIR.Label "entry" in
 let trueBlock = LIR.Label "a_true" in
 let falseBlock = LIR.Label "z_false" in
 let join = LIR.Label "join" in
 let block label terminator : LIR.basicBlock = {label;instrs=[];terminator} in
 {entry;blocks=LIR.LabelMap.of_list [entry,block entry (LIR.Branch (LIR.Physical LIR.X0,trueBlock,falseBlock));trueBlock,block trueBlock (LIR.Jump join);falseBlock,block falseBlock (LIR.Jump join);join,block join LIR.Ret]}
let preserved (cfg : LIR.cfg) blocks = List.length blocks = LIR.LabelMap.cardinal cfg.LIR.blocks && LIR.LabelMap.equal (=) (LIR.LabelMap.of_list (List.map (fun (block : LIR.basicBlock) -> block.LIR.label,block) blocks)) cfg.LIR.blocks
let testLayoutDefersSharedReturn () =
 let cfg = branchFixture () in
 match LIR.layoutBlocks cfg with
 | Error e -> Error e
 | Ok blocks ->
  let labels = List.map (fun (block : LIR.basicBlock) -> block.LIR.label) blocks in
  let expected = [LIR.Label "entry";LIR.Label "z_false";LIR.Label "a_true";LIR.Label "join"] in
  if not (preserved cfg blocks) then Error "Layout changed or duplicated CFG blocks"
  else if labels = expected then Ok ()
  else Error ("Expected deterministic fallthrough layout " ^ labelsText expected ^ ", got " ^ labelsText labels)
let testLayoutPreservesOtherReturnShapes () =
 let entry = LIR.Label "entry" in
 let middle = LIR.Label "z_middle" in
 let last = LIR.Label "a_last" in
 let block label terminator : LIR.basicBlock = {label;instrs=[LIR.Mov (LIR.Physical LIR.X0,LIR.Imm 7L)];terminator} in
 let branch yes no = LIR.Branch (LIR.Physical LIR.X0,yes,no) in
 let fixtures = [
  "single entry return",[entry,LIR.Ret],[entry];
  "entry is shared return",[entry,LIR.Ret;middle,LIR.Jump entry;last,LIR.Jump entry],[entry;last;middle];
  "multiple returns",[entry,branch last middle;middle,LIR.Ret;last,LIR.Ret],[entry;middle;last];
  "no return loop",[entry,LIR.Jump middle;middle,LIR.Jump entry;last,LIR.Jump middle],[entry;middle;last];
  "unshared return chain",[entry,LIR.Jump middle;middle,LIR.Jump last;last,LIR.Ret],[entry;middle;last];
  "duplicate edges are one predecessor",[entry,branch middle middle;middle,LIR.Ret;last,LIR.Jump entry],[entry;middle;last]] in
 List.fold_left (fun result (name,terminators,expected) -> Result.bind result (fun () ->
  let cfg : LIR.cfg = {entry;blocks=LIR.LabelMap.of_list (List.map (fun (label,terminator) -> label,block label terminator) terminators)} in
  Result.bind (LIR.layoutBlocks cfg) (fun blocks ->
   let labels = List.map (fun (block : LIR.basicBlock) -> block.LIR.label) blocks in
   if not (preserved cfg blocks) then Error (name ^ ": layout changed or duplicated CFG blocks")
   else if labels = expected then Ok ()
   else Error (name ^ ": expected " ^ labelsText expected ^ ", got " ^ labelsText labels)))) (Ok ()) fixtures
let testLayoutLeavesMissingSuccessorValidationToConsumers () =
 let entry = LIR.Label "entry" in
 let missing = LIR.Label "missing" in
 let entryBlock : LIR.basicBlock = {label=entry;instrs=[];terminator=LIR.Jump missing} in
 let cfg : LIR.cfg = {entry;blocks=LIR.LabelMap.singleton entry entryBlock} in
 match LIR.layoutBlocks cfg with
 | Ok [block] when block.LIR.label = entry -> Ok ()
 | Ok blocks -> Error ("Expected only the entry block, got " ^ labelsText (List.map (fun (block : LIR.basicBlock) -> block.LIR.label) blocks))
 | Error e -> Error ("Layout preempted consumer validation of a missing successor: " ^ e)
let testLayoutReportsMissingEntryBlock () =
 let entry = LIR.Label "entry" in
 let cfg : LIR.cfg = {entry;blocks=LIR.LabelMap.empty} in
 match LIR.layoutBlocks cfg with
 | Error e when HostText.contains e "missing entry block" -> Ok ()
 | Error e -> Error ("Expected missing entry block error, got '" ^ e ^ "'")
 | Ok _ -> Error "Expected layout to reject a missing entry block"
let tests = [
 "LIR layout defers shared return",testLayoutDefersSharedReturn;
 "LIR layout preserves other return shapes",testLayoutPreservesOtherReturnShapes;
 "LIR layout leaves missing successor validation to consumers",testLayoutLeavesMissingSuccessorValidationToConsumers;
 "LIR layout reports missing entry block",testLayoutReportsMissingEntryBlock]
