(*
   ChordalGraphTests.fs - Integration tests for register-allocation graph construction.
   Table-driven graph coloring and MCS properties live in .graphcolor fixtures.
   These tests retain CFG/liveness construction assertions that require direct compiler types.
*)
(* CFG integration assertions for integer interference, coloring and coalescing. *)
[@@@warning "-4-42"]
open Dark_compiler
open LIR
module Set=MemoryModel.IntSet
type testResult=(unit,string) result
let colorOf=AllocationModel.colorOf
let graphNeighbors graph id=Set.of_list (AllocationModel.graphNeighbors graph id)
let graphHasVertex=AllocationModel.graphHasVertex
let showSet values=HostStructuralFormat.format (StructuralValue.Union ("set",[StructuralValue.Sequence (List.map (fun n->StructuralValue.Scalar (string_of_int n)) (Set.elements values))]))
let cfg entry blocks={entry;blocks=LabelMap.of_list (List.map (fun (block:basicBlock)->block.label,block) blocks)}
(*
   Test 16: Build interference graph from real LIR CFG
   Simulates `fun a b -> a * b`, where parameters should interfere.
   Create a minimal CFG for: fun a b -> a * b
   Virtual 0 = a (parameter)
   Virtual 1 = b (parameter)
   Virtual 2 = a * b (result)
   v2 = v0 * v1
   X0 = v2 (return value)
   Build interference graph
   v0 and v1 should both be in the graph
   v0 and v1 should interfere (they're both live at Mul instruction)
*)
let testBuildFromCFG ()=
 let v0=Virtual 0 and v1=Virtual 1 and v2=Virtual 2 in
 let entryLabel=Label "entry" in
 let entryBlock={label=entryLabel;instrs=[Mul (v2,v0,v1);Mov (Physical X0,Reg v2)];terminator=Ret} in
 let graph=RegisterInterference.buildInterferenceGraphBitset (cfg entryLabel [entryBlock]) [0;1] in
 if not (graphHasVertex graph 0) then Error "v0 should be in interference graph"
 else if not (graphHasVertex graph 1) then Error "v1 should be in interference graph"
 else if not (graphHasVertex graph 2) then Error "v2 should be in interference graph"
 else let v0Neighbors=graphNeighbors graph 0 and v1Neighbors=graphNeighbors graph 1 in
 if Set.mem 1 v0Neighbors && Set.mem 0 v1Neighbors then Ok ()
 else Error ("v0 and v1 should interfere. v0 neighbors: "^showSet v0Neighbors^", v1 neighbors: "^showSet v1Neighbors)
(*
   Test 16b: Bitset-based interference graph matches expected edges
*)
let testBuildFromCFGBitsetMatches ()=
 let v0=Virtual 0 and v1=Virtual 1 and v2=Virtual 2 and v3=Virtual 3 and v4=Virtual 4 in
 let labelA=Label "A" and labelB=Label "B" and labelC=Label "C" and labelD=Label "D" in
 let blockA={label=labelA;instrs=[Mov (v0,Imm 1L)];terminator=Branch (v0,labelB,labelC)} in
 let blockB={label=labelB;instrs=[Mov (v1,Imm 2L)];terminator=Jump labelD} in
 let blockC={label=labelC;instrs=[Mov (v2,Imm 3L)];terminator=Jump labelD} in
 let blockD={label=labelD;instrs=[Phi (v3,[Reg v1,labelB;Reg v2,labelC],None);Add (v4,v3,Imm 1L);Mov (Physical X0,Reg v4)];terminator=Ret} in
 let graph=RegisterInterference.buildInterferenceGraphBitset (cfg labelA [blockA;blockB;blockC;blockD]) [0] in
 let expectedVertices=Set.of_list [0;1;2;3;4] in
 let neighbors=graphNeighbors graph in
 if Set.exists (fun v->not (graphHasVertex graph v)) expectedVertices then Error ("Bitset graph vertices differ. Expected: "^showSet expectedVertices)
 else if Set.mem 2 (neighbors 1) || Set.mem 1 (neighbors 2) then Error ("v1 and v2 should not interfere across diamond branches. v1 neighbors: "^showSet (neighbors 1)^", v2 neighbors: "^showSet (neighbors 2))
 else if Set.mem 1 (neighbors 3) || Set.mem 2 (neighbors 3) then Error ("Phi dest should not interfere with its operands. v3 neighbors: "^showSet (neighbors 3)) else Ok ()
(*
   Test 17: Full pipeline - CFG to allocation using chordal graph coloring
   Verify that interfering parameters get different register colors
   Same CFG as testBuildFromCFG
   Build interference graph and run chordal coloring
   16 available colors
   v0 and v1 must have different colors (they interfere)
*)
let testFullChordalPipeline ()=
 let v0=Virtual 0 and v1=Virtual 1 and v2=Virtual 2 in
 let entryLabel=Label "entry" in
 let entryBlock={label=entryLabel;instrs=[Mul (v2,v0,v1);Mov (Physical X0,Reg v2)];terminator=Ret} in
 let graph=RegisterInterference.buildInterferenceGraphBitset (cfg entryLabel [entryBlock]) [0;1] in
 let colorResult=RegisterColoring.chordalGraphColor graph [] 16 [] [] in
 match colorOf colorResult 0,colorOf colorResult 1 with
 |Some c0,Some c1->if c0<>c1 then Ok () else Error (Printf.sprintf "v0 and v1 should have different colors, both got color %d" c0)
 |_->Error "v0 or v1 not found in coloring."
(*
   Test 18: Simulates apply2(f, a, b) = f(a, b) pattern
   f, a, b are all used in the ClosureCall - they should all interfere
   Simulate: def apply2(f, a, b) = f(a, b)
   Virtual 0 = f (function parameter)
   Virtual 1 = a (first int parameter)
   Virtual 2 = b (second int parameter)
   Virtual 3 = result of f(a, b)
   f
   a
   b
   result
   ClosureCall(result, closure, args)
   Build interference graph
   All of v0, v1, v2 should be in the graph and interfere with each other
   Check all pairs interfere
   Now check that coloring assigns different colors
*)
let testApply2Pattern ()=
 let v0=Virtual 0 and v1=Virtual 1 and v2=Virtual 2 and v3=Virtual 3 in
 let entryLabel=Label "entry" in
 let entryBlock={label=entryLabel;instrs=[ClosureCall (v3,v0,[Reg v1;Reg v2]);Mov (Physical X0,Reg v3)];terminator=Ret} in
 let graph=RegisterInterference.buildInterferenceGraphBitset (cfg entryLabel [entryBlock]) [0;1;2] in
 let v0Neighbors=graphNeighbors graph 0 and v1Neighbors=graphNeighbors graph 1 and v2Neighbors=graphNeighbors graph 2 in
 if not (graphHasVertex graph 0) then Error "v0 not in graph."
 else if not (graphHasVertex graph 1) then Error "v1 not in graph."
 else if not (graphHasVertex graph 2) then Error "v2 not in graph."
 else if not (Set.mem 1 v0Neighbors && Set.mem 2 v0Neighbors) then Error ("v0 should interfere with v1 and v2. v0 neighbors: "^showSet v0Neighbors)
 else if not (Set.mem 0 v1Neighbors && Set.mem 2 v1Neighbors) then Error ("v1 should interfere with v0 and v2. v1 neighbors: "^showSet v1Neighbors)
 else if not (Set.mem 0 v2Neighbors && Set.mem 1 v2Neighbors) then Error ("v2 should interfere with v0 and v1. v2 neighbors: "^showSet v2Neighbors)
 else let colorResult=RegisterColoring.chordalGraphColor graph [] 16 [] [] in
 match colorOf colorResult 1,colorOf colorResult 2 with
 |Some c1,Some c2->if c1<>c2 then Ok () else Error (Printf.sprintf "v1 and v2 got same color %d." c1)
 |_->Error "v1 or v2 not found in coloring."
(*
   Test 19: Copy move should create coalescing pairs
*)
let testMoveCoalescingPreference ()=
 let v0=Virtual 0 and v1=Virtual 1 in
 let entryLabel=Label "entry" in
 let entryBlock={label=entryLabel;instrs=[Mov (v1,Reg v0);Mov (Physical X0,Reg v1)];terminator=Ret} in
 let graph=cfg entryLabel [entryBlock] in
 let blocks=LabelMap.bindings graph.blocks |> List.map snd |> Array.of_list in
 let pairs=RegisterCoalescing.collectMovePairs blocks in
 let normalize (a,b)=if a<b then a,b else b,a in
 if List.mem (0,1) (List.map normalize pairs) then Ok () else
 let formatPair (a,b)=StructuralValue.Tuple [StructuralValue.Scalar (string_of_int a);StructuralValue.Scalar (string_of_int b)] in
 Error ("Expected move pair (0, 1), got "^HostStructuralFormat.format (StructuralValue.Sequence (List.map formatPair pairs)))
let tests=["Build from real CFG",testBuildFromCFG;"Bitset graph matches",testBuildFromCFGBitsetMatches;"Full chordal pipeline",testFullChordalPipeline;"Apply2 pattern",testApply2Pattern;"Move coalescing pairs",testMoveCoalescingPreference]
let runAllTests ()=List.map (fun (name,run)->name,run ()) tests
