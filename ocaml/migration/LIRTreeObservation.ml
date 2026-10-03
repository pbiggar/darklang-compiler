(* Complete LIR call-edge, tree-shaking and target register-policy observations. *)
[@@@warning "-4"]
open Dark_compiler
module L = LIR
module FS = SpecializationIdentity.FunctionSet
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let fid value = SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind",`String "uint64";"value",`String (Printf.sprintf "%Lu" (AST.functionIdValue value))]]
let set values = `Assoc ["set",list fid (FS.elements values)]
let callGraph graph = `Assoc ["map",list (fun (key,value) -> tuple [fid key;set value]) (FunctionIdMap.toList graph)]
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let makeFunction id name instructions : L.functionDef =
 let label = L.Label name in
 let block : L.basicBlock = {L.label;instrs=instructions;terminator=L.Ret} in
 {L.id;name;typedParams=[];cfg={L.entry=label;blocks=L.LabelMap.singleton label block};stackSize=0;usedCalleeSaved=[];codegenFacts=None}
let observe source =
 let types = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TBool;AST.TString;AST.TChar;AST.TFloat64;AST.TUnit;AST.TTuple [AST.TInt64];AST.TList AST.TString;AST.TRecord (source,[])] in
 let names = List.filter_map ListDisplay.getDisplayStringFunc types in
 let ids = StringOrder.Map.of_list (List.mapi (fun i name -> name,AST.functionId (Int64.of_int (i+10))) names) in
 let extra = List.map (fun typ -> L.PrintSum (L.Virtual 0,["C",0,Some (AST.TList typ);"D",1,Some AST.TInt64;"E",2,None],false)) types in
 let operands = [L.FuncAddr (AST.functionId 1L);L.FuncAddr (AST.functionId (-1L));L.Imm 0L;L.StringSymbol source;L.Reg (L.Virtual 3)] in
 let instructionCases = List.concat_map (fun operand -> List.concat_map (fun instruction -> List.map (fun ids ->
  let func = makeFunction (AST.functionId 0L) source [instruction] in
  let blocks = Array.of_list (List.map snd (L.LabelMap.bindings func.L.cfg.L.blocks)) in
  tuple [ProductionLIR.instr instruction;attempt set (fun () -> DeadCodeElimination.getCalledFunctions ids func);
   `Bool (DeadCodeElimination.requiresListDisplayHelpers func);`Bool (RegisterPolicy.isNonTailCall instruction);`Bool (RegisterPolicy.hasNonTailCalls blocks);
   list (fun arch -> tuple [list ProductionLIR.physReg (RegisterPolicy.calleeSavedRegsFor arch);list ProductionLIR.physReg (RegisterPolicy.getAllocatableRegs arch blocks)]) [Platform.ARM64;Platform.X86_64]]) [StringOrder.Map.empty;ids]) (LIRFixtures.instructionsWithOperand source operand @ extra)) operands in
 let graphCases = List.map (fun variant ->
  let f n name instructions = makeFunction (AST.functionId (Int64.of_int n)) name instructions in
  let users = [f 0 "main" [L.Call (L.Virtual 1,AST.functionId 1L,[]);L.Mov (L.Virtual 2,L.FuncAddr (AST.functionId 3L))];f 1 "helper" [L.TailCall (AST.functionId (if variant mod 2=0 then 0L else 4L),[])];f 2 (if variant=3 then "main" else "unused") [L.LoadFuncAddr (L.Virtual 1,AST.functionId 5L)]] in
  let stdlib = [f 3 "s3" [L.TailCall (AST.functionId 4L,[])];f 4 "s4" [L.TailCall (AST.functionId (if variant mod 3=0 then 3L else 99L),[])];f 5 "s5" [];makeFunction (AST.functionId (-1L)) "max" [L.ClosureAlloc (L.Virtual 1,AST.functionId 3L,[])]] in
  let all = users @ stdlib in
  let names = StringOrder.Map.of_list (List.map (fun (func:L.functionDef) -> func.L.name,func.L.id) all) in
  let userGraph = DeadCodeElimination.buildCallGraph names users in
  let stdlibGraph = DeadCodeElimination.buildCallGraph names stdlib in
  let incompleteGraph = if variant mod 2=0 then FunctionIdMap.remove (AST.functionId 1L) userGraph else userGraph in
  let roots = FS.of_list [AST.functionId 0L;AST.functionId (-1L);AST.functionId 77L] in
  let anfFunc : ANF.functionDef = {ANF.id=AST.functionId 1L;name="helper";typedParams=[];returnType=AST.TUnit;returnOwnership=ANF.OwnedReturn;body=ANF.Let (ANF.TempId 1,ANF.Call (AST.functionId 3L,[]),ANF.Return ANF.UnitLiteral)} in
  let anf = ANF.Program ([anfFunc],ANF.Let (ANF.TempId 2,ANF.ClosureAlloc (AST.functionId (if variant mod 2=0 then 1L else 5L),[]),ANF.Return ANF.UnitLiteral)) in
  tuple [callGraph userGraph;callGraph stdlibGraph;set (DeadCodeElimination.findReachable (FunctionIdMap.merge userGraph stdlibGraph) roots);
   set (DeadCodeElimination.directCallsFromFunctions incompleteGraph users);
   list (fun entry -> tuple [attempt (list ProductionLIR.functionDef) (fun () -> FunctionTreeShaking.filterUserFunctionsWithCallGraph entry incompleteGraph users);attempt (list ProductionLIR.functionDef) (fun () -> FunctionTreeShaking.filterUserFunctions entry users)]) [None;Some "main";Some "helper";Some "unused";Some "missing"];
   list ProductionLIR.functionDef (DeadCodeElimination.filterFunctionsWithUserCallGraph stdlibGraph incompleteGraph users stdlib);
   list ProductionLIR.functionDef (DeadCodeElimination.filterFunctions stdlibGraph names users stdlib);
   list ProductionLIR.functionDef (FunctionTreeShaking.filterStdlibFunctionsWithUserCallGraph stdlibGraph incompleteGraph users stdlib);
   list ProductionLIR.functionDef (FunctionTreeShaking.filterStdlibFunctions stdlibGraph users stdlib);
   attempt set (fun () -> FunctionTreeShaking.getReachableStdlibNames stdlibGraph anf)]) [0;1;2;3;4;5] in
 tuple [`List instructionCases;`List graphCases;list ProductionLIR.physReg RegisterPolicy.callerSavedRegs;
  list (fun arch -> list ProductionLIR.physReg (RegisterPolicy.getAllocatableRegs arch [||])) [Platform.ARM64;Platform.X86_64]]
