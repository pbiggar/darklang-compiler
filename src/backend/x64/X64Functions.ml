[@@@warning "-42"]

(* X64Functions.ml - Lower allocated LIR functions with target frame and return conventions. *)
open X64Operands
open X64Printing
open X64Frames
open X64CodeGenTypes
open X64Blocks

(* Translate a LIR function to x86-64 instructions. *)
let translateFunction enableLeakCheck recordRegistry sumShapeRegistry
    functionNames (func : LIR.functionDef) =
  let epilogueLabel = "_epilogue_" ^ func.LIR.name in
  let prologue = genPrologue func.LIR.stackSize func.LIR.usedCalleeSaved in
  (* Float parameter setup: the register allocator inserts FMov instructions
    at the start of the entry block (e.g., "D1 <- FMov(D0)"). These are
    handled by the FMov case in translateInstr. No extra codegen needed. *)
  let translateBlocks blocks =
    let ctx =
      {
        functionName = func.LIR.name;
        stackSize = func.LIR.stackSize;
        usedCalleeSaved = func.LIR.usedCalleeSaved;
        enableLeakCheck;
        recordRegistry;
        sumShapeRegistry;
        functionNames;
      }
    in
    let rec loop acc blocks =
      match blocks with
      | [] -> Ok (List.rev acc |> List.concat)
      | block :: rest -> (
          let nextBlock =
            match rest with [] -> None | next :: _ -> Some next
          in
          match translateBlock ctx epilogueLabel nextBlock block with
          | Error e -> Error e
          | Ok instrs -> loop (instrs :: acc) rest)
    in
    loop [] blocks
  in
  (* Layout blocks into deterministic fallthrough chains before translation. *)
  let allBlocksResult =
    LIR.layoutBlocks func.LIR.cfg
    |> Result.map_error (fun e ->
        "x64 codegen: function " ^ func.LIR.name ^ ": " ^ e)
  in
  match Result.bind allBlocksResult translateBlocks with
  | Error e -> Error e
  | Ok blockInstrs ->
      (* Heap initialization for _start only. *)
      let heapInit = if func.LIR.name = "_start" then genHeapInit () else [] in
      let funcLabel = [ X86_64.Label func.LIR.name ] in
      (* Generate leak check report for _start exit. *)
      let leakReport =
        if func.LIR.name = "_start" && enableLeakCheck then
          genLeakCheckReport ()
        else []
      in
      let epilogue =
        [ X86_64.Label epilogueLabel ]
        @ genEpilogue func.LIR.stackSize func.LIR.usedCalleeSaved
        @
        if func.LIR.name = "_start" then
          (* _start: report leaks then exit(0). *)
          leakReport @ loadImm64 X86_64.RDI 0L @ genExitSyscall
        else [ X86_64.RET ]
      in
      (* The root frame terminates the normal frame-pointer chain. This lets
     CLI helpers recover process arguments from a known stack-relative
     address without reserving R13 for the lifetime of the program. *)
      let rootFrameInit =
        if func.LIR.name = "_start" then
          [ X86_64.XOR_reg (X86_64.RBP, X86_64.RBP) ]
        else []
      in
      Ok
        (funcLabel @ rootFrameInit @ prologue @ heapInit @ blockInstrs
       @ epilogue)
