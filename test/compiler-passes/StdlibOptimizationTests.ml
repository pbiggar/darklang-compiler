[@@@warning "-4-42"]
(* StdlibOptimizationTests.ml - Check optimization in prebuilt stdlib output. *)
open Dark_compiler
type testResult=(unit,string) result
let testStdlibPowerMask (stdlib:CompilationContexts.stdlibResult) ()=
 match List.find_opt (fun (func:LIR.functionDef)->func.LIR.name="Darklang.Stdlib.Int64.__powerLoop") stdlib.CompilationContexts.allocatedFunctions with
 |None->Error "Missing prebuilt Stdlib.Int64.__powerLoop"
 |Some func->let instructions=LIR.LabelMap.bindings func.LIR.cfg.LIR.blocks |> List.concat_map (fun (_,block)->block.LIR.instrs) in
 if List.exists (function LIR.And_imm (_,_,1L)->true|_->false) instructions then Ok () else Error "Expected an exponent bit mask in prebuilt Stdlib.Int64.__powerLoop"
let tests stdlib=["prebuilt stdlib power uses a bit mask",testStdlibPowerMask stdlib]
