(*
   OptimizationTestRunner.ml - Test runner for optimization verification
   Compiles source code, captures IR at specific stages, and compares
   against expected output to verify optimizations work correctly.
   Result of running an optimization test
   Normalize IR output for comparison
   - Trim whitespace
   - Normalize line endings
   - Remove trailing whitespace from each line
   - Alpha-rename temporary IDs, which are allocation-order details shared
   with unrelated functions in the compiled standard library.
   - Alpha-rename generated closure IDs for the same reason.
   Compile source and get ANF after optimization
   Type check
   Convert to ANF
   Optimize ANF
   Pretty-print the result
   Compile source and get MIR after optimization
   Type check
   Convert to ANF
   Optimize ANF
   Generated output participates in reference-count insertion.
   Convert to MIR
   SSA construction
   MIR optimization
   SSA form is now preserved (phi resolution happens in register allocation)
   Pretty-print the optimized MIR (still in SSA form)
   Compile source and get LIR after optimization
   Type check
   Convert to ANF
   Optimize ANF
   Generated output participates in reference-count insertion.
   Convert to MIR
   SSA construction and optimization
   SSA form is now preserved (phi resolution happens in register allocation)
   Convert to LIR
   LIR optimization
   Pretty-print
   Run a single optimization test
   Load and run tests from a file
*)
[@@@warning "-4-42"]

open Dark_compiler
open OptimizationFormat
module M = StringOrder.Map

let ( let* ) = Result.bind

let measure recorder name operation =
  let start = Mtime_clock.elapsed_ns () in
  let result = operation () in
  let elapsed = Int64.sub (Mtime_clock.elapsed_ns ()) start in
  Option.iter
    (fun record -> record { CompilerOptions.pass = name; elapsed })
    recorder;
  result

type optimizationTestResult = TestOutcome.t = {
  success : bool;
  message : string;
  expected : string option;
  actual : string option;
}

let externalReturnTypes =
  List.map
    (fun (name, typ) -> (TestIds.functionIdForName name, (name, typ)))
    [
      ("__hash_i64", AST.TInt64);
      ("__hash_str", AST.TInt64);
      ("__hash_bool", AST.TInt64);
      ("__key_eq_i64", AST.TBool);
      ("__key_eq_str", AST.TBool);
      ("__key_eq_bool", AST.TBool);
      ("__string_hash", AST.TInt64);
    ]
  |> FunctionIdMap.ofList

let externalFunctionNames =
  FunctionIdMap.map (fun _ (name, _) -> name) externalReturnTypes

let returnTypesFor (stdlib : CompilationContexts.stdlibResult) =
  FunctionIdMap.fold
    (fun types id value -> FunctionIdMap.add id value types)
    stdlib.CompilationContexts.context.CompilationContexts.returnTypes
    externalReturnTypes

let typeCheckWithStdlib (stdlib : CompilationContexts.stdlibResult) source =
  WrittenChecking.checkSourceUnitsWithBase
    stdlib.CompilationContexts.context.CompilationContexts.writtenEnvironment
    true false [ source ]
  |> Result.map_error (fun error -> "Type error: " ^ error)
  |> Result.map (fun (typ, program, _) -> (typ, program))

let parseOptimizationSource source =
  let* parsed =
    WrittenParsing.parse Validation.Script source
    |> Result.map_error (fun error -> "Parse error: " ^ error)
  in
  if
    (Validation.ValidatedSourceFile.toWrittenTypes parsed)
      .WrittenTypes.exprsToEval <> []
  then Ok (parsed, false)
  else
    WrittenParsing.parse Validation.Script (source ^ "\n\n0L")
    |> Result.map_error (fun error -> "Parse error: " ^ error)
    |> Result.map (fun parsed -> (parsed, true))

let convertTypedProgram (stdlib : CompilationContexts.stdlibResult) recorder
    typedAst =
  SourcePreparation.convertTypedProgramToUserOnlyWithTrace
    stdlib.CompilationContexts.context recorder typedAst
  |> Result.map (fun (c : AST_to_ANF.userOnlyResult) ->
      {
        AST_to_ANF.program =
          ANF.Program (c.AST_to_ANF.userFunctions, c.AST_to_ANF.mainExpr);
        ownershipContracts = c.AST_to_ANF.ownershipContracts;
        recursiveMembers = c.AST_to_ANF.recursiveMembers;
        typeReg = c.AST_to_ANF.typeReg;
        recordFieldsReg = c.AST_to_ANF.recordFieldsReg;
        recordTypeParamsReg = c.AST_to_ANF.recordTypeParamsReg;
        variantLookup = c.AST_to_ANF.variantLookup;
        rcSumShapeReg = c.AST_to_ANF.rcSumShapeReg;
        funcReg = c.AST_to_ANF.funcReg;
        funcParams = c.AST_to_ANF.funcParams;
        moduleRegistry = c.AST_to_ANF.moduleRegistry;
      })

let optimizeContextFromConversionResult (c : AST_to_ANF.conversionResult) =
  {
    ANFConstants.typeReg = c.AST_to_ANF.recordFieldsReg;
    recordTypeParams = c.AST_to_ANF.recordTypeParamsReg;
    sumShapeReg = c.AST_to_ANF.rcSumShapeReg;
    functionNames =
      FunctionIdMap.map (fun _ (name, _) -> name) c.AST_to_ANF.funcReg;
    functionIds =
      FunctionIdMap.toList c.AST_to_ANF.funcReg
      |> List.map (fun (id, (name, _)) -> (name, id))
      |> M.of_list;
  }

let normalizeIR ir =
  let units = Text.scalars ir in
  let white u =
    Uchar.is_valid u && Uucp.White.is_white_space (Uchar.of_int u)
  in
  let lines = ref [] and start = ref 0 in
  let flush finish =
    let last = ref finish in
    while !last > !start && white units.(!last - 1) do
      decr last
    done;
    if !last > !start then
      lines :=
        Text.ofScalars (Array.sub units !start (!last - !start)) :: !lines
  in
  Array.iteri
    (fun index u ->
      if u = 10 || u = 13 then (
        flush index;
        start := index + 1))
    units;
  flush (Array.length units);
  let normalized = Text.scalars (String.concat "\n" (List.rev !lines)) in
  let size = Array.length normalized in
  let asciiWord u =
    (u >= 65 && u <= 90)
    || (u >= 97 && u <= 122)
    || (u >= 48 && u <= 57)
    || u = 95
  in
  let boundaryWord u =
    if u = 0x200c || u = 0x200d then true
    else if not (Uchar.is_valid u) then false
    else
      let ch = Uchar.of_int u in
      match Uucp.Age.age ch with
      | `Version version when version <= (16, 0) -> (
          match Uucp.Gc.general_category ch with
          | `Ll | `Lu | `Lt | `Lo | `Lm | `Mn | `Nd | `Pc -> true
          | _ -> false)
      | _ -> false
  in
  let starts arr index text =
    let ascii = String.length text in
    index + ascii <= Array.length arr
    &&
    let rec loop i =
      i = ascii || (arr.(index + i) = Char.code text.[i] && loop (i + 1))
    in
    loop 0
  in
  let quotedEnd index =
    let rec loop pos =
      if pos >= size then None
      else if normalized.(pos) = 34 then Some (pos + 1)
      else if normalized.(pos) = 92 then
        if pos + 1 < size && normalized.(pos + 1) <> 10 then loop (pos + 2)
        else None
      else loop (pos + 1)
    in
    loop (index + 1)
  in
  let output = Buffer.create (String.length ir)
  and ids = ref M.empty
  and next = ref 0 in
  let appendSlice arr pos length =
    Buffer.add_string output (Text.ofScalars (Array.sub arr pos length))
  in
  let canonical key =
    match M.find_opt key !ids with
    | Some id -> id
    | None ->
        let id = !next in
        incr next;
        ids := M.add key id !ids;
        id
  in
  let rec temps pos =
    if pos < size then
      match if normalized.(pos) = 34 then quotedEnd pos else None with
      | Some finish ->
          appendSlice normalized pos (finish - pos);
          temps finish
      | None ->
          let prefix =
            if
              (pos = 0 || not (asciiWord normalized.(pos - 1)))
              && starts normalized pos "TempId "
            then 7
            else if
              (pos = 0 || not (asciiWord normalized.(pos - 1)))
              && normalized.(pos) = 116
            then 1
            else 0
          in
          if prefix = 0 then (
            appendSlice normalized pos 1;
            temps (pos + 1))
          else
            let ending = ref (pos + prefix) in
            while !ending < size && Text.isDigit normalized.(!ending) do
              incr ending
            done;
            if
              !ending = pos + prefix
              || (!ending < size && boundaryWord normalized.(!ending))
            then (
              appendSlice normalized pos 1;
              temps (pos + 1))
            else
              let key =
                Text.ofScalars
                  (Array.sub normalized (pos + prefix) (!ending - pos - prefix))
              in
              let id = canonical key in
              Buffer.add_string output
                ((if prefix = 1 then "t" else "TempId ") ^ string_of_int id);
              temps !ending
  in
  temps 0;
  let arr = Text.scalars (Buffer.contents output) in
  Buffer.clear output;
  ids := M.empty;
  next := 0;
  let length = Array.length arr in
  let rec closures pos =
    if pos < length then (
      if not (starts arr pos "__closure_") then (
        appendSlice arr pos 1;
        closures (pos + 1))
      else
        let digits = pos + 10 in
        let comparison = starts arr digits "comparison_" in
        let digitStart = digits + if comparison then 11 else 0 in
        let ending = ref digitStart in
        while !ending < length && Text.isDigit arr.(!ending) do
          incr ending
        done;
        if !ending = digitStart then (
          appendSlice arr pos 1;
          closures (pos + 1))
        else
          let key = Text.ofScalars (Array.sub arr pos (!ending - pos)) in
          let id = canonical key in
          Buffer.add_string output
            ("__closure_"
            ^ (if comparison then "comparison_" else "")
            ^ string_of_int id);
          closures !ending)
  in
  closures 0;
  Text.ofScalars (Text.scalars (Buffer.contents output))

let withoutSyntheticANFMain ir =
  let suffix = "\n\nMain:\nreturn 0" in
  if Filename.check_suffix ir suffix then
    String.sub ir 0 (String.length ir - String.length suffix)
  else ir

let formatANFForOptimizationTest synthetic program =
  let formatted = ANFPrinter.formatANF program in
  if synthetic then withoutSyntheticANFMain formatted else formatted

let removeSyntheticMIREntry (MIR.Program (functions, variants, records)) =
  MIR.Program
    ( List.filter (fun (f : MIR.functionDef) -> f.MIR.name <> "_start") functions,
      variants,
      records )

let formatMIRForOptimizationTest names synthetic program =
  let names =
    FunctionIdMap.fold
      (fun acc id name -> FunctionIdMap.add id name acc)
      names externalFunctionNames
  in
  MIRPrinter.formatMIRWithFunctionNames names
    (if synthetic then removeSyntheticMIREntry program else program)

let removeSyntheticLIREntry (LIR.Program (functions, variants, records)) =
  LIR.Program
    ( List.filter (fun (f : LIR.functionDef) -> f.LIR.name <> "_start") functions,
      variants,
      records )

let formatLIRForOptimizationTest synthetic program =
  LIRPrinter.formatLIR
    (if synthetic then removeSyntheticLIREntry program else program)

let prepare stdlib recorder source =
  let* ast, synthetic =
    measure recorder "Optimization detail: Parse" (fun () ->
        parseOptimizationSource source)
  in
  let* programType, typedAst =
    measure recorder "Optimization detail: Type checking" (fun () ->
        typeCheckWithStdlib stdlib ast)
  in
  let* converted =
    measure recorder "Optimization detail: AST to ANF" (fun () ->
        convertTypedProgram stdlib recorder typedAst)
    |> Result.map_error (fun error -> "ANF conversion error: " ^ error)
  in
  Ok (programType, converted, synthetic)

let optimize (converted : AST_to_ANF.conversionResult) =
  ANF_Optimize.optimizeProgramWithOptions
    (optimizeContextFromConversionResult converted)
    ANFConstants.defaultOptimizeOptions converted.AST_to_ANF.program

let getOptimizedANF stdlib recorder source =
  let* _, converted, synthetic = prepare stdlib recorder source in
  let optimized =
    measure recorder "Optimization detail: ANF optimization" (fun () ->
        optimize converted)
  in
  measure recorder "Optimization detail: IR formatting" (fun () ->
      Ok (formatANFForOptimizationTest synthetic optimized))

let getOptimizedStdlibANF (stdlib : CompilationContexts.stdlibResult) name =
  match M.find_opt name stdlib.CompilationContexts.stdlibAnfFunctions with
  | None -> Error ("Prebuilt stdlib ANF function not found: " ^ name)
  | Some func ->
      let names =
        M.bindings stdlib.CompilationContexts.stdlibAnfFunctions
        |> List.map (fun (_, (f : ANF.functionDef)) -> (f.ANF.id, f.ANF.name))
        |> FunctionIdMap.ofList
      in
      Ok (ANFPrinter.formatANFFunction names func)

let mirPipeline stdlib programType (converted : AST_to_ANF.conversionResult) =
  let (ANF.Program (functions, mainExpr)) = optimize converted in
  let names =
    FunctionIdMap.map (fun _ (name, _) -> name) converted.AST_to_ANF.funcReg
  in
  let ids =
    FunctionIdMap.toList names
    |> List.map (fun (id, name) -> (name, id))
    |> M.of_list
  in
  let printed = PrintInsertion.insertPrint ids functions mainExpr programType in
  let optimized = { converted with AST_to_ANF.program = printed } in
  let* afterRC, typeMap =
    RefCountInsertion.insertRCInProgram optimized
    |> Result.map_error (fun e -> "RC insertion error: " ^ e)
  in
  let afterTCO = TailCallDetection.detectTailCallsInProgram afterRC in
  let* mir =
    ANF_to_MIR.toMIR afterTCO typeMap M.empty programType
      optimized.AST_to_ANF.variantLookup
      (TypeRegistries.recordFieldsRegistry optimized.AST_to_ANF.typeReg)
      false (returnTypesFor stdlib)
      (FunctionIdMap.add (AST.functionId 0L) "_start" names)
    |> Result.map_error (fun e -> "MIR conversion error: " ^ e)
  in
  let ssa = SSA_Construction.convertToSSA mir in
  Ok (MIR_Optimize.optimizeProgram ssa, names)

let getOptimizedMIR stdlib recorder source =
  let* typ, converted, synthetic = prepare stdlib recorder source in
  measure recorder "Optimization detail: MIR pipeline" (fun () ->
      let* mir, names = mirPipeline stdlib typ converted in
      Ok (formatMIRForOptimizationTest names synthetic mir))

let getOptimizedLIR stdlib recorder source =
  let* typ, converted, synthetic = prepare stdlib recorder source in
  measure recorder "Optimization detail: LIR pipeline" (fun () ->
      let* mir, _ = mirPipeline stdlib typ converted in
      let* lir =
        MIR_to_LIR.toLIR mir
        |> Result.map_error (fun e -> "LIR conversion error: " ^ e)
      in
      Ok
        (formatLIRForOptimizationTest synthetic
           (LIR_Peephole.optimizeProgram lir)))

let equalLIR (LIR.Program (left, lv, lr)) (LIR.Program (right, rv, rr)) =
  let equalFunction (a : LIR.functionDef) (b : LIR.functionDef) =
    a.LIR.id = b.LIR.id && a.LIR.name = b.LIR.name
    && a.LIR.typedParams = b.LIR.typedParams
    && a.LIR.cfg.LIR.entry = b.LIR.cfg.LIR.entry
    && LIR.LabelMap.equal ( = ) a.LIR.cfg.LIR.blocks b.LIR.cfg.LIR.blocks
    && a.LIR.stackSize = b.LIR.stackSize
    && a.LIR.usedCalleeSaved = b.LIR.usedCalleeSaved
    && a.LIR.codegenFacts = b.LIR.codegenFacts
  in
  List.length left = List.length right
  && List.for_all2 equalFunction left right
  && M.equal ( = ) lv rv && M.equal ( = ) lr rr

let listDisplay values =
  let short =
    List.filteri (fun index _ -> index < 3) values
    |> List.map (fun instr ->
        StructuralFormat.format (MachineDiagnostic.x64Instr instr))
  in
  "[" ^ String.concat "; " short
  ^ (if List.length values > 3 then "; ... " else "")
  ^ "]"

let success =
  { success = true; message = "Test passed"; expected = None; actual = None }

let runOptimizationTest stdlib recorder (test : optimizationTest) =
  let sourceIRResult =
    match (test.stage, test.input) with
    | ANF, Source source -> getOptimizedANF stdlib recorder source
    | ANF, StdlibFunction name -> getOptimizedStdlibANF stdlib name
    | MIR, Source source -> getOptimizedMIR stdlib recorder source
    | LIR, Source source -> getOptimizedLIR stdlib recorder source
    | (MIR | LIR), StdlibFunction _ ->
        Error "STDLIB-FUNCTION is supported only for ANF optimization tests"
    | (DirectLIR | DirectARM64 | DirectLIR2X64), _ ->
        Error "Direct optimization stages use structural comparison"
  in
  let structuralResult =
    match (test.stage, test.input) with
    | DirectLIR, Source source ->
        let input = LIRParser.parseLIR source in
        let expected = LIRParser.parseLIR test.expectedIR in
        Some
          (match (input, expected) with
          | Error e, _ -> Error ("Failed to parse INPUT LIR: " ^ e)
          | _, Error e -> Error ("Failed to parse EXPECTED LIR: " ^ e)
          | Ok input, Ok expected ->
              let actual = LIR_Peephole.optimizeProgram input in
              if equalLIR actual expected then Ok ()
              else
                Error
                  ("LIR mismatch\nExpected:\n"
                  ^ LIRPrinter.formatLIR expected
                  ^ "\nActual:\n"
                  ^ LIRPrinter.formatLIR actual))
    | DirectARM64, Source source ->
        let input = ARM64SymbolicParser.parseARM64Symbolic source in
        let expected = ARM64SymbolicParser.parseARM64Symbolic test.expectedIR in
        Some
          (match (input, expected) with
          | Error e, _ -> Error ("Failed to parse INPUT ARM64: " ^ e)
          | _, Error e -> Error ("Failed to parse EXPECTED ARM64: " ^ e)
          | Ok input, Ok expected ->
              let actual = Peephole.peepholeOptimize input in
              if actual = expected then Ok ()
              else
                let render xs =
                  String.concat "\n"
                    (List.map PassTestRunner.prettyPrintARM64Instr xs)
                in
                Error
                  ("ARM64 mismatch\nExpected:\n" ^ render expected
                 ^ "\nActual:\n" ^ render actual))
    | DirectLIR2X64, Source source ->
        let input = LIRParser.parseLIR source in
        let expected = X86_64Parser.parseX64 test.expectedIR in
        Some
          (match (input, expected) with
          | Error e, _ -> Error ("Failed to parse INPUT LIR: " ^ e)
          | _, Error e -> Error ("Failed to parse EXPECTED x64: " ^ e)
          | Ok (LIR.Program ([ func ], _, _) as input), Ok expected -> (
              let rec starts expected remaining =
                match (expected, remaining) with
                | [], _ -> true
                | e :: es, r :: rs when e = r -> starts es rs
                | _ -> false
              in
              let rec contains remaining =
                starts expected remaining
                ||
                match remaining with
                | [] -> false
                | _ :: tail -> contains tail
              in
              match CodeGen_X86_64.translateProgram input false with
              | Error e -> Error ("x64 lowering failed: " ^ e)
              | Ok emitted ->
                  let rec skip = function
                    | [] -> []
                    | instr :: tail as rest ->
                        if instr = X86_64.Label func.LIR.name then rest
                        else skip tail
                  in
                  let rec take = function
                    | [] -> []
                    | instr :: tail ->
                        if instr = X86_64.Label ("_epilogue_" ^ func.LIR.name)
                        then []
                        else instr :: take tail
                  in
                  let body = take (skip emitted) in
                  if contains body then Ok ()
                  else
                    Error
                      ("Expected x64 instruction sequence was not selected in "
                     ^ func.LIR.name ^ "\nExpected sequence: "
                     ^ listDisplay expected ^ "\nActual function: "
                     ^ listDisplay body))
          | Ok _, Ok _ -> Error "INPUT LIR must contain exactly one function")
    | (DirectLIR | DirectARM64 | DirectLIR2X64), StdlibFunction _ ->
        Some (Error "Direct optimization stages require an INPUT section")
    | (ANF | MIR | LIR), _ -> None
  in
  let failure message expected actual =
    { success = false; message; expected = Some expected; actual }
  in
  match structuralResult with
  | Some (Ok ()) -> success
  | Some (Error error) -> failure error test.expectedIR None
  | None -> (
      match sourceIRResult with
      | Error error -> failure error test.expectedIR None
      | Ok actualIR ->
          let expected = normalizeIR test.expectedIR in
          let actual = normalizeIR actualIR in
          if expected = actual then success
          else failure "IR mismatch" expected (Some actual))

let runTestFile stdlib recorder shouldRun stage path =
  OptimizationFormat.parseTestFile stage path
  |> Result.map (fun tests ->
      List.filter shouldRun tests
      |> List.map (fun test -> (test, runOptimizationTest stdlib recorder test)))
