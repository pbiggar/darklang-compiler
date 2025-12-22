// Program.fs - Compiler CLI Entry Point
//
// The main entry point for the Darklang compiler CLI.
//
// This module:
// - Parses command-line arguments
// - Orchestrates the compilation pipeline through all passes
// - Handles errors and provides user feedback
//
// Compilation pipeline:
//   1. Parser: Source → AST
//   2. AST_to_ANF: AST → ANF
//   3. ANF_to_MIR: ANF → MIR
//   4. MIR_to_LIR: MIR → LIR
//   5. RegisterAllocation: LIR (virtual) → LIR (physical)
//   6. CodeGen: LIR → ARM64 instructions
//   7. ARM64_Encoding: ARM64 instructions → Machine code
//   8. Binary_Generation: Machine code → Mach-O executable

module Program

open System
open System.IO

/// Compile source expression to executable
let compile (source: string) (outputPath: string) : unit =
    printfn "Compiling: %s" source

    // Parse
    printfn "  [1/8] Parsing..."
    let ast = Parser.parseString source

    // Convert to ANF
    printfn "  [2/8] Converting to ANF..."
    let (AST.Program expr) = ast
    let (anfExpr, _) = AST_to_ANF.toANF expr (ANF.VarGen 0)
    let anfProgram = ANF.Program anfExpr

    // Convert to MIR
    printfn "  [3/8] Converting to MIR..."
    let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0)

    // Convert to LIR
    printfn "  [4/8] Converting to LIR..."
    let lirProgram = MIR_to_LIR.toLIR mirProgram

    // Allocate registers
    printfn "  [5/8] Allocating registers..."
    let (LIR.Program funcs) = lirProgram
    let func = List.head funcs
    let allocResult = RegisterAllocation.allocateRegisters func
    let allocatedFunc = { func with Body = allocResult.Instrs; StackSize = allocResult.StackSize }
    let allocatedProgram = LIR.Program [allocatedFunc]

    // Generate ARM64 code
    printfn "  [6/8] Generating ARM64 code..."
    let arm64Code = CodeGen.generateARM64 allocatedProgram
    printfn "    Generated %d instructions" arm64Code.Length

    // Encode to machine code and generate binary
    printfn "  [7/8] Encoding ARM64 to machine code..."
    let machineCode = arm64Code |> List.collect ARM64_Encoding.encode

    printfn "  [8/8] Generating binary..."
    let binary = Binary_Generation.createExecutable machineCode

    // Write to file
    Binary_Generation.writeToFile outputPath binary
    printfn "Successfully wrote %d bytes to %s" binary.Length outputPath

/// Print usage information
let printUsage () =
    printfn "Darklang Compiler"
    printfn ""
    printfn "Usage:"
    printfn "  DarkCompiler <expression> <output>"
    printfn "  DarkCompiler --file <input-file> <output>"
    printfn ""
    printfn "Examples:"
    printfn "  DarkCompiler \"2 + 3\" output"
    printfn "  DarkCompiler \"2 + 3 * 4\" output"
    printfn "  DarkCompiler --file prog.dark output"
    printfn ""
    printfn "Note: Generated executables may require code signing to run on macOS"

[<EntryPoint>]
let main argv =
    try
        match argv with
        | [| expr; output |] ->
            compile expr output
            0

        | [| "--file"; inputFile; output |] ->
            if not (File.Exists inputFile) then
                printfn "Error: Input file '%s' not found" inputFile
                1
            else
                let source = File.ReadAllText inputFile
                compile source output
                0

        | _ ->
            printUsage ()
            1

    with
    | ex ->
        printfn "Error: %s" ex.Message
        printfn "%s" ex.StackTrace
        1
