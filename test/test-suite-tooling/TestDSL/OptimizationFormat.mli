(*
   OptimizationFormat.mli - Parser for optimization test files
   Parses test files that verify IR optimizations work correctly.
   Each test contains source code and expected IR output at a specific stage.
   Format:
   ---NAME---
   test_name
   ---INPUT---
   source code
   or:
   ---STDLIB-FUNCTION---
   fully qualified prebuilt stdlib function name
   ---EXPECTED---
   exact IR output
   Stage of IR to verify
   After ANF optimization
   After MIR optimization (SSA-based)
   After LIR peephole optimization
   Direct symbolic LIR before and after the peephole pass
   Direct symbolic ARM64 before and after target peepholes
   Allocated LIR lowered to selected x64 instructions
   Optimization test specification
   Parse a single test from sections
   Parse multiple tests from a single file
   Tests are separated by ---NAME--- sections
*)
type irStage=ANF | MIR | LIR | DirectLIR | DirectARM64 | DirectLIR2X64
type optimizationInput=Source of string | StdlibFunction of string
type optimizationTest={name:string;input:optimizationInput;expectedIR:string;stage:irStage;sourceFile:string}
val parseContent : irStage -> string -> string -> (optimizationTest list,string) result
val parseTestFile : irStage -> string -> (optimizationTest list,string) result
