<!-- test-tidyup-plan.md - PRD-style backlog for compiler driver + test framework tidyups -->
[
  {
    "category": "refactor",
    "description": "Remove dead compileWithStdlibAST path in CompilerLibrary",
    "steps": [
      "Confirm no references to compileWithStdlibAST with rg",
      "Delete compileWithStdlibAST and any now-unused helpers",
      "Verify compileWithOptions and compileTestWithPreamble remain the only public compile paths",
      "Run ./run-tests"
    ],
    "passes": false
  },
  {
    "category": "refactor",
    "description": "Factor shared CLI logic in Program.fs",
    "steps": [
      "Add buildCompilerOptions: CliOptions -> CompilerLibrary.CompilerOptions",
      "Add shouldShowNormal: VerbosityLevel -> bool",
      "Replace duplicated option construction and showNormal checks in compile/run",
      "Confirm CLI output is unchanged for compile and run modes"
    ],
    "passes": false
  },
  {
    "category": "testing",
    "description": "Fix stale PassTestRunner comment and MIR pretty printing",
    "steps": [
      "Update the module comment to state pass tests are active",
      "Replace placeholder MIR printers with CFG-aware output",
      "Keep formatting pinned to current outputs unless a bug is found",
      "Run ./run-tests and only update expected files if outputs were previously incorrect"
    ],
    "passes": false
  },
  {
    "category": "refactor",
    "description": "Consolidate IR pretty printers with pinned formatting",
    "steps": [
      "Create a shared IRPrinter module with formatANF/formatMIR/formatLIR/formatLIRSymbolic",
      "Ensure output matches existing formatting exactly (pin formatting)",
      "Update CompilerLibrary, PassTestRunner, and OptimizationTestRunner to use IRPrinter",
      "Run ./run-tests and confirm no formatting drift"
    ],
    "passes": false
  },
  {
    "category": "refactor",
    "description": "Reduce duplication in TestRunner suite execution",
    "steps": [
      "Introduce a helper to run file-based suites (load, run, timing, progress, failure output)",
      "Apply it to ANF->MIR, MIR->LIR, LIR->ARM64, ARM64 encoding, typecheck, and optimization suites",
      "Keep E2E flow separate (preamble precompile and timing details)",
      "Verify output is unchanged for success/failure cases"
    ],
    "passes": false
  },
  {
    "category": "tooling",
    "description": "Normalize test scripts into a shared helper",
    "steps": [
      "Add scripts/test-common.sh with build_tests/find_test_exe/run_tests helpers",
      "Update run-tests, run-verification-tests, and run-dark-coverage to use the helpers",
      "Preserve CLI flags and environment behavior",
      "Run ./run-tests and ./run-verification-tests to confirm behavior"
    ],
    "passes": false
  },
  {
    "category": "testing",
    "description": "Unify stdlib compilation entry points for tests (plumbing only)",
    "steps": [
      "Create a StdlibTestHarness module to provide a shared StdlibResult",
      "Add explicit cache reset helpers in the harness for tests that require cold caches",
      "Update E2ETestRunner, CompilerCachingTests, PreamblePrecompileTests, and TestRunner.runWithStdlib to use the harness",
      "Keep all suites and categories intact; no test removal"
    ],
    "passes": false
  },
  {
    "category": "refactor",
    "description": "Optimization tests use shared pipeline and pinned IR printing",
    "steps": [
      "Add a CompilerLibrary entry to return ANF/MIR/LIR snapshots with options",
      "Ensure coverage is a shared pipeline option, not a separate code path",
      "Refactor OptimizationTestRunner to use the shared pipeline and IRPrinter",
      "Run ./run-tests and verify optimization output is unchanged"
    ],
    "passes": false
  },
  {
    "category": "testing",
    "description": "Align pass tests with current IR representation while keeping suites",
    "steps": [
      "Decide whether pass tests compare symbolic or resolved LIR and document the choice",
      "Update PassTestRunner to match the chosen representation",
      "Use IRPrinter formatting to keep output stable",
      "Do not remove any pass suites; only simplify plumbing"
    ],
    "passes": false
  }
]
