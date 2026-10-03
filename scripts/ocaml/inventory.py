#!/usr/bin/env python3
"""Freeze the reference source/fixture inventory and verify migration coverage."""

import argparse
import hashlib
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ORACLE = "df9dae7e1647275f6bc9104618f20ef84a7251be"
REFERENCE = "27edbf054b400623f1856803fa0aef051f443cf8"
MANIFEST = ROOT / "ocaml/inventory.json"


def git(*args):
    return subprocess.check_output(["git", *args], cwd=ROOT)


def kind(path):
    if path.startswith("src/DarkCompiler/stdlib/"):
        return "stdlib"
    if path.startswith("src/DarkCompiler/") and path.endswith(".fs"):
        return "compiler"
    if path.startswith("src/Tests/"):
        return "test-source" if path.endswith(".fs") else "test-input"
    if path in ("dark", "build", "run-tests", "Dockerfile", "global.json"):
        return "entrypoint"
    return None


def fixture(path):
    return kind(path) in ("stdlib", "test-input")


# Preserve F# namespace distinctions in Dune's unqualified module graph.
OWNER_OVERRIDES = {
    "src/DarkCompiler/passes/mir/MIR_Optimize.fs": "ocaml/lib/passes/mir/MIR_Optimize.ml",
    "src/DarkCompiler/passes/mir/optimization/SparseConditionalConstants.fs": "ocaml/lib/passes/mir/optimization/MIRSparseConditionalConstants.ml",
    "src/DarkCompiler/passes/mir/optimization/Induction.fs": "ocaml/lib/passes/mir/optimization/MIRInduction.ml",
    "src/DarkCompiler/passes/mir/optimization/Unrolling.fs": "ocaml/lib/passes/mir/optimization/MIRUnrolling.ml",
    "src/DarkCompiler/passes/mir/optimization/LoopInvariantMotion.fs": "ocaml/lib/passes/mir/optimization/MIRLoopInvariantMotion.ml",
    "src/DarkCompiler/passes/mir/optimization/CommonExpressions.fs": "ocaml/lib/passes/mir/optimization/MIRCommonExpressions.ml",
    "src/DarkCompiler/passes/mir/optimization/ControlFlow.fs": "ocaml/lib/passes/mir/optimization/MIRControlFlow.ml",
    "src/DarkCompiler/passes/mir/optimization/LoopTopology.fs": "ocaml/lib/passes/mir/optimization/MIRLoopTopology.ml",
    "src/DarkCompiler/passes/mir/optimization/CopyPropagation.fs": "ocaml/lib/passes/mir/optimization/MIRCopyPropagation.ml",
    "src/DarkCompiler/passes/mir/optimization/Facts.fs": "ocaml/lib/passes/mir/optimization/MIROptimizationFacts.ml",
    "src/DarkCompiler/passes/mir/optimization/DeadCode.fs": "ocaml/lib/passes/mir/optimization/MIRDeadCode.ml",
    "src/DarkCompiler/passes/anf/ownership/SSARefCountInsertion.fs": "ocaml/lib/passes/anf/ownership/RcSSARefCountInsertion.ml",
    "src/DarkCompiler/passes/anf/ownership/SSAValueLiveness.fs": "ocaml/lib/passes/anf/ownership/RcSSAValueLiveness.ml",
    "src/DarkCompiler/passes/anf/ownership/SSAReturnAnalysis.fs": "ocaml/lib/passes/anf/ownership/RcSSAReturnAnalysis.ml",
    "src/Tests/compiler-passes/ownership/JoinTests.fs": "ocaml/tests/compiler-passes/ownership/RcJoinTests.ml",
    "src/Tests/compiler-passes/ownership/TypeFactTests.fs": "ocaml/tests/compiler-passes/ownership/RcTypeFactTests.ml",
    "src/DarkCompiler/passes/anf/ownership/InsertExpression.fs": "ocaml/lib/passes/anf/ownership/RcInsertExpression.ml",
    "src/DarkCompiler/passes/anf/ownership/Cleanup.fs": "ocaml/lib/passes/anf/ownership/RcCleanup.ml",
    "src/DarkCompiler/passes/anf/ownership/ShapePlanning.fs": "ocaml/lib/passes/anf/ownership/RcShapePlanning.ml",
    "src/DarkCompiler/passes/anf/ownership/TypeFacts.fs": "ocaml/lib/passes/anf/ownership/RcTypeFacts.ml",
    "src/DarkCompiler/passes/anf/ownership/ReturnAnalysis.fs": "ocaml/lib/passes/anf/ownership/RcReturnAnalysis.ml",
    "src/DarkCompiler/passes/anf/optimization/Accumulators.fs": "ocaml/lib/passes/anf/optimization/ANFAccumulatorOptimization.ml",
    "src/DarkCompiler/passes/anf/optimization/Substitution.fs": "ocaml/lib/passes/anf/optimization/ANFSubstitution.ml",
    "src/DarkCompiler/passes/hir/ConstructFunctions.fs": "ocaml/lib/passes/hir/ConstructHIRFunctions.ml",
    "src/DarkCompiler/ir/anf/Continuations.fs": "ocaml/lib/ir/anf/Continuations.ml",
    'src/DarkCompiler/passes/anf/lowering/Aggregates.fs': 'ocaml/lib/passes/anf/lowering/LoweringAggregates.ml',
    'src/DarkCompiler/passes/anf/lowering/Operators.fs': 'ocaml/lib/passes/anf/lowering/LoweringOperators.ml',
    'src/DarkCompiler/passes/anf/lowering/TypeInference.fs': 'ocaml/lib/passes/anf/lowering/LoweringTypeInference.ml',

    "src/DarkCompiler/passes/anf/lowering/Primitives.fs": "ocaml/lib/passes/anf/lowering/LoweringPrimitives.ml",
    "src/DarkCompiler/Stdlib.fs": "ocaml/lib/DarkStdlib.ml",
    "src/DarkCompiler/backend/arm64/Binary_Generation_ELF.fs": "ocaml/lib/backend/arm64/Backend_Arm64_Binary_Generation_ELF.ml",
    "src/DarkCompiler/backend/x64/Binary_Generation_ELF.fs": "ocaml/lib/backend/x64/Binary_Generation_ELF_X86_64.ml",
    "src/DarkCompiler/backend/arm64/Blocks.fs": "ocaml/lib/backend/arm64/ARM64Blocks.ml",
    "src/DarkCompiler/backend/x64/Blocks.fs": "ocaml/lib/backend/x64/X64Blocks.ml",
    "src/DarkCompiler/backend/arm64/CalleeClobbers.fs": "ocaml/lib/backend/arm64/ARM64CalleeClobbers.ml",
    "src/DarkCompiler/backend/x64/CalleeClobbers.fs": "ocaml/lib/backend/x64/X64CalleeClobbers.ml",
    "src/DarkCompiler/backend/arm64/CodeGen.fs": "ocaml/lib/backend/arm64/Backend_Arm64_CodeGen.ml",
    "src/DarkCompiler/backend/x64/CodeGen.fs": "ocaml/lib/backend/x64/CodeGen_X86_64.ml",
    "src/DarkCompiler/backend/arm64/CodeGenTypes.fs": "ocaml/lib/backend/arm64/ARM64CodeGenTypes.ml",
    "src/DarkCompiler/backend/x64/CodeGenTypes.fs": "ocaml/lib/backend/x64/X64CodeGenTypes.ml",
    "src/DarkCompiler/backend/arm64/Encoding.fs": "ocaml/lib/backend/arm64/ARM64_Encoding.ml",
    "src/DarkCompiler/backend/x64/Encoding.fs": "ocaml/lib/backend/x64/X86_64_Encoding.ml",
    "src/DarkCompiler/backend/arm64/Frames.fs": "ocaml/lib/backend/arm64/ARM64Frames.ml",
    "src/DarkCompiler/backend/x64/Frames.fs": "ocaml/lib/backend/x64/X64Frames.ml",
    "src/DarkCompiler/backend/arm64/Functions.fs": "ocaml/lib/backend/arm64/ARM64Functions.ml",
    "src/DarkCompiler/backend/x64/Functions.fs": "ocaml/lib/backend/x64/X64Functions.ml",
    "src/DarkCompiler/frontend/checking/Functions.fs": "ocaml/lib/frontend/checking/CheckFunctions.ml",
    "src/DarkCompiler/backend/arm64/ISA.fs": "ocaml/lib/backend/arm64/ARM64.ml",
    "src/DarkCompiler/backend/x64/ISA.fs": "ocaml/lib/backend/x64/X86_64.ml",
    "src/DarkCompiler/backend/arm64/InstructionContext.fs": "ocaml/lib/backend/arm64/ARM64InstructionContext.ml",
    "src/DarkCompiler/backend/x64/InstructionContext.fs": "ocaml/lib/backend/x64/X64InstructionContext.ml",
    "src/DarkCompiler/backend/arm64/Instructions.fs": "ocaml/lib/backend/arm64/ARM64Instructions.ml",
    "src/DarkCompiler/backend/x64/Instructions.fs": "ocaml/lib/backend/x64/X64Instructions.ml",
    "src/DarkCompiler/backend/arm64/Operands.fs": "ocaml/lib/backend/arm64/ARM64Operands.ml",
    "src/DarkCompiler/backend/x64/Operands.fs": "ocaml/lib/backend/x64/X64Operands.ml",
    "src/DarkCompiler/backend/arm64/PrepareFunctions.fs": "ocaml/lib/backend/arm64/ARM64PrepareFunctions.ml",
    "src/DarkCompiler/passes/preparation/PrepareFunctions.fs": "ocaml/lib/passes/preparation/PrepareFunctions.ml",
    "src/DarkCompiler/backend/arm64/Resolve.fs": "ocaml/lib/backend/arm64/ARM64_Resolve.ml",
    "src/DarkCompiler/backend/x64/Resolve.fs": "ocaml/lib/backend/x64/X86_64_Resolve.ml",
    "src/DarkCompiler/backend/arm64/instructions/Buffers.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitBuffers.ml",
    "src/DarkCompiler/backend/x64/instructions/Buffers.fs": "ocaml/lib/backend/x64/instructions/X64EmitBuffers.ml",
    "src/DarkCompiler/backend/arm64/instructions/Calls.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitCalls.ml",
    "src/DarkCompiler/backend/x64/instructions/Calls.fs": "ocaml/lib/backend/x64/instructions/X64EmitCalls.ml",
    "src/DarkCompiler/backend/arm64/instructions/Files.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitFiles.ml",
    "src/DarkCompiler/backend/x64/instructions/Files.fs": "ocaml/lib/backend/x64/instructions/X64EmitFiles.ml",
    "src/DarkCompiler/backend/arm64/instructions/FloatingPoint.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitFloatingPoint.ml",
    "src/DarkCompiler/backend/x64/instructions/FloatingPoint.fs": "ocaml/lib/backend/x64/instructions/X64EmitFloatingPoint.ml",
    "src/DarkCompiler/backend/arm64/instructions/Integer.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitInteger.ml",
    "src/DarkCompiler/backend/x64/instructions/Integer.fs": "ocaml/lib/backend/x64/instructions/X64EmitInteger.ml",
    "src/DarkCompiler/backend/arm64/instructions/Memory.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitMemory.ml",
    "src/DarkCompiler/backend/x64/instructions/Memory.fs": "ocaml/lib/backend/x64/instructions/X64EmitMemory.ml",
    "src/DarkCompiler/backend/arm64/instructions/NativeEffects.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitNativeEffects.ml",
    "src/DarkCompiler/backend/x64/instructions/NativeEffects.fs": "ocaml/lib/backend/x64/instructions/X64EmitNativeEffects.ml",
    "src/DarkCompiler/backend/arm64/instructions/Printing.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitPrinting.ml",
    "src/DarkCompiler/backend/x64/instructions/Printing.fs": "ocaml/lib/backend/x64/instructions/X64EmitPrinting.ml",
    "src/DarkCompiler/backend/x64/runtime/Printing.fs": "ocaml/lib/backend/x64/runtime/X64Printing.ml",
    "src/DarkCompiler/ir/Printing.fs": "ocaml/lib/ir/IRPrinting.ml",
    "src/DarkCompiler/backend/arm64/instructions/ReferenceCounts.fs": "ocaml/lib/backend/arm64/instructions/ARM64EmitReferenceCounts.ml",
    "src/DarkCompiler/backend/x64/instructions/ReferenceCounts.fs": "ocaml/lib/backend/x64/instructions/X64EmitReferenceCounts.ml",
    "src/DarkCompiler/backend/arm64/runtime/ClosureReferenceCounts.fs": "ocaml/lib/backend/arm64/runtime/ARM64ClosureReferenceCounts.ml",
    "src/DarkCompiler/backend/x64/runtime/ClosureReferenceCounts.fs": "ocaml/lib/backend/x64/runtime/X64ClosureReferenceCounts.ml",
    "src/DarkCompiler/backend/arm64/runtime/DictReferenceCounts.fs": "ocaml/lib/backend/arm64/runtime/ARM64DictReferenceCounts.ml",
    "src/DarkCompiler/backend/x64/runtime/DictReferenceCounts.fs": "ocaml/lib/backend/x64/runtime/X64DictReferenceCounts.ml",
    "src/DarkCompiler/backend/arm64/runtime/ListReferenceCounts.fs": "ocaml/lib/backend/arm64/runtime/ARM64ListReferenceCounts.ml",
    "src/DarkCompiler/backend/x64/runtime/ListReferenceCounts.fs": "ocaml/lib/backend/x64/runtime/X64ListReferenceCounts.ml",
    "src/DarkCompiler/backend/arm64/runtime/ReleaseSelection.fs": "ocaml/lib/backend/arm64/runtime/ARM64ReleaseSelection.ml",
    "src/DarkCompiler/backend/x64/runtime/ReleaseSelection.fs": "ocaml/lib/backend/x64/runtime/X64ReleaseSelection.ml",
    "src/DarkCompiler/driver/Diagnostics.fs": "ocaml/lib/driver/PipelineDiagnostics.ml",
    "src/DarkCompiler/frontend/checking/Diagnostics.fs": "ocaml/lib/frontend/checking/CheckingDiagnostics.ml",
    "src/DarkCompiler/frontend/checking/Expressions.fs": "ocaml/lib/frontend/checking/CheckExpressions.ml",
    "src/DarkCompiler/passes/anf/lowering/Expressions.fs": "ocaml/lib/passes/anf/lowering/LoweringExpressions.ml",
    "src/DarkCompiler/passes/anf/optimization/Expressions.fs": "ocaml/lib/passes/anf/optimization/ANFExpressionOptimization.ml",
    "src/DarkCompiler/frontend/interpreter/Effects.fs": "ocaml/lib/frontend/interpreter/LibExecution_Effects.ml",
    "src/DarkCompiler/passes/anf/optimization/Effects.fs": "ocaml/lib/passes/anf/optimization/ANFEffects.ml",
    "src/DarkCompiler/ir/anf/Printer.fs": "ocaml/lib/ir/anf/ANFPrinter.ml",
    "src/DarkCompiler/ir/lir/Printer.fs": "ocaml/lib/ir/lir/LIRPrinter.ml",
    "src/DarkCompiler/ir/mir/Printer.fs": "ocaml/lib/ir/mir/MIRPrinter.ml",
    "src/DarkCompiler/passes/anf/optimization/Constants.fs": "ocaml/lib/passes/anf/optimization/ANFConstants.ml",
    "src/DarkCompiler/passes/mir/optimization/Constants.fs": "ocaml/lib/passes/mir/optimization/MIRConstants.ml"
}


def owner(path):
    if path in OWNER_OVERRIDES:
        return OWNER_OVERRIDES[path]
    if kind(path) == "compiler":
        return "ocaml/lib/" + path[len("src/DarkCompiler/"):-3] + ".ml"
    if kind(path) == "test-source":
        return "ocaml/tests/" + path[len("src/Tests/"):-3] + ".ml"
    return path


def capture():
    entries = []
    for path in git("ls-tree", "-r", "--name-only", ORACLE).decode().splitlines():
        category = kind(path)
        if category is None:
            continue
        content = git("show", f"{ORACLE}:{path}")
        entries.append({"source": path, "kind": category,
                        "sha256": hashlib.sha256(content).hexdigest(),
                        "owner": owner(path)})
    MANIFEST.write_text(json.dumps({"reference": REFERENCE, "oracle": ORACLE,
                                   "entries": entries}, indent=2) + "\n")
    print(f"Frozen {len(entries)} source, fixture, and entrypoint records")


def verify(require_complete):
    manifest = json.loads(MANIFEST.read_text())
    errors = []
    translated = 0
    implementation_count = 0
    for entry in manifest["entries"]:
        source = ROOT / entry["source"]
        if fixture(entry["source"]):
            if not source.is_file() or hashlib.sha256(source.read_bytes()).hexdigest() != entry["sha256"]:
                errors.append(f"Frozen input changed: {entry['source']}")
        if entry["kind"] in ("compiler", "test-source"):
            implementation_count += 1
            implementation = ROOT / entry["owner"]
            interface = implementation.with_suffix(".mli")
            if implementation.is_file() and interface.is_file():
                translated += 1
            elif implementation.is_file() != interface.is_file():
                errors.append(f"Missing interface or implementation: {entry['owner']}")
            elif require_complete:
                errors.append(f"Unported: {entry['source']}")
    print(f"Coverage: {translated}/{implementation_count} implementation/interface pairs")
    for error in errors[:20]:
        print(error)
    if len(errors) > 20:
        print(f"... {len(errors) - 20} more failures")
    return 1 if errors else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("capture", "verify"))
    parser.add_argument("--require-complete", action="store_true")
    args = parser.parse_args()
    if args.command == "capture":
        capture()
        return 0
    return verify(args.require_complete)


if __name__ == "__main__":
    raise SystemExit(main())
