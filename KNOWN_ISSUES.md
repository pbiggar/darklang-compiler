# Known Issues

## Register Spilling Segfault (High Priority)

**Status**: Infrastructure complete, root cause unidentified

**Symptoms**:
- Programs requiring >10 virtual registers segfault (exit code 139)
- Programs with ≤10 vregs work correctly
- Segfault occurs even in minimal test (just arithmetic, no printInt)

**Test Cases**:
- `(((209 * 1) * (-55 - -3)) / (-1 + (-29 - -809)))` → segfault
- `1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11` → segfault
- `1 * 2 * 3` → works (no spilling)

**Extensive Verification Done**:
- ✓ STUR/LDUR encoding verified byte-by-byte against ARM64 spec (e.g., 0xF81F83AB decodes correctly)
- ✓ STP/LDP encoding verified (signed offset mode, not pre-index)
- ✓ Stack layout correct:
  ```
  [FP+8] = saved LR
  [FP+0] = saved FP
  [FP-8] = spill slot 1 (allocated, SP+8)
  [FP-16] = spill slot 2 (allocated, SP+0)
  ```
- ✓ Stack size calculation correct (16 bytes for 2 spills, 16-byte aligned)
- ✓ Prologue/epilogue sequences correct
- ✓ Register allocator correctly assigns vregs 1000, 1001 to stack slots -8, -16
- ✓ Store instructions generated correctly (Store (-8, X11))
- ✓ Load instructions generated correctly (Mov (X12, StackSlot -8))
- ✓ CodeGen translates Store → STUR and StackSlot → LDUR correctly
- ✓ Register usage (X11 for temp, X12/X13 for loads) doesn't conflict
- ✓ No issue with printInt stack usage (it's BELOW our spill slots)
- ✓ Stack alignment maintained (16-byte aligned throughout)

**What We DON'T Know**:
- Exact instruction where segfault occurs (need gdb/lldb)
- Whether issue is in our code or macOS/Linux kernel interaction
- Whether there's a subtle ARM64 ABI violation we're missing

**Debugging Limitations**:
- gdb/lldb not available in current environment
- Docker not available for containerized debugging
- strace/readelf not available for binary analysis
- Minimal reproducer still segfaults, suggesting deep issue

**Next Steps** (when debugging tools available):
1. Run under gdb/lldb to get exact crash location and register state
2. Examine memory contents at FP-8 and FP-16 before/after stores
3. Single-step through prologue to verify stack pointer values
4. Compare generated assembly with known-good ARM64 spilling code
5. Test with hand-written assembly to isolate compiler vs. runtime issue

**Workaround**:
For now, programs are limited to ≤10 virtual registers (no spilling). The fuzzer has 99.8% success rate (998/1000 tests pass).

**Files**:
- `src/DarkCompiler/passes/5_RegisterAllocation.fs`
- `src/DarkCompiler/passes/6_CodeGen.fs`
- `src/DarkCompiler/passes/7_ARM64_Encoding.fs`
- `src/DarkCompiler/ARM64.fs`
- `src/DarkCompiler/LIR.fs`
