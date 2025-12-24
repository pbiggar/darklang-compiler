# Known Issues

## Register Spilling Segfault (High Priority)

**Status**: Infrastructure complete, runtime bug remains

**Symptoms**:
- Programs requiring >10 virtual registers segfault (exit code 139)
- Programs with ≤10 vregs work correctly- STUR/LDUR encodings verified correct
- Stack allocation verified correct

**Test Cases**:
- `(((209 * 1) * (-55 - -3)) / (-1 + (-29 - -809)))` → segfault
- `1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11` → segfault  
- `1 * 2 * 3` → works (no spilling)

**Investigation Done**:
- ✓ STUR/LDUR encoding matches ARM64 spec
- ✓ Stack layout correct (FP-relative addressing)
- ✓ Load/store instruction sequence appears valid
- ✓ Prologue allocates correct stack space
- ✓ Register allocator produces correct LIR

**Next Steps**:
1. Use gdb/lldb to get exact crash location
2. Verify with simple hand-written assembly test
3. Check if issue is in printInt runtime vs our code
4. Review ARM64 memory alignment requirements

**Files**:
- `src/DarkCompiler/passes/5_RegisterAllocation.fs`
- `src/DarkCompiler/passes/6_CodeGen.fs`
- `src/DarkCompiler/passes/7_ARM64_Encoding.fs`
