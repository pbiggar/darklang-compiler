# Investigation: Flaky UUID Test SIGSEGV Crashes

## Problem Description

The test `(Uuid.generate() != Uuid.generate())` intermittently crashes with SIGSEGV (exit code 139).
- Crash rate: ~5-7% of runs
- Only affects heap-allocated strings from complex functions like `Uuid.generate()`
- Simple string comparisons work fine

## Reproduction

The crash can be reproduced with a test file that performs many `byteToHex` operations:

```dark
let x = Random.int64() in
let y = Random.int64() in
let s1 =
  Stdlib.Uuid.__byteToHex(x, 0) ++
  Stdlib.Uuid.__byteToHex(x, 8) ++
  ... (32 total byteToHex calls)
```

### Threshold Discovery

- **27 byteToHex calls:** 0/500 crashes (100% success)
- **28 byteToHex calls:** ~2.5% crash rate
- **32 byteToHex calls:** ~5-7% crash rate

The threshold is exactly at 28 `byteToHex` calls.

## Crash Characteristics

### Register State at Crash
```
PC = 0x401ff8 or 0x402030 (varies with binary)
X14 = 0xfffffXXXX000 (page-aligned address)
X16 = 1 (refcount value, sometimes)
X27 = heap base
X28 = heap bump pointer
```

### Key Observations
1. Crash always occurs at a **page-aligned address** (ending in `000`)
2. Crash happens during `STRB` (store byte) instruction in StringConcat
3. The faulting address is within the heap region (between X27 and X27+64KB)
4. Crashes are intermittent due to ASLR varying stack position

## Hypotheses Tested

### Hypothesis 1: Unmapped Stack Pages (DISPROVED)

**Theory:** The 64KB heap is allocated on the stack via `SUB SP, SP, 64KB`. On Linux, stack pages are demand-paged, and jumping 64KB might skip over guard pages into unmapped memory.

**Testing:**
1. Added stack probing code to touch each 4KB page before allocation
2. Tried 16 pages (64KB) and 32 pages (128KB)
3. Used `SUB_reg` with X17=4096 (since `SUB_imm` has 12-bit limit of 4095)

**Results:** Crash rate unchanged (~5-7%). Pages ARE mapped.

**Code tested:**
```fsharp
// Probe 16 pages by subtracting 4KB and touching each
ARM64.MOV_reg (ARM64.X15, ARM64.SP)
ARM64.MOVZ (ARM64.X16, 0us, 0)
ARM64.MOVZ (ARM64.X17, 0x1000us, 0)  // 4096
ARM64.SUB_reg (ARM64.X15, ARM64.X15, ARM64.X17)
ARM64.STRB_reg (ARM64.X16, ARM64.X15)  // Touch page 1
// ... repeated 16 times
```

### Hypothesis 2: Refcount Alignment Mismatch (PARTIALLY INVESTIGATED)

**Theory:** StringConcat stores refcount at unaligned offset, but RefCountDec reads from aligned offset.

**Investigation:**
- StringConcat's `storeRefcount` computes: `dest + 8 + aligned(total)`
- RefCountDec computes: `addr + 8 + aligned(length)`
- Both use `aligned(x) = ((x + 7) >> 3) << 3`

**Status:** Code inspection shows both use the same formula. More investigation needed.

### Hypothesis 3: SUB_imm Overflow (FIXED)

**Theory:** `SUB_imm` with 0x1000 (4096) overflows the 12-bit immediate field.

**Verification:**
- `SUB_imm` encodes immediate as 12-bit value (max 4095)
- 0x1000 = 4096 would overflow, resulting in SUB with imm=0

**Resolution:** Changed to use `SUB_reg` with register holding 4096. However, this didn't fix the crashes because the root cause is elsewhere.

## Valgrind Analysis

Under Valgrind, crashes happen during probing itself (different memory layout):
```
Invalid write of size 1
at 0x400094: ??? (in /tmp/debug_test)
Address 0x1ffefff170 is not stack'd, malloc'd or (recently) free'd
```

This indicates Valgrind uses a different stack layout where probing reaches beyond mapped regions.

## Remaining Theories

1. **Buffer overflow in StringConcat:** The copy loop might write past the allocated region under certain conditions

2. **Heap bump allocator bug:** The allocation size calculation might be incorrect for certain string lengths

3. **Page boundary interaction:** Something specific happens when string data crosses a 4KB page boundary that causes corruption

4. **Register clobbering:** Some code path might be clobbering registers used by StringConcat

## Files Involved

- `src/DarkCompiler/passes/6_CodeGen.fs` - StringConcat implementation (~line 1750-1870)
- `src/DarkCompiler/passes/6_CodeGen.fs` - generateHeapInit (~line 2724)
- `src/DarkCompiler/passes/6_CodeGen.fs` - RefCountDecString (~line 2564)

## How to Debug Further

1. **Binary analysis:** Disassemble crash location with:
   ```bash
   xxd -s <offset> -l 32 /tmp/debug_test
   ```

2. **GDB debugging:**
   ```bash
   gdb -batch -ex "r" -ex "info registers" /tmp/debug_test
   ```

3. **Catch crash with script:**
   ```bash
   for i in $(seq 1 100); do
     gdb -batch -ex "r" -ex "info registers" /tmp/test 2>&1 | grep -q SIGSEGV && break
   done
   ```

4. **Test threshold:**
   ```bash
   crashes=0
   for i in $(seq 1 200); do
     /tmp/test > /dev/null 2>&1 || crashes=$((crashes+1))
   done
   echo "Crashes: $crashes/200"
   ```

## Conclusion

The root cause of the flaky SIGSEGV crashes remains unknown. The issue is NOT unmapped stack pages (probing doesn't help). The crashes consistently occur at page-aligned addresses during StringConcat operations when there are 28+ byteToHex calls. Further investigation is needed to identify the actual memory corruption source.

---
*Investigation conducted: December 2024*
