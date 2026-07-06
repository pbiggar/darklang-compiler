# Benchmark Investigation: Primes

## Executive Summary

**Benchmark:** primes (prime counting via trial division)
**Dark Performance:** 6,864,180 instructions (5.49x slower than Rust)
**Rust Performance:** 1,249,930 instructions (baseline)
**OCaml Performance:** 8,621,503 instructions (6.90x slower than Rust)

Dark is actually faster than OCaml on this benchmark, but 5.49x slower than Rust. This investigation identifies the key optimization opportunities.

## Benchmark Implementation Comparison

### Dark Implementation
```dark
def isqrt(n: Int64, guess: Int64) : Int64 =
    if guess * guess > n then guess - 1
    else if (guess + 1) * (guess + 1) > n then guess
    else isqrt(n, guess + 1)

def isDivisible(n: Int64, d: Int64, limit: Int64) : Bool =
    if d > limit then false
    else if n % d == 0 then true
    else isDivisible(n, d + 1, limit)

def isPrime(n: Int64) : Bool =
    if n < 2 then false
    else if n == 2 then true
    else if n % 2 == 0 then false
    else
        let limit = isqrt(n, 1) in
        if isDivisible(n, 3, limit) then false else true

def countPrimes(n: Int64, count: Int64) : Int64 =
    if n <= 1 then count
    else if isPrime(n) then countPrimes(n - 1, count + 1)
    else countPrimes(n - 1, count)
```

### Rust Implementation
```rust
fn is_prime(n: i64) -> bool {
    // ... guards ...
    let limit = (n as f64).sqrt() as i64;
    for d in 3..=limit {
        if n % d == 0 { return false; }
    }
    true
}
```

**Key Difference:** Rust uses hardware `fsqrt` instruction (1 cycle) while Dark recursively computes integer square root.

---

## Optimization 1: Integer Square Root via Hardware fsqrt

### Impact Estimate: ~30-40% improvement

### Root Cause
Dark computes `isqrt(n, 1)` using recursive trial-and-error, requiring O(sqrt(n)) iterations per prime check. For n=10000, sqrt(10000)=100, meaning up to 100 recursive calls per `isqrt`.

**Dark ANF (isqrt hot loop):**
```
Function isqrt:
let TempId 2 = t1 * t1              ; guess * guess
let TempId 3 = t2 > t0              ; > n?
if t3 then
    let TempId 4 = t1 - 1
    return t4
else
    let TempId 5 = t1 + 1           ; REDUNDANT: computed twice
    let TempId 6 = t1 + 1           ; REDUNDANT: same computation
    let TempId 7 = t5 * t6
    let TempId 8 = t7 > t0
    if t8 then
        return t1
    else
        let TempId 9 = t1 + 1       ; REDUNDANT: computed third time
        let TempId 10 = TailCall(isqrt, [t0, t9])
```

**Dark Generated Assembly (isqrt):**
```asm
isqrt_body:
    mul x1, x3, x3              ; guess * guess
    cmp x1, x4
    b.gt isqrt_L0               ; branch if guess^2 > n
    b isqrt_L1                  ; else path
isqrt_L1:
    add x2, x3, #1              ; guess + 1 (first)
    mov x1, x2
    mul x1, x2, x2              ; (guess+1)^2
    cmp x1, x4
    b.gt isqrt_L3               ; if (guess+1)^2 > n
    b isqrt_L4
isqrt_L4:
    add x2, x3, #1              ; guess + 1 (REDUNDANT - third computation!)
    mov x1, x4
    mov x1, x2
    mov x3, x2                  ; setup for tail call
    b isqrt_body                ; tail call loop
```

**Rust Generated Assembly (sqrt):**
```asm
ucvtf d0, x12       ; convert int to double
fsqrt d0, d0        ; SINGLE hardware instruction
fcvtzs x13, d0      ; convert back to int
```

### Evidence
- Dark: ~11 instructions per isqrt iteration, O(sqrt(n)) iterations
- Rust: 3 instructions total for sqrt
- For n=10000: Dark executes ~1100 instructions, Rust executes 3

### Implementation Approach
1. **Option A (Easy):** Add `Int64.sqrt` builtin that uses fsqrt
   - Runtime: ~2 cycles
   - Implementation: Add to stdlib, emit `ucvtf`/`fsqrt`/`fcvtzs` sequence

2. **Option B (Better):** Pattern-match isqrt recursion and replace with fsqrt
   - Compile-time detection of integer sqrt patterns
   - More complex but benefits existing code

### Files to Modify
- `src/codegen/lir_to_arm64.rs` - Add fsqrt emission
- `src/stdlib/` - Add Int64.sqrt builtin
- `src/types/` - Type checking for sqrt

---

## Optimization 2: Common Subexpression Elimination (CSE) in ANF

### Impact Estimate: ~15-25% improvement

### Root Cause
The ANF shows `guess + 1` computed 3 times in the same branch:

**Before optimization:**
```
let TempId 5 = t1 + 1           ; first computation
let TempId 6 = t1 + 1           ; REDUNDANT
let TempId 7 = t5 * t6
...
let TempId 9 = t1 + 1           ; REDUNDANT (third time)
```

**After CSE should be:**
```
let TempId 5 = t1 + 1           ; computed once
let TempId 7 = t5 * t5          ; reuse t5
...
let TempId 10 = TailCall(isqrt, [t0, t5])  ; reuse t5
```

### Evidence from Generated Assembly

**Dark (countPrimes hot loop) - Excessive register shuffling:**
```asm
countPrimes_L12:
    sub x3, x21, #1             ; n - 1
    add x21, x20, #1            ; count + 1
    mov x20, x3
    mov x19, x21
    ldur x16, [x29, #-104]      ; STACK ACCESS
    stur x16, [x29, #-96]       ; STACK ACCESS
    mov x4, x3                  ; REDUNDANT MOV
    mov x3, x21                 ; REDUNDANT MOV
    mov x21, x20                ; REDUNDANT MOV
    mov x20, x19                ; REDUNDANT MOV
    b countPrimes_body
```

The LIR shows 22 phi nodes in `countPrimes_body` - evidence of poor SSA construction or excessive variable copies.

**Rust (same logic):**
```asm
8418:   add x8, x8, #1          ; count++
841c:   cmp x11, x10            ; bounds check
8420:   mov x12, x11            ; one register move
8424:   b.ge 847c               ; branch
```

### Implementation Approach
1. Add CSE pass after ANF conversion
2. Hash expressions by operation + operands
3. Replace duplicates with references to first computation

### Files to Modify
- `src/anf/optimizations/mod.rs` - Add CSE pass
- `src/anf/optimizations/cse.rs` - New file for CSE implementation

---

## Optimization 3: Better Register Allocation Across Function Calls

### Impact Estimate: ~10-20% improvement

### Root Cause
The register allocator spills excessively around function calls, even when callee-saved registers are available.

**Dark LIR (before register allocation):**
```
Label "countPrimes_L16":
    SaveRegs([X1, X2, X3, X4, X5, X6, X7], [])  ; Save 7 registers!
    ArgMoves(X0 <- Reg X21, X1 <- Imm 1)
    v10100 <- Call(isqrt, ...)
    RestoreRegs([X1, X2, X3, X4, X5, X6, X7], [])  ; Restore 7 registers!
```

**After register allocation - excessive stack traffic:**
```
countPrimes_L16:
    sub sp, sp, #0x50           ; allocate stack frame
    stp x1, x2, [sp]            ; spill
    stp x3, x4, [sp, #16]       ; spill
    stp x5, x6, [sp, #32]       ; spill
    str x7, [sp, #48]           ; spill
    ... call ...
    ldp x1, x2, [sp]            ; restore
    ldp x3, x4, [sp, #16]       ; restore
    ldp x5, x6, [sp, #32]       ; restore
    ldr x7, [sp, #48]           ; restore
    add sp, sp, #0x50           ; deallocate
```

**Rust** - efficient register usage:
```asm
; No spills around division/modulo - keeps values in callee-saved registers
8464:   udiv x15, x12, x14
8468:   msub x16, x15, x14, x12
846c:   cinc x14, x14, lt
; Loop continues with no stack access
```

### Implementation Approach
1. Prioritize callee-saved registers (X19-X28) for loop-carried values
2. Reduce live range of temporaries to minimize register pressure
3. Consider splitting live ranges at call sites

### Files to Modify
- `src/codegen/register_allocator.rs` - Improve allocation strategy
- `src/lir/` - Better live range analysis

---

## Summary of Optimization Opportunities

| Optimization | Est. Impact | Complexity | Priority |
|-------------|-------------|------------|----------|
| 1. Hardware fsqrt for integer sqrt | 30-40% | Medium | High |
| 2. CSE in ANF | 15-25% | Medium | High |
| 3. Better register allocation | 10-20% | High | Medium |

**Combined potential improvement:** 40-65%, bringing Dark closer to 2-3x Rust performance.

## Instruction Count Analysis

### Hot Loop Comparison (isDivisible)

**Dark (per iteration):**
```
isDivisible_body:
    cmp x2, x3                  ; d > limit
    b.gt isDivisible_L0         ; 1 branch
    b isDivisible_L1            ; 1 branch (fallthrough would be better)
isDivisible_L1:
    sdiv x1, x4, x2             ; division
    msub x1, x1, x2, x4         ; modulo (combined)
    cbz x1, isDivisible_L3      ; branch on zero
    b isDivisible_L4            ; branch
isDivisible_L4:
    add x2, x2, #1              ; d + 1
    mov x1, x4                  ; UNNECESSARY
    mov x1, x2                  ; UNNECESSARY
    mov x1, x3                  ; UNNECESSARY
    b isDivisible_body
```
**Total: ~12 instructions per iteration**

**OCaml (per iteration):**
```
4ec04:   cmp x1, x2              ; d > limit
4ec08:   b.le 4ec1c
...
4ec28:   sdiv x16, x4, x3        ; division
4ec2c:   msub x5, x16, x3, x4    ; modulo
4ec44:   lsl x7, x5, #1          ; tagged integer manipulation
4ec48:   add x8, x7, #1
4ec4c:   cmp x8, #1
4ec64:   add x1, x1, #2          ; increment (tagged)
4ec68:   b 4ebf8
```
**Total: ~12 instructions per iteration (plus tagging overhead)**

**Rust (per iteration):**
```
8464:   udiv x15, x12, x14      ; division (unsigned - faster)
8468:   msub x16, x15, x14, x12 ; modulo
846c:   cinc x14, x14, lt       ; conditional increment
8470:   cset w15, ge            ; condition
8474:   cbnz x16, 8458          ; loop control
```
**Total: ~5-6 instructions per iteration**

### Key Takeaways

1. **Rust uses `udiv` (unsigned) vs Dark/OCaml using `sdiv` (signed)** - unsigned division is faster
2. **Rust eliminates branch with `cinc`/`cset`** - branchless iteration
3. **Dark has 3-4 unnecessary `mov` instructions** per iteration from poor register allocation
4. **Rust inlines everything** into a single tight loop with no function calls

---

## Appendix: Full IR Dumps

### Dark ANF (key functions)

See `/tmp/dark_ir_dump.txt` for complete IR dumps.

### Assembly Comparison Files

- Dark: `/tmp/dark_disasm.txt`
- Rust: `/tmp/rust_disasm.txt`
- OCaml: `/tmp/ocaml_disasm.txt`
