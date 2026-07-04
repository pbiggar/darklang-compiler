# Ackermann Benchmark Optimization Investigation

## Summary

The ackermann benchmark computes A(3, 12) = 32765 using the classic Ackermann function, a stress test for deep recursion and function call overhead.

**Performance Results (instruction counts):**
- Rust: 5,009,839,130 (baseline)
- Dark: 15,744,118,817 (3.14x Rust)
- OCaml: 8,946,136,766 (1.79x Rust)
- **Dark is ~3.14x slower than Rust and ~1.76x slower than OCaml**

## Benchmark Source Code

### Dark (`benchmarks/problems/ackermann/dark/main.dark`)
```dark
def ackermann(m: Int64, n: Int64) : Int64 =
    if m == 0 then n + 1
    else if n == 0 then ackermann(m - 1, 1)
    else ackermann(m - 1, ackermann(m, n - 1))

ackermann(3, 12)
```

### Rust (`benchmarks/problems/ackermann/rust/main.rs`)
```rust
fn ackermann(m: i64, n: i64) -> i64 {
    if m == 0 {
        n + 1
    } else if n == 0 {
        ackermann(m - 1, 1)
    } else {
        ackermann(m - 1, ackermann(m, n - 1))
    }
}

fn main() {
    let result = ackermann(3, 12);
    println!("{}", result);
}
```

### OCaml (`benchmarks/problems/ackermann/ocaml/main.ml`)
```ocaml
let rec ackermann m n =
  if m = 0 then n + 1
  else if n = 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

let () = Printf.printf "%d\n" (ackermann 3 12)
```

## Analysis

### Rust Optimized Code (~37 instructions for ackermann function)

```asm
; From Rust disassembly (ARM64)
000000000000870c <ackermann>:
    870c: cbz x0, 8798           ; if m == 0, jump to base case
    8710: stp x29, x30, [sp, #-32]!  ; save frame pointer and link register
    8714: str x19, [sp, #16]     ; save callee-saved register
    8718: mov x29, sp            ; set frame pointer
    871c: mov x19, x0            ; x19 = m (save m)
    8720: cbz x1, 8750           ; if n == 0, jump to case 2
    ; Case 3: recursive case
    8724: sub x1, x1, #1         ; n - 1
    8728: mov x0, x19            ; x0 = m
    872c: bl 870c                ; ackermann(m, n-1)
    8730: subs x8, x19, #1       ; x8 = m - 1, set flags
    8734: mov x1, x0             ; x1 = ackermann(m, n-1)
    8738: b.eq 8790              ; if m == 1, skip to epilogue
    873c: cbnz x1, 875c          ; if n != 0, go to loop
    ; ... more optimized paths
    8798: add x0, x1, #1         ; base case: return n + 1
    879c: ret
```

**Key observations:**
1. Rust uses `cbz`/`cbnz` (compare-and-branch-on-zero) for efficient branching
2. Only 1 callee-saved register needed (x19 for m)
3. LLVM performs loop unrolling for certain patterns
4. Multiple specialized fast paths for common cases

### OCaml Optimized Code (~25 instructions for ackermann function)

```asm
; From OCaml disassembly (ARM64)
000000000004e938 <camlMain__ackermann_267>:
    4e938: sub sp, sp, #16       ; allocate stack frame
    4e93c: str x30, [sp, #8]     ; save return address
    4e940: ldr x16, [x28]        ; GC stack limit check
    4e944: cmp x27, x16          ; compare stack pointer
    4e948: b.ls 4e994            ; if stack overflow, call GC
    4e94c: cmp x0, #1            ; if m == 0 (tagged: 1 = 0)
    4e950: b.ne 4e964            ; if not zero, branch
    4e954: add x0, x1, #2        ; return n + 1 (tagged)
    4e958: ldr x30, [sp, #8]     ; restore return address
    4e95c: add sp, sp, #16       ; deallocate frame
    4e960: ret
    4e964: cmp x1, #1            ; if n == 0
    4e968: b.ne 4e978            ; if not zero, branch
    4e96c: orr x1, xzr, #3       ; n = 1 (tagged)
    4e970: sub x0, x0, #2        ; m = m - 1 (tagged)
    4e974: b 4e940               ; tail call via jump (!)
    4e978: str x0, [sp]          ; save m on stack
    4e97c: sub x1, x1, #2        ; n - 1 (tagged)
    4e980: bl 4e938              ; ackermann(m, n-1)
    4e984: mov x1, x0            ; result to x1
    4e988: ldr x8, [sp]          ; restore m
    4e98c: sub x0, x8, #2        ; m - 1 (tagged)
    4e990: b 4e940               ; tail call via jump (!)
```

**Key observations:**
1. OCaml uses tagged integers (multiply by 2, add 1 for immediates)
2. **Excellent tail call optimization** - both recursive cases jump back instead of calling
3. Only needs stack save for non-tail recursive call
4. GC stack check at entry (overhead but necessary)

### Dark Generated Code (LIR after Register Allocation)

```
ackermann:
  Label "ackermann_entry":
    X24 <- Mov(Reg X0)           ; \
    X23 <- Mov(Reg X1)           ;  |
    X16 <- Mov(Reg X22)          ;  |
    X22 <- Mov(Reg X19)          ;  | 9 consecutive MOV instructions!
    X19 <- Mov(Reg X23)          ;  | This is excessive register shuffling
    X23 <- Mov(Reg X20)          ;  |
    X20 <- Mov(Reg X24)          ;  |
    X24 <- Mov(Reg X21)          ;  |
    X21 <- Mov(Reg X16)          ; /
    Jump(Label "ackermann_body")

  Label "ackermann_body":
    BranchZero(X20, Label "ackermann_L0", Label "ackermann_L1")

  Label "ackermann_L0":
    X19 <- Add(X19, Imm 1)
    X19 <- Mov(Reg X19)          ; REDUNDANT self-move!
    Jump(Label "ackermann_L2")

  Label "ackermann_L1":
    BranchZero(X19, Label "ackermann_L3", Label "ackermann_L4")

  Label "ackermann_L2":
    X0 <- Mov(Reg X19)
    Ret

  Label "ackermann_L3":
    X21 <- Sub(X20, Imm 1)
    X20 <- Mov(Reg X21)
    X19 <- Mov(Imm 1)
    Jump(Label "ackermann_body")  ; Good: tail call converted to jump

  Label "ackermann_L4":
    X24 <- Sub(X20, Imm 1)
    X23 <- Sub(X19, Imm 1)
    SaveRegs([], [])             ; Empty save/restore pairs
    ArgMoves(X0 <- Reg X20, X1 <- Reg X23)
    X22 <- Call(ackermann, [Reg X20, Reg X23])
    RestoreRegs([], [])
    X22 <- Mov(Reg X0)
    X20 <- Mov(Reg X24)
    X19 <- Mov(Reg X22)
    Jump(Label "ackermann_body")  ; Good: outer tail call converted to jump

  Label "ackermann_L5":          ; DEAD CODE - never reached!
    X19 <- Mov(Reg X19)
    Jump(Label "ackermann_L2")
```

**Major issues identified:**

1. **Excessive register shuffling at entry (9 MOVs)** - The function entry has 9 consecutive MOV instructions to set up registers. This is executed on every function call.

2. **Dead code block** (ackermann_L5) that is never jumped to

3. **BranchZero pattern vs cbz** - Using separate compare-and-branch instead of fused ARM64 `cbz`/`cbnz` instructions

4. **Empty SaveRegs/RestoreRegs pairs** - Still generating markers even when no registers need saving

## Identified Optimization Opportunities

### 1. Register Allocation Entry Block Optimization

**Impact: ~20-30% performance improvement (estimated)**

**Root Cause:**
The function entry block contains 9 consecutive MOV instructions to shuffle registers into their assigned locations. This is a result of phi node resolution and poor register assignment for incoming parameters.

**Evidence from Dark LIR:**
```
Label "ackermann_entry":
    X24 <- Mov(Reg X0)
    X23 <- Mov(Reg X1)
    X16 <- Mov(Reg X22)
    X22 <- Mov(Reg X19)
    X19 <- Mov(Reg X23)
    X23 <- Mov(Reg X20)
    X20 <- Mov(Reg X24)
    X24 <- Mov(Reg X21)
    X21 <- Mov(Reg X16)
    Jump(Label "ackermann_body")
```

**Evidence from Rust (only 2 moves at entry):**
```asm
871c: mov x19, x0            ; Only need to save m
```

**Evidence from OCaml (0 entry moves - parameters used directly):**
```asm
; OCaml uses x0 and x1 directly without moves
4e94c: cmp x0, #1
```

**Implementation Approach:**
1. Improve register coalescing to assign function parameters directly to their use locations
2. Detect and optimize parallel move sequences using cycle detection algorithm
3. Pre-color function parameters to their ABI registers when possible

**Files to Modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Improve coalescing and parameter assignment
- Consider adding a move coalescing pass after phi elimination

---

### 2. Dead Code Elimination (Post-Register Allocation)

**Impact: ~2-5% improvement (reduces code size, improves icache)**

**Root Cause:**
The `ackermann_L5` block is never jumped to but still exists in the output.

**Evidence:**
```
Label "ackermann_L5":          ; No incoming edges!
    X19 <- Mov(Reg X19)
    Jump(Label "ackermann_L2")
```

**Implementation Approach:**
1. Build CFG reachability from entry
2. Remove unreachable blocks before code generation

**Files to Modify:**
- `src/DarkCompiler/passes/4.5_LIR_Optimizations.fs` - Add DCE pass
- Or `src/DarkCompiler/passes/6_CodeGen.fs` - Skip unreachable blocks

---

### 3. BranchZero to CBZ/CBNZ Fusion

**Impact: ~5-10% performance improvement (estimated)**

**Root Cause:**
Dark generates `BranchZero` as a pattern, but this may not map efficiently to ARM64's `cbz`/`cbnz` instructions which combine compare-and-branch.

**Evidence from Dark LIR:**
```
BranchZero(X20, Label "ackermann_L0", Label "ackermann_L1")
```

**Evidence from Rust (using cbz directly):**
```asm
870c: cbz x0, 8798            ; Compare-and-branch-if-zero fused
8720: cbz x1, 8750
873c: cbnz x1, 875c           ; Compare-and-branch-if-not-zero
```

**Implementation Approach:**
1. Ensure `BranchZero` in LIR maps to `cbz`/`cbnz` instructions in code generation
2. Pattern-match `Cmp(reg, 0)` followed by conditional branch and convert to cbz/cbnz

**Files to Modify:**
- `src/DarkCompiler/passes/6_CodeGen.fs` - Ensure BranchZero generates cbz/cbnz

---

### 4. Empty SaveRegs/RestoreRegs Elimination

**Impact: ~2-5% performance improvement (estimated)**

**Root Cause:**
Empty save/restore pairs still appear in IR:
```
SaveRegs([], [])
...
RestoreRegs([], [])
```

**Implementation Approach:**
Skip code generation when register lists are empty.

**Files to Modify:**
- `src/DarkCompiler/passes/6_CodeGen.fs` - Check for empty lists before generating

---

## Summary of Expected Improvements

| Optimization | Estimated Impact | Complexity |
|--------------|-----------------|------------|
| Register Entry Block Optimization | 20-30% | High |
| Dead Code Elimination | 2-5% | Low |
| BranchZero to CBZ/CBNZ Fusion | 5-10% | Medium |
| Empty SaveRegs Elimination | 2-5% | Low |

**Total estimated improvement: ~35-50%** (bringing Dark from 3.14x to ~1.5-2x Rust)

Note: Dark already has good tail call optimization (both tail-recursive cases use jumps), so the main issues are register allocation quality and instruction selection.

## Recommended Implementation Order

1. **Empty SaveRegs Elimination** - Quick win, very low complexity
2. **Dead Code Elimination** - Quick win, low complexity
3. **BranchZero to CBZ/CBNZ Fusion** - Medium complexity, good improvement
4. **Register Entry Block Optimization** - High complexity, highest impact

## Appendix: Full IR Dumps

### Dark ANF (after Tail Call Detection)
```
Function ackermann:
let TempId 2 = t0 == 0
if t2 then
let TempId 3 = t1 + 1
return t3
else
let TempId 4 = t1 == 0
if t4 then
let TempId 5 = t0 - 1
let TempId 6 = TailCall(ackermann, [t5, 1])
return t6
else
let TempId 7 = t0 - 1
let TempId 8 = t1 - 1
let TempId 9 = ackermann(t0, t8)
let TempId 10 = TailCall(ackermann, [t7, t9])
return t10

Main:
let TempId 11 = ackermann(3, 12)
return t11
```

### Dark MIR (Control Flow Graph)
```
Function ackermann:
  ackermann_entry:
    jump ackermann_body
  ackermann_L0:
    v3 <- v1 + 1 : TInt64
    v11 <- v3 : TInt64
    jump ackermann_L2
  ackermann_L1:
    v4 <- v1 == 0 : TInt64
    branch v4 ? ackermann_L3 : ackermann_L4
  ackermann_L2:
    ret v11
  ackermann_L3:
    v5 <- v0 - 1 : TInt64
    v0 <- v5 : TInt64
    v1 <- 1 : TInt64
    jump ackermann_body
  ackermann_L4:
    v7 <- v0 - 1 : TInt64
    v8 <- v1 - 1 : TInt64
    v9 <- Call(ackermann, [v0, v8])
    v0 <- v7 : TInt64
    v1 <- v9 : TFunction ([TInt64; TInt64], TInt64)
    jump ackermann_body
  ackermann_L5:
    v11 <- v12 : TInt64
    jump ackermann_L2
  ackermann_body:
    v2 <- v0 == 0 : TInt64
    branch v2 ? ackermann_L0 : ackermann_L1
```

### Dark LIR (After Register Allocation)
```
ackermann:
  Label "ackermann_L0":
    X19 <- Add(X19, Imm 1)
    X19 <- Mov(Reg X19)
    Jump(Label "ackermann_L2")
  Label "ackermann_L1":
    BranchZero(X19, Label "ackermann_L3", Label "ackermann_L4")
  Label "ackermann_L2":
    X0 <- Mov(Reg X19)
    Ret
  Label "ackermann_L3":
    X21 <- Sub(X20, Imm 1)
    X20 <- Mov(Reg X21)
    X19 <- Mov(Imm 1)
    Jump(Label "ackermann_body")
  Label "ackermann_L4":
    X24 <- Sub(X20, Imm 1)
    X23 <- Sub(X19, Imm 1)
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X20, X1 <- Reg X23)
    X22 <- Call(ackermann, [Reg X20, Reg X23])
    RestoreRegs([], [])
    X22 <- Mov(Reg X0)
    X20 <- Mov(Reg X24)
    X19 <- Mov(Reg X22)
    Jump(Label "ackermann_body")
  Label "ackermann_L5":
    X19 <- Mov(Reg X19)
    Jump(Label "ackermann_L2")
  Label "ackermann_body":
    BranchZero(X20, Label "ackermann_L0", Label "ackermann_L1")
  Label "ackermann_entry":
    X24 <- Mov(Reg X0)
    X23 <- Mov(Reg X1)
    X16 <- Mov(Reg X22)
    X22 <- Mov(Reg X19)
    X19 <- Mov(Reg X23)
    X23 <- Mov(Reg X20)
    X20 <- Mov(Reg X24)
    X24 <- Mov(Reg X21)
    X21 <- Mov(Reg X16)
    Jump(Label "ackermann_body")
```
