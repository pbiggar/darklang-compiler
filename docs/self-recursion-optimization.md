# Self-Recursion to Loop Optimization

## Overview

Self-recursive tail calls are optimized to loops, eliminating function call overhead entirely. This converts:

```dark
def sumTo(n: Int64, acc: Int64): Int64 =
    if n <= 0 then acc else sumTo(n - 1, acc + n)
```

Into an efficient loop that updates parameters in place and jumps back to the loop header.

## How It Works

### 1. Detection (ANF_to_MIR.fs)

When converting ANF to MIR, we detect `TailCall` where the function name matches the current function:

```fsharp
| ANF.TailCall (funcName, args) when funcName = builder.FuncName ->
    // Self-recursive call detected
```

### 2. Loop Structure Generation

Instead of generating a tail call, we emit:

1. **Parameter assignments**: `Mov param_i, arg_i` for each argument
2. **Jump to loop body**: `Jump funcName_body`

The function entry creates a separate entry block that jumps to the loop body, ensuring the loop body has two predecessors (entry and recursive call) so SSA can insert phi nodes.

```
entry_block  →  loop_body  ←  recurse_block
                    ↓
               return_block
```

### 3. SSA Construction (3.1_SSA_Construction.fs)

SSA construction sees multiple definitions of each parameter:
- In entry block: from function arguments
- In recurse block: from the Mov assignments

This triggers phi node insertion at the loop body:
```
loop_body:
  n_ssa = phi [(n_entry, entry), (n_recurse, recurse)]
  acc_ssa = phi [(acc_entry, entry), (acc_recurse, recurse)]
```

### 4. SSA Destruction (3.9_SSA_Destruction.fs)

Phi nodes are converted back to copies at the end of predecessor blocks:
- Entry block gets: `n_ssa = n_entry; acc_ssa = acc_entry`
- Recurse block gets: `n_ssa = n_recurse; acc_ssa = acc_recurse`

### 5. Register Allocation & Code Generation

The loop becomes tight ARM64 code:
```arm64
loop_body:
    cmp x3, #0x0           // Check n <= 0
    b.le return            // If so, return
    sub x2, x3, #0x1       // n - 1
    add x1, x4, x3         // acc + n
    mov x4, x1             // Update acc
    mov x3, x2             // Update n
    b loop_body            // Loop back
```

## Swap Pattern Handling

For calls like `swapInt(b, a, n-1)` where arguments reference parameters that will be overwritten, we use temp registers:

```fsharp
// Detect if any argument directly references a parameter
let needsTemps = argOperands |> List.exists argReferencesParam

if needsTemps then
    // Capture all values to temps first
    temp0 = b
    temp1 = a
    // Then assign to params
    a = temp0
    b = temp1
```

For computed expressions like `sumTo(n-1, acc+n)`, no temps are needed since the values are already in fresh registers.

## Current Limitations

1. **Float parameters**: Functions with float parameters fall back to regular TailCall (phi nodes lack type information)

2. **Mutual recursion**: Only self-recursion is optimized; `A → B → A` chains use regular tail calls

## Performance Results

| Benchmark | Before | After | Improvement |
|-----------|--------|-------|-------------|
| sum_to_n | ~20M instructions | 9M instructions | 55% reduction |
| collatz | 292M instructions | 184M instructions | 37% reduction |

---

## Remaining Optimization Opportunities

### 1. Compare-and-Branch Fusion (HIGH IMPACT)

**Current code:**
```arm64
cmp x3, #0x0         // Compare n with 0
cset x1, le          // x1 = (n <= 0) ? 1 : 0
cbnz x1, return      // If x1 != 0, branch
```

**Optimal code:**
```arm64
cmp x3, #0x0         // Compare n with 0
b.le return          // If n <= 0, branch directly
```

**Savings:** ~2 instructions per loop iteration

**Implementation options:**
- Pattern match `cset + cbnz` in code generation to emit conditional branch
- Change MIR Branch to take comparison expression instead of boolean
- Add peephole pass after code generation

### 2. Register Coalescing (MEDIUM IMPACT)

**Current loop:**
```arm64
sub x2, x3, #0x1     // x2 = n - 1
add x1, x4, x3       // x1 = acc + n
mov x4, x1           // Copy for phi
mov x3, x2           // Copy for phi
```

**Optimal loop:**
```arm64
add x1, x1, x0       // acc += n (same register)
sub x0, x0, #0x1     // n-- (same register)
```

**Savings:** 2 instructions per iteration

**Implementation:** Improve register allocator with move coalescing (Chaitin-Briggs style)

### 3. Loop Rotation (LOW-MEDIUM IMPACT)

Put condition check at loop end instead of beginning to eliminate one branch per iteration.

**Current:** `entry → header → body → header`
**Optimal:** `entry → body → check → body` (do-while form)

### 4. Float Parameter Support

Add type information to phi nodes to support self-recursion optimization for functions with float parameters:

```fsharp
// Current
| Phi of dest:VReg * sources:(Operand * Label) list

// Proposed
| Phi of dest:VReg * sources:(Operand * Label) list * valueType:AST.Type option
```
