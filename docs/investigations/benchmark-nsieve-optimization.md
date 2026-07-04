# NSieve Benchmark Optimization Investigation

## Summary

The nsieve benchmark implements the Sieve of Eratosthenes to count prime numbers. The Dark implementation uses a fundamentally different data structure (Dict/HAMT) compared to Rust/OCaml (contiguous arrays), causing both algorithmic complexity issues and stack overflow at standard benchmark sizes.

**Performance Results (instruction counts):**
- Rust: 234,163,043 (n=100000, 100 iterations)
- OCaml: 559,365,264 (n=100000, 100 iterations)
- **Dark: Stack overflow at n=100000; reduced to n=1000 (168 primes)**

**Key Issues:**
1. **Data structure mismatch**: Dark uses `Dict<Int64, Bool>` (HAMT) vs arrays
2. **Stack overflow**: Deep recursion in `markMultiples` causes stack overflow at scale
3. **No mutable arrays**: Dark lacks mutable array primitives needed for efficient sieve

## Benchmark Source Code

### Dark (`benchmarks/problems/nsieve/dark/main.dark`)
```dark
// Mark all multiples of prime as composite
def markMultiples(j: Int64, step: Int64, n: Int64, composites: Dict<Int64, Bool>) : Dict<Int64, Bool> =
    if j > n then composites
    else markMultiples(j + step, step, n, Stdlib.Dict.set<Int64, Bool>(composites, j, true))

// Run sieve for numbers from i to n
def sieveLoop(i: Int64, n: Int64, composites: Dict<Int64, Bool>) : Dict<Int64, Bool> =
    if i > n then composites
    else
        let newComposites =
            if Stdlib.Dict.contains<Int64, Bool>(composites, i) then composites
            else markMultiples(i + i, i, n, composites)
        in
        sieveLoop(i + 1, n, newComposites)

// Count primes (numbers not in composites set)
def countPrimes(i: Int64, n: Int64, composites: Dict<Int64, Bool>, count: Int64) : Int64 =
    if i > n then count
    else
        let newCount = if Stdlib.Dict.contains<Int64, Bool>(composites, i) then count else count + 1 in
        countPrimes(i + 1, n, composites, newCount)

def nsieve(n: Int64) : Int64 =
    let composites = sieveLoop(2, n, Stdlib.Dict.empty<Int64, Bool>()) in
    countPrimes(2, n, composites, 0)

// Single run with n=1000 due to stack depth limits
nsieve(1000)
```

### Rust (`benchmarks/problems/nsieve/rust/main.rs`)
```rust
fn nsieve(n: usize) -> usize {
    let mut is_prime = vec![true; n + 1];  // O(n) contiguous array
    let mut count = 0;

    for i in 2..=n {
        if is_prime[i] {
            count += 1;
            let mut j = i + i;
            while j <= n {
                is_prime[j] = false;  // O(1) array write
                j += i;
            }
        }
    }
    count
}
```

### OCaml (`benchmarks/problems/nsieve/ocaml/main.ml`)
```ocaml
let nsieve n =
  let is_prime = Array.make (n + 1) true in  (* O(n) mutable array *)
  let count = ref 0 in
  for i = 2 to n do
    if is_prime.(i) then begin
      incr count;
      let j = ref (i + i) in
      while !j <= n do
        is_prime.(!j) <- false;  (* O(1) array write *)
        j := !j + i
      done
    end
  done;
  !count
```

## Analysis

### Problem 1: Dict vs Array - Algorithmic Complexity

The nsieve algorithm fundamentally requires:
1. **O(1) random access writes** to mark composites
2. **O(n) space** for a dense boolean array

**Rust/OCaml approach:**
- Allocate contiguous array of `n` booleans
- Each write: single store instruction (`strb wzr, [base, offset]`)
- Total marking operations: O(n log log n) at O(1) each

**Dark's Dict approach:**
- Uses HAMT (Hash Array Mapped Trie) for sparse storage
- Each write: O(log n) tree traversal + allocations
- Creates new tree nodes on each insertion (persistent/immutable)
- Total marking operations: O(n log log n) at O(log n) each

**Evidence from Dark IR - markMultiples hot loop:**
```
Function markMultiples:
  markMultiples_L1:
    v297 <- v292 + v293              ; j + step
    v298 <- Call(Stdlib.Dict.set_i64_bool, [...])  ; COMPLEX FUNCTION CALL
    ; ... recursive call to markMultiples
```

**Evidence from Rust disassembly - inner marking loop (only 4 instructions!):**
```asm
940c: strb wzr, [x19, x10]    ; is_prime[j] = false (SINGLE INSTRUCTION)
9410: add x10, x10, x8        ; j += step
9414: cmp x10, x21            ; j <= n
9418: b.cc 940c               ; loop back
```

**Complexity comparison for n=100000:**
| Operation | Rust/OCaml | Dark |
|-----------|-----------|------|
| Array init | O(n) | O(1) |
| Mark composite | O(1) | O(log n) |
| Check if prime | O(1) | O(log n) |
| Total | O(n log log n) | O(n log log n * log n) |

### Problem 2: Stack Overflow from Deep Recursion

**Root Cause:**
The `markMultiples` function makes a recursive call for each composite number. For n=100000, marking multiples of 2 alone requires ~50000 recursive calls - each consuming stack space.

**Current evidence from MIR - markMultiples is NOT tail-call optimized:**
```
Function markMultiples:
  markMultiples_L1:
    v334 <- v329 + v330 : TInt64
    v452 <- Call(__hash_i64, [v329])
    v453 <- Call(Stdlib.__HAMT.__setHelper_i64_bool, [v332, v329, v452, true, 0])
    v335 <- v453 : TDict (TInt64, TBool)
    v336 <- Call(markMultiples, [v334, v330, v331, v335])
    RefCountDec(v453, size=8, kind=dict)
    v1011 <- v336 : TDict (TInt64, TBool)
    jump markMultiples_L2
```

As of commit `d658fdaf`, `markMultiples` still has a post-recursive
`RefCountDec`, so the self-call is not in tail position in MIR. This means the
benchmark has two stack contributors: the sieve's own recursive marking loop and
the recursive `Stdlib.__HAMT.__setHelper_i64_bool` path used by each Dict write.
The Dict helper can recurse through internal nodes and allocates memory on each
level.

**Evidence from `Stdlib.__HAMT.__setHelper_i64_bool` complexity:**
```
Function Stdlib.__HAMT.__setHelper_i64_bool:
  ; Checks tag (leaf, internal, collision)
  ; For internal nodes: recursively calls __setHelper with depth+1
  v280 <- Call(Stdlib.__HAMT.__setHelper_i64_bool, [v277, v247, v248, v249, v279])
  ; Can recurse up to depth 10 before creating collision node
```

### Problem 3: Immutable Data Structure Overhead

**Root Cause:**
Dict operations create new nodes on every insertion. For marking ~100000 composite numbers, this creates ~100000 new HAMT nodes.

**Evidence from Dict.set operations:**
```
Function Stdlib.Dict.__setHelper_i64_bool:
  ; When updating existing internal node:
  v190 <- RawAlloc(...)               ; Allocate new internal node
  v192 <- __copyChildrenWithInsert... ; Copy children to new node
  ; When creating new leaf:
  v204 <- RawAlloc(16)                ; Allocate new leaf (16 bytes)
  RawSet(v205, 0, t202)               ; Set key
  RawSet(v205, 8, t203)               ; Set value
```

Each Dict.set creates 1-10 new allocations depending on tree depth, while Rust/OCaml's array write is a single store instruction with zero allocations.

## Identified Optimization Opportunities

### 1. Add Mutable Array Primitive

**Impact: 10-50x performance improvement for this benchmark**

**Root Cause:**
Dark lacks mutable array primitives. The sieve algorithm inherently requires O(1) random access mutation. Using Dict as a workaround introduces logarithmic overhead per operation.

**Proposed Solution:**
Add `Stdlib.Array` module with:
```dark
// Stdlib.Array
def make<T>(size: Int64, default: T) : Array<T>
def get<T>(arr: Array<T>, index: Int64) : T
def set<T>(arr: Array<T>, index: Int64, value: T) : Unit
def length<T>(arr: Array<T>) : Int64
```

**Implementation Approach:**
1. Add `TArray` type to AST
2. Implement array operations as intrinsics:
   - `make`: calls `mmap` or `malloc` + memset
   - `get`: bounds check + load
   - `set`: bounds check + store
3. Track array lifetime for deallocation

**Files to Modify:**
- `src/DarkCompiler/AST.fs` - Add TArray type
- `src/DarkCompiler/Stdlib.fs` - Add Array module definition
- `src/DarkCompiler/passes/6_CodeGen.fs` - Implement array intrinsics
- `src/DarkCompiler/passes/2.5_RefCountInsertion.fs` - Handle array lifetime

**With arrays, Dark nsieve could be:**
```dark
def nsieve(n: Int64) : Int64 =
    let is_prime = Stdlib.Array.make<Bool>(n + 1, true) in
    let count = nsieveLoop(2, n, is_prime, 0) in
    count

def nsieveLoop(i: Int64, n: Int64, is_prime: Array<Bool>, count: Int64) : Int64 =
    if i > n then count
    else
        if Stdlib.Array.get(is_prime, i) then
            let _ = markMultiples(i + i, i, n, is_prime) in
            nsieveLoop(i + 1, n, is_prime, count + 1)
        else
            nsieveLoop(i + 1, n, is_prime, count)
```

---

### 2. Increase Default Stack Size

**Impact: Allows running full benchmark size (n=100000)**

**Root Cause:**
The default stack size (typically 1MB on Linux) is insufficient for deep recursion. With ~50000 recursive calls for marking multiples of 2, and each frame using ~48 bytes, we need ~2.4MB of stack.

**Evidence:**
```
- At n=1000: ~500 stack frames max, runs successfully
- At n=100000: ~50000 stack frames max, stack overflow
```

**Implementation Approaches:**

A. **Compiler flag for stack size:**
```bash
dark --stack-size=8M main.dark
```

B. **Runtime stack growth:**
Use `sigaltstack` to detect stack overflow and grow stack dynamically.

C. **Trampolined recursion:**
Transform deep recursion to use an explicit stack in the heap.

**Files to Modify:**
- `src/DarkCompiler/passes/8_Binary_Generation_ELF.fs` - Set stack size in ELF headers
- Or add runtime stack growth in generated code

---

### 3. Dict Path Copying Optimization

**Impact: ~30% improvement in Dict-heavy code**

**Root Cause:**
Current HAMT implementation copies all children when updating an internal node. Could use path copying only for affected branches.

**Evidence from IR:**
```
Function Stdlib.Dict.__copyInternalWithUpdate_i64_bool:
  v178 <- RawAlloc(...)                            ; Allocate new node
  v180 <- __copyChildren_i64_bool(old, new, ...)   ; Copy ALL children
```

**Proposed Solution:**
Implement more efficient path copying that only copies the spine from root to modified leaf.

**Files to Modify:**
- Need to examine Dict implementation (likely in separate .dark file or generated code)

---

### 4. Specialize Dict for Bool Values (Bitset)

**Impact: 8x space reduction, improved cache locality**

**Root Cause:**
`Dict<Int64, Bool>` stores a full 8-byte pointer for each boolean value. For dense integer keys (like in nsieve), a bitset would be far more efficient.

**Evidence:**
Current: Each composite number stores `(key: 8 bytes, value: 8 bytes)` = 16 bytes
Bitset: Each composite number uses 1 bit = 0.125 bytes

**Proposed Solution:**
Detect `Dict<Int64, Bool>` pattern where keys are dense integers and optimize to bitset:
```dark
// Internally represented as:
type Bitset = {
    base: Int64,     // Minimum key
    bits: Array<Int64>  // Packed bits, 64 per word
}
```

**Files to Modify:**
- Could be a library-level optimization in stdlib
- Or a compiler optimization that detects the pattern

---

## Summary of Expected Improvements

| Optimization | Estimated Impact | Complexity | Blocks Benchmark |
|--------------|-----------------|------------|------------------|
| Mutable Arrays | 10-50x | High | Yes |
| Increase Stack Size | Enables full size | Low | Yes |
| Dict Path Copying | ~30% | Medium | No |
| Bitset Specialization | 8x for this pattern | Medium | No |

**Total estimated improvement with arrays: 10-50x faster** (bringing Dark closer to Rust/OCaml)

## Recommended Implementation Order

1. **Increase Stack Size** - Quick fix to enable running full benchmark
2. **Mutable Arrays** - Essential for algorithmic parity
3. **Dict Path Copying** - General Dict performance improvement
4. **Bitset Specialization** - Advanced optimization for specific patterns

## Appendix: Full IR Dumps

### Dark ANF (key functions)
```
Function markMultiples:
let TempId 296 = t292 > t294
if t296 then
return t295
else
let TempId 297 = t292 + t293
let TempId 298 = Stdlib.Dict.set_i64_bool(t295, t292, true)
let TempId 299 = markMultiples(t297, t293, t294, t298)
return t299

Function sieveLoop:
let TempId 303 = t300 > t301
if t303 then
return t302
else
let TempId 304 = Stdlib.Dict.contains_i64_bool(t302, t300)
let TempId 305 = t300 + t300
let TempId 306 = markMultiples(t305, t300, t301, t302)
let TempId 307 = if t304 then t302 else t306
let TempId 308 = t307
let TempId 309 = t300 + 1
let TempId 310 = sieveLoop(t309, t301, t308)
return t310
```

### Dark MIR (Control Flow Graph)
```
Function markMultiples:
  markMultiples_body:
    v296 <- v292 > v294 : TInt64
    branch v296 ? markMultiples_L0 : markMultiples_L1
  markMultiples_L0:
    v300 <- v295 : TInt64
    jump markMultiples_L2
  markMultiples_L1:
    v297 <- v292 + v293 : TInt64
    v298 <- Call(Stdlib.Dict.set_i64_bool, [v295, v292, true])
    ; Parameters shuffled for loop back
    v292 <- v297, v293 <- v293, v294 <- v294, v295 <- v298
    jump markMultiples_body
  markMultiples_L2:
    ret v300

Function sieveLoop:
  sieveLoop_body:
    v303 <- v300 > v301 : TInt64
    branch v303 ? sieveLoop_L0 : sieveLoop_L1
  sieveLoop_L1:
    v373 <- Call(Stdlib.Dict.__containsHelper_i64_bool, [v302, v300, v300, 0])
    v305 <- v300 + v300 : TInt64
    v306 <- Call(markMultiples, [v305, v300, v301, v302])
    branch v304 ? sieveLoop_L3 : sieveLoop_L4
```

### Rust Hot Loop (Assembly)
```asm
; Inner marking loop - only 4 instructions!
940c: strb wzr, [x19, x10]    ; is_prime[j] = false
9410: add x10, x10, x8        ; j += step
9414: cmp x10, x21            ; j <= n
9418: b.cc 940c               ; loop if j < n

; Outer sieve loop
93e8: ldrb w10, [x19, x8]     ; load is_prime[i]
93ec: cmp x8, x22             ; compare i with n-1
93f0: csinc x9, x22, x8, eq   ; next = (i == n-1) ? n-1 : i+1
93f4: cmp w10, #1             ; is_prime[i] == true?
93f8: b.ne 941c               ; skip marking if not prime
```

### OCaml Hot Loop (Assembly)
```asm
; Inner marking loop
531f0: cmp x14, x9            ; j <= n
531f4: b.gt 53228             ; exit if j > n
53208: orr x21, xzr, #0x1     ; value = false (OCaml encoding)
5320c: stur x21, [x20, #-4]   ; is_prime[j] = false
53210: add x22, x14, x6       ; j += step
53214: sub x14, x22, #1       ; adjust for OCaml tagging
53220: b.hi 531f0             ; loop back
```
