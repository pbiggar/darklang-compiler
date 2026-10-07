# SSA inlining test DSL

[`ssa.inline`](ssa.inline) contains text fixtures for the production SSA
inliner. Each case has a name, one or more functions, and expectations for the
last function after inlining. The parser builds the ANF eligibility input and
SSA body from the same function text.

```text
---NAME---
literal argument inlines
---FUNCTION---
helper(t0:Int64) -> Int64
let t1:Int64 = add(t0,1)
return t1
---FUNCTION---
caller() -> Int64
let t2:Int64 = call helper(41)
return t2
---EXPECT---
calls helper = 0
```

The body syntax has typed `let`, `if`/`else`/`endif`, and `return`. Atoms are
integer, Float, or Boolean literals and `tN` variables. Types are `Int64`,
`Float`, `Bool`, `Tuple<Int64,Int64>`, `Body`, `Tuple<Body,Body>`,
`Tuple<Body,Body,Bool>`, `Option<Int64>`, and `Option<Float>`. `Body` is a two-field managed
record stand-in for benchmark records and lists. Operations used by the
fixtures are `add`, `mul`, `div`, `gte`, `lt`, `bitand`, `bitxor`, `tuple`,
`tuple3`, `get`, `typed`, `copy`, `body`, `field`, `closure`, `closure_call`,
`some`, `none`, `payload`,
`some_float`, `none_float`, `payload_float`, and `call`.
The `---OPTIMIZE-SSA---` marker runs the SSA optimizer before inlining;
`---NO-INLINE---` checks the optimized body alone. The same plain-text format
also supplies the focused SSA optimization cases in `../ssa-optimization/ssa.opt`.
Bind IDs must be unique within a function. `---EXTERNAL-FUNCTION---` declares
an external inline candidate, separate from the local functions compiled in
the case.

Expectations use `calls FUNCTION = N`, `calls FUNCTION >= N`,
`ops mul|option_alloc|tuple_alloc|body_alloc = N`, or `blocks = N`. `after escape` runs SSA escape analysis before
checking an operation count. Two expansion forms keep repeated benchmark
patterns short: `repeat_add t1..t58 from t0` and
`project_chain callee 10 from t0 at t200 of Body aliases 2` (ten calls,
each followed by both tuple projections with two typed aliases per field),
and `call_chain callee 8 from t0 at t1`.

| Benchmark source | Inlining situation | Fixture |
| --- | --- | --- |
| `nbody/dark/main.dark` | `applyPair` has 59 bindings, returns fresh managed records, and has ten projected calls in `advanceStep`. | 59-binding producer with fresh records at ten sites and a 13-site growth boundary. |
| `nbody/dark/main.dark` | Projected tuple eligibility depends on aliases removed by SSA optimization. | Optimized-body case keeps both projections and removes the callee tuple. |
| `merkletrees/dark/main.dark` | `hashVal` calls `hashLoop` at literal index zero. | Eight-round expansion, nine-round boundary, and symbolic start. |
| `fasta`, `quicksort` | Lookup helpers return `Option<Int64>` on different paths. | Two-return scalar projection, three-return tag branch with payload read in a successor block, and a managed-result copy budget. |
| `spectral_norm` | The hot lookup returns `Option<Float>`. | Float Option return and projection, plus the shared three-return CFG shape. |
| `spectral_norm/dark/main.dark` | Small scalar helpers are called repeatedly. | Five sequential calls. |
| `huffman/dark/main.dark` | `treeComesFirst` calls branching scalar accessors. | Two calls to a two-return helper. |
| `raytracer/dark/main.dark` | Vector helpers form short nested call chains. | Three-deep helper chain. |
| `fannkuch/dark/main.dark` | `nextPerm` returns a projected triple but is recursive. | Recursive triple producer stays a call. |
| Lookup-heavy workloads | Eligible Stdlib wrappers appear at repeated call sites. | Eight external sites inline; nine stay bounded. |

The `nbody` fixture includes the two typed aliases found between real tuple
projections. It checks call removal and scalar replacement: only the returned
`Body` allocation survives escape analysis. The actual `advanceStep` has no
`applyPair` calls or temporary tuples, and five final body allocations. Its
Cachegrind count is 10,805,793 versus 10,805,795 for the task parent. The
three-return Option fixture now covers a tag branch and a successor-block
payload read. The resulting `quicksort` and `spectral_norm` MIR has no
temporary 16-byte Option allocations, matching the parent. The full parent
benchmark gate passes at aggregate ratio 0.999744.

`regex_lite` also has immediate tuple projections, while `fft` has nested
record helpers. Add fixtures for them when their optimized IR shows a distinct
inlining behavior. The current assertions cover call removal, expansion
limits, and allocation elimination for their stated shapes. A passing fixture
does not establish that the compiled benchmark has the same shape or performance.
