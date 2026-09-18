# Optimization Catalog

This is a compact map of retained optimization behavior. Source and focused
before/after fixtures are authoritative; generated benchmark documents own
current performance measurements. Historical trial ratios and sandbox-session
notes are intentionally omitted because they become stale and are recoverable
from Git history.

## Retained transformation index

- **Constant folding:** Float negation, absolute value, square root,
  Int64-to-Float, Float-to-Int64, Float-to-bits, Float comparisons, string
  concatenation, UInt64 comparisons, typed canonical-buffer equality, Int64
  shifts, Int64 bitwise operations, UInt64 bitwise operations, and UInt64
  arithmetic.
- **Algebraic simplification:** signed and unsigned division/modulo identities;
  integer, Float, Boolean, and string self-comparisons; bitwise and Boolean
  idempotence, absorption, complement, zero, and all-ones rules; empty string
  concatenation; double not/negation; Float absolute-value rules; identical and
  Boolean-literal branches; negated branch/comparison folding; shift identities;
  subtraction/addition identities; integer reassociation, cancellation, and
  common-factor combination; and safe Float/UInt64 multiplicative identities.
- **Strength reduction:** integer self-addition, Float multiplication by two,
  and signed/unsigned multiplication, division, and modulo by powers of two.
- **Common-subexpression elimination:** dominator-scoped MIR scalar reuse,
  effect-free direct-call reuse, barrier-aware scalar heap-load reuse through
  supported pure scalar operations, and ANF pure-value reuse with commutative
  and reversed-relational canonicalization.
- **Interprocedural and aggregate work:** uniform literal direct-parameter
  propagation, bounded scalar-literal cloning, ownership-safe tuple projection
  forwarding, projection-only scalar tuple and record replacement, unique Float
  record-clone allocation reuse, and unused ANF binding elimination.
- **Loops and control flow:** bounded recursive-loop unrolling, tail recursion
  modulo native-width wrapping addition, recursive-left subtraction,
  multiplication, or immutable list/sum/record constructors; effect-free call
  and Float-load hoisting, affine induction reduction, factor-two counted-loop
  unrolling, same-target and redundant-successor branches, fallthrough block
  placement, and linear block merging.
- **Instruction selection and allocation:** ARM64 bit-clear fusion, addition
  with a single-use negation, dead multiply-subtract and Float-copy fusion,
  ARM64 entry-parameter copy elimination, and floating-point phi coalescing.
- **Code motion and closures:** shared leading conditional-binding hoisting and
  capture-free local closure devirtualization.

## ANF simplification

`passes/anf/ANF_Optimize.fs` and `src/Tests/optimization/anf.opt` own:

- literal folding for float negation, absolute value, square root,
  Int64/Float conversions, Float bit conversion and comparison, string
  concatenation, typed `CanonicalBufferEq`, and UInt64 arithmetic, comparison,
  shifts, and bitwise work;
- integer, UInt64, Float, Boolean, bitwise, shift, negation, comparison, and
  empty-string identities;
- conditional simplification, negated-condition folding, integer
  reassociation/cancellation/factorization, and safe multiplication/division
  strength reduction;
- common-subexpression reuse through a dedicated, exhaustive value key for
  conditional values, tuple projections, non-owning non-Float scalar record
  projections, scalar conversions, Float unary operations, commutative
  operations, and reversed relational comparisons;
- ownership-safe local tuple projection forwarding and unused-binding
  elimination; and
- shared leading conditional binding hoisting and capture-free local closure
  devirtualization.

Floating-point rewrites retain NaN, signed-zero, rounding, overflow, and
evaluation-order restrictions. Managed-value forwarding retains ownership
restrictions. Allocations, mutable-memory observations, managed and Float
record projections, calls, and ownership operations are not merged. Focused
negative fixtures are part of each transformation's contract.

## Direct-call specialization

`passes/anf/ANF_DirectCallSpecialization.fs` specializes internal calls when
the callee identity and argument values are statically known. Uniform
parameters are removed, while differing values create bounded clones selected
by estimated argument-setup savings. Facts cover scalar and text literals,
Char and DateTime immediates, nullary constructor tags, Int128 and UInt128 word
constructions, and tuples or records of at most three literal fields. Exact
construction values are rematerialized inside the clone, and only the matching
now-unused caller construction is removed.

Calls through an explicit function reference become direct before analysis.
The original function remains as a fallback, with limits of four clones per
function and sixteen per program. Address-taken functions, closures, larger or
dynamic aggregates, and non-exact ranges remain excluded. Focused tests own
caps, recursive signatures, float-bit identity, known indirect targets,
construction ownership, fallback routing, and exclusions.

## Higher-order specialization

`passes/anf/ANF_HigherOrderSpecialization.fs` propagates statically known
callable identities through local aliases, equal branch values and joins, and
function returns. At a direct helper call, one clone specializes every eligible
known functional argument together. Captured fields become ordinary parameters;
static function references call their target directly without allocating a
closure. The original helper remains available for dynamic call sites.

Pre-reference-count external ANF candidates provide the same proof boundary
across compilation units: any required helper and closure-target clones are
emitted into the current unit while the prebuilt originals remain unchanged.
Helpers larger than 256 nodes, targets larger than 32 nodes, unsupported uses
of a functional parameter, and calls beyond the shared sixteen-pair budget are
left on the generic closure path.

## Escape analysis and scalar replacement

`passes/anf/ANF_EscapeAnalysis.fs` removes fixed-layout tuple and record
allocations whose fields are immediate scalar values, including Float64, and whose
complete lexical use set consists only of projections, local aliases, and
representation-only record-clone sources. Escaping clones retain their own
allocation even when an eligible source allocation is removed.

Returns, calls, closure capture, storage, raw operations, managed fields, and
unknown uses preserve scalar-replacement allocations. Escaping clones retain
their own allocation while eligible Float source and intermediate aggregates
are scalar-replaced. After scalar replacement, a remaining uniquely owned
Float record may still transfer its allocation to a sole escaping clone.
Focused tests cover projection, alias, clone-chain, and branch shapes plus the
conservative call and managed-field boundaries.

## MIR optimization

`passes/mir/MIR_Optimize.fs` and `src/Tests/optimization/mir.opt` own:

- sparse conditional constant propagation over explicitly typed integer and
  Boolean SSA values and executable CFG edges, including phi constants and
  non-executable predecessor removal;
- dominator-scoped scalar and effect-free-call common-subexpression reuse;
- barrier-aware exact scalar heap-load reuse through `FloatSqrt`, `FloatAbs`, `FloatNeg`,
  `Int64ToFloat`, `FloatToInt64`, and `FloatToBits` locally, without exporting
  availability into dominated blocks;
- bounded recursive-loop unrolling, tail recursion modulo wrapping native
  integer operations or immutable constructors, effect-free call hoisting,
  affine induction reduction, and narrow counted-loop unrolling;
- same-target and redundant-successor branch elimination; and
- linear basic-block merging with typed phi repair.

Memory, allocation, ownership, unknown-call, managed-result, and aliasing
barriers remain conservative unless a focused proof says otherwise.
Functions containing float-producing MIR remain on the existing local
optimizer because float error paths currently carry integer return placeholders
that are valid only while they remain terminator operands.

Before ANF, semantic HIR leaf operations carry typed effect and alias
contracts. These contracts currently drive list-region verification and
liveness: constructors are fresh, map/reverse may reuse their source, and fold
has no managed result. They establish the proof boundary for future HIR
reordering and reuse passes; no optimizer treats opaque AST operands as pure.

## LIR, allocation, and backend optimization

`passes/lir/LIR_Peephole.fs`, `passes/lir/RegisterAllocation.fs`, and the native
backends own:

- floating constant-load motion;
- dead multiply/subtract and floating arithmetic-copy fusion;
- ARM64 bit-clear fusion;
- floating-point phi coalescing;
- authoritative entry-parameter placement; and
- CFG block placement that exposes native fallthroughs.

`LIR.layoutBlocks` follows false and jump successor chains, but places a single
non-entry return shared by multiple predecessor blocks last. This lets the
return fall through into the native epilogue without duplicating its cleanup or
instructions. Entry-return, multiple-return, and non-returning CFGs retain the
ordinary successor-chain policy.

The LIR fixtures, allocation tests, and target-specific generated-code tests
own register-liveness, interference, flags, and instruction-encoding safety.

## Rejected trials

| Trial | Why it is not retained | Revisit condition |
|---|---|---|
| Dead direct-parameter elimination | Added whole-program signature machinery but produced no measured workload improvement | A representative workload contains hot provably dead direct parameters |
| General captured-closure scalarization | A local one-scalar-capture prototype did not improve a real workload | A known higher-order chain, such as List filtering, demonstrates benefit while preserving capture ownership |

Do not treat a rejected trial as a permanent prohibition. Require a new
workload or correctness argument before rebuilding the discarded machinery.
