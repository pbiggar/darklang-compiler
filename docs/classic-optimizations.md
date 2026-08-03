# Classic Optimizations Backlog

Persistent backlog for audit-driven classic compiler optimization work.

## Constant folding

### Constant FloatNeg folding

- Optimization name: Constant FloatNeg folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes ANF-specific float negation after constant propagation exposes a literal operand.
- Notes: Implemented for `FloatNeg(floatLiteral) -> floatLiteral` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `const_fold_float_neg` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Constant FloatAbs folding

- Optimization name: Constant FloatAbs folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes ANF-specific float absolute value operations after constant propagation exposes a literal operand.
- Notes: Implemented for `FloatAbs(floatLiteral) -> floatLiteral` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `const_fold_float_abs` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Constant FloatSqrt folding

- Optimization name: Constant FloatSqrt folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes ANF-specific square root operations after constant propagation exposes a literal operand.
- Notes: Implemented for `FloatSqrt(floatLiteral) -> floatLiteral` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `const_fold_float_sqrt` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Constant Int64.toFloat folding

- Optimization name: Constant Int64.toFloat folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes ANF-specific numeric conversion after constant propagation exposes an Int64 literal operand.
- Notes: Implemented for `Int64ToFloat(Int64 literal) -> Float literal` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `const_fold_int64_to_float` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Constant Float.toInt folding

- Optimization name: Constant Float.toInt folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes ANF-specific numeric conversion after constant propagation exposes a finite in-range Float literal operand.
- Notes: Implemented for finite in-range `FloatToInt64(Float literal) -> Int64 literal` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `const_fold_float_to_int64` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Constant Float.toBits folding

- Optimization name: Constant Float.toBits folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes a pure runtime bit-copy conversion when constant propagation exposes a float literal.
- Notes: Implemented for `FloatToBits(floatLiteral) -> UInt64 literal` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `const_fold_float_to_bits` in `src/Tests/optimization/anf.opt`; existing float stdlib and float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Constant Float comparison folding

- Optimization name: Constant Float comparison folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes literal-only float comparisons and exposes constant branches to the existing ANF optimizer.
- Notes: Implemented for literal Float `==`, `!=`, `<`, `>`, `<=`, and `>=` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `const_fold_float_lt` and `const_fold_float_eq` in `src/Tests/optimization/anf.opt`; existing float comparison E2E tests provide correctness coverage but do not isolate this micro-pattern.

### Constant string concatenation folding

- Optimization name: Constant string concatenation folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that eliminates runtime allocation when both concatenation operands are known string literals.
- Notes: Implemented for `StringConcat(StringLiteral, StringLiteral) -> StringLiteral` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `const_fold_string_concat` in `src/Tests/optimization/anf.opt`; existing benchmarks do not isolate this micro-pattern.

### Constant UInt64 comparison folding

- Optimization name: Constant UInt64 comparison folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes literal-only unsigned comparisons and exposes constant branches to existing ANF cleanup.
- Notes: Implemented for literal UInt64 `==`, `!=`, `<`, `>`, `<=`, and `>=` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by the six `const_fold_uint64_*` before/after ANF snapshots in `src/Tests/optimization/anf.opt`; existing benchmarks do not isolate this micro-pattern.

### Constant internal string equality folding

- Optimization name: Constant internal string equality folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes literal-only internal string equality calls and exposes resolved string-match branches to existing ANF cleanup.
- Notes: Implemented for `__string_eq(stringLiteral, stringLiteral) -> Bool literal` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by equal and unequal literal branch-elimination snapshots in `src/Tests/optimization/anf.opt`; existing string tests and benchmarks provide broader coverage but do not isolate this micro-pattern.

### Constant Int64 shift folding

- Optimization name: Constant Int64 shift folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes literal-only shift operations in ANF before MIR lowering.
- Notes: Implemented for `intLiteral << shiftLiteral` and logical `intLiteral >> shiftLiteral` when the shift count is known to be in the backend immediate range `0..63`. Covered by `const_fold_int_shl` and `const_fold_int_shr` in `src/Tests/optimization/anf.opt`; existing bitwise and benchmark tests provide broader shift coverage but do not isolate this micro-pattern.

### Constant Int64 bitwise folding

- Optimization name: Constant Int64 bitwise folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk canonical fold that removes runtime bitwise operations when both Int64 operands are literals.
- Notes: Implemented for literal `BitAnd`, `BitOr`, and `BitXor` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `const_fold_int_bitand`, `const_fold_int_bitor`, and `const_fold_int_bitxor` in `src/Tests/optimization/anf.opt`; existing integer-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Constant UInt64 bitwise folding

- Optimization name: Constant UInt64 bitwise folding
- Taxonomy category: Constant folding
- Priority/rationale: Small, low-risk unsigned companion to Int64 bitwise folding that removes runtime operations and preserves the UInt64 literal type.
- Notes: Implemented for literal `BitAnd`, `BitOr`, and `BitXor` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `const_fold_uint64_bitand`, `const_fold_uint64_bitor`, and `const_fold_uint64_bitxor` in `src/Tests/optimization/anf.opt`; existing UInt64-heavy stdlib code exercises these patterns but no benchmark isolates them.

## Algebraic simplification

### Modulo by one elimination

- Optimization name: Modulo by one elimination
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that removes a runtime remainder operation when the divisor is the literal `1`. Low-risk canonical identity; previously used as the first Phase 1 sandbox trial target.
- Notes: Implemented in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during sandbox testing. Covered by `identity_mod_one` in `src/Tests/optimization/anf.opt`; quick benchmark check did not report a regression. Prior sandbox trial implemented `x % 1 -> 0` in ANF optimization.

### Modulo by negative one elimination

- Optimization name: Modulo by negative one elimination
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that removes a runtime remainder operation when the divisor is the literal `-1`.
- Notes: Implemented for `x % -1 -> 0` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_mod_negative_one` in `src/Tests/optimization/anf.opt`; existing integer-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### UInt64 division and modulo by one elimination

- Optimization name: UInt64 division and modulo by one elimination
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk unsigned companion to existing Int64 division and modulo identities that removes redundant runtime arithmetic before backend lowering.
- Notes: Implemented for `x / 1UL -> x` and `x % 1UL -> 0UL` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_uint64_div_one` and `identity_uint64_mod_one` in `src/Tests/optimization/anf.opt`; existing integer-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Integer self-comparison simplification

- Optimization name: Integer self-comparison simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, canonical boolean simplification with direct fit in MIR SSA constant folding; useful for eliminating provably constant branches.
- Notes: Implemented in MIR constant folding during the second Phase 1 sandbox trial and extended to integer ordering self-comparisons in the Guided review revision. ANF strength reduction now also folds same-parameter integer comparisons for `==`, `!=`, `<`, `>`, `<=`, and `>=`, including sized unsigned integer parameters. Covered by ANF tests in `src/Tests/optimization/anf.opt` and pipeline optimization snapshots for branch elimination; negative MIR tests cover float equality and source variable shadowing so the fold remains type- and operand-aware.

### Bitwise idempotence simplification

- Optimization name: Bitwise idempotence simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes redundant bitwise operations in ANF before backend lowering.
- Notes: Implemented for `x & x -> x` and `x ||| x -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Phase 2 sandbox trial. Covered by `identity_bitand_self` and `identity_bitor_self` in `src/Tests/optimization/anf.opt`.

### Bitwise zero identity simplification

- Optimization name: Bitwise zero identity simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes redundant bitwise operations in ANF before backend lowering.
- Notes: Implemented for `x & 0 -> 0`, `0 & x -> 0`, `x ||| 0 -> x`, `0 ||| x -> x`, `x ^ 0 -> x`, and `0 ^ x -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by corresponding bitwise zero identity tests in `src/Tests/optimization/anf.opt`.

### Bitwise XOR self simplification

- Optimization name: Bitwise XOR self simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes redundant XOR operations in ANF before backend lowering.
- Notes: Implemented for `x ^ x -> 0` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Guided sandbox trial. Covered by `identity_bitxor_self` in `src/Tests/optimization/anf.opt`.

### Bitwise XOR all-ones simplification

- Optimization name: Bitwise XOR all-ones simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that reuses unary bitwise-not lowering for XOR with an all-ones literal.
- Notes: Implemented for `x ^ -1 -> ~~~x` and `-1 ^ x -> ~~~x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Expanded Guided sandbox trial. Covered by `identity_bitxor_all_ones_right` and `identity_bitxor_all_ones_left` in `src/Tests/optimization/anf.opt`.

### Boolean idempotence simplification

- Optimization name: Boolean idempotence simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical boolean simplification that removes redundant boolean self-operations in ANF before backend lowering.
- Notes: Implemented for `x && x -> x` and `x || x -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Guided sandbox trial. Covered by `identity_bool_and_self` and `identity_bool_or_self` in `src/Tests/optimization/anf.opt`.

### Boolean self-comparison simplification

- Optimization name: Boolean self-comparison simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical boolean simplification that removes redundant equality comparisons on the same Bool SSA variable in ANF before backend lowering.
- Notes: Implemented for `x == x -> true` and `x != x -> false` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `self_comparison_bool_eq` and `self_comparison_bool_neq` in `src/Tests/optimization/anf.opt`; existing boolean-heavy test coverage exercises correctness, while benchmark programs do not isolate this micro-pattern.

### Boolean absorption simplification

- Optimization name: Boolean absorption simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical boolean simplification that removes redundant adjacent ANF boolean operations before backend lowering.
- Notes: Implemented for adjacent ANF forms of `x && (x || y) -> x` and `x || (x && y) -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_bool_and_absorption_left` and `identity_bool_or_absorption_left` in `src/Tests/optimization/anf.opt`; existing boolean-heavy tests provide regression coverage but do not isolate benchmark impact.

### Boolean comparison constant simplification

- Optimization name: Boolean comparison constant simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical boolean simplification that removes redundant equality comparisons against boolean literals in ANF before backend lowering.
- Notes: Implemented for `x == true -> x`, `true == x -> x`, `x == false -> !x`, `false == x -> !x`, `x != true -> !x`, `true != x -> !x`, `x != false -> x`, and `false != x -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Guided sandbox trials. Boolean inequality literal cases are matched explicitly rather than relying on `not (x == false)` to become a later double-negation cleanup. Covered by corresponding boolean equality, inequality, and double-negation tests in `src/Tests/optimization/anf.opt`.

### Boolean complement simplification

- Optimization name: Boolean complement simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical boolean simplification that folds complement laws exposed as adjacent ANF bindings.
- Notes: Implemented for `x && !x -> false`, `!x && x -> false`, `x || !x -> true`, and `!x || x -> true` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_bool_and_complement_right` and `identity_bool_or_complement_right` in `src/Tests/optimization/anf.opt`; existing boolean and branch-heavy tests provide regression coverage but do not isolate this micro-pattern.

### String self-comparison simplification

- Optimization name: String self-comparison simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that avoids a runtime string equality call when both operands are the same ANF variable.
- Notes: Implemented only for the internal `__string_eq(x, x) -> true` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Source-level `x == x`, `x != x`, and `String.equals(x, x)` remain explicit stdlib calls. Covered by `internal_string_eq_self_comparison`, `source_string_eq_self_not_folded`, `source_string_neq_self_not_folded`, and `stdlib_string_equals_self_not_folded` in `src/Tests/optimization/anf.opt`; existing string benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Empty string concatenation simplification

- Optimization name: Empty string concatenation simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical string simplification that removes unnecessary allocation for concatenating an already-known empty string.
- Notes: Implemented for `x ++ "" -> x` and `"" ++ x -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_string_concat_empty_right` and `identity_string_concat_empty_left` in `src/Tests/optimization/anf.opt`; existing string E2E tests provide correctness coverage while benchmark programs do not isolate this micro-pattern.

### Bitwise double-not simplification

- Optimization name: Bitwise double-not simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes redundant paired bitwise-not operations in ANF before backend lowering.
- Notes: Implemented for `~~~(~~~x) -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Guided sandbox trial. Covered by `identity_bitwise_double_not` in `src/Tests/optimization/anf.opt`.

### Integer double negation simplification

- Optimization name: Integer double negation simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes redundant paired unary negation operations in ANF before backend lowering.
- Notes: Implemented for `-(-x) -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Expanded Guided sandbox trial. Covered by `identity_integer_double_negation` in `src/Tests/optimization/anf.opt`.

### Float double negation simplification

- Optimization name: Float double negation simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical float simplification that removes redundant paired float negation operations in ANF before backend lowering.
- Notes: Implemented for `-(-x) -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_float_double_negation` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Float abs idempotence simplification

- Optimization name: Float abs idempotence simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical float simplification that removes redundant paired absolute-value operations in ANF before backend lowering.
- Notes: Implemented for `Float.abs(Float.abs(x)) -> Float.abs(x)` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_float_abs_idempotent` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Float abs negation simplification

- Optimization name: Float abs negation simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical float simplification that removes redundant negation before absolute value in ANF before backend lowering.
- Notes: Implemented for `Float.abs(-x) -> Float.abs(x)` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_float_abs_negation` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Identical if branch simplification

- Optimization name: Identical if branch simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical control-flow simplification that removes a conditional when both optimized branches are syntactically identical.
- Notes: Implemented for ANF `if cond then expr else expr -> expr` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_if_same_branches` in `src/Tests/optimization/anf.opt`; existing branch-heavy tests provide correctness coverage but do not isolate this micro-pattern.

### Boolean literal if branch simplification

- Optimization name: Boolean literal if branch simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical control-flow simplification that removes a conditional when the branches return the condition's boolean literals directly.
- Notes: Implemented for ANF `if cond then true else false -> cond` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_if_bool_literal_branches` in `src/Tests/optimization/anf.opt`; existing branch-heavy and boolean-heavy tests provide broader regression coverage but do not isolate this micro-pattern. The inverse `if cond then false else true -> !cond` remains intentionally unfused because this optimizer stage does not currently introduce fresh temporaries for new bindings.

### Negated branch condition simplification

- Optimization name: Negated branch condition simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical control-flow simplification that removes a single-use Boolean negation and exposes the original condition directly to later passes.
- Notes: Implemented for adjacent ANF `let negated = !cond in if negated then a else b -> if cond then b else a`, only when `negated` is absent from both branches. Covered by `branch_on_single_use_bool_negation` and `branch_on_reused_bool_negation_not_rewritten` in `src/Tests/optimization/anf.opt`; existing branch-heavy benchmarks provide regression coverage but do not isolate this micro-pattern.

### Bitwise all-ones identity simplification

- Optimization name: Bitwise all-ones identity simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes redundant bitwise operations in ANF before backend lowering.
- Notes: Implemented for `x & -1 -> x`, `-1 & x -> x`, `x ||| -1 -> -1`, `-1 ||| x -> -1`, `x ^ -1 -> ~~~x`, and `-1 ^ x -> ~~~x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during sandbox trials. Covered by corresponding bitwise all-ones identity tests in `src/Tests/optimization/anf.opt`.

### Shift by zero elimination

- Optimization name: Shift by zero elimination
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer identity that removes redundant shift operations in ANF before MIR lowering.
- Notes: Implemented for `x << 0 -> x` and `x >> 0 -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Phase 2 sandbox trial. Covered by `identity_shl_zero` and `identity_shr_zero` in `src/Tests/optimization/anf.opt`; these tests use the current optimization test format with function-only input and no required main expression.

### Zero shift source simplification

- Optimization name: Zero shift source simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes shifts whose left operand is already zero before MIR lowering.
- Notes: Implemented for `0 << x -> 0` and `0 >> x -> 0` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Guided sandbox trial. Covered by `identity_shl_zero_left` and `identity_shr_zero_left` in `src/Tests/optimization/anf.opt`.

### Subtraction from zero strength reduction

- Optimization name: Subtraction from zero strength reduction
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that reuses unary negation lowering instead of materializing a binary subtract from literal zero.
- Notes: Implemented for `0 - x -> -x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Guided sandbox trial. Covered by `strength_reduce_sub_from_zero` in `src/Tests/optimization/anf.opt`.

### Integer self-subtraction simplification

- Optimization name: Integer self-subtraction simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that removes subtraction when both integer operands are the same SSA variable.
- Notes: Implemented for `x - x -> 0` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `self_subtraction_int` in `src/Tests/optimization/anf.opt`.

### UInt64 additive identity simplification

- Optimization name: UInt64 additive identity simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that removes redundant unsigned addition and subtraction by literal zero in ANF before MIR lowering.
- Notes: Implemented for `x + 0UL -> x`, `0UL + x -> x`, and `x - 0UL -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `identity_uint64_add_zero_right`, `identity_uint64_add_zero_left`, and `identity_uint64_sub_zero` in `src/Tests/optimization/anf.opt`; existing UInt64 stdlib tests provide behavioral coverage but do not isolate this micro-pattern.

### Subtraction of negative literal strength reduction

- Optimization name: Subtraction of negative literal strength reduction
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that turns subtraction of a known negative literal into addition of its positive counterpart.
- Notes: Implemented for `x - -n -> x + n` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`, excluding `Int64.MinValue` where negation cannot produce a positive Int64 literal. Covered by `strength_reduce_sub_negative_literal` in `src/Tests/optimization/anf.opt`.

### Addition of negative literal strength reduction

- Optimization name: Addition of negative literal strength reduction
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that turns addition of a known negative literal into subtraction of its positive counterpart.
- Notes: Implemented for `x + -n -> x - n` and `-n + x -> x - n` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`, excluding `Int64.MinValue` where negation cannot produce a positive Int64 literal. Covered by `strength_reduce_add_negative_literal_right` and `strength_reduce_add_negative_literal_left` in `src/Tests/optimization/anf.opt`.

### Integer addition reassociation

- Optimization name: Integer addition reassociation
- Taxonomy category: Algebraic simplification
- Priority/rationale: Canonical, low-risk reassociation that exposes adjacent Int64 literals to constant folding and makes a single-use intermediate addition dead.
- Notes: Implemented for adjacent ANF bindings representing `(x + a) + b -> x + (a + b)` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Restricted to Int64 addition because floating-point addition is not safely reassociative. Covered by `reassociate_integer_add_constants` and `reassociation_preserves_float_add` in `src/Tests/optimization/anf.opt`; `licm_skip_loop_with_call` in `src/Tests/optimization/lir.opt` verifies that a longer chain reaches LIR as one addition. No current routine benchmark contains the exact two-literal chain; `benchmarks/problems/edigits` is the identified follow-up benchmark because its repeated Int64 index arithmetic is the closest workload fit.

### Multiplication by negative one strength reduction

- Optimization name: Multiplication by negative one strength reduction
- Taxonomy category: Strength reduction
- Priority/rationale: Small, low-risk canonical integer simplification that reuses unary negation lowering instead of materializing multiplication by literal negative one.
- Notes: Implemented for `x * -1 -> -x` and `-1 * x -> -x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during an Expanded Guided sandbox trial. Covered by `strength_reduce_mul_neg_one_right` and `strength_reduce_mul_neg_one_left` in `src/Tests/optimization/anf.opt`.

### Float negative one strength reduction

- Optimization name: Float negative one strength reduction
- Taxonomy category: Strength reduction
- Priority/rationale: Small, low-risk canonical float simplification that reuses existing `FloatNeg` lowering instead of materializing multiplication or division by literal negative one.
- Notes: Implemented for `x * -1.0 -> FloatNeg(x)`, `-1.0 * x -> FloatNeg(x)`, and `x / -1.0 -> FloatNeg(x)` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `strength_reduce_float_mul_neg_one_right`, `strength_reduce_float_mul_neg_one_left`, and `strength_reduce_float_div_neg_one` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Float subtraction from zero strength reduction

- Optimization name: Float subtraction from zero strength reduction
- Taxonomy category: Strength reduction
- Priority/rationale: Small, low-risk canonical float simplification that reuses existing `FloatNeg` lowering instead of materializing a binary subtraction from literal zero.
- Notes: Implemented for `0.0 - x -> FloatNeg(x)` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `strength_reduce_float_sub_from_zero` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Float division by negative power-of-two strength reduction

- Optimization name: Float division by negative power-of-two strength reduction
- Taxonomy category: Strength reduction
- Priority/rationale: Small, low-risk extension of the existing exact power-of-two float division lowering that removes division instructions for negative power-of-two divisors.
- Notes: Implemented for `x / -2.0` through `x / -256.0` by multiplying by exact negative reciprocal literals in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `strength_reduce_float_div_negative_2` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Division by negative one strength reduction

- Optimization name: Division by negative one strength reduction
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that reuses unary negation lowering instead of materializing a binary divide by literal `-1`.
- Notes: Implemented for `x / -1 -> -x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Expanded Guided sandbox trial. Covered by `strength_reduce_div_negative_one` in `src/Tests/optimization/anf.opt`.

### UInt64 multiplicative identity simplification

- Optimization name: UInt64 multiplicative identity simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical UInt64 simplification that removes redundant multiplication around identity and absorbing literals.
- Notes: Implemented for `x * 1UL -> x`, `1UL * x -> x`, `x * 0UL -> 0UL`, and `0UL * x -> 0UL` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by corresponding UInt64 multiplication identity tests in `src/Tests/optimization/anf.opt`.

## Strength reduction

### Integer self-addition strength reduction

- Optimization name: Integer self-addition strength reduction
- Taxonomy category: Strength reduction
- Priority/rationale: Small, low-risk canonical integer simplification that reuses existing shift lowering for doubling an Int64 SSA value.
- Notes: Implemented for `x + x -> x << 1` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `strength_reduce_add_self` in `src/Tests/optimization/anf.opt`; existing integer-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Float multiplication by two strength reduction

- Optimization name: Float multiplication by two strength reduction
- Taxonomy category: Strength reduction
- Priority/rationale: Existing ANF optimization; keep as catalog evidence rather than a candidate for this iteration.
- Notes: Implemented for `2.0 * x -> x + x` and `x * 2.0 -> x + x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `strength_reduce_float_mul_two_left` and `strength_reduce_float_mul_two_right` in `src/Tests/optimization/anf.opt`.

### Multiplication by power-of-two lowering

- Optimization name: Multiplication by power-of-two lowering
- Taxonomy category: Strength reduction
- Priority/rationale: Existing ANF optimization; keep as catalog evidence rather than a candidate for this iteration.
- Notes: Implemented in `tryStrengthReduce`.

### UInt64 division by power-of-two lowering

- Optimization name: UInt64 division by power-of-two lowering
- Taxonomy category: Strength reduction
- Priority/rationale: Canonical, low-risk lowering that replaces an expensive unsigned division by a literal power of two with a logical right shift.
- Notes: Implemented for the full UInt64 power-of-two range in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `strength_reduce_uint64_div_power_of_two` in `src/Tests/optimization/anf.opt`. No current benchmark exercises this source pattern; a future UInt64 bitmap word-index workload using repeated runtime-value division by `64UL` would isolate it without distorting unrelated routine benchmarks.

## Common subexpression elimination

### Duplicate IfValue reuse

- Optimization name: Duplicate IfValue reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE for a pure conditional value selection; avoids lowering repeated identical selections into duplicate control flow.
- Notes: Implemented for repeated `IfValue(condition, thenValue, elseValue)` expressions in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `cse_reuses_duplicate_if_value` in `src/Tests/optimization/anf.opt`; existing branch-heavy tests and benchmarks provide broader regression coverage but do not isolate this micro-pattern.

### Duplicate tuple projection reuse

- Optimization name: Duplicate tuple projection reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE for immutable tuple reads; avoids repeating the same tuple field load before MIR lowering.
- Notes: Implemented for repeated `TupleGet(tuple, index)` expressions in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `cse_reuses_duplicate_tuple_get` in `src/Tests/optimization/anf.opt`; tuple-heavy tests and benchmarks provide broader regression coverage but do not isolate this micro-pattern.

### Duplicate Float.toInt conversion reuse

- Optimization name: Duplicate Float.toInt conversion reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE for pure scalar conversions; avoids repeating identical Float-to-Int64 conversions before MIR lowering.
- Notes: Implemented for repeated `FloatToInt64(atom)` expressions in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `cse_reuses_duplicate_float_to_int64` in `src/Tests/optimization/anf.opt`; existing float-heavy tests and benchmarks provide broader regression coverage but do not isolate this micro-pattern.

### Duplicate FloatNeg reuse

- Optimization name: Duplicate FloatNeg reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE for pure float negation; avoids repeating equivalent unary float negation before MIR lowering.
- Notes: Implemented for repeated `FloatNeg(x)` expressions in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `cse_reuses_duplicate_float_neg` in `src/Tests/optimization/anf.opt`; existing float-heavy tests and benchmarks provide regression coverage but do not isolate this micro-pattern.

### Duplicate Int64.toFloat conversion reuse

- Optimization name: Duplicate Int64.toFloat conversion reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE for a pure numeric conversion; avoids repeating the same Int64-to-Float conversion before MIR lowering.
- Notes: Implemented for repeated `Int64ToFloat(value)` expressions in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `cse_reuses_duplicate_int64_to_float` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmarks provide regression coverage but do not isolate this micro-pattern.

### Duplicate Float.toBits conversion reuse

- Optimization name: Duplicate Float.toBits conversion reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE for a pure numeric bit-copy conversion; avoids repeating the same float-to-bits operation before MIR lowering.
- Notes: Implemented for repeated `FloatToBits(atom)` expressions in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `cse_reuses_duplicate_float_to_bits` in `src/Tests/optimization/anf.opt`; existing float stdlib and float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Duplicate Float.abs reuse

- Optimization name: Duplicate Float.abs reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE for a pure unary float operation; avoids repeating the same absolute-value computation before MIR lowering.
- Notes: Implemented for repeated `FloatAbs(atom)` expressions in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `cse_reuses_duplicate_float_abs` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

### Duplicate Float.sqrt reuse

- Optimization name: Duplicate Float.sqrt reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE for a pure floating-point operation; avoids repeating the same square-root computation before MIR lowering.
- Notes: Implemented for repeated `FloatSqrt(atom)` expressions in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `cse_reuses_duplicate_float_sqrt` in `src/Tests/optimization/anf.opt`; existing float-heavy benchmark programs provide regression coverage but do not isolate this micro-pattern.

## Aggregate simplification

### Local tuple projection forwarding

- Optimization name: Local tuple projection forwarding
- Taxonomy category: Aggregate simplification
- Priority/rationale: Canonical, low-risk scalar replacement step that exposes locally constructed tuple elements directly and lets existing dead-code elimination remove unused tuple allocations.
- Notes: Implemented for `TupleGet(TupleAlloc(elements), index)` when the selected atom is ownership-safe to forward in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by positive and managed-ownership negative ANF snapshots in `src/Tests/optimization/anf.opt`; `binary_trees` provides broader tuple benchmark coverage but does not isolate this local construction/projection pattern.

## Dead code elimination

### Unused ANF binding elimination

- Optimization name: Unused ANF binding elimination
- Taxonomy category: Dead code elimination
- Priority/rationale: Existing optimization; keep as catalog evidence rather than a candidate for this iteration.
- Notes: Covered by existing ANF optimization tests.

## Control-flow simplification

### Same-target branch elimination

- Optimization name: Same-target branch elimination
- Taxonomy category: Control-flow simplification
- Priority/rationale: Small, low-risk canonical CFG simplification that makes an unconditional successor explicit and allows dead-code elimination to remove a now-unused condition computation.
- Notes: Implemented for MIR `Branch (cond, target, target) -> Jump target` in `src/DarkCompiler/passes/3.5_MIR_Optimize.fs`. Covered by `testSameTargetBranchBecomesJumpAndDropsCondition` in `src/Tests/optimizations/MIROptimizeTests.fs`, which checks both the terminator rewrite and removal of the dead condition at the optimizer fixpoint. The routine benchmark profile retained every recorded instruction count, so measured improvement/loss is 0%; direct source-level identical branches are already removed in ANF, making synthetic MIR coverage the focused exercise for this CFG shape.

## Common subexpression elimination

### Commutative ANF CSE

- Optimization name: Commutative ANF CSE
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, canonical extension of existing ANF CSE that reuses equivalent pure binary expressions when only commutative operand order differs.
- Notes: Implemented for commutative `Prim` operations in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Bounded Autonomous sandbox testing. Covered by `cse_reuses_commuted_integer_add` in `src/Tests/optimization/anf.opt`; existing integer-heavy benchmarks provide regression coverage but do not isolate this micro-pattern.

### Reversed relational comparison reuse

- Optimization name: Reversed relational comparison reuse
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Small, low-risk extension of ANF CSE that removes duplicate relational comparisons when both the operator and operand order are reversed.
- Notes: Implemented by canonicalizing `b > a` to the same CSE key as `a < b`, and `b >= a` to the same key as `a <= b`, in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `cse_reuses_reversed_strict_comparison` and `cse_reuses_reversed_inclusive_comparison` in `src/Tests/optimization/anf.opt`; existing comparison-heavy benchmarks provide regression coverage but do not isolate this micro-pattern.

## Instruction combining

### Dead multiply-subtract fusion

- Optimization name: Dead multiply-subtract temporary fusion
- Taxonomy category: Instruction combining
- Priority/rationale: Canonical low-risk fusion that exposes the native ARM64 multiply-subtract instruction and mirrors the existing multiply-add peephole.
- Notes: Implemented for adjacent integer `Mul temp, left, right; Sub dest, minuend, temp` when `temp` is not subsequently read. Direct LIR tests cover both fusion and live-temporary preservation, `fuse_multiply_subtract` covers source-to-LIR output, and `multiply_subtract` provides focused benchmark coverage. On ARM64, the focused Cachegrind benchmark fell from 8,000,111 to 7,000,111 instructions (12.5%); an earlier x64 comparison was unchanged because that backend expands `Msub`.
