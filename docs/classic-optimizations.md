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

### Constant UInt64 arithmetic folding

- Optimization name: Constant UInt64 arithmetic folding
- Taxonomy category: Constant folding
- Priority/rationale: Canonical, low-risk folding that removes literal-only unsigned arithmetic while preserving wrapping UInt64 semantics.
- Notes: Implemented for literal UInt64 addition, subtraction, multiplication, and division/modulo with nonzero divisors in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by the five `const_fold_uint64_*` arithmetic snapshots in `src/Tests/optimization/anf.opt`; the existing UInt64 stdlib E2E tests provide behavioral coverage, while current benchmarks do not isolate literal-only UInt64 arithmetic.

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

### Float strict self-comparison simplification

- Optimization name: Float strict self-comparison simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that removes strict Float comparisons whose operands are the same SSA value and exposes constant branches to fixed-point ANF cleanup.
- Notes: Implemented for `x < x -> false` and `x > x -> false` when `x` is typed as Float; these identities hold for finite values, infinities, and NaN. Float `==`, `!=`, `<=`, and `>=` remain dynamic because their self-comparison result depends on whether `x` is NaN. Focused ANF snapshots cover both dead-branch eliminations and retention of all four NaN-sensitive relations; existing benchmarks do not isolate this micro-pattern.

### Bitwise idempotence simplification

- Optimization name: Bitwise idempotence simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes redundant bitwise operations in ANF before backend lowering.
- Notes: Implemented for `x & x -> x` and `x ||| x -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during the Phase 2 sandbox trial. Covered by `identity_bitand_self` and `identity_bitor_self` in `src/Tests/optimization/anf.opt`.

### Bitwise absorption simplification

- Optimization name: Bitwise absorption simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical integer simplification that removes two redundant adjacent ANF bitwise operations from common masking expressions.
- Notes: Implemented for adjacent ANF forms of `x & (x ||| y) -> x` and `x ||| (x & y) -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `identity_bitand_absorption` and `identity_bitor_absorption` in `src/Tests/optimization/anf.opt`.

### Bitwise zero identity simplification

- Optimization name: Bitwise zero identity simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical signed and unsigned integer simplification that removes redundant bitwise operations in ANF before backend lowering.
- Notes: Implemented for Int64 and UInt64 `x & 0 -> 0`, `0 & x -> 0`, `x ||| 0 -> x`, `0 ||| x -> x`, `x ^ 0 -> x`, and `0 ^ x -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. The corresponding before/after snapshots in `src/Tests/optimization/anf.opt` cover all six UInt64 operand/operator arrangements and retain representative nonzero UInt64 AND, OR, and XOR expressions. Current quick workloads do not contain the dynamic UInt64-with-zero pattern; a tight UInt64 mask/flag loop alternating all six forms is the identified focused benchmark if this idiom becomes common in source programs.

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

### Int64 bitwise complement simplification

- Optimization name: Int64 bitwise complement simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that removes a dynamic bitwise operation and, when dead, its intermediate complement binding.
- Notes: Implemented for `x & ~~~x -> 0`, `x ||| ~~~x -> -1`, and `x ^ ~~~x -> -1` in either operand order in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. The complement binding is retained when a later expression reuses it. Focused ANF snapshots cover every operand order and the live-reuse case in `src/Tests/optimization/anf.opt`; no existing quick benchmark contains a dynamic source-level instance of these patterns.

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
- Notes: Implemented for both full ANF `if cond then expr else expr -> expr` branches and atom-position `IfValue(cond, value, value) -> value` selections in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `identity_if_same_branches` and `identity_if_value_same_branches_drops_condition` in `src/Tests/optimization/anf.opt`; the latter also verifies dead-code elimination removes the unused condition computation.

### Boolean literal if branch simplification

- Optimization name: Boolean literal if branch simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical control-flow simplification that removes a conditional when the branches return the condition's boolean literals directly.
- Notes: Implemented for ANF `if cond then true else false -> cond` and `if cond then false else true -> !cond` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. The inverse rewrite uses a program-wide fresh `TempId` so the negation remains valid ANF; the LIR peephole pass removes a materialized negation used only by a branch by swapping its successor labels. Covered by `identity_if_bool_literal_branches` and `invert_if_bool_literal_branches` in `src/Tests/optimization/anf.opt` plus a focused LIR peephole regression test. The routine `primes` benchmark contains the exact inverse form in its hot `isPrime` path and measured the unchanged baseline count of 5,443,919 instructions; the aggregate routine performance ratio remained 7.84x.

### Negated branch condition simplification

- Optimization name: Negated branch condition simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical control-flow simplification that removes a single-use Boolean negation and exposes the original condition directly to later passes.
- Notes: Implemented for adjacent ANF `let negated = !cond in if negated then a else b -> if cond then b else a`, only when `negated` is absent from both branches. Covered by `branch_on_single_use_bool_negation` and `branch_on_reused_bool_negation_not_rewritten` in `src/Tests/optimization/anf.opt`; existing branch-heavy benchmarks provide regression coverage but do not isolate this micro-pattern.

### Negated integer comparison simplification

- Optimization name: Negated integer comparison simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical instruction combination that removes a single-use Boolean negation after an ordered integer comparison.
- Notes: Implemented for adjacent ANF integer comparisons followed by Boolean negation, using `== <-> !=`, `< <-> >=`, `> <-> <=`, and their reverse complements in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Floating-point comparisons remain unchanged because complementary ordered relations are not equivalent for NaN operands. Covered by positive Int64 and negative Float snapshots in `src/Tests/optimization/anf.opt`. The routine profile retained every recorded instruction count (0% improvement/loss; performance ratio 7.84x); a focused integer range-classification loop using negated runtime comparisons is the identified follow-up benchmark because current workloads do not isolate this micro-pattern.

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

### Int64 multiplication reassociation

- Optimization name: Int64 multiplication reassociation
- Taxonomy category: Algebraic simplification
- Priority/rationale: Canonical, low-risk reassociation that combines adjacent Int64 literals with wrapping multiplication and removes a multiply when the intermediate has no other use.
- Notes: Implemented for adjacent ANF bindings representing `(x * a) * b -> x * (a * b)` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Restricted to Int64 multiplication because Float reassociation can change rounding, overflow, and underflow behavior; recursive liveness cleanup retains the inner binding when it has another use. Focused ANF snapshots cover ordinary and overflowing literal products, live-intermediate preservation, and unchanged Float expressions. No canonical workload contains the exact pattern; a temporary untracked diagnostic loop measured the rewrite without adding a benchmark to the repository.

### Integer add/subtract cancellation

- Optimization name: Integer add/subtract cancellation
- Taxonomy category: Algebraic simplification
- Priority/rationale: Canonical, low-risk integer simplification that removes paired arithmetic while preserving wrapping Int64 semantics.
- Notes: Implemented for adjacent ANF bindings representing `(x + y) - y -> x`, `(x + y) - x -> y`, `(x - y) + y -> x`, and `y + (x - y) -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. The rewrite is restricted to typed Int64 operands, retains an intermediate when it has another use, and leaves same-shaped Float expressions unchanged. Covered by focused before/after snapshots in `src/Tests/optimization/anf.opt`; no current routine benchmark isolates the exact pattern, so the optimization tests are the focused exercise for this micro-pattern.

### Int64 shared-factor combining

- Optimization name: Int64 shared-factor combining
- Taxonomy category: Algebraic simplification
- Priority/rationale: Canonical, low-risk factoring that removes an adjacent Int64 addition and can expose multiplication by a power of two to existing strength reduction.
- Notes: Implemented for adjacent ANF forms of `(x * c) + x` and `x + (c * x)` in either multiplication operand order. Coefficient addition uses wrapping Int64 semantics, recursive liveness cleanup retains a reused product, and Float expressions remain unchanged. Focused snapshots in `src/Tests/optimization/anf.opt` cover all four operand arrangements, power-of-two shift reduction, live-product preservation, and the Float safety boundary; existing quick workloads do not isolate the pattern.

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

### Int64 multiplication by power-of-two lowering

- Optimization name: Int64 multiplication by power-of-two lowering
- Taxonomy category: Strength reduction
- Priority/rationale: Existing ANF optimization; keep as catalog evidence rather than a candidate for this iteration.
- Notes: Implemented in `tryStrengthReduce`.

### UInt64 multiplication by power-of-two lowering

- Optimization name: UInt64 multiplication by power-of-two lowering
- Taxonomy category: Strength reduction
- Priority/rationale: Canonical, low-risk lowering that replaces unsigned multiplication by a nonzero power-of-two literal with a left shift while preserving modulo-2^64 wrapping semantics.
- Notes: Implemented for either operand order and the full UInt64 power-of-two range in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `strength_reduce_uint64_mul_power_of_two_right` and `strength_reduce_uint64_mul_power_of_two_left` in `src/Tests/optimization/anf.opt`, including the high-bit literal. No routine benchmark uses UInt64 source arithmetic; a UInt64 byte-packing workload that repeatedly scales dynamic words by `256UL` is the identified follow-up benchmark.

### UInt64 division by power-of-two lowering

- Optimization name: UInt64 division by power-of-two lowering
- Taxonomy category: Strength reduction
- Priority/rationale: Canonical, low-risk lowering that replaces an expensive unsigned division by a literal power of two with a logical right shift.
- Notes: Implemented for the full UInt64 power-of-two range in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Covered by `strength_reduce_uint64_div_power_of_two` in `src/Tests/optimization/anf.opt`. No current benchmark exercises this source pattern; a future UInt64 bitmap word-index workload using repeated runtime-value division by `64UL` would isolate it without distorting unrelated routine benchmarks.

### UInt64 modulo by power-of-two lowering

- Optimization name: UInt64 modulo by power-of-two lowering
- Taxonomy category: Strength reduction
- Priority/rationale: Canonical, low-risk lowering that replaces expensive unsigned remainder by a literal power of two with a bit mask.
- Notes: Implemented for dynamic `x % 2^kUL -> x & (2^k - 1)UL` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`, while preserving zero and non-power-of-two divisors. Covered by focused before/after and negative snapshots in `src/Tests/optimization/anf.opt`.

## Common subexpression elimination

### Dominator-scoped MIR CSE

- Optimization name: Dominator-scoped MIR common subexpression elimination
- Taxonomy category: Common subexpression elimination
- Priority/rationale: Canonical extension of local MIR CSE that removes repeated pure scalar computations across a basic-block boundary while keeping new live ranges bounded for the current backend.
- Notes: Implemented for binary expressions with concrete scalar types and unary expressions computed in trailing scalar/copy regions of dominating blocks. Expression availability is passed independently to dominator-tree siblings, cleared by reference-count/free operations, and not exported across calls, allocations, or memory/runtime instructions. Direct MIR before/after tests cover multi-block dominating binary/unary reuse, sibling-path preservation, reference-count and call barriers, and rejection of non-scalar binary types. The routine benchmark profile retained every recorded instruction count (0% measured gain/loss; performance ratio 7.43x); source-level ANF CSE removes the straightforward source patterns, so the direct MIR fixtures provide the isolating transformation coverage, while `fannkuch` covers the non-scalar/call-boundary safety cases.

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

## Interprocedural optimization

### Uniform literal direct-parameter propagation

- Optimization name: Uniform literal direct-parameter propagation
- Taxonomy category: Interprocedural constant propagation
- Priority/rationale: Specializing internal signatures exposes program-wide literal parameters inside recursive helpers while removing their call-sequence argument setup.
- Notes: Implemented in `src/DarkCompiler/passes/2.4.5_ANF_DirectCallSpecialization.fs` for internal functions with at least one known direct call and no `FuncRef` or `ClosureAlloc` use. Normal, borrowed, and tail calls are rewritten consistently. Focused ANF tests cover mutual recursion, differing literals, and indirect-use exclusions. The isolated ARM64 routine trial improved quicksort by 11,088 instructions and spectral norm by 60. After rebasing onto counted-loop unrolling, the combined routine recording also improves matmul by 4,000,000 instructions, has no regressions, and records a 0.999895 current/baseline geometric ratio; the displayed aggregate performance ratio remains 2.73x.

### Dead direct-parameter elimination

- Optimization name: Dead direct-parameter elimination
- Taxonomy category: Dead argument elimination
- Priority/rationale: Canonical signature cleanup, but retain only if existing workloads justify sharing the whole-program direct-call analysis.
- Notes: Trialed with recursive normal/tail-call and indirect-use ANF coverage. The five-workload ARM64 quick matrix was exactly neutral (5,264,292 instructions before and after), so the transformation was removed from the retained candidate.

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

## Loop optimization

### Bounded recursive-loop unrolling

- Optimization name: Bounded recursive-loop unrolling
- Taxonomy category: Loop unrolling / partial evaluation
- Priority/rationale: Small fixed-trip scalar loops benefit from removing both recursive-call and loop-control overhead without exposing general recursion to code-size growth.
- Notes: Implemented in ANF inlining for direct `Int64` recursion guarded by a literal `i >= bound`, entered with a literal induction value, and proven to advance by `i + 1`. Only primitive loop bodies are eligible; default limits are eight iterations and 48 expanded bindings per call site. Focused inlining tests cover the eight-round expansion and both caps, while an E2E hash case covers wrapping multiplication and XOR semantics. The routine merkletrees benchmark improves from 724,164,737 to 416,150,237 instructions (42.5%) with every other routine count unchanged.

### Tail recursion modulo Int64 addition

- Optimization name: Tail recursion modulo Int64 addition
- Taxonomy category: Recursion-to-loop conversion
- Priority/rationale: Eliminating one sibling recursive call turns the second branch of Fibonacci-style recursion into an accumulator-carrying loop while retaining the original asymptotic work and maximum call depth.
- Notes: Implemented in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` for functions whose complete recursion consists of two direct self calls combined by a final `Int64` addition. A generated helper carries a zero-initialized accumulator; existing tail-call detection lowers its second call to a loop. Restricting the rewrite to wrapping `Int64` addition preserves reassociation and overflow semantics and excludes floating-point arithmetic. Focused ANF snapshots cover the positive and Float-negative shapes, a MIR snapshot records the accumulator phis and single remaining call, and E2E tests cover base cases and overflow. The routine fib benchmark improved from 642,006,238 to 477,772,383 instructions (25.58%); all other routine counts were unchanged and the aggregate performance ratio improved from 2.42x to 2.38x versus Rust.

### Tail recursion modulo Int64 multiplication

- Optimization name: Tail recursion modulo Int64 multiplication
- Taxonomy category: Recursion-to-loop conversion
- Priority/rationale: A direct recursive product such as factorial need not retain a call frame for each multiplier when the factor is a scalar parameter or literal.
- Notes: Implemented in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` only for complete direct self-recursion whose final operation is one wrapping `Int64` multiplication. The helper carries a one-initialized accumulator and tail-call detection lowers the remaining self call to a jump. Matching permits only primitive/atom bindings, an `Int64` parameter or literal factor, and one direct self call; it therefore rejects Float, managed values, effects, indirect calls, multiple calls, and unsupported control-flow values. Associativity modulo 2^64 preserves overflow behavior. Focused ANF/MIR snapshots cover the loop shape, Float and multiple-call exclusions; the factorial E2E fixture covers overflow. A generated ARM64 comparison of factorial shows the recursive `bl` is replaced by a back-edge `b`, reducing hot recursive calls from one to zero. The MIR fixture repair keeps each expected block delimited correctly and updates the LICM hoisting snapshot because its eligible factorial is intentionally transformed as well.

### Effect-free direct-call hoisting

- Optimization name: Effect-free direct-call hoisting
- Taxonomy category: Loop-invariant code motion
- Priority/rationale: Whole-program MIR effect analysis lets LICM move expensive invariant scalar computations out of lowered tail-recursive loops while retaining conservative ownership and external-call boundaries.
- Notes: Implemented with a greatest-fixed-point direct-call analysis in `src/DarkCompiler/passes/3.5_MIR_Optimize.fs`, so self-recursive and mutually recursive components are proven together while any component reaching an intrinsic effect, indirect call, or unknown callee is rejected. LICM only hoists direct calls with loop-invariant operands and non-owning scalar results. Positive recursive-factorial and negative effecting-recursion snapshots live in `src/Tests/optimization/mir.opt`. The unchanged canonical factorial benchmark improved from 4,030,203 to 60,603 Dark instructions in a focused Cachegrind run.

### Floating-point constant load hoisting

- Optimization name: Floating-point constant load hoisting
- Taxonomy category: Loop-invariant code motion
- Priority/rationale: Small, low-risk extension of existing LIR constant hoisting that removes repeated floating-point constant materialization from pure loop bodies.
- Notes: Implemented for virtual-register `FLoad` instructions in pure natural loops with a unique preheader in `src/DarkCompiler/passes/4.5_LIR_Peephole.fs`. Covered by the `licm_hoist_loop_float_constant` before/after LIR snapshot in `src/Tests/optimization/lir.opt`. Routine benchmarks improved `pisum` from 55,014,671 to 50,015,171 instructions (9.1%) and `mandelbrot` from 21,791,658 to 20,790,992 instructions (4.6%); the aggregate Dark ratio improved from 7.84x to 7.78x versus Rust.

### Canonical affine induction strength reduction

- Optimization name: Canonical affine induction strength reduction
- Taxonomy category: Loop optimization
- Priority/rationale: Carrying `2 * i + 1` as a derived induction value removes its repeated shift and addition from a hot `i + 1` loop.
- Notes: Implemented in MIR for a deliberately narrow two-block, single-backedge `Int64` loop. The preheader computes the initial affine value, a header phi carries it, and the latch advances it by two after its last use. The `derived_induction_affine_shift` MIR snapshot covers the exact before/after shape. The ARM64 quick Leibniz workload improves from 1,000,143 to 900,145 instructions (10.0%); the other quick workloads are unchanged relative to the same current-main build.

### Factor-two canonical counted-loop unrolling

- Optimization name: Factor-two canonical counted-loop unrolling
- Taxonomy category: Loop unrolling
- Priority/rationale: Two consecutive scalar iterations share one backedge, reducing loop-control overhead without the code growth of full unrolling.
- Notes: Implemented in MIR only for a two-block natural loop guarded by an invariant `Int64` upper bound, with a proven `i + 1` backedge, a scalar return path, and at most 12 scalar latch instructions. Calls, allocation, memory access, and ownership operations are excluded. A cloned remainder return handles the final odd iteration; cloned floating-point instructions retain their original sequential order. MIR and LIR snapshots cover the generated shape, while end-to-end tests cover zero, even, odd, and `Int64.MaxValue`-adjacent trip counts. Routine Cachegrind counts improved `leibniz` from 900,001,522 to 850,001,522 instructions (5.6%) and `merkletrees` from 724,164,737 to 684,843,737 (5.4%); all other routine counts were unchanged and the aggregate Dark/Rust ratio improved from 2.75x to 2.73x.

## Control-flow simplification

### Same-target branch elimination

- Optimization name: Same-target branch elimination
- Taxonomy category: Control-flow simplification
- Priority/rationale: Small, low-risk canonical CFG simplification that makes an unconditional successor explicit and allows dead-code elimination to remove a now-unused condition computation.
- Notes: Implemented for MIR `Branch (cond, target, target) -> Jump target` in `src/DarkCompiler/passes/3.5_MIR_Optimize.fs`. Covered by `testSameTargetBranchBecomesJumpAndDropsCondition` in `src/Tests/optimizations/MIROptimizeTests.fs`, which checks both the terminator rewrite and removal of the dead condition at the optimizer fixpoint. The routine benchmark profile retained every recorded instruction count, so measured improvement/loss is 0%; direct source-level identical branches are already removed in ANF, making synthetic MIR coverage the focused exercise for this CFG shape.

### Redundant successor branch elimination

- Optimization name: Redundant successor branch elimination
- Taxonomy category: Control-flow simplification
- Priority/rationale: Small, canonical jump-threading step that removes a branch when the same SSA Boolean condition is already established by the block's sole predecessor edge.
- Notes: Implemented for both true and false predecessor edges in `src/DarkCompiler/passes/3.5_MIR_Optimize.fs`, with exact-register, sole-edge, and non-entry safety gates. Direct CFG tests in `src/Tests/optimizations/MIROptimizeTests.fs` cover both edge polarities and `redundant_successor_branch_elimination` in `src/Tests/optimization/mir.opt` records the source-to-optimized-MIR result; `--dump-mir` exposes that post-optimization IR.

### CFG fallthrough block placement

- Optimization name: CFG fallthrough block placement
- Taxonomy category: Control-flow simplification
- Priority/rationale: Deterministic successor-chain layout makes a conditional false edge and direct jumps fall through, removing avoidable backend branches without profile data.
- Notes: Implemented in `LIR.layoutBlocks` and both native code generators. Entry remains first; chains prefer the false successor of conditional terminators and the target of direct jumps, with remaining chains begun in label order. LIR layout and ARM64/x64 generated-code tests retain the focused proof that a three-block branch removes its false-edge jump and the final return-to-epilogue jump. ARM64 keeps the shared allocation-overflow trap after the terminating epilogue so allocation-using functions preserve that final fallthrough without entering the cold error path. Layout follows only successors present in the supplied block map, leaving malformed-edge validation to backend consumers so established missing-entry and comparison-context diagnostics are not masked. On the focused `sum_to_n` Cachegrind input, dynamic instructions decrease from the pinned 71,817 baseline to 61,695 (10,122 fewer, 14.09%).

### Linear basic-block merging

- Optimization name: Linear basic-block merging
- Taxonomy category: Control-flow simplification
- Priority/rationale: Canonical CFG cleanup that removes unconditional jumps and exposes a combined instruction stream to existing block-local optimizations with low implementation risk.
- Notes: Implemented for MIR blocks that jump to a non-entry successor with exactly one predecessor. Successor phis become typed copies and outgoing phi source labels are rewritten to the retained predecessor label. Direct MIR tests cover the structural before/after form, phi correctness, and newly exposed local CSE; MIR/LIR snapshots cover pipeline effects. The routine performance ratio improved from 7.99x to 7.84x with no benchmark regressions; `leibniz` improved from 1,100,000,144 to 1,000,000,143 instructions (9.09%), `factorial` improved from 4,420,203 to 4,030,203 (8.82%), and `fib` improved from 686,796,263 to 642,005,209 (6.52%).

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

### ARM64 bit-clear fusion

- Optimization name: ARM64 bit-clear fusion
- Taxonomy category: Instruction combining
- Priority/rationale: Canonical low-risk instruction selection that replaces three dependent instructions with the native ARM64 bit-clear operation on a hot N-Queens bitmask pattern.
- Notes: Implemented for dead `MOVN #0; EOR; AND` sequences whose `AND` overwrites the inverted temporary and whose all-ones mask is dead before the next control-flow boundary, producing one `BIC`. Focused ARM64 symbolic before/after, temporary-lifetime, and machine encoding tests cover the transformation; the existing `nqueen` routine benchmark exercises `allOnes & (~~~blocked)`.

### Addition with single-use Int64 negation

- Optimization name: Addition with single-use Int64 negation
- Taxonomy category: Instruction combining
- Priority/rationale: Small, canonical rewrite that replaces two dependent arithmetic operations with one subtraction while preserving wrapping Int64 semantics.
- Notes: Implemented for `x + (-y) -> x - y` and `(-y) + x -> x - y` when the negation temporary occurs only once in the addition and has no later use in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`. Focused ANF snapshots cover both operand orders and preserve negations reused later or as both addition operands in `src/Tests/optimization/anf.opt`. No routine benchmark contains the source pattern; a dedicated repeated `negated_addition` accumulator loop is the identified follow-up microbenchmark if broader source programs begin emitting it.

### Dead multiply-subtract fusion

- Optimization name: Dead multiply-subtract temporary fusion
- Taxonomy category: Instruction combining
- Priority/rationale: Canonical low-risk fusion that exposes the native ARM64 multiply-subtract instruction and mirrors the existing multiply-add peephole.
- Notes: Implemented for adjacent integer `Mul temp, left, right; Sub dest, minuend, temp` when `temp` is not subsequently read. Direct LIR tests cover both fusion and live-temporary preservation, and `fuse_multiply_subtract` covers source-to-LIR output.

### Dead floating arithmetic copy elimination

- Optimization name: Dead floating arithmetic copy elimination
- Taxonomy category: Instruction combining
- Priority/rationale: Canonical low-risk result retargeting that removes one floating-point copy from adjacent arithmetic/copy chains without extending the temporary's lifetime.
- Notes: Implemented for adjacent `FAdd`, `FSub`, `FMul`, or `FDiv` results copied by `FMov` when the arithmetic temporary has no later instruction uses. Direct LIR tests cover all four operations and live-temporary preservation; `retarget_dead_float_arithmetic_move` records the source-to-LIR result. The routine `pisum` benchmark exercises the pattern in its hot loop and fell from 50,015,171 to 45,015,171 instructions (10.0%); all other routine counts were unchanged. The committed aggregate ratio is 7.78x, and applying the measured `pisum` result yields 7.74x pending orchestrator recording.

## Register allocation

### ARM64 entry parameter copy elimination

- Optimization name: ARM64 entry parameter copy elimination
- Taxonomy category: Post-allocation copy elimination
- Priority/rationale: The allocator already emits ordered parameter stack stores and a cycle-safe parallel entry shuffle, so a second blanket save-and-restore through `X9`-`X15` adds work to every integer-parameter function and cannot represent the eighth argument.
- Notes: Implemented by making the allocator-generated entry instructions authoritative in `src/DarkCompiler/passes/arm64/6_CodeGen.fs`. Generated-code regressions cover an identity parameter, mixed integer/float parameters, a spilled integer parameter whose store precedes the shuffle, an `X16`-mediated swap, and the eight-integer-argument boundary. Against the current ARM64 target baseline, all 19 routine workloads improved, from 23,073,872,749 to 21,204,130,845 aggregate instructions (8.10%); `ackermann` improved 12.50%, `fib` 12.50%, `tak` 17.65%, and `nqueen` 19.94%. The recorded routine ratio improved from 2.38x to 2.25x.

### Floating-point phi coalescing

- Optimization name: Floating-point phi coalescing
- Taxonomy category: Register coalescing
- Priority/rationale: Canonical copy elimination that assigns non-interfering FPhi destinations, incoming sources, and direct source-copy chains to one physical floating-point register before phi resolution.
- Notes: Implemented in `src/DarkCompiler/passes/5_RegisterAllocation.fs` by feeding FPhi pairs and only the `FMov` pairs that define FPhi sources into the existing interference-checked coalescer. Float parameters participating in those phis retain their ABI register colors so coalescing cannot trade a loop copy for a return copy. Focused allocation/IR regressions in `src/Tests/compiler-passes/PhiResolutionTests.fs` verify equal physical assignments, no coalesced backedge `FMov`, and preservation of an existing `D0` return assignment. The ARM64 quick benchmark improved `mandelbrot` from 1,300,758 to 1,107,797 instructions (14.8%); `fasta`, `leibniz`, `pisum`, and `spectral_norm` also improved, with no quick-workload regressions.

## Code motion

### Shared leading conditional binding hoisting

- Optimization name: Shared leading conditional binding hoisting
- Taxonomy category: Partial redundancy elimination / code motion
- Priority/rationale: Canonical, bounded cross-branch sharing that reduces duplicated generated code while reusing the existing ANF purity classification.
- Notes: Implemented for identical side-effect-free leading bindings in both branches when the local condition producer and the remaining branch bodies are also side-effect-free. The shared binding moves before the condition producer so compare/branch combining remains available; branch bodies containing calls or other effects are conservatively excluded to avoid extending live ranges across them. ANF snapshots cover the rewrite and effectful-leading-binding exclusion.

## Closure optimization

### Capture-free local closure devirtualization

- Optimization name: Capture-free local closure devirtualization
- Taxonomy category: Devirtualization / scalar replacement
- Priority/rationale: A known capture-free closure does not need a heap object or an indirect call when every use is a direct local invocation and the closure never escapes.
- Notes: Implemented in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs`; ordinary copy propagation first removes public-syntax aliases, then the lifted function keeps its unused hidden ABI slot while callers pass `Unit` directly. The analysis rejects closures that are captured, returned, stored, or forwarded to another call. Focused ANF, MIR, LIR, and native execution tests cover capture-free direct calls, captured-closure preservation, escaping closures, and repeated local calls. On the refreshed target, a repeated 100,000-call Cachegrind probe improved from `6,701,640` to `801,631` instructions (`88.0%`), while full quicksort remained `378,608,148` and the 19-workload routine profile was exactly neutral.

### Interprocedural captured-closure scalarization

- Optimization name: Interprocedural captured-closure scalarization
- Taxonomy category: Escape analysis / partial specialization
- Priority/rationale: Quicksort passes pivot-capturing predicates to `Stdlib.List.filter`, so removing those allocations and indirect calls requires specializing a known higher-order call chain rather than only rewriting lexical calls.
- Notes: A local-only one-scalar-capture prototype was rejected because its signature/body rewrite machinery did not improve a real workload. In the refreshed retained-candidate comparison, the scalar-captured 100,000-call control remained `7,701,640` instructions and quicksort remained `378,608,148`; the routine source scan found no other higher-order workload. A future attempt should specialize the known `Stdlib.List.filter` and recursive `Stdlib.List.__filterToReverse` chain while preserving capture evaluation and ownership; closures crossing unknown calls remain ineligible.
