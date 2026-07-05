# Classic Optimizations Backlog

Persistent backlog for audit-driven classic compiler optimization work.

## Algebraic simplification

### Modulo by one elimination

- Optimization name: Modulo by one elimination
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that removes a runtime remainder operation when the divisor is the literal `1`. Low-risk canonical identity; previously used as the first Phase 1 sandbox trial target.
- Notes: Implemented in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during sandbox testing. Covered by `identity_mod_one` in `src/Tests/optimization/anf.opt`; quick benchmark check did not report a regression. Prior sandbox trial implemented `x % 1 -> 0` in ANF optimization.

### Integer self-comparison simplification

- Optimization name: Integer self-comparison simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, canonical boolean simplification with direct fit in MIR SSA constant folding; useful for eliminating provably constant branches.
- Notes: Implemented in MIR constant folding during the second Phase 1 sandbox trial and extended to integer ordering self-comparisons in the Guided review revision. Covered by MIR tests for `==`, `!=`, `<`, `>`, `<=`, and `>=`; negative tests cover float equality and source variable shadowing so the fold remains type- and operand-aware.

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

### Boolean comparison constant simplification

- Optimization name: Boolean comparison constant simplification
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical boolean simplification that removes redundant equality comparisons against boolean literals in ANF before backend lowering.
- Notes: Implemented for `x == true -> x`, `true == x -> x`, `x == false -> !x`, `false == x -> !x`, `x != true -> !x`, `true != x -> !x`, `x != false -> x`, and `false != x -> x` in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during Guided sandbox trials. The inequality cases use generic double-negation elimination so the false-literal forms do not need a one-off ANF shape. Covered by corresponding boolean equality, inequality, and double-negation tests in `src/Tests/optimization/anf.opt`.

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

## Strength reduction

### Multiplication by power-of-two lowering

- Optimization name: Multiplication by power-of-two lowering
- Taxonomy category: Strength reduction
- Priority/rationale: Existing ANF optimization; keep as catalog evidence rather than a candidate for this iteration.
- Notes: Implemented in `tryStrengthReduce`.

## Dead code elimination

### Unused ANF binding elimination

- Optimization name: Unused ANF binding elimination
- Taxonomy category: Dead code elimination
- Priority/rationale: Existing optimization; keep as catalog evidence rather than a candidate for this iteration.
- Notes: Covered by existing ANF optimization tests.
