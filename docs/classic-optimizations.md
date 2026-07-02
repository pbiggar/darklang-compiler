# Classic Optimizations

Persistent backlog for audit-driven classic compiler optimization work.

## Algebraic Simplification

- Optimization name: Modulo by one elimination
- Taxonomy category: Algebraic simplification
- Priority/rationale: Small, low-risk canonical simplification that removes a runtime remainder operation when the divisor is the literal `1`.
- Notes: Implemented in `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` during sandbox testing. Covered by `identity_mod_one` in `src/Tests/optimization/anf.opt`; quick benchmark check did not report a regression.

## Strength Reduction

- Optimization name: Multiplication by power-of-two lowering
- Taxonomy category: Strength reduction
- Priority/rationale: Existing ANF optimization; keep as catalog evidence rather than a candidate for this iteration.
- Notes: Implemented in `tryStrengthReduce`.

## Dead Code Elimination

- Optimization name: Unused ANF binding elimination
- Taxonomy category: Dead code elimination
- Priority/rationale: Existing optimization; keep as catalog evidence rather than a candidate for this iteration.
- Notes: Covered by existing ANF optimization tests.
