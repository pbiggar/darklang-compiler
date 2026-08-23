# Conditional and Sequence Parity

This matrix records the revalidated behavior of conditional and sequential
expressions. It supersedes conditional/statement implications in the historical
DCB1 report at commit `8a402797`; that report was used only as an inventory.

## Evidence revisions

- Approved compiler evidence revision: `b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899`.
- Compiler implementation revision: `50ccf016ee8faeb37640840d81bad99d932574f1`,
  rebased onto `6cf18024e08406246c81c1399913c996651105b7`.
- Post-rebase name-resolution repair revision:
  `aee595658b4a6dbf926f5c870dc6c0232d08695e`.
- Darklang interpreter revision compared in every row:
  `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.

The interpreter revision was checked out directly and inspected at that exact
HEAD. Compiler history is reproducible with `git show <revision>:<path>`.

## Source map

Compiler ownership is split between the canonical
[parser](../src/DarkCompiler/passes/1_InterpreterParser.fs),
[type checker](../src/DarkCompiler/passes/1.5_TypeChecking.fs), and
[AST-to-ANF lowering](../src/DarkCompiler/passes/2_AST_to_ANF.fs). The ANF `If`
is converted to a typed shared result register and CFG join in
[ANF-to-MIR](../src/DarkCompiler/passes/3_ANF_to_MIR.fs). Runtime output support
in [Runtime.fs](../src/DarkCompiler/Runtime.fs) makes selected results observable;
focused failures use the existing compiler-generated runtime-error operation,
whose callable contract is outside this work item.

The pinned interpreter sources are:

- [conditional and block parsing](https://github.com/darklang/dark/blob/04fbe9dcc995c6188757d583e273cbd30a3e2d3d/backend/src/LibParser/Parser.fs#L817-L847), including [`if`/`elif`](https://github.com/darklang/dark/blob/04fbe9dcc995c6188757d583e273cbd30a3e2d3d/backend/src/LibParser/Parser.fs#L1240-L1303);
- [`EIf` and `EStatement` program types](https://github.com/darklang/dark/blob/04fbe9dcc995c6188757d583e273cbd30a3e2d3d/backend/src/LibExecution/ProgramTypes.fs#L415-L514);
- [conditional lowering](https://github.com/darklang/dark/blob/04fbe9dcc995c6188757d583e273cbd30a3e2d3d/backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs#L766-L821) and [statement lowering](https://github.com/darklang/dark/blob/04fbe9dcc995c6188757d583e273cbd30a3e2d3d/backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs#L1177-L1186);
- [runtime Boolean-condition check](https://github.com/darklang/dark/blob/04fbe9dcc995c6188757d583e273cbd30a3e2d3d/backend/src/LibExecution/Interpreter.fs#L1935-L1942) and [runtime Unit-statement check](https://github.com/darklang/dark/blob/04fbe9dcc995c6188757d583e273cbd30a3e2d3d/backend/src/LibExecution/Interpreter.fs#L2027-L2038).

The pinned interpreter `TypeChecker.fs` has no `EIf` or `EStatement` case.
Those expression contracts are dynamic at this revision, as the evaluator
locations above demonstrate.

## Behavior matrix

Unless a row says otherwise, both sides agree. Probe line references name the
same compiler source exercised through the full language pipeline.

| Area | Pinned interpreter behavior | Compiler behavior | Classification | Probe |
|---|---|---|---|---|
| Conditional syntax | `if … then …`, optional `else`, `elif`, and `else if` | Same shapes; `elif` lowers to nested `If` and a missing `else` supplies Unit | matched | [`conditional_sequence_parity.e2e`](../src/Tests/e2e/conditional_sequence_parity.e2e), lines 4-5; `testConditionalSequenceSameSourceShape` |
| Rejected conditional syntax | Missing `then` is a parse error | Same precise parse error after `if` or `elif` | matched | syntax fixtures and the E2E corpus parse contract |
| Condition type | A non-Boolean condition fails when `JumpByIfFalse` executes | The type checker requires `Bool`, so an otherwise identical failure occurs during compilation | intentional timing divergence: static compiler | E2E condition-type case |
| Branch result types | Branches may have different runtime value types; only the selected value is produced | Both branches are recursively unified before lowering; heterogeneous arms are rejected | intentional type-system divergence: static compiler | E2E string/Int64 rejection and nested/inferred-list cases |
| Nested conditionals | Nested `EIf` values use nested selected control flow | Recursive type checking and nested CFG joins preserve the unified result type | matched, subject to static typing above | E2E nested scalar and inferred-list cases |
| One-arm conditional | A missing `else` produces Unit only when the condition is false | Parser inserts Unit; therefore a non-Unit `then` arm is rejected by branch unification, while a Unit arm is accepted | intentional type-system divergence for non-Unit arms | E2E Unit case and upstream `eif.dark` cases |
| Sequence syntax | Blocks accept same-column newline or `;` separators and construct right-nested `EStatement` | Parenthesized `;` sequences construct right-nested `Sequence`; interpreter-mode trailing-let blocks now use `Sequence`, never wildcard `Match` | matched for the shared semicolon form | E2E final-value cases; same-source AST test |
| Sequence head type | Each non-final result is checked for Unit immediately before the next expression | Every non-final expression is checked against Unit during compilation | intentional timing divergence: static compiler | E2E non-Unit-head rejection |
| Sequence result type | The final expression supplies the runtime value | Only the final expression supplies the inferred enclosing type, recursively | matched | E2E Int64, String, nested, and conditional-branch cases |
| Conditional evaluation | Condition runs once; only the selected branch runs | Condition ANF is bound once. Computed branches cannot use eager `IfValue`; they lower to branch blocks and a shared typed join | matched; historical eager-primitive bug resolved | E2E divide-by-zero and runtime-error skipped-arm cases; raw-pointer condition-once case |
| Sequence evaluation | Head, Unit check, then tail; a failing head prevents the tail | `bindReturns` orders head before tail and carries only the tail value; failure or non-termination prevents continuation | matched | E2E first-vs-second failure case and raw-pointer write-order case |

## Syntax inventory and migration

Repository-wide searches of the parser productions and language sources found no
compiler-only conditional source form to migrate. Optional `else`, `else if`, and
`elif` are all interpreter-supported at the pinned revision.

The canonical parser formerly contained a compiler-only representation: a
parenthesized statement followed by a trailing `let` was encoded as a wildcard
`Match`. That production has been migrated to the explicit `Sequence` AST case,
and its parser regression now asserts that shape. No standard-library, fixture,
example, or documentation expression depended on the wildcard-match encoding.

The compiler consumes the interpreter-compatible grammar through this single
parser. Parenthesized semicolon sequences and layout-sensitive statement blocks
therefore share one parsing path before type checking and lowering.

## Probe coverage

The focused language matrix covers accepted syntax, Boolean and rejected
conditions, equal and unequal branch types, inferred and nested arms, Unit and
distinct final sequence types, selected and skipped branches, primitive failure
in an untaken arm, first failure, condition-once, and left-to-right effects.
Malformed conditional sources are parser-level regressions because the E2E
corpus roundtrip contract requires every E2E source itself to parse.

Performance is not part of this comparison. The raw-pointer and runtime-error
operations are used only to expose selection and order; their contracts remain
owned by their runtime modules.
