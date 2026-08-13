---
format: 1
event-id: 019ffc309ede7f53ad3e6b53ba2ccdc6
entity-id: 019ff58b47c77dcaa4abc5287cf76501
entity-kind: issue
event-type: analyzed
occurred-at: 2026-08-13T17:34:32.6700474+00:00
author: worker:65eb66d2e522:3461018:019ffc2be36570229c864f9f740c6c48
previous: 019ff7d76bc77acf9579b34ab6620461
base-commit: 40167e8190e0af58d7be55de242f6639b0ab8b2c
constraints-hash: e646027115bd63ae6634df307340bf0add1f07691802d085d332d9e6fd77a483
revision: 019ff7d76bc77acf9579b34ab6620461
---
# Breakdown

Re-audit compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899 against darklang/dark 04fbe9dcc995c6188757d583e273cbd30a3e2d3d. DCB1 report 8a402797 and current parity documents are leads only. Each child owns its feature from source syntax through observable execution, including focused same-source evidence and affected repository-source migration. Unclassified legacy public syntax is removed; performance-only differences are excluded. Interpreter F# Builtins define public contracts, not compiler architecture: equivalent compiler Dark/native implementations remain, List and Dict contracts stay with their feature children, and the complete compiler Dict implementation is retained while its exposed types are restricted to interpreter limits.

## Proposed issue 019ffc309edd7aba9eaab8fdf910f85d

Title: Primitive literals and scalar values

Outcome:

Match interpreter unit, Boolean, integer, float, character, and string syntax and observable value semantics.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own tokenization, escape decoding, parsing, literal typing, constant lowering, rendering, and runtime representation together.
- Cover numeric boundaries, malformed tokens, Unicode scalar handling, string and character escapes, negative-literal boundaries, and interpreter-exposed floating-point special values.
- Use src/DarkCompiler/passes/1_Parser.fs, src/DarkCompiler/passes/1.5_TypeChecking.fs, src/DarkCompiler/passes/2_AST_to_ANF.fs, src/DarkCompiler/Runtime.fs, and literal e2e cases as compiler anchors; record corresponding pinned interpreter parser, type, runtime-value, and test locations.
- Remove superseded literal spellings and migrate literal-bearing tests, stdlib code, examples, and executable fixtures.

## Proposed issue 019ffc309edd715895df83208a72850d

Title: Identifier, keyword, and qualified-name grammar

Outcome:

Accept the interpreter’s lexical grammar for identifiers, reserved words, capitalization, and qualified names.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own lexical acceptance and parse structure only; semantic lookup of parsed names belongs to callable and namespace resolution.
- Cover value names, bindings, parameters, types, constructors, fields, modules, packages, qualification segments, reserved words, and ambiguous token boundaries.
- Anchor compiler evidence in src/DarkCompiler/passes/1_Parser.fs and parser-facing e2e cases; record matching pinned interpreter lexer/parser definitions and focused probes.
- Remove keyword aliases, capitalization exceptions, and legacy identifier forms from repository-authored source.

## Proposed issue 019ffc309edd734f912a26ece696bd02

Title: Operators, precedence, and pipelines

Outcome:

Match interpreter operator tokens, precedence, associativity, partial application, desugaring, and pipeline behavior.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Treat prefix, infix, parenthesized, sectioned, partially applied, and pipeline positions as one expression-grammar boundary.
- Own primitive operator invocation when desugaring determines argument order, evaluation order, arity, or failure timing; general comparison semantics remain in the comparability child.
- Anchor compiler evidence in src/DarkCompiler/passes/1_Parser.fs, src/DarkCompiler/passes/2_AST_to_ANF.fs, src/DarkCompiler/Runtime.fs, src/DarkCompiler/Stdlib.fs, and operator e2e cases; record corresponding interpreter parser and Builtins evidence.
- Remove legacy tokens, precedence rules, pipeline rewrites, and operator aliases while migrating all affected expressions.

## Proposed issue 019ffc309edd7f4e9049233e0434440f

Title: Bindings, scopes, and shadowing

Outcome:

Implement interpreter binding syntax with matching visibility, destructuring, shadowing, and duplicate-binding behavior.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own non-recursive bindings across nested expressions, conditionals, matches, lambdas, and sequential expressions; recursive availability belongs to the recursion child.
- Cover pattern-bound names, annotation placement, use-before-binding, duplicate names, nested shadowing, and visibility after failed or skipped expressions.
- Anchor compiler evidence in src/DarkCompiler/passes/1_Parser.fs, src/DarkCompiler/passes/1.5_TypeChecking.fs, src/DarkCompiler/passes/2_AST_to_ANF.fs, and binding e2e cases; record matching interpreter parser, name-binding, and evaluation evidence.
- Remove compiler-specific binding forms and migrate affected repository-authored source.

## Proposed issue 019ffc309edd7035b788d87fea1c78ee

Title: Functions, application, and closures

Outcome:

Match interpreter lambdas, function declarations, currying, application, arity behavior, and lexical closures.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own parameter syntax, annotations, parameter patterns, application typing, partial application, closure capture, invocation, and argument evaluation order.
- Cover nested and returned closures, shadowed captures, zero or multiple parameters, under-application, over-application, higher-order calls, and observable arity failures; recursive binding availability remains separate.
- Anchor compiler evidence in the parser, type checker, ANF conversion, closure/runtime support, and function e2e corpus; record corresponding pinned interpreter function-value and application evidence.
- Remove legacy lambda, declaration, and call fallbacks while migrating compiler-authored functions and call sites.

## Proposed issue 019ffc309edd705b80fa4d76b2fe2b4c

Title: Recursion and mutually recursive groups

Outcome:

Match interpreter recursive declaration syntax and observable recursive and mutually recursive binding semantics.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Represent recursive availability and group membership explicitly through resolution, typing, and lowering without spelling conventions or sentinels.
- Cover recursive functions and values, mutual groups, annotations, polymorphic restrictions, shadowing, invalid cycles, and references across group boundaries.
- Treat stack or tail-call behavior as parity evidence only when it changes whether a valid same-source program completes or fails observably.
- Anchor compiler evidence in the parser, type checker, ANF conversion, recursion tests, and tail-call tests; remove legacy recursive syntax after migrating affected sources.

## Proposed issue 019ffc309edd7548a2e311333ed37f53

Title: Conditional and sequential evaluation

Outcome:

Match interpreter conditional and sequencing syntax, typing, evaluation order, and failure timing.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own condition typing, branch unification, nested conditionals, sequence result typing, skipped-branch behavior, and left-to-right execution.
- Use terminating, failing, or observable builtins only as focused probes of branch selection and order; their callable contracts remain in their owning module.
- Anchor compiler evidence in the parser, type checker, ANF conversion, Runtime.fs, and control-flow e2e cases; record matching interpreter parsing, typing, and evaluation locations.
- Remove compiler-only conditional or sequencing forms and migrate affected expressions.

## Proposed issue 019ffc309edd7473ac93e8368d96a8ca

Title: Callable and namespace resolution

Outcome:

Resolve interpreter-spelled local, module, package, builtin, constructor, value, function, and type names with matching precedence and ambiguity behavior.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Consume qualified names produced by the lexical grammar child and model resolvable name kinds explicitly rather than using spelling-based fallbacks.
- Cover collisions among local values, functions, constructors, user types, modules, packages, and builtins, including unresolved and ambiguous references.
- Anchor compiler evidence in src/DarkCompiler/passes/1.5_TypeChecking.fs, src/DarkCompiler/passes/2_AST_to_ANF.fs, src/DarkCompiler/Stdlib.fs, and resolution e2e cases; record corresponding interpreter resolution and Builtins registration locations.
- Remove implicit builtin qualification, compiler-only lookup shortcuts, and callable/value coercion fallbacks.

## Proposed issue 019ffc309edd74118e1cde555ad84356

Title: Tuple language support

Outcome:

Match interpreter tuple construction, typing, access, destructuring, matching, equality, and runtime values.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own tuple arity and element order through parsing, typing, ANF, pattern lowering, equality, rendering, and runtime representation.
- Cover grouping ambiguity, singleton rejection or interpretation, nested tuples, heterogeneous elements, destructuring, match behavior, and evaluation order; unit value semantics remain in primitive literals.
- Anchor compiler evidence in the parser, type checker, ANF conversion, Runtime.fs, and tuple e2e cases; record matching interpreter tuple-type, value, pattern, and test locations.
- Remove superseded tuple syntax and access forms while migrating tuple-bearing sources.

## Proposed issue 019ffc309edd71d283c728ef8bc0d52b

Title: List language and List module parity

Outcome:

Match interpreter list and cons language behavior plus the public List module contract while retaining equivalent compiler implementations.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own list literals, cons syntax, inference, construction, matching, equality, runtime representation, and List callable signatures and behavior.
- Cover empty-list inference, heterogeneous rejection, nested and cons patterns, associativity, construction and callback order, supported element types, and language-visible failures.
- Use the interpreter’s F# Builtins List module as the public behavioral contract without requiring its implementation architecture; retain compiler Dark/native List code where same-source behavior matches.
- Anchor compiler evidence in the parser, type checker, ANF conversion, Runtime.fs, Stdlib.fs, and list e2e cases; keep individual missing List types, functions, and behavioral differences as contained tasks in this child.
- Remove compiler-only list syntax and public List aliases while migrating affected sources.

## Proposed issue 019ffc309edd78fca7b921ceafda6574

Title: Dict type and module parity

Outcome:

Preserve the compiler’s complete Dict implementation while matching the interpreter’s exposed Dict types, callable surface, and observable behavior.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Do not replace or reduce the HAMT/runtime implementation; restrict accepted key, value, and generic type combinations to the pinned interpreter’s public limitations.
- Match public Dict type names, callable names, signatures, arities, construction and lookup results, duplicate-key behavior, traversal guarantees, equality behavior, and language-visible failures.
- Use the interpreter’s F# Builtins Dict module as the contract while allowing compiler Dark code and native representation to remain implementation details.
- Anchor compiler evidence in src/DarkCompiler/Stdlib.fs, src/DarkCompiler/Runtime.fs, docs/features/dict-hamt.md, the type checker, and Dict e2e cases; keep individual callable and type differences as contained tasks in this child.
- Remove only public Dict extensions or aliases that exceed the parity surface; preserve non-public implementation helpers.

## Proposed issue 019ffc309edd74eb93127dc9270bf9bf

Title: Record language support

Outcome:

Match interpreter record declaration, construction, access, update, typing, pattern, equality, and runtime identity behavior.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own parser, type checker, lowering, matching, equality, rendering, and runtime-representation changes for records.
- Cover declaration syntax, nominal versus structural identity, field order, duplicate or missing fields, nested access and updates, update evaluation order, generic records, and record patterns.
- Anchor compiler evidence in the parser, type checker, ANF conversion, Runtime.fs, and record e2e cases; record matching pinned interpreter record-type, expression, pattern, and runtime evidence.
- Remove legacy declaration, literal, access, update, and pattern forms while migrating record-bearing sources.

## Proposed issue 019ffc309edd79809d0c699e6fc5a809

Title: Algebraic data types and constructors

Outcome:

Match interpreter user-defined type and constructor syntax, resolution, typing, construction, and runtime identity.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own nullary and payload constructors, generic parameters, duplicate definitions, constructor namespaces, payload evaluation, rendering, and equality identity.
- Cover constructor collisions across types and modules, incorrect payload arity, recursive types, generic instantiation, qualified construction, and invalid declarations.
- Anchor compiler evidence in src/DarkCompiler/passes/1_Parser.fs, src/DarkCompiler/passes/1.5_TypeChecking.fs, src/DarkCompiler/passes/2_AST_to_ANF.fs, Runtime.fs, and ADT e2e cases; record matching interpreter type-declaration and runtime-value evidence.
- Remove compiler-specific type declarations and constructor forms while migrating affected sources.

## Proposed issue 019ffc309edd76599a240568be916476

Title: Pattern matching

Outcome:

Match interpreter match syntax, supported patterns, bindings, guards, branch selection, exhaustiveness behavior, and match failures.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Inventory only pattern forms exposed by the pinned interpreter, including applicable literal, variable, wildcard, tuple, list, record, constructor, nested, guarded, and alternative patterns.
- Preserve left-to-right tests, binding availability, repeated-name behavior, guard typing and evaluation, unreachable-branch diagnostics, and observable non-exhaustive failure.
- Keep value-specific construction and representation in tuple, list, record, and ADT children; this child owns shared pattern grammar, typing, decision behavior, and failure semantics.
- Anchor compiler evidence in the parser, type checker, ANF conversion, Runtime.fs, and match e2e cases; remove unsupported compiler-only patterns and migrate match expressions.

## Proposed issue 019ffc309edd7a1ba18826b069266ab7

Title: Type inference, annotations, and polymorphism

Outcome:

Match interpreter type-variable syntax and source-visible inference, unification, generalization, instantiation, and annotation behavior.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own source-level generic behavior across bindings, functions, recursive definitions, user types, constructors, annotations, and constrained uses.
- Cover generalization boundaries, value restrictions if exposed, annotation subsumption, occurs checks, repeated type variables, inference failures, and error phase.
- Keep monomorphization internal and permit compiler-specific specialization only when every accepted program and language-visible result matches the interpreter.
- Anchor compiler evidence in the parser, src/DarkCompiler/passes/1.5_TypeChecking.fs, src/DarkCompiler/passes/2_AST_to_ANF.fs, generic e2e cases, and docs/features/generics.md; record corresponding interpreter type-checker locations.
- Remove compiler-only generic syntax and implicit type fallbacks while migrating annotations and type parameters.

## Proposed issue 019ffc309edd70169604258f7ca923d8

Title: Equality, ordering, and comparability

Outcome:

Match the interpreter’s equality and ordering operations across every supported and rejected value category.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Cover unit, Boolean, integer, float edge cases, character, string, tuple, list, record, constructor, Dict where exposed, nested values, and function values.
- Match structural versus nominal behavior, ordering rules, NaN and signed-zero behavior, rejection phase, and language-visible failure values or messages where public behavior distinguishes them.
- Own comparison type constraints, operator lowering, and generic runtime comparison; Dict-specific admissible key restrictions remain in the Dict child.
- Anchor compiler evidence in the type checker, operator lowering, Runtime.fs, Stdlib.fs, and comparison e2e cases; use interpreter operator spelling in all probes and remove comparison aliases.

## Proposed issue 019ffc309edd7d59a562a72a37d8a4bb

Title: Program declarations, modules, and entry structure

Outcome:

Match interpreter program-level syntax, declaration visibility, ordering, module structure, package references, entry selection, and whole-program validation.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Own interactions among top-level values, functions, recursive groups, types, constructors, modules, packages, and executable entry expressions.
- Cover declaration ordering, duplicate declarations, module boundaries, qualified visibility, forward references, source-file composition, missing or multiple entry points, and validation timing.
- Exclude native object and executable layout unless it changes source-level entry behavior or program completion.
- Anchor compiler evidence in src/DarkCompiler/CompilerLibrary.fs, the parser, type checker, whole-program e2e fixtures, and CLI entry handling; remove implicit entry fallbacks and legacy top-level forms while migrating repository programs.

## Proposed issue 019ffc309edd7d63a48ddb89bfcd8e90

Title: Builtin and package callable contracts

Outcome:

Match the interpreter’s remaining public builtin and package callable contracts while preserving equivalent compiler Dark and native implementations.

Constraints:

- Evidence revisions: compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899; interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d.
- Use interpreter F# functions registered through Builtins as the behavioral baseline for names, signatures, arities, type restrictions, results, evaluation order, and language-visible failures; do not require compiler calls to share that implementation route.
- Retain compiler implementations written in Dark or backed by compiler-specific native representations whenever public behavior matches; classify non-public runtime helpers separately from language-callable extensions.
- Exclude operators, List, and Dict contracts owned by their respective children; inventory remaining scalar, text, numeric, option/result, conversion, and package/platform modules without creating children for individual functions or types.
- Keep missing functions, missing types, overload differences, and individual result differences as contained module tasks; organize platform-service evidence and tests by independently testable public module rather than by callable.
- Anchor compiler evidence in src/DarkCompiler/Stdlib.fs, src/DarkCompiler/Runtime.fs, the type checker, callable registration/lowering, and builtin/package e2e cases; record the exact pinned interpreter Builtins registration and library-module locations for every retained finding.
- Remove public compiler-only callable aliases or extensions unless explicitly recorded as an intentional divergence; preserve internal facilities that are unreachable from the parity surface.
