# Parsed and checked AST boundary

This plan tracks the source-to-checked syntax split. The goal is that a public
parser result cannot contain compiler-generated or partially resolved nodes,
while a checked program cannot contain unchecked names, missing inferred types,
or unresolved declarations. Semantic validity that depends on an environment
(including match exhaustiveness and type compatibility) remains an ahead-of-time
checker obligation.

## 1. Source-only parsed tree

- [x] Give parsed expressions, patterns, references, declarations, and programs
  phase-specific types instead of instantiating the checker's generic syntax.
- [x] Exclude `IndirectApply`, `Closure`, `RuntimeError`, and `BoundaryRender`
  from parsed expressions. Preserve source order, written annotations, and
  parser diagnostics.
- [x] Make the parser, name normalization, formatting, and public package
  entry points use the source-only types.

## 2. Explicit checker input

- [x] Convert parsed syntax once into an internal semantic/checker tree.
  Conversion must be exhaustive over the source-only cases and introduce no
  generated operation implicitly.
- [x] Keep generated forms needed by the checker in that internal tree; document
  where they are introduced. Remove obsolete parsed-case handling.
- [x] Keep public source checks at the parser/checker boundary, including the
  privileged raw-pointer spelling rule.

## 3. Checked output

- [x] Make successful checking the initial construction path for checked programs.
  Carry resolved IDs, complete lambda types, validated value bodies, resolved
  recursive evidence, and declaration types in checked-specific forms.
- [x] Remove phase-spanning optional fields and candidate variants from checked
  output. Preserve compile-time match validation and generated helper behavior.
- [x] Build with `./build --ai`, run `./run-tests --ai`, verify parent-relative
  performance with `./benchmarks/run_benchmarks.sh --verify-parent full`, and
  review the final diff.

## Later decision

A separate prepared expression tree is useful only if an inventory finds forms
introduced exclusively after checking. Do not duplicate the checked tree merely
to rename it. Backend representation and ABI type classes are separate work.

The checker converts the source-only tree into the internal
`AST.ExprNode<SemanticType>`. Checker-generated runtime errors, closures, indirect
calls, and rendering operations remain there. Successful checking creates the
private `CheckedAST.Program`; compiler-internal preparation transformations
rebuild it through an internal function. External callers can inspect but cannot
construct or replace a checked program. The checked declaration wrapper
certifies its type fields while preserving the semantic declaration shape needed
for layout and alias registries.
