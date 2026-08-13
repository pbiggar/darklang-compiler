---
format: 1
event-id: 019ffc4be6d978c3bbb66c42acedcf07
entity-id: 019ffc309edd7035b788d87fea1c78ee
entity-kind: issue
event-type: design-proposed
occurred-at: 2026-08-13T18:04:20.5693988+00:00
author: worker:65eb66d2e522:3476523:019ffc477bd07f42bc37b64ed73e8581
previous: 019ffc3515e17f408ed5bc668fc1d5b0
base-commit: 476902e8b1930c87d8d03029bdf29ceaf055e70c
constraints-hash: e10f8675600f684e4dfb6971ac9d55feab283675a4a3f69e23af9cb6cd6a7d93
revision: 019ffc3515e17f408ed5bc668fc1d5b0
---
# Design brief

## Problem

Compiler function syntax, typing, application, arity, and closure behavior are split across inconsistent paths and legacy fallbacks, creating observable gaps from the interpreter baseline.

## Goals

- Use compiler b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899 and interpreter 04fbe9dcc995c6188757d583e273cbd30a3e2d3d as the exact comparison baseline.
- Unify lambdas and declarations under consistent parameter, annotation, pattern, currying, application, and lexical-capture semantics.
- Match interpreter behavior for nested and returned closures, shadowed captures, argument evaluation, partial and excess application, higher-order calls, and arity failures.
- Classify compiler-only extensions and intentional divergences while removing legacy function and call fallbacks.

## Proposed solution

Adopt one canonical function-value and application model across parsing, typing, lowering, closure creation, and invocation. Preserve parameter structure and lexical environments consistently, and apply the pinned interpreter’s observable currying, evaluation-order, and failure semantics. Migrate compiler-authored functions and call sites to the canonical forms, then retire legacy fallbacks. Record revision-pinned source and same-source behavioral evidence for every retained difference.

## Success criteria

- Equivalent source produces matching values or observable failures on both pinned revisions across the required function and closure scenarios.
- Parameter syntax, annotations, patterns, application typing, closure capture, invocation, and arity behavior follow one coherent semantic model.
- Every remaining compiler-only behavior is explicitly classified as an extension or intentional divergence.
- Legacy lambda, declaration, and call fallbacks are absent after internal usages migrate.

## Open questions

- none

## Delivery shape

single issue
