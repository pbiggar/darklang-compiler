---
format: 1
event-id: 019ffc081fba7c34bdd62529ffdbf4ed
entity-id: 019ff58b47c772318f41cbbcb86ff291
entity-kind: issue
event-type: design-proposed
occurred-at: 2026-08-13T16:50:18.6826476+00:00
author: worker:65eb66d2e522:3436513:019ffbf929d7713bbbadb2c500f56323
previous: 019ffc06f3777c52918ccfe2cf94645f
base-commit: e8caee384b079767c611231d40c2edb0f6b4c59d
constraints-hash: e646027115bd63ae6634df307340bf0add1f07691802d085d332d9e6fd77a483
revision: 019ffc06f3777c52918ccfe2cf94645f
---
# Design brief

## Problem

There is no fully revalidated, version-locked inventory of observable behavior differences between the compiler and the current interpreter for capabilities both claim to support.

## Goals

- Establish the current interpreter’s public behavior as the parity baseline.
- Identify semantic, error, ordering, representation, and side-effect differences while excluding already-owned feature gaps and non-observable performance differences.
- Distinguish parity defects, compiler-only extensions, and intentional divergences using current evidence.
- Define contained, independently deliverable work without fragmenting related behavior.

## Proposed solution

Create a comparison ledger pinned to the exact compiler HEAD and darklang/dark revision. Treat the prior report and current parity documentation only as leads, then revalidate each candidate against current source and, where practical, a focused same-source comparison. Remove findings already owned by module or language-feature efforts, classify every retained difference, consolidate closely related function, type, and behavioral evidence, and propose one child issue for each independently observable difference or genuine design boundary.

## Success criteria

- Every retained comparison records both exact revisions and current supporting evidence.
- Every proposed gap reflects behavior supported by both implementations and is classified against the interpreter baseline.
- Compiler-only extensions and intentional divergences are explicitly separated from parity defects.
- Focused comparisons substantiate findings wherever practical, and unsupported or stale findings are removed.
- Existing ownership is respected, related differences remain contained, and each proposed child is independently reviewable.
- Performance-only differences are absent unless they alter observable behavior.

## Open questions

- none

## Delivery shape

child issues
