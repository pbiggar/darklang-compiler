# Pass Optimization Tooling Evidence

## Operational Surface

`tools/pass_optimization.py` implements all three commands registered in
`agent.json`. Each requires an explicit benchmark. Inspect and verify also
require an exact recognized pass; verify additionally requires two revisions,
an IR kind, and the named `executable` focused check. No command edits compiler
files or uses the network. Deterministic input-derived artifact names make
reruns replace only their own ignored evidence directory.

The timing parser accepts integer and decimal stage numbers, normalizes Unicode
arrows and display suffixes, and pairs a stage only with its immediately next
timing line. This matters for current nested stages: `ANF Inlining` is followed
by `ANF Direct-Call Specialization`, so the outer timing is deliberately not
misattributed. Unknown adjacent labels and a timing set with no recognized
pairs are invalid evidence.

## Exact Representative Commands

```text
python3 agents/001-pass-optimization/tools/pass_optimization.py inventory --benchmark benchmarks/problems/edigits/dark/main.dark
python3 agents/001-pass-optimization/tools/pass_optimization.py inspect --pass "SSA Construction" --benchmark benchmarks/problems/edigits/dark/main.dark
python3 agents/001-pass-optimization/tools/pass_optimization.py verify --pass "SSA Construction" --benchmark benchmarks/problems/edigits/dark/main.dark --baseline HEAD --candidate HEAD --ir mir --focused-check executable
```

The identical-revision verification is a negative control: correctness gates
must pass, but the contract decision must be `reject` with reason
`same-revision-control`.

## Measurements And Correctness

| Role | Child commands | Stdout bytes | Estimated stdout tokens | Correctness result |
| --- | ---: | ---: | ---: | --- |
| inventory | 1 | 424 | 106 | 19 adjacent recognized pairs; SSA Construction present in the ranked timings |
| inspect | 1 | 517 | 130 | SSA mapped to `3.1_SSA_Construction.fs`; bounded history and structural evidence retained |
| verify | 18 | 378 | 95 | executable bytes, normalized MIR, and execution output identical; unchanged candidate rejected |
| **Combined** | **20** | **1,319** | **330** | **all registered-command gates passed** |

The verify control measured baseline SSA samples with a 203.4 ms median and
candidate samples with a 231.1 ms median. Its 64.8 ms noise threshold prevented
the timing difference from implying a win; the stronger unchanged-revision
rule produced `reject: same-revision-control`. The complete command records,
samples, hashes, and output are in the ignored artifact
`.dcb/tool-artifacts/pass-optimization-verify-fab0c630b877`.

## Stop Limits And Error Behavior

- One repository-local benchmark of at most 2 MiB; captured child output at
  most 16 MiB; stdout at most 4096 bytes.
- Inventory ranks at most 10 passes. Inspect reads at most 20 history entries
  and reports at most 200 bounded matches. Verify uses exactly one warmup and
  three measured runs per revision.
- Compiler children time out at an explicitly bounded 1–600 seconds;
  executable checks stop after 60 seconds. Verify runs no broad gates.
- Exit 2: invalid arguments or unsupported target/pass/check. Exit 3: invalid
  evidence, timeout, or failed child. Exit 4: unexpected internal failure or
  summary overflow. Valid `retain` and `reject` decisions exit zero.
- All complete bounded output, commands, samples, hashes, searches, and stop
  metadata remain in the reported ignored artifact; stdout is one concise JSON
  object and never contains the artifact body.

## Baseline And Acceptance Gates

The prior full-size edigits SSA investigation used 77 commands, 125,038 stdout
bytes, approximately 31,260 stdout tokens at four bytes/token, 6,035,750 input
tokens, and 36,876 output tokens in its reported model telemetry. The new tools
used 20 commands (74.0% fewer), 1,319 stdout bytes (98.9% fewer), and about 330
stdout tokens (98.9% fewer). Every command exited zero, every concise summary
was below 4096 bytes, and all inventory, inspection, equality, decision, and
artifact gates passed. These measurements satisfy operational promotion.

## Remaining Limitations

The pass map is intentionally explicit and fails closed when compiler stage
labels change. Structural hypotheses are lexical leads, not profiling proof.
Only executable execution is currently a supported focused check; pass-specific
test filters and the broad verification policy remain deliberate follow-up
commands after a real candidate is retained.
