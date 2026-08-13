---
format: 1
event-id: 019ff8edc3ce74df8aba6a39b2f9f94e
entity-id: 019ff64a76b07d5b98253f043067cced
entity-kind: issue
event-type: trial-result
occurred-at: 2026-08-13T02:22:39.5663738+00:00
author: worker:65eb66d2e522:565637:019ff82b05da79b58291b12a490d55a1
previous: 019ff7d8e5087ebf961a5dde4a974f53
attempt: 019ff8d59ea97bdd80b2751b474f0169
constraints-hash: 1fdde340df530a344d772be118d649d26dad31ba97cabb4a40cf9e423bcc37e2
result: no-improvement
revision: 019ff7d8e5087ebf961a5dde4a974f53
---
# Trial result

Rejected: the last-use index regressed fasta LIR Peephole median by 48.0%. Candidate and instrumentation removed; no commit or integration.

## Evidence

Profile: fasta made 12 suffix-scan calls examining 0 instructions total, confirming the scans were not material.

10 alternating fasta samples (LIR Peephole ms): before [8.8,5.1,5.1,5.7,4.7,9.2,4.9,4.8,7.1,4.7], median 5.10; after [5.2,5.2,7.6,5.4,12.7,14.7,7.3,9.8,7.7,7.5], median 7.55; delta +2.45 ms (+48.0%). Total compilation ms: before [367.0,349.6,329.8,384.3,330.8,335.9,336.7,342.1,350.2,333.2], median 339.4; after [331.1,344.6,339.8,343.0,345.3,356.1,347.1,340.7,355.6,334.7], median 343.8; delta +4.4 ms (+1.3%).

Routine one-shot LIR/total deltas (ms): ackermann +0.0/-0.3; binary_trees +0.0/+1.7; collatz +0.0/+7.7; edigits +0.1/+7.1; factorial +0.0/+3.5; fasta +0.9/-0.4; fib +0.0/-16.5; leibniz +0.0/-5.9; mandelbrot +0.0/-4.8; matmul -0.2/-20.8; merkletrees -0.1/+2.2; nbody +0.2/+14.9; nqueen +0.0/-6.6; pisum +0.0/-0.9; primes +0.0/+1.0; quicksort +0.2/-1.9; spectral_norm +0.0/-5.5; sum_to_n +0.0/-2.2; tak +0.0/-3.2. These rounded single samples were noisy; the alternating fasta medians determined rejection.

10 ./run-tests --ai wall-clock samples (s): before [41.881,36.179,35.445,35.662,35.800,35.599,35.953,35.932,35.755,35.614], median 35.778; after [36.113,36.037,35.838,35.764,35.671,36.106,36.010,35.782,35.630,35.694], median 35.810; delta +0.032 s (+0.09%). All 20 runs passed; focused LIR tests also passed 12/12.

Correctness: 19/19 optimized LIR dumps, binaries, and native benchmark outputs were byte-identical before/after.

./benchmarks/run_benchmarks.sh --verify routine was run but failed against the rebased repository state. Measured instructions: ackermann 11,450,298,699; binary_trees 685,136,037; collatz 81,543,096; factorial 64,259; fib 642,006,247; leibniz 900,001,537; mandelbrot 17,701,943; matmul 2,124,302,103; merkletrees 724,164,802; nbody 1,239,502,637; nqueen 295,286,952; pisum 95,458; primes 2,075,933; quicksort 378,608,163; spectral_norm 143,862,590; sum_to_n 71,885; tak 63,580,601. Edigits and fasta were skipped because the runner expected unquoted output but both identical baseline/candidate binaries produced quoted output. The committed RESULTS.md performance ratio is 2.74x.

Candidate diff temporarily replaced five floating-result suffix checks with one Map-based last-use index per instruction list (one file, 44 insertions/33 deletions). It was removed after rejection. Final worktree is clean; no commit was created.
