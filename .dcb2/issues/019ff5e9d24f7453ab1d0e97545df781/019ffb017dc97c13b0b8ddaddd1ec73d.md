---
format: 1
event-id: 019ffb017dc97c13b0b8ddaddd1ec73d
entity-id: 019ff5e9d24f7453ab1d0e97545df781
entity-kind: issue
event-type: trial-result
occurred-at: 2026-08-13T12:03:26.7931965+00:00
author: worker:65eb66d2e522:1846739:019ffaced0427095a7c17c113dc069be
previous: 019ff7d8e50c7c9ba18fd7ce28976432
attempt: 019ffacee50e738ab4fd21ea43394478
constraints-hash: 1ee40d765f065ff477ef0395700771a24f492028cf5c80d36f03a6db20667320
result: no-improvement
revision: 019ff7d8e50c7c9ba18fd7ce28976432
---
# Trial result

The functional worklist replacement was neutral/noisy for fasta and regressed test-suite wall time, so it was discarded. The worktree is clean and no commit was created.

## Evidence

Profiling confirmed material execution: 4,483 reachability calls, 41,393 steps, and 54,879 queue cells copied per fasta compile. Interleaved fasta SSA timings (ms), baseline=[13.6,14.5,13.0,13.8,20.1,14.5,14.4,12.9,15.8,16.3], candidate=[12.7,17.4,16.8,13.5,13.2,14.0,19.3,15.2,14.2,13.5]; medians 14.45 -> 14.10, delta -0.35 ms (-2.42%), with heavily overlapping/noisy samples. Total compilation (ms), baseline=[266.0,263.9,269.0,255.1,272.2,276.9,283.1,262.2,276.7,280.6], candidate=[271.9,260.2,296.8,265.1,262.3,259.7,268.4,272.3,272.2,276.3]; medians 270.60 -> 270.15, delta -0.45 ms (-0.17%), neutral. Warmed ./run-tests --ai wall-clock (s), baseline=[30.858,30.570,30.969,33.098,32.894,32.022,31.698,33.692,31.822,32.320], candidate=[29.551,29.978,38.358,44.241,33.614,33.580,34.594,36.885,34.901,33.625]; medians 31.922 -> 34.110, delta +2.188 s (+6.85%); all 20 runs passed. Routine single-run SSA/total deltas in ms (baseline -> candidate): ackermann 0.1->0.1/+4.4; binary_trees 0.3->0.2/+2.5; collatz 0.2->0.2/+1.3; edigits 5.5->6.6/-7.5; factorial 0.1->0.1/-3.5; fasta 13.3->15.9/+16.3; fib 0.1->0.1/-0.2; leibniz 0.2->0.2/-1.0; mandelbrot 0.5->0.5/-0.4; matmul 4.7->4.3/-6.2; merkletrees 0.3->0.3/+0.2; nbody 1.4->1.4/+3.1; nqueen 0.3->1.1/-8.2; pisum 0.2->0.2/-2.2; primes 0.7->0.7/-2.5; quicksort 3.9->3.9/-1.6; spectral_norm 6.9->5.6/-2.9; sum_to_n 0.1->0.1/-8.4; tak 0.1->0.2/-1.4. Correctness: all 19 routine binaries were byte-identical baseline/candidate; fasta binary SHA-256=17abc12f13f7a8ce88a4669b4d6dab9c2e62b350535956f0b89cb8aebb1c612e and output SHA-256=e5ad8a7e8b1af2f584213ebb78dd8609a45bd7fb16075ea5878d90dcac0f91bf were identical; SSA dumps were identical after excluding the output-path status line. ./benchmarks/run_benchmarks.sh --verify routine did not pass on the rebased baseline: edigits/fasta had existing quoted-output mismatches and all 17 collected counts differed from RESULTS.md. Measured counts: ackermann=11,450,298,699; binary_trees=685,136,037; collatz=81,543,096; factorial=64,259; fib=642,006,247; leibniz=900,001,537; mandelbrot=17,701,943; matmul=2,124,302,103; merkletrees=724,164,802; nbody=1,239,502,637; nqueen=295,286,952; pisum=95,458; primes=2,075,933; quicksort=378,608,163; spectral_norm=143,862,590; sum_to_n=71,885; tak=63,580,601; edigits/fasta unavailable due output validation failure. RESULTS.md performance ratio: 2.74x. Exploratory instrumentation and the foldBack candidate were removed; git status is clean, with no commit or integration.
