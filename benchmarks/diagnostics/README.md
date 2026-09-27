# Call liveness diagnostic

`call_live_values.dark` keeps two integer values live across a call to a known
leaf through a direct wrapper, then repeats that caller 100,000 times. The
expected output is `5002850000`.
It isolates call-save and register-color choices without changing the canonical
benchmark workloads under `benchmarks/problems/`.

After `./build --ai`, run from the repository root:

```sh
./dark benchmarks/diagnostics/call_live_values.dark -o /tmp/call-live-values
/tmp/call-live-values
valgrind --tool=cachegrind --cache-sim=no --branch-sim=no \
  --cachegrind-out-file=/dev/null /tmp/call-live-values
```

Use Cachegrind's `I refs` count for repeatable comparisons. The full benchmark
gate remains `./benchmarks/run_benchmarks.sh --verify-parent full`.
