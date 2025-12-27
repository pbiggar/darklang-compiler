# Benchmark History

Performance history of Darklang compiler across versions.

---

## 2025-12-27 11:03:18

**Commit:** `4eb27503` - Add benchmarking tools to Docker image

### factorial

| Language | Mean | Stddev | Min | Max | Median | vs Rust |
|----------|------|--------|-----|-----|--------|---------|
| Rust | 716.4 us | 967.1 us | 143.8 us | 3.2 ms | 326.6 us | baseline |
| Dark | 959.7 us | 600.9 us | 456.3 us | 2.5 ms | 766.2 us | 1.3x |
| Python | 25.9 ms | 4.5 ms | 20.6 ms | 31.5 ms | 24.4 ms | 36.2x |
---


## 2025-12-27 11:02:54

**Commit:** `4eb27503` - Add benchmarking tools to Docker image

### fib

| Language | Mean | Stddev | Min | Max | Median | vs Rust |
|----------|------|--------|-----|-----|--------|---------|
| Rust | 17.3 ms | 844.1 us | 16.3 ms | 18.9 ms | 17.1 ms | baseline |
| Dark | 39.9 ms | 2.0 ms | 38.2 ms | 45.0 ms | 39.5 ms | 2.3x |
| Python | 944.5 ms | 121.2 ms | 779.2 ms | 1.16 s | 959.5 ms | 54.5x |
---
