# Benchmark History

Performance history of Darklang compiler across versions (instruction counts via Cachegrind).

---

## 2025-12-27 11:38:14

**Commit:** `fb7b7b95` - Make cachegrind the default benchmark mode

### factorial

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|------------|---------|---------|------------|---------|
|     Rust |      255,726 | baseline |    100,023 |   3,563 |   3,925 |     45,590 |    9.9% |
|     Dark |    6,920,218 |   27.06x |  4,440,060 |  29,808 |  25,982 |    210,024 |    4.8% |
|   Python |  182,073,541 |  711.99x | 76,333,111 | 831,348 | 427,256 | 25,506,447 |    5.9% |

### fib

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss | LL Miss | Branches      | Mispred |
|----------|----------------|----------|---------------|---------|---------|---------------|---------|
|     Rust |    272,526,559 | baseline |   119,542,835 |   3,566 |   3,934 |    29,906,298 |    9.0% |
|     Dark |    955,542,595 |    3.51x |   642,005,141 |      55 |      66 |    29,860,714 |    9.0% |
|   Python | 15,135,204,347 |   55.54x | 6,347,493,821 | 715,737 | 361,803 | 1,932,965,368 |    6.4% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss   | LL Miss   | Branches    | Mispred |
|----------|--------------|----------|-------------|-----------|-----------|-------------|---------|
|     Rust |      255,662 | baseline |     100,008 |     3,563 |     3,924 |      45,597 |    9.9% |
|     Dark |   36,006,941 |  140.84x |  22,004,549 | 2,949,758 | 2,185,612 |   1,000,213 |    0.0% |
|   Python |  940,727,339 | 3679.57x | 400,724,712 | 6,787,404 | 5,593,487 | 131,742,167 |    3.4% |
---
