# Benchmark History

Performance history of Darklang compiler across versions (instruction counts via Cachegrind).

---

## 2025-12-27 11:31:02

**Commit:** `603035f7` - Add persistent benchmark history log

### factorial

| Language | Instructions | vs Rust | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|-------------|---------|-----------|---------|---------|----------|---------|
| Rust | 255,726 | baseline | 100,023 | 3,563 | 3,925 | 45,590 | 9.9% |
| Dark | 6,920,218 | 27.06x | 4,440,060 | 29,808 | 25,982 | 210,024 | 4.8% |
| Python | 181,972,927 | 711.59x | 76,313,681 | 830,088 | 427,050 | 25,480,601 | 5.9% |

### fib

| Language | Instructions | vs Rust | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|-------------|---------|-----------|---------|---------|----------|---------|
| Rust | 272,526,559 | baseline | 119,542,835 | 3,566 | 3,934 | 29,906,298 | 9.0% |
| Dark | 955,542,595 | 3.51x | 642,005,141 | 55 | 66 | 29,860,714 | 9.0% |
| Python | 15,135,205,293 | 55.54x | 6,347,493,181 | 717,441 | 362,161 | 1,932,965,367 | 6.4% |

### sum_to_n

| Language | Instructions | vs Rust | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|-------------|---------|-----------|---------|---------|----------|---------|
| Rust | 255,662 | baseline | 100,008 | 3,563 | 3,924 | 45,597 | 9.9% |
| Dark | 36,006,941 | 140.84x | 22,004,549 | 2,949,758 | 2,185,612 | 1,000,213 | 0.0% |
| Python | 940,718,267 | 3679.54x | 400,722,251 | 6,787,208 | 5,593,580 | 131,739,821 | 3.4% |
---
