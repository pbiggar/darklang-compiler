# Benchmark History

Performance history of Darklang compiler across versions (instruction counts via Cachegrind).

---

## 2025-12-28 09:58:08

**Commit:** `40f939c3` - Cache Rust/Python benchmark baselines from HISTORY.md

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,140,220 | baseline | 4,450,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|-------------|---------|---------|------------|---------|
|     Dark |  985,403,298 | baseline | 642,005,141 |      55 |      66 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   38,007,343 | baseline | 22,004,649 | 2,949,759 | 2,185,614 | 1,000,213 |    0.0% |
---


## 2025-12-28 09:56:02

**Commit:** `bd7fe8e9` - Fix test expectations and disable SSA passes

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,140,220 | baseline | 4,450,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|-------------|---------|---------|------------|---------|
|     Dark |  985,403,298 | baseline | 642,005,141 |      55 |      66 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   38,007,343 | baseline | 22,004,649 | 2,949,759 | 2,185,614 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.04x)
- Dark: 7,140,220 (baseline)
- Python: 182,064,112 (25.5x)

**fib:**
- Rust: 272,526,559 (0.28x)
- Dark: 985,403,298 (baseline)
- Python: 15,135,188,032 (15.4x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 38,007,343 (baseline)
- Python: 940,685,845 (24.8x)

---


## 2025-12-28 09:54:47

**Commit:** `bd7fe8e9` - Fix test expectations and disable SSA passes

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,140,220 | baseline | 4,450,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|-------------|---------|---------|------------|---------|
|     Dark |  985,403,298 | baseline | 642,005,141 |      55 |      66 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   38,007,343 | baseline | 22,004,649 | 2,949,759 | 2,185,614 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.04x)
- Dark: 7,140,220 (baseline)
- Python: 182,064,112 (25.5x)

**fib:**
- Rust: 272,526,559 (0.28x)
- Dark: 985,403,298 (baseline)
- Python: 15,135,188,032 (15.4x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 38,007,343 (baseline)
- Python: 940,685,845 (24.8x)

---


## 2025-12-28 09:44:39

**Commit:** `bd7fe8e9` - Fix test expectations and disable SSA passes

### factorial

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|------------|---------|---------|------------|---------|
|     Rust |      255,726 | baseline |    100,023 |   3,563 |   3,925 |     45,590 |    9.9% |
|     Dark |    7,140,220 |   27.92x |  4,450,060 |  29,808 |  25,983 |    210,024 |    4.8% |
|   Python |  182,064,112 |  711.95x | 76,330,867 | 831,325 | 427,423 | 25,503,613 |    5.9% |

### fib

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss | LL Miss | Branches      | Mispred |
|----------|----------------|----------|---------------|---------|---------|---------------|---------|
|     Rust |    272,526,559 | baseline |   119,542,835 |   3,566 |   3,934 |    29,906,298 |    9.0% |
|     Dark |    985,403,298 |    3.62x |   642,005,141 |      55 |      66 |    29,860,714 |    9.0% |
|   Python | 15,135,188,032 |   55.54x | 6,347,490,290 | 716,822 | 362,180 | 1,932,960,942 |    6.4% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss   | LL Miss   | Branches    | Mispred |
|----------|--------------|----------|-------------|-----------|-----------|-------------|---------|
|     Rust |      255,662 | baseline |     100,008 |     3,563 |     3,924 |      45,597 |    9.9% |
|     Dark |   38,007,343 |  148.66x |  22,004,649 | 2,949,758 | 2,185,613 |   1,000,213 |    0.0% |
|   Python |  940,685,845 | 3679.41x | 400,716,956 | 6,786,949 | 5,593,668 | 131,731,959 |    3.4% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Dark: 7,140,220 (baseline)

**fib:**
- Dark: 985,403,298 (baseline)

**sum_to_n:**
- Dark: 38,007,343 (baseline)

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
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.04x)
- Dark: 7,140,220 (baseline)
- Python: 182,064,112 (25.5x)

**fib:**
- Rust: 272,526,559 (0.28x)
- Dark: 985,403,298 (baseline)
- Python: 15,135,188,032 (15.4x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 38,007,343 (baseline)
- Python: 940,685,845 (24.8x)

---
