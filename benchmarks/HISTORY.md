# Benchmark History

Performance history of Darklang compiler across versions (instruction counts via Cachegrind).

---

## 2025-12-29 20:12:55

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### tak

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss   | LL Miss   | Branches      | Mispred |
|----------|----------------|----------|---------------|-----------|-----------|---------------|---------|
|     Dark |    897,605,872 | baseline |   299,201,906 |        38 |        52 |    24,933,506 |   17.4% |
|     Node |  1,603,293,582 |    1.79x |   870,554,957 | 5,712,393 | 3,064,150 |   257,197,325 |    4.0% |
|   Python | 12,227,227,976 |   13.62x | 5,175,801,892 |   702,238 |   363,699 | 1,720,308,546 |    5.7% |
---


## 2025-12-29 20:12:39

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss   | LL Miss   | Branches    | Mispred |
|----------|--------------|----------|-------------|-----------|-----------|-------------|---------|
|     Rust |      257,051 | baseline |     100,281 |     3,572 |     3,922 |      45,956 |    9.9% |
|     Dark |   20,004,831 |   77.82x |   4,001,233 |         5 |        15 |   1,000,213 |    0.0% |
|     Node |  386,305,908 | 1502.84x | 172,801,249 | 5,504,180 | 2,898,268 |  55,569,706 |    9.0% |
|   Python |  940,743,831 | 3659.76x | 400,729,117 | 6,793,194 | 5,593,735 | 131,746,688 |    3.4% |
#### Instruction Counts (Cachegrind)

**tak:**
- Dark: 897,605,872 (baseline)
- Node: 1,603,293,582 (1.8x)
- Python: 12,227,227,976 (13.6x)

---


## 2025-12-29 20:12:27

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### primes

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss   | LL Miss   | Branches   | Mispred |
|----------|--------------|----------|-------------|-----------|-----------|------------|---------|
|     Rust |    1,250,904 | baseline |     100,275 |     3,572 |     3,927 |    306,001 |    3.1% |
|     Dark |   12,990,837 |   10.39x |   1,958,433 |         6 |        25 |    932,988 |    1.2% |
|   Python |   91,397,277 |   73.06x |  37,751,390 |   843,968 |   368,442 | 13,910,304 |    5.6% |
|     Node |  432,434,240 |  345.70x | 192,900,756 | 5,670,820 | 2,972,057 | 63,904,085 |    8.7% |
#### Instruction Counts (Cachegrind)

**sum_to_n:**
- Rust: 257,051 (0.01x)
- Dark: 20,004,831 (baseline)
- Node: 386,305,908 (19.3x)
- Python: 940,743,831 (47.0x)

---


## 2025-12-29 20:10:01

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### leibniz

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss   | LL Miss   | Branches    | Mispred |
|----------|---------------|----------|-------------|-----------|-----------|-------------|---------|
|     Node |   388,457,894 | baseline | 173,647,914 | 5,574,568 | 2,932,269 |  55,947,339 |    9.0% |
|     Dark | 3,100,000,166 |    7.98x | 400,000,042 |         5 |        14 | 100,000,014 |    0.0% |
#### Instruction Counts (Cachegrind)

**primes:**
- Rust: 1,250,904 (0.10x)
- Dark: 12,990,837 (baseline)
- Python: 91,397,277 (7.0x)
- Node: 432,434,240 (33.3x)

---


## 2025-12-29 20:09:52

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### helloworld

| Language | Instructions | vs Rust     | Data Refs   | L1 Miss   | LL Miss   | Branches   | Mispred |
|----------|--------------|-------------|-------------|-----------|-----------|------------|---------|
|     Dark |           66 |    baseline |          23 |         3 |         9 |          6 |   33.3% |
|   Python |   34,113,033 |  516864.14x |  14,270,687 |   692,099 |   360,533 |  6,028,154 |    8.1% |
|     Node |  388,836,906 | 5891468.27x | 173,851,951 | 5,705,037 | 3,033,052 | 56,022,836 |    9.1% |
#### Instruction Counts (Cachegrind)

**leibniz:**
- Node: 388,457,894 (0.13x)
- Dark: 3,100,000,166 (baseline)

---


## 2025-12-29 20:09:38

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### factorial

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss   | LL Miss   | Branches   | Mispred |
|----------|--------------|----------|-------------|-----------|-----------|------------|---------|
|     Rust |      257,091 | baseline |     100,287 |     3,572 |     3,923 |     45,947 |    9.9% |
|     Dark |    5,750,208 |   22.37x |   1,450,044 |        33 |        43 |    210,024 |    4.8% |
|   Python |  182,042,211 |  708.08x |  76,327,742 |   829,019 |   427,741 | 25,496,868 |    5.9% |
|     Node |  568,896,079 | 2212.82x | 256,983,043 | 6,493,957 | 3,179,462 | 77,771,590 |    7.2% |
#### Instruction Counts (Cachegrind)

**helloworld:**
- Dark: 66 (baseline)
- Python: 34,113,033 (516864.1x)
- Node: 388,836,906 (5891468.3x)

---


## 2025-12-29 20:08:38

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### collatz

| Language | Instructions  | vs Rust  | Data Refs     | L1 Miss   | LL Miss   | Branches      | Mispred |
|----------|---------------|----------|---------------|-----------|-----------|---------------|---------|
|     Dark |   303,483,600 | baseline |    44,815,393 |         5 |        17 |    21,707,693 |   16.7% |
|     Node |   424,626,770 |    1.40x |   189,238,699 | 5,584,906 | 2,917,403 |    63,122,279 |    8.5% |
|   Python | 8,739,724,145 |   28.80x | 3,507,233,322 | 1,161,835 |   363,660 | 1,204,644,814 |    8.2% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 257,091 (0.04x)
- Dark: 5,750,208 (baseline)
- Python: 182,042,211 (31.7x)
- Node: 568,896,079 (98.9x)

---


## 2025-12-29 20:06:12

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### binary_trees

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss    | LL Miss   | Branches    | Mispred |
|----------|---------------|----------|-------------|------------|-----------|-------------|---------|
|     Dark |   183,500,628 |    0.10x |  45,875,432 |         27 |        40 |   6,553,612 |   12.6% |
|     Node |   704,669,680 |    0.38x | 329,443,872 |  5,727,448 | 3,053,716 | 113,446,632 |    6.4% |
|     Rust | 1,842,792,925 | baseline | 882,809,012 | 10,874,039 | 9,441,843 | 260,986,438 |    3.6% |
#### Instruction Counts (Cachegrind)

**collatz:**
- Dark: 303,483,600 (baseline)
- Node: 424,626,770 (1.4x)
- Python: 8,739,724,145 (28.8x)

---


## 2025-12-29 20:00:37

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### ackermann

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss       | LL Miss     | Branches      | Mispred |
|----------|----------------|----------|---------------|---------------|-------------|---------------|---------|
|     Node |  3,097,098,158 |    0.62x | 1,554,636,929 |    83,109,761 |  29,149,766 |   552,851,219 |    0.9% |
|     Rust |  5,009,840,100 | baseline | 2,146,994,291 |   341,677,857 | 167,763,959 |   715,742,776 |    0.0% |
|     Dark | 20,754,029,457 |    4.14x | 5,725,181,742 | 1,056,965,104 | 840,086,210 | 1,073,512,521 |    0.0% |
#### Instruction Counts (Cachegrind)

**binary_trees:**
- Dark: 183,500,628 (baseline)
- Node: 704,669,680 (3.8x)
- Rust: 1,842,792,925 (10.0x)

---


## 2025-12-29 19:58:44

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### fib

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss   | LL Miss   | Branches      | Mispred |
|----------|----------------|----------|---------------|-----------|-----------|---------------|---------|
|     Rust |    272,527,935 | baseline |   119,543,100 |     3,575 |     3,931 |    29,906,656 |    9.0% |
|     Dark |    806,239,074 |    2.96x |   209,024,946 |        54 |        62 |    29,860,714 |    9.0% |
|     Node |  1,833,527,728 |    6.73x |   865,690,646 | 5,712,326 | 3,058,690 |   311,593,670 |    5.8% |
|   Python | 15,135,161,972 |   55.54x | 6,347,485,668 |   717,772 |   362,169 | 1,932,954,249 |    6.4% |
#### Instruction Counts (Cachegrind)

**ackermann:**
- Node: 3,097,098,158 (0.15x)
- Rust: 5,009,840,100 (0.24x)
- Dark: 20,754,029,457 (baseline)

---


## 2025-12-29 19:56:49

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### tak

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss | LL Miss | Branches      | Mispred |
|----------|----------------|----------|---------------|---------|---------|---------------|---------|
|     Dark |    897,605,872 | baseline |   299,201,906 |      37 |      51 |    24,933,506 |   17.4% |
|   Python | 12,227,144,233 |   13.62x | 5,175,785,348 | 699,461 | 363,634 | 1,720,287,269 |    5.7% |
#### Instruction Counts (Cachegrind)

**fib:**
- Rust: 272,527,935 (0.34x)
- Dark: 806,239,074 (baseline)
- Node: 1,833,527,728 (2.3x)
- Python: 15,135,161,972 (18.8x)

---


## 2025-12-29 19:56:41

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss   | LL Miss   | Branches    | Mispred |
|----------|--------------|----------|-------------|-----------|-----------|-------------|---------|
|     Rust |      257,142 | baseline |     100,295 |     3,581 |     3,925 |      45,979 |    9.9% |
|     Dark |   20,004,831 |   77.80x |   4,001,233 |         4 |        14 |   1,000,213 |    0.0% |
|   Python |  940,712,212 | 3658.34x | 400,723,729 | 6,785,502 | 5,593,400 | 131,738,347 |    3.4% |
#### Instruction Counts (Cachegrind)

**tak:**
- Dark: 897,605,872 (baseline)
- Python: 12,227,144,233 (13.6x)

---


## 2025-12-29 19:56:38

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### primes

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|------------|---------|---------|------------|---------|
|     Rust |    1,251,008 | baseline |    100,297 |   3,581 |   3,930 |    306,025 |    3.1% |
|     Dark |   12,990,837 |   10.38x |  1,958,433 |       6 |      25 |    932,988 |    1.2% |
|   Python |   91,416,512 |   73.07x | 37,755,182 | 847,367 | 368,241 | 13,914,455 |    5.6% |
#### Instruction Counts (Cachegrind)

**sum_to_n:**
- Rust: 257,142 (0.01x)
- Dark: 20,004,831 (baseline)
- Python: 940,712,212 (47.0x)

---


## 2025-12-29 19:54:22

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### leibniz

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches    | Mispred |
|----------|---------------|----------|-------------|---------|---------|-------------|---------|
|     Dark | 3,100,000,166 | baseline | 400,000,042 |       5 |      14 | 100,000,014 |    0.0% |
#### Instruction Counts (Cachegrind)

**primes:**
- Rust: 1,251,008 (0.10x)
- Dark: 12,990,837 (baseline)
- Python: 91,416,512 (7.0x)

---


## 2025-12-29 19:54:20

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### helloworld

| Language | Instructions | vs Rust    | Data Refs  | L1 Miss | LL Miss | Branches  | Mispred |
|----------|--------------|------------|------------|---------|---------|-----------|---------|
|     Dark |           66 |   baseline |         23 |       3 |       9 |         6 |   33.3% |
|   Python |   34,096,475 | 516613.26x | 14,267,285 | 693,145 | 360,462 | 6,023,788 |    8.1% |
#### Instruction Counts (Cachegrind)

**leibniz:**
- Dark: 3,100,000,166 (baseline)

---


## 2025-12-29 19:52:55

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### fib

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss | LL Miss | Branches      | Mispred |
|----------|----------------|----------|---------------|---------|---------|---------------|---------|
|     Rust |    272,528,026 | baseline |   119,543,114 |   3,586 |   3,935 |    29,906,679 |    9.0% |
|     Dark |    806,239,074 |    2.96x |   209,024,946 |      54 |      62 |    29,860,714 |    9.0% |
|   Python | 15,135,143,664 |   55.54x | 6,347,484,157 | 715,987 | 362,097 | 1,932,949,676 |    6.4% |
#### Instruction Counts (Cachegrind)

**helloworld:**
- Dark: 66 (baseline)
- Python: 34,096,475 (516613.3x)

---


## 2025-12-29 19:52:52

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### factorial

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|------------|---------|---------|------------|---------|
|     Rust |      257,169 | baseline |    100,293 |   3,589 |   3,928 |     45,969 |    9.9% |
|     Dark |    5,750,208 |   22.36x |  1,450,044 |      33 |      43 |    210,024 |    4.8% |
|   Python |  182,022,126 |  707.79x | 76,325,241 | 830,120 | 427,649 | 25,492,683 |    5.9% |
#### Instruction Counts (Cachegrind)

**fib:**
- Rust: 272,528,026 (0.34x)
- Dark: 806,239,074 (baseline)
- Python: 15,135,143,664 (18.8x)

---


## 2025-12-29 19:52:00

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### collatz

| Language | Instructions  | vs Rust  | Data Refs     | L1 Miss   | LL Miss | Branches      | Mispred |
|----------|---------------|----------|---------------|-----------|---------|---------------|---------|
|     Dark |   303,483,600 | baseline |    44,815,393 |         4 |      16 |    21,707,693 |   16.7% |
|   Python | 8,739,811,985 |   28.80x | 3,507,249,419 | 1,164,120 | 363,595 | 1,204,667,430 |    8.2% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 257,169 (0.04x)
- Dark: 5,750,208 (baseline)
- Python: 182,022,126 (31.7x)

---


## 2025-12-29 19:49:46

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### binary_trees

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss    | LL Miss   | Branches    | Mispred |
|----------|---------------|----------|-------------|------------|-----------|-------------|---------|
|     Dark |   183,500,628 |    0.10x |  45,875,432 |         27 |        40 |   6,553,612 |   12.6% |
|     Rust | 1,842,793,016 | baseline | 882,809,026 | 10,903,685 | 9,441,894 | 260,986,461 |    3.6% |
#### Instruction Counts (Cachegrind)

**collatz:**
- Dark: 303,483,600 (baseline)
- Python: 8,739,811,985 (28.8x)

---


## 2025-12-29 19:44:19

**Commit:** `3bd13c7b` - Add Node.js implementations and new benchmarks from Benchmarks Game

### ackermann

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss       | LL Miss     | Branches      | Mispred |
|----------|----------------|----------|---------------|---------------|-------------|---------------|---------|
|     Rust |  5,009,840,178 | baseline | 2,146,994,297 |   341,677,873 | 167,763,962 |   715,742,798 |    0.0% |
|     Dark | 20,754,029,457 |    4.14x | 5,725,181,742 | 1,056,965,102 | 840,086,208 | 1,073,512,521 |    0.0% |
#### Instruction Counts (Cachegrind)

**binary_trees:**
- Dark: 183,500,628 (baseline)
- Rust: 1,842,793,016 (10.0x)

---


## 2025-12-29 18:12:47

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### primes

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|------------|---------|---------|------------|---------|
|     Rust |    1,250,531 | baseline |    100,206 |   3,577 |   3,927 |    305,900 |    3.1% |
|     Dark |   12,990,837 |   10.39x |  1,958,433 |       6 |      25 |    932,988 |    1.2% |
|   Python |   91,368,767 |   73.06x | 37,746,501 | 801,354 | 367,963 | 13,901,396 |    5.6% |
#### Instruction Counts (Cachegrind)

**ackermann:**
- Rust: 5,009,840,178 (0.24x)
- Dark: 20,754,029,457 (baseline)

---


## 2025-12-29 18:07:34

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### ackermann

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss       | LL Miss     | Branches      | Mispred |
|----------|----------------|----------|---------------|---------------|-------------|---------------|---------|
|     Rust |  5,009,839,714 | baseline | 2,146,994,214 |   341,647,151 | 167,747,580 |   715,742,674 |    0.0% |
|     Dark | 20,754,029,457 |    4.14x | 5,725,181,742 | 1,056,965,103 | 840,086,209 | 1,073,512,521 |    0.0% |
#### Instruction Counts (Cachegrind)

**primes:**
- Rust: 1,250,531 (0.10x)
- Dark: 12,990,837 (baseline)
- Python: 91,368,767 (7.0x)

---


## 2025-12-29 17:28:49

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### binary_trees

| Language | Instructions   | vs Rust  | Data Refs      | L1 Miss     | LL Miss     | Branches      | Mispred |
|----------|----------------|----------|----------------|-------------|-------------|---------------|---------|
|     Dark |    183,500,628 |    0.10x |     45,875,432 |          27 |          40 |     6,553,612 |   12.6% |
|     Rust |  1,842,792,039 | baseline |    882,808,841 |  10,889,334 |   9,441,889 |   260,986,208 |    3.6% |
|   Python | 24,066,592,928 |   13.06x | 11,608,672,480 | 208,039,698 | 120,609,893 | 3,816,938,589 |    4.0% |
#### Instruction Counts (Cachegrind)

**ackermann:**
- Rust: 5,009,839,714 (0.24x)
- Dark: 20,754,029,457 (baseline)

---


## 2025-12-29 17:28:32

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss   | LL Miss   | Branches    | Mispred |
|----------|--------------|----------|-------------|-----------|-----------|-------------|---------|
|     Rust |      256,187 | baseline |     100,112 |     3,572 |     3,920 |      45,728 |    9.9% |
|     Dark |   20,004,831 |   78.09x |   4,001,233 |         5 |        15 |   1,000,213 |    0.0% |
|   Python |  940,692,466 | 3671.90x | 400,718,969 | 6,789,659 | 5,592,876 | 131,732,878 |    3.4% |
#### Instruction Counts (Cachegrind)

**binary_trees:**
- Dark: 183,500,628 (baseline)
- Rust: 1,842,792,039 (10.0x)
- Python: 24,066,592,928 (131.2x)

---


## 2025-12-29 17:28:20

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### factorial

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|------------|---------|---------|------------|---------|
|     Rust |      256,227 | baseline |    100,118 |   3,572 |   3,921 |     45,719 |    9.9% |
|     Dark |    5,750,208 |   22.44x |  1,450,044 |      33 |      43 |    210,024 |    4.8% |
|   Python |  181,992,256 |  710.28x | 76,319,398 | 822,481 | 426,540 | 25,485,290 |    5.9% |
#### Instruction Counts (Cachegrind)

**sum_to_n:**
- Rust: 256,187 (0.01x)
- Dark: 20,004,831 (baseline)
- Python: 940,692,466 (47.0x)

---


## 2025-12-29 17:27:01

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### fib

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss | LL Miss | Branches      | Mispred |
|----------|----------------|----------|---------------|---------|---------|---------------|---------|
|     Rust |    272,527,060 | baseline |   119,542,930 |   3,587 |   3,938 |    29,906,427 |    9.0% |
|     Dark |    806,239,074 |    2.96x |   209,024,946 |      54 |      62 |    29,860,714 |    9.0% |
|   Python | 15,135,137,343 |   55.54x | 6,347,481,275 | 716,856 | 362,119 | 1,932,948,073 |    6.4% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 256,227 (0.04x)
- Dark: 5,750,208 (baseline)
- Python: 181,992,256 (31.6x)

---


## 2025-12-29 17:21:30

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### ackermann

| Language | Instructions   | vs Rust  | Data Refs     | L1 Miss       | LL Miss     | Branches      | Mispred |
|----------|----------------|----------|---------------|---------------|-------------|---------------|---------|
|     Dark | 20,754,029,457 | baseline | 5,725,181,742 | 1,056,965,103 | 840,086,209 | 1,073,512,521 |    0.0% |
#### Instruction Counts (Cachegrind)

**fib:**
- Rust: 272,527,060 (0.34x)
- Dark: 806,239,074 (baseline)
- Python: 15,135,137,343 (18.8x)

---


## 2025-12-29 17:21:19

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### binary_trees

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss | LL Miss | Branches  | Mispred |
|----------|--------------|----------|------------|---------|---------|-----------|---------|
|     Dark |  183,500,628 | baseline | 45,875,432 |      27 |      40 | 6,553,612 |   12.6% |
#### Instruction Counts (Cachegrind)

**ackermann:**
- Dark: 20,754,029,457 (baseline)

---


## 2025-12-29 17:21:11

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches  | Mispred |
|----------|--------------|----------|-----------|---------|---------|-----------|---------|
|     Dark |   20,004,831 | baseline | 4,001,233 |       4 |      14 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**binary_trees:**
- Dark: 183,500,628 (baseline)

---


## 2025-12-29 17:21:04

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    5,750,208 | baseline | 1,450,044 |      33 |      43 |  210,024 |    4.8% |
#### Instruction Counts (Cachegrind)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 20,004,831 (baseline)
- Python: 940,685,845 (47.0x)

---


## 2025-12-29 17:20:54

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### fib

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|-------------|---------|---------|------------|---------|
|     Dark |  806,239,074 | baseline | 209,024,946 |      54 |      62 | 29,860,714 |    9.0% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.04x)
- Dark: 5,750,208 (baseline)
- Python: 182,064,112 (31.7x)

---


## 2025-12-29 17:20:45

**Commit:** `460b0112` - Add CLI flags to disable individual compiler optimizations

### primes

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |   12,990,837 | baseline | 1,958,433 |       6 |      25 |  932,988 |    1.2% |
#### Instruction Counts (Cachegrind)

**fib:**
- Rust: 272,526,559 (0.34x)
- Dark: 806,239,074 (baseline)
- Python: 15,135,188,032 (18.8x)

---


## 2025-12-29 10:28:45

**Commit:** `0ac77f8e` - Revert buggy parallel move fix, restore original stack-based approach

### fib

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|--------------|----------|-------------|---------|---------|------------|---------|
|     Dark |  776,378,372 | baseline | 209,024,946 |      54 |      62 | 29,860,714 |    9.0% |
#### Instruction Counts (Cachegrind)

**primes:**
- Dark: 12,990,837 (baseline)

---


## 2025-12-29 08:43:50

**Commit:** `d2e1e3ae` - Fix parallel move clobbering for tail calls

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches  | Mispred |
|----------|--------------|----------|-----------|---------|---------|-----------|---------|
|     Dark |   20,007,159 | baseline | 4,004,065 |       8 |      21 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**fib:**
- Rust: 272,526,559 (0.35x)
- Dark: 776,378,372 (baseline)
- Python: 15,135,188,032 (19.5x)

---


## 2025-12-29 08:43:37

**Commit:** `d2e1e3ae` - Fix parallel move clobbering for tail calls

### fib

| Language | Instructions  | vs Rust  | Data Refs     | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|---------------|---------|---------|------------|---------|
|     Dark | 1,463,174,546 | baseline | 1,119,776,389 |      90 |     103 | 29,860,714 |    9.0% |
#### Instruction Counts (Cachegrind)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 20,007,159 (baseline)
- Python: 940,685,845 (47.0x)

---


## 2025-12-29 08:43:26

**Commit:** `d2e1e3ae` - Fix parallel move clobbering for tail calls

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |   10,160,236 | baseline | 7,430,076 |      55 |      70 |  210,024 |    4.8% |
#### Instruction Counts (Cachegrind)

**fib:**
- Rust: 272,526,559 (0.19x)
- Dark: 1,463,174,546 (baseline)
- Python: 15,135,188,032 (10.3x)

---


## 2025-12-28 12:28:19

**Commit:** `0f060e54` - Add algebraic simplifications and bitwise identities

### ackermann

| Language | Instructions   | vs Rust  | Data Refs      | L1 Miss       | LL Miss     | Branches      | Mispred |
|----------|----------------|----------|----------------|---------------|-------------|---------------|---------|
|     Dark | 29,700,141,739 | baseline | 15,744,544,522 | 1,057,124,891 | 840,212,509 | 1,073,512,521 |    0.0% |

### binary_trees

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches  | Mispred |
|----------|--------------|----------|-------------|---------|---------|-----------|---------|
|     Dark |  226,099,941 | baseline | 140,902,648 |     177 |     192 | 6,553,612 |   12.6% |

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,350,221 | baseline | 4,440,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,015,264,001 | baseline | 642,005,141 |      55 |      67 | 29,860,714 |    9.0% |

### primes

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |   22,470,125 | baseline | 9,695,513 |  29,747 |  25,934 |  932,988 |    1.2% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   39,007,544 | baseline | 21,004,549 | 2,949,759 | 2,185,615 | 1,000,213 |    0.0% |

### tak

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,022,273,555 | baseline | 573,470,532 |      53 |      71 | 24,933,506 |   17.4% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.03x)
- Dark: 10,160,236 (baseline)
- Python: 182,064,112 (17.9x)

---


## 2025-12-28 12:28:07

**Commit:** `0f060e54` - Add algebraic simplifications and bitwise identities

### primes

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |   22,470,125 | baseline | 9,695,513 |  29,747 |  25,934 |  932,988 |    1.2% |
#### Instruction Counts (Cachegrind)

**ackermann:**
- Dark: 29,700,141,739 (baseline)

**binary_trees:**
- Dark: 226,099,941 (baseline)

**factorial:**
- Rust: 255,726 (0.03x)
- Dark: 7,350,221 (baseline)
- Python: 182,064,112 (24.8x)

**fib:**
- Rust: 272,526,559 (0.27x)
- Dark: 1,015,264,001 (baseline)
- Python: 15,135,188,032 (14.9x)

**primes:**
- Dark: 22,470,125 (baseline)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 39,007,544 (baseline)
- Python: 940,685,845 (24.1x)

**tak:**
- Dark: 1,022,273,555 (baseline)

---


## 2025-12-28 12:25:08

**Commit:** `0f060e54` - Add algebraic simplifications and bitwise identities

### tak

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,022,273,555 | baseline | 573,470,532 |      53 |      71 | 24,933,506 |   17.4% |
#### Instruction Counts (Cachegrind)

**primes:**
- Dark: 22,470,125 (baseline)

---


## 2025-12-28 12:22:23

**Commit:** `0f060e54` - Add algebraic simplifications and bitwise identities

### ackermann

| Language | Instructions   | vs Rust  | Data Refs      | L1 Miss       | LL Miss     | Branches      | Mispred |
|----------|----------------|----------|----------------|---------------|-------------|---------------|---------|
|     Dark | 29,700,141,739 | baseline | 15,744,544,522 | 1,057,124,891 | 840,212,509 | 1,073,512,521 |    0.0% |
#### Instruction Counts (Cachegrind)

**tak:**
- Dark: 1,022,273,555 (baseline)

---


## 2025-12-28 12:22:12

**Commit:** `0f060e54` - Add algebraic simplifications and bitwise identities

### binary_trees

| Language | Instructions | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches  | Mispred |
|----------|--------------|----------|-------------|---------|---------|-----------|---------|
|     Dark |  226,099,941 | baseline | 140,902,648 |     177 |     192 | 6,553,612 |   12.6% |
#### Instruction Counts (Cachegrind)

**ackermann:**
- Dark: 29,700,141,739 (baseline)

---


## 2025-12-28 12:05:04

**Commit:** `d118287a` - Add Common Subexpression Elimination (CSE) optimization

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,350,221 | baseline | 4,440,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,015,264,001 | baseline | 642,005,141 |      55 |      67 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   39,007,544 | baseline | 21,004,549 | 2,949,759 | 2,185,615 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**binary_trees:**
- Dark: 226,099,941 (baseline)

---


## 2025-12-28 12:01:44

**Commit:** `dddd50a5` - Enable copy propagation (fix phi handling)

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,350,221 | baseline | 4,440,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,015,264,001 | baseline | 642,005,141 |      55 |      67 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   39,007,544 | baseline | 21,004,549 | 2,949,759 | 2,185,615 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.03x)
- Dark: 7,350,221 (baseline)
- Python: 182,064,112 (24.8x)

**fib:**
- Rust: 272,526,559 (0.27x)
- Dark: 1,015,264,001 (baseline)
- Python: 15,135,188,032 (14.9x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 39,007,544 (baseline)
- Python: 940,685,845 (24.1x)

---


## 2025-12-28 11:57:13

**Commit:** `35e02c77` - Enable DCE and constant folding optimizations

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,350,221 | baseline | 4,440,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,015,264,001 | baseline | 642,005,141 |      55 |      67 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   39,007,544 | baseline | 21,004,549 | 2,949,759 | 2,185,615 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.03x)
- Dark: 7,350,221 (baseline)
- Python: 182,064,112 (24.8x)

**fib:**
- Rust: 272,526,559 (0.27x)
- Dark: 1,015,264,001 (baseline)
- Python: 15,135,188,032 (14.9x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 39,007,544 (baseline)
- Python: 940,685,845 (24.1x)

---


## 2025-12-28 11:39:37

**Commit:** `b07c030c` - Fix SSA with liveness analysis and correct VReg numbering

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,350,221 | baseline | 4,440,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,015,264,001 | baseline | 642,005,141 |      55 |      67 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   39,007,544 | baseline | 21,004,549 | 2,949,759 | 2,185,615 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.03x)
- Dark: 7,350,221 (baseline)
- Python: 182,064,112 (24.8x)

**fib:**
- Rust: 272,526,559 (0.27x)
- Dark: 1,015,264,001 (baseline)
- Python: 15,135,188,032 (14.9x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 39,007,544 (baseline)
- Python: 940,685,845 (24.1x)

---


## 2025-12-28 11:33:46

**Commit:** `b07c030c` - Fix SSA with liveness analysis and correct VReg numbering

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,350,221 | baseline | 4,440,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,015,264,001 | baseline | 642,005,141 |      55 |      67 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   39,007,544 | baseline | 21,004,549 | 2,949,759 | 2,185,615 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.03x)
- Dark: 7,350,221 (baseline)
- Python: 182,064,112 (24.8x)

**fib:**
- Rust: 272,526,559 (0.27x)
- Dark: 1,015,264,001 (baseline)
- Python: 15,135,188,032 (14.9x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 39,007,544 (baseline)
- Python: 940,685,845 (24.1x)

---


## 2025-12-28 11:12:39

**Commit:** `550a9f37` - Fix ARM64 stack offsets, improve SSA phi handling

### factorial

| Language | Instructions | vs Rust  | Data Refs | L1 Miss | LL Miss | Branches | Mispred |
|----------|--------------|----------|-----------|---------|---------|----------|---------|
|     Dark |    7,350,221 | baseline | 4,440,060 |  29,807 |  25,982 |  210,024 |    4.8% |

### fib

| Language | Instructions  | vs Rust  | Data Refs   | L1 Miss | LL Miss | Branches   | Mispred |
|----------|---------------|----------|-------------|---------|---------|------------|---------|
|     Dark | 1,015,264,001 | baseline | 642,005,141 |      55 |      67 | 29,860,714 |    9.0% |

### sum_to_n

| Language | Instructions | vs Rust  | Data Refs  | L1 Miss   | LL Miss   | Branches  | Mispred |
|----------|--------------|----------|------------|-----------|-----------|-----------|---------|
|     Dark |   39,007,544 | baseline | 21,004,549 | 2,949,759 | 2,185,615 | 1,000,213 |    0.0% |
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.03x)
- Dark: 7,350,221 (baseline)
- Python: 182,064,112 (24.8x)

**fib:**
- Rust: 272,526,559 (0.27x)
- Dark: 1,015,264,001 (baseline)
- Python: 15,135,188,032 (14.9x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 39,007,544 (baseline)
- Python: 940,685,845 (24.1x)

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
#### Instruction Counts (Cachegrind)

**factorial:**
- Rust: 255,726 (0.03x)
- Dark: 7,350,221 (baseline)
- Python: 182,064,112 (24.8x)

**fib:**
- Rust: 272,526,559 (0.27x)
- Dark: 1,015,264,001 (baseline)
- Python: 15,135,188,032 (14.9x)

**sum_to_n:**
- Rust: 255,662 (0.01x)
- Dark: 39,007,544 (baseline)
- Python: 940,685,845 (24.1x)

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
