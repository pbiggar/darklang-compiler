#!/usr/bin/env python3
# Fasta Benchmark - DNA Sequence Generation
# From: Computer Language Benchmarks Game
# Generates pseudo-random DNA sequences and computes checksum

IM = 139968
IA = 3877
IC = 29573

class Random:
    def __init__(self, seed):
        self.last = seed

    def next(self, max_val):
        self.last = (self.last * IA + IC) % IM
        return max_val * self.last / IM

ALU = b"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

def make_cumulative(table):
    cp = 0.0
    for item in table:
        cp += item[1]
        item[1] = cp

def select_random(table, r):
    for c, p in table:
        if r < p:
            return c
    return table[-1][0]

def make_random_fasta(n, table, rand):
    checksum = 0
    for i in range(n):
        r = rand.next(1.0)
        c = select_random(table, r)
        checksum = (checksum + c * (i + 1)) % 1000000007
    return checksum

def make_repeat_fasta(n, seq):
    checksum = 0
    seq_len = len(seq)
    for i in range(n):
        c = seq[i % seq_len]
        checksum = (checksum + c * (i + 1)) % 1000000007
    return checksum

n = 100000

iub = [
    [ord('a'), 0.27],
    [ord('c'), 0.12],
    [ord('g'), 0.12],
    [ord('t'), 0.27],
    [ord('B'), 0.02],
    [ord('D'), 0.02],
    [ord('H'), 0.02],
    [ord('K'), 0.02],
    [ord('M'), 0.02],
    [ord('N'), 0.02],
    [ord('R'), 0.02],
    [ord('S'), 0.02],
    [ord('V'), 0.02],
    [ord('W'), 0.02],
    [ord('Y'), 0.02],
]

homosapiens = [
    [ord('a'), 0.3029549426680],
    [ord('c'), 0.1979883004921],
    [ord('g'), 0.1975473066391],
    [ord('t'), 0.3015094502008],
]

make_cumulative(iub)
make_cumulative(homosapiens)

rand = Random(42)

# Generate sequences and compute combined checksum
c1 = make_repeat_fasta(n * 2, ALU)
c2 = make_random_fasta(n * 3, iub, rand)
c3 = make_random_fasta(n * 5, homosapiens, rand)

total = (c1 + c2 + c3) % 1000000007
print(total)
