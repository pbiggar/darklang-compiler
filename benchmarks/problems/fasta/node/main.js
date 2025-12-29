// Fasta Benchmark - DNA Sequence Generation
// From: Computer Language Benchmarks Game
// Generates pseudo-random DNA sequences and computes checksum

const IM = 139968;
const IA = 3877;
const IC = 29573;

class Random {
    constructor(seed) {
        this.last = seed;
    }

    next(max) {
        this.last = (this.last * IA + IC) % IM;
        return max * this.last / IM;
    }
}

const ALU = Buffer.from("GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA");

function makeCumulative(table) {
    let cp = 0.0;
    for (const item of table) {
        cp += item.p;
        item.p = cp;
    }
}

function selectRandom(table, r) {
    for (const item of table) {
        if (r < item.p) {
            return item.c;
        }
    }
    return table[table.length - 1].c;
}

function makeRandomFasta(n, table, rand) {
    let checksum = 0;
    for (let i = 0; i < n; i++) {
        const r = rand.next(1.0);
        const c = selectRandom(table, r);
        checksum = (checksum + c * (i + 1)) % 1000000007;
    }
    return checksum;
}

function makeRepeatFasta(n, seq) {
    let checksum = 0;
    const len = seq.length;
    for (let i = 0; i < n; i++) {
        const c = seq[i % len];
        checksum = (checksum + c * (i + 1)) % 1000000007;
    }
    return checksum;
}

const n = 100000;

const iub = [
    { c: 'a'.charCodeAt(0), p: 0.27 },
    { c: 'c'.charCodeAt(0), p: 0.12 },
    { c: 'g'.charCodeAt(0), p: 0.12 },
    { c: 't'.charCodeAt(0), p: 0.27 },
    { c: 'B'.charCodeAt(0), p: 0.02 },
    { c: 'D'.charCodeAt(0), p: 0.02 },
    { c: 'H'.charCodeAt(0), p: 0.02 },
    { c: 'K'.charCodeAt(0), p: 0.02 },
    { c: 'M'.charCodeAt(0), p: 0.02 },
    { c: 'N'.charCodeAt(0), p: 0.02 },
    { c: 'R'.charCodeAt(0), p: 0.02 },
    { c: 'S'.charCodeAt(0), p: 0.02 },
    { c: 'V'.charCodeAt(0), p: 0.02 },
    { c: 'W'.charCodeAt(0), p: 0.02 },
    { c: 'Y'.charCodeAt(0), p: 0.02 },
];

const homosapiens = [
    { c: 'a'.charCodeAt(0), p: 0.3029549426680 },
    { c: 'c'.charCodeAt(0), p: 0.1979883004921 },
    { c: 'g'.charCodeAt(0), p: 0.1975473066391 },
    { c: 't'.charCodeAt(0), p: 0.3015094502008 },
];

makeCumulative(iub);
makeCumulative(homosapiens);

const rand = new Random(42);

// Generate sequences and compute combined checksum
const c1 = makeRepeatFasta(n * 2, ALU);
const c2 = makeRandomFasta(n * 3, iub, rand);
const c3 = makeRandomFasta(n * 5, homosapiens, rand);

const total = (c1 + c2 + c3) % 1000000007;
console.log(total);
