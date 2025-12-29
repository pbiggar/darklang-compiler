// Fasta Benchmark - DNA Sequence Generation
// From: Computer Language Benchmarks Game
// Generates pseudo-random DNA sequences and computes checksum

const IM: u64 = 139968;
const IA: u64 = 3877;
const IC: u64 = 29573;

struct Random {
    last: u64,
}

impl Random {
    fn new(seed: u64) -> Self {
        Random { last: seed }
    }

    fn next(&mut self, max: f64) -> f64 {
        self.last = (self.last * IA + IC) % IM;
        max * (self.last as f64) / (IM as f64)
    }
}

const ALU: &[u8] = b"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

struct IUB {
    c: u8,
    p: f64,
}

fn make_cumulative(table: &mut [IUB]) {
    let mut cp = 0.0;
    for item in table.iter_mut() {
        cp += item.p;
        item.p = cp;
    }
}

fn select_random(table: &[IUB], r: f64) -> u8 {
    for item in table {
        if r < item.p {
            return item.c;
        }
    }
    table.last().unwrap().c
}

fn make_random_fasta(n: usize, table: &[IUB], rand: &mut Random) -> u64 {
    let mut checksum: u64 = 0;
    for i in 0..n {
        let r = rand.next(1.0);
        let c = select_random(table, r);
        checksum = (checksum + (c as u64) * ((i as u64) + 1)) % 1000000007;
    }
    checksum
}

fn make_repeat_fasta(n: usize, seq: &[u8]) -> u64 {
    let mut checksum: u64 = 0;
    let len = seq.len();
    for i in 0..n {
        let c = seq[i % len];
        checksum = (checksum + (c as u64) * ((i as u64) + 1)) % 1000000007;
    }
    checksum
}

fn main() {
    let n = 100000;

    let mut iub = vec![
        IUB { c: b'a', p: 0.27 },
        IUB { c: b'c', p: 0.12 },
        IUB { c: b'g', p: 0.12 },
        IUB { c: b't', p: 0.27 },
        IUB { c: b'B', p: 0.02 },
        IUB { c: b'D', p: 0.02 },
        IUB { c: b'H', p: 0.02 },
        IUB { c: b'K', p: 0.02 },
        IUB { c: b'M', p: 0.02 },
        IUB { c: b'N', p: 0.02 },
        IUB { c: b'R', p: 0.02 },
        IUB { c: b'S', p: 0.02 },
        IUB { c: b'V', p: 0.02 },
        IUB { c: b'W', p: 0.02 },
        IUB { c: b'Y', p: 0.02 },
    ];

    let mut homosapiens = vec![
        IUB { c: b'a', p: 0.3029549426680 },
        IUB { c: b'c', p: 0.1979883004921 },
        IUB { c: b'g', p: 0.1975473066391 },
        IUB { c: b't', p: 0.3015094502008 },
    ];

    make_cumulative(&mut iub);
    make_cumulative(&mut homosapiens);

    let mut rand = Random::new(42);

    // Generate sequences and compute combined checksum
    let c1 = make_repeat_fasta(n * 2, ALU);
    let c2 = make_random_fasta(n * 3, &iub, &mut rand);
    let c3 = make_random_fasta(n * 5, &homosapiens, &mut rand);

    let total = (c1 + c2 + c3) % 1000000007;
    println!("{}", total);
}
