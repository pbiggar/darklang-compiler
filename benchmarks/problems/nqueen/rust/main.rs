// N-Queens Benchmark - Rust reference implementation
// From: plb2 (Programming Language Benchmark v2)
// Counts solutions to the N-queens problem using bit manipulation

fn nqueen(n: u32) -> u64 {
    let all_ones: u32 = (1 << n) - 1;

    fn solve(cols: u32, diag1: u32, diag2: u32, all_ones: u32) -> u64 {
        if cols == all_ones {
            return 1;
        }
        let mut count = 0u64;
        let mut avail = all_ones & !(cols | diag1 | diag2);
        while avail != 0 {
            let pos = avail & avail.wrapping_neg();
            avail -= pos;
            count += solve(cols | pos, (diag1 | pos) << 1, (diag2 | pos) >> 1, all_ones);
        }
        count
    }

    solve(0, 0, 0, all_ones)
}

fn main() {
    // N=13 gives reasonable runtime
    let result = nqueen(13);
    println!("{}", result);
}
