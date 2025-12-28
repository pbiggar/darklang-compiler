// Ackermann Benchmark - Rust reference implementation

fn ackermann(m: i64, n: i64) -> i64 {
    if m == 0 {
        n + 1
    } else if n == 0 {
        ackermann(m - 1, 1)
    } else {
        ackermann(m - 1, ackermann(m, n - 1))
    }
}

fn main() {
    // A(3, 12) = 32765
    let result = ackermann(3, 12);
    println!("{}", result);
}
