// Collatz Benchmark - Rust reference implementation
// Counts steps in Collatz sequences for numbers 1 to n

fn collatz_steps(mut n: i64) -> i64 {
    let mut steps = 0;
    while n != 1 {
        if n % 2 == 0 {
            n = n / 2;
        } else {
            n = 3 * n + 1;
        }
        steps += 1;
    }
    steps
}

fn sum_collatz(limit: i64) -> i64 {
    let mut total = 0;
    for i in 1..=limit {
        total += collatz_steps(i);
    }
    total
}

fn main() {
    // Reduced from 100,000 candidates to 1,000.
    let result = sum_collatz(1000);
    println!("{}", result);
}
