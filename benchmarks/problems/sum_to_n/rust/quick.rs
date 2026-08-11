// Sum-to-n benchmark - Rust quick reference implementation.

fn sum_to(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        sum_to(n - 1, acc + n)
    }
}

fn repeat(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        repeat(n - 1, sum_to(10000, 0))
    }
}

fn main() {
    // Reduced from 100 repetitions to 10.
    println!("{}", repeat(10, 0));
}
