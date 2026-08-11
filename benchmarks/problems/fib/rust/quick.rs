// Fibonacci benchmark - Rust quick reference implementation.

fn fib(n: i64) -> i64 {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    // Reduced from fib(35) to fib(20).
    println!("{}", fib(20));
}
