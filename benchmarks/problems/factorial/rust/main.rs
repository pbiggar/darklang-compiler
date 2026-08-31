// main.rs - Parameterized Rust benchmark implementation.
fn argument(index: usize) -> i64 {
    std::env::args().nth(index + 1)
        .expect("missing benchmark argument")
        .parse::<i64>()
        .expect("benchmark argument must be an i64")
}

fn factorial(n: i64) -> i64 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn repeat(n: i64, factorial_input: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        repeat(n - 1, factorial_input, factorial(factorial_input))
    }
}

fn main() {
    println!("{}", repeat(argument(0), argument(1), 0));
}
