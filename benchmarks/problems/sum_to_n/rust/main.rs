// main.rs - Parameterized Rust benchmark implementation.
fn sum_to(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        sum_to(n - 1, acc + n)
    }
}

fn repeat(n: i64, sum_input: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        repeat(n - 1, sum_input, sum_to(sum_input, 0))
    }
}

fn argument(index: usize) -> i64 {
    std::env::args().nth(index + 1)
        .expect("missing benchmark argument")
        .parse::<i64>()
        .expect("benchmark argument must be an i64")
}

fn main() {
    println!("{}", repeat(argument(0), argument(1), 0));
}
