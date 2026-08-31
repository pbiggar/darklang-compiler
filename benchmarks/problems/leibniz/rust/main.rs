// main.rs - Parameterized Rust benchmark implementation.
// Leibniz Pi Benchmark - Rust reference implementation
// Computes pi using Leibniz formula: pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...

fn leibniz_pi(n: i64) -> f64 {
    let mut s: f64 = 0.0;
    let mut sign: f64 = 1.0;
    for i in 0..n {
        s += sign / (2 * i + 1) as f64;
        sign = -sign;
    }
    s * 4.0
}

fn argument(index: usize) -> i64 {
    std::env::args().nth(index + 1)
        .expect("missing benchmark argument")
        .parse::<i64>()
        .expect("benchmark argument must be an i64")
}

fn main() {
    let result = leibniz_pi(argument(0));
    // Output as integer (multiply by large factor for precision)
    println!("{}", (result * 100000000.0) as i64);
}
