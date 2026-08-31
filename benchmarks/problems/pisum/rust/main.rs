// main.rs - Parameterized Rust benchmark implementation.
// Pi Summation Benchmark - Rust reference implementation
// From: Julia micro-benchmarks
// Computes partial sum of 1/k^2 series (converges to pi^2/6)

fn pisum(rounds: i64, n: i64) -> f64 {
    let mut s: f64 = 0.0;
    for _ in 0..rounds {
        s = 0.0;
        for k in 1..=n {
            s += 1.0 / ((k * k) as f64);
        }
    }
    s
}

fn argument(index: usize) -> i64 {
    std::env::args().nth(index + 1)
        .expect("missing benchmark argument")
        .parse::<i64>()
        .expect("benchmark argument must be an i64")
}

fn main() {
    let result = pisum(argument(0), argument(1));
    // Output as integer (multiply by large factor for precision)
    println!("{}", (result * 1000000000000.0) as i64);
}
