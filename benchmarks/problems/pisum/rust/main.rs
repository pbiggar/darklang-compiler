// Pi Summation Benchmark - Rust reference implementation
// From: Julia micro-benchmarks
// Computes partial sum of 1/k^2 series (converges to pi^2/6)

fn pisum(n: i64) -> f64 {
    let mut s: f64 = 0.0;
    for _ in 0..500 {
        s = 0.0;
        for k in 1..=n {
            s += 1.0 / ((k * k) as f64);
        }
    }
    s
}

fn main() {
    // n=10000 gives reasonable runtime
    let result = pisum(10000);
    // Output as integer (multiply by large factor for precision)
    println!("{}", (result * 1000000000000.0) as i64);
}
