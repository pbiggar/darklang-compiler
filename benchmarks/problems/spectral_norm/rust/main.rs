// main.rs - Parameterized Rust benchmark implementation.
// Spectral Norm Benchmark - Rust reference implementation
// From: Computer Language Benchmarks Game

fn a(i: usize, j: usize) -> f64 {
    1.0 / ((i + j) * (i + j + 1) / 2 + i + 1) as f64
}

fn av(n: usize, v: &[f64], out: &mut [f64]) {
    for i in 0..n {
        let mut s = 0.0;
        for j in 0..n {
            s += a(i, j) * v[j];
        }
        out[i] = s;
    }
}

fn atv(n: usize, v: &[f64], out: &mut [f64]) {
    for i in 0..n {
        let mut s = 0.0;
        for j in 0..n {
            s += a(j, i) * v[j];
        }
        out[i] = s;
    }
}

fn atav(n: usize, v: &[f64], out: &mut [f64], tmp: &mut [f64]) {
    av(n, v, tmp);
    atv(n, tmp, out);
}

fn spectral_norm(n: usize, iterations: i64) -> f64 {
    let mut u = vec![1.0; n];
    let mut v = vec![0.0; n];
    let mut tmp = vec![0.0; n];

    for _ in 0..iterations {
        atav(n, &u, &mut v, &mut tmp);
        atav(n, &v, &mut u, &mut tmp);
    }

    let mut vbv = 0.0;
    let mut vv = 0.0;
    for i in 0..n {
        vbv += u[i] * v[i];
        vv += v[i] * v[i];
    }

    (vbv / vv).sqrt()
}

fn argument(index: usize) -> i64 {
    std::env::args().nth(index + 1)
        .expect("missing benchmark argument")
        .parse::<i64>()
        .expect("benchmark argument must be an i64")
}

fn main() {
    let result = spectral_norm(argument(0) as usize, argument(1));
    println!("{}", (result * 1_000_000_000.0) as i64);
}
