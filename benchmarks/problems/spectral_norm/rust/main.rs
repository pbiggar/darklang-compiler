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

fn spectral_norm(n: usize) -> f64 {
    let mut u = vec![1.0; n];
    let mut v = vec![0.0; n];
    let mut tmp = vec![0.0; n];

    for _ in 0..10 {
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

fn main() {
    // n = 100 is a reasonable size
    let result = spectral_norm(100);
    println!("{}", (result * 1_000_000_000.0) as i64);
}
