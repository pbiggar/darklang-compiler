// Matrix Multiplication Benchmark - Rust reference implementation
// From: plb2 and Julia micro-benchmarks
// Multiplies two NxN matrices using naive O(n^3) algorithm

fn matmul(a: &Vec<Vec<i64>>, b: &Vec<Vec<i64>>, n: usize) -> Vec<Vec<i64>> {
    let mut c = vec![vec![0i64; n]; n];
    for i in 0..n {
        for j in 0..n {
            let mut s: i64 = 0;
            for k in 0..n {
                s += a[i][k] * b[k][j];
            }
            c[i][j] = s;
        }
    }
    c
}

fn generate_matrix(n: usize, seed: u64) -> Vec<Vec<i64>> {
    let mut matrix = Vec::with_capacity(n);
    let mut x = seed;
    for _ in 0..n {
        let mut row = Vec::with_capacity(n);
        for _ in 0..n {
            x = (x.wrapping_mul(1103515245).wrapping_add(12345)) % (1u64 << 31);
            row.push((x % 100) as i64);
        }
        matrix.push(row);
    }
    matrix
}

fn checksum(m: &Vec<Vec<i64>>, n: usize) -> i64 {
    let mut result: i64 = 0;
    for i in 0..n {
        for j in 0..n {
            result = (result + m[i][j] * ((i * n + j + 1) as i64)) % 1000000007;
        }
    }
    result
}

fn main() {
    // Use 100x100 matrices for reasonable runtime
    let n = 100;
    let a = generate_matrix(n, 42);
    let b = generate_matrix(n, 123);
    let c = matmul(&a, &b, n);
    println!("{}", checksum(&c, n));
}
