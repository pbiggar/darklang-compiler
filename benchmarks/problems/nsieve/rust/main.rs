// main.rs - Parameterized Rust benchmark implementation.
fn argument(index: usize) -> i64 {
    std::env::args().nth(index + 1)
        .expect("missing benchmark argument")
        .parse::<i64>()
        .expect("benchmark argument must be an i64")
}

// Nsieve Benchmark - Sieve of Eratosthenes
// From: Computer Language Benchmarks Game
// Counts primes up to n using sieve algorithm

fn nsieve(n: usize) -> usize {
    let mut is_prime = vec![true; n + 1];
    let mut count = 0;

    for i in 2..=n {
        if is_prime[i] {
            count += 1;
            // Mark multiples as not prime
            let mut j = i + i;
            while j <= n {
                is_prime[j] = false;
                j += i;
            }
        }
    }
    count
}

fn main() {
    // Run sieve multiple times for meaningful benchmark
    // Each run counts primes up to 100000
    let mut total = 0;
    for _ in 0..argument(1) {
        total = nsieve(argument(0) as usize);
    }
    println!("{}", total);
}
