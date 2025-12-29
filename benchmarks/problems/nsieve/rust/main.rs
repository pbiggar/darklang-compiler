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
    for _ in 0..100 {
        total = nsieve(100000);
    }
    println!("{}", total);
}
