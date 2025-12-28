// Prime Counting Benchmark - Rust reference implementation

fn is_prime(n: i64) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 {
        return true;
    }
    if n % 2 == 0 {
        return false;
    }
    let limit = (n as f64).sqrt() as i64;
    for d in 3..=limit {
        if n % d == 0 {
            return false;
        }
    }
    true
}

fn count_primes(n: i64) -> i64 {
    let mut count = 0;
    for i in 2..=n {
        if is_prime(i) {
            count += 1;
        }
    }
    count
}

fn main() {
    // Count primes up to 10000
    let result = count_primes(10000);
    println!("{}", result);
}
