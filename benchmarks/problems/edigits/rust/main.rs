// Edigits Benchmark - Computing digits of e
// Uses series expansion: e = sum(1/n!) for n=0 to infinity
// Computes checksum of first N digits

// Compute e using Taylor series with fixed-point arithmetic
// e = 1 + 1/1! + 1/2! + 1/3! + ...
fn compute_e_digits(num_digits: usize) -> Vec<u8> {
    // We need extra precision for computation
    let precision = num_digits + 10;
    let base: u64 = 10;

    // Work with scaled integers
    // Start with e = 1 (scaled by 10^precision)
    let mut e_scaled: Vec<u64> = vec![0; precision + 1];
    e_scaled[0] = 1; // represents 1.0

    // Add 1/n! for n = 1, 2, 3, ...
    // 1/n! = (1/(n-1)!) / n
    let mut term: Vec<u64> = vec![0; precision + 1];
    term[0] = 1; // start with 1

    for n in 1..=50 {
        // term = term / n
        let mut carry: u64 = 0;
        for i in 0..=precision {
            let current = carry * base + term[i];
            term[i] = current / n;
            carry = current % n;
        }

        // e_scaled += term
        let mut carry2: u64 = 0;
        for i in (0..=precision).rev() {
            let sum = e_scaled[i] + term[i] + carry2;
            e_scaled[i] = sum % base;
            carry2 = sum / base;
        }
    }

    // Extract digits
    let mut digits = Vec::with_capacity(num_digits);
    for i in 0..num_digits {
        digits.push(e_scaled[i] as u8);
    }
    digits
}

fn main() {
    // Compute first 1000 digits of e multiple times
    let mut checksum: u64 = 0;
    for _ in 0..10 {
        let digits = compute_e_digits(1000);
        checksum = 0;
        for (i, &d) in digits.iter().enumerate() {
            checksum = (checksum + (d as u64) * ((i as u64) + 1)) % 1000000007;
        }
    }
    println!("{}", checksum);
}
