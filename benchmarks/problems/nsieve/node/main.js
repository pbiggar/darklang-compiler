// Nsieve Benchmark - Sieve of Eratosthenes
// Counts primes up to n using sieve algorithm

function nsieve(n) {
    const isPrime = new Array(n + 1).fill(true);
    let count = 0;

    for (let i = 2; i <= n; i++) {
        if (isPrime[i]) {
            count++;
            // Mark multiples as not prime
            for (let j = i + i; j <= n; j += i) {
                isPrime[j] = false;
            }
        }
    }
    return count;
}

// Run sieve multiple times for meaningful benchmark
let total = 0;
for (let i = 0; i < 100; i++) {
    total = nsieve(100000);
}
console.log(total);
