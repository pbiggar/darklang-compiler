// main.js - Parameterized Node.js benchmark implementation.
function argument(index) {
    const value = Number.parseInt(process.argv[index + 2], 10);
    if (!Number.isFinite(value)) throw new Error(`invalid benchmark argument ${index}`);
    return value;
}

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
for (let i = 0; i < argument(1); i++) {
    total = nsieve(argument(0));
}
console.log(total);
