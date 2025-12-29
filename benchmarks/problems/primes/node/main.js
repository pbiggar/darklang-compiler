// Prime Counting Benchmark
// Counts primes up to N using trial division

function isqrt(n, guess) {
    if (guess * guess > n) {
        return guess - 1;
    } else if ((guess + 1) * (guess + 1) > n) {
        return guess;
    } else {
        return isqrt(n, guess + 1);
    }
}

function isDivisible(n, d, limit) {
    if (d > limit) {
        return false;
    } else if (n % d === 0) {
        return true;
    } else {
        return isDivisible(n, d + 1, limit);
    }
}

function isPrime(n) {
    if (n < 2) {
        return false;
    } else if (n === 2) {
        return true;
    } else if (n % 2 === 0) {
        return false;
    } else {
        const limit = isqrt(n, 1);
        return !isDivisible(n, 3, limit);
    }
}

function countPrimes(n, count) {
    if (n <= 1) {
        return count;
    } else if (isPrime(n)) {
        return countPrimes(n - 1, count + 1);
    } else {
        return countPrimes(n - 1, count);
    }
}

// Count primes up to 10000
// Expected: 1229 primes
console.log(countPrimes(10000, 0));
