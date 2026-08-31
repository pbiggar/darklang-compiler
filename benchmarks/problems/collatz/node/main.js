// main.js - Parameterized Node.js benchmark implementation.
function argument(index) {
    const value = Number.parseInt(process.argv[index + 2], 10);
    if (!Number.isFinite(value)) throw new Error(`invalid benchmark argument ${index}`);
    return value;
}

// Collatz Benchmark
// Counts steps in Collatz sequences for numbers 1 to n

function collatzSteps(n, steps) {
    if (n === 1) {
        return steps;
    } else if (n % 2 === 0) {
        return collatzSteps(n / 2, steps + 1);
    } else {
        return collatzSteps(3 * n + 1, steps + 1);
    }
}

function sumCollatzRange(i, limit, total) {
    if (i > limit) {
        return total;
    }
    return sumCollatzRange(i + 1, limit, total + collatzSteps(i, 0));
}

// Sum steps for numbers 1 to 100000
console.log(sumCollatzRange(1, argument(0), 0));
