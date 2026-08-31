// main.js - Parameterized Node.js benchmark implementation.
function argument(index) {
    const value = Number.parseInt(process.argv[index + 2], 10);
    if (!Number.isFinite(value)) throw new Error(`invalid benchmark argument ${index}`);
    return value;
}

// Leibniz Pi Benchmark
// Computes pi using Leibniz formula: pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...

function leibnizLoop(i, n, sum, sign) {
    if (i >= n) {
        return sum * 4.0;
    }
    const term = sign / (2 * i + 1);
    return leibnizLoop(i + 1, n, sum + term, -sign);
}

function leibnizPi(n) {
    return leibnizLoop(0, n, 0.0, 1.0);
}

// Use 100 million iterations for timing
// Output as integer (multiply by large factor for precision)
console.log(Math.floor(leibnizPi(argument(0)) * 100000000));
