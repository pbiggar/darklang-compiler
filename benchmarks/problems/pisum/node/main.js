// main.js - Parameterized Node.js benchmark implementation.
function argument(index) {
    const value = Number.parseInt(process.argv[index + 2], 10);
    if (!Number.isFinite(value)) throw new Error(`invalid benchmark argument ${index}`);
    return value;
}

// Pi Summation Benchmark
// Computes partial sum of 1/k^2 series (converges to pi^2/6)

function pisum(rounds, n) {
    let s = 0.0;
    for (let i = 0; i < rounds; i++) {
        s = 0.0;
        for (let k = 1; k <= n; k++) {
            s += 1.0 / (k * k);
        }
    }
    return s;
}

// n=10000 gives reasonable runtime
const result = pisum(argument(0), argument(1));
// Output as integer (multiply by large factor for precision)
console.log(Math.floor(result * 1000000000000));
