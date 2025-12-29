// Pi Summation Benchmark
// Computes partial sum of 1/k^2 series (converges to pi^2/6)

function pisum(n) {
    let s = 0.0;
    for (let i = 0; i < 500; i++) {
        s = 0.0;
        for (let k = 1; k <= n; k++) {
            s += 1.0 / (k * k);
        }
    }
    return s;
}

// n=10000 gives reasonable runtime
const result = pisum(10000);
// Output as integer (multiply by large factor for precision)
console.log(Math.floor(result * 1000000000000));
