// main.js - Parameterized Node.js benchmark implementation.
function argument(index) {
    const value = Number.parseInt(process.argv[index + 2], 10);
    if (!Number.isFinite(value)) throw new Error(`invalid benchmark argument ${index}`);
    return value;
}

// Tak (Takeuchi) Benchmark
// Tests recursion and function call overhead

function tak(x, y, z) {
    if (x <= y) {
        return z;
    }
    return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
}

// Repeat multiple times for meaningful measurement
function repeat(n, x, y, z, acc) {
    if (n <= 0) {
        return acc;
    }
    return repeat(n - 1, x, y, z, tak(x, y, z));
}

console.log(repeat(argument(0), argument(1), argument(2), argument(3), 0));
