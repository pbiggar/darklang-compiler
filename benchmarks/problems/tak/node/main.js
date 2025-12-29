// Tak (Takeuchi) Benchmark
// Tests recursion and function call overhead

function tak(x, y, z) {
    if (x <= y) {
        return z;
    }
    return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
}

// Repeat multiple times for meaningful measurement
function repeat(n, acc) {
    if (n <= 0) {
        return acc;
    }
    return repeat(n - 1, tak(24, 16, 8));
}

console.log(repeat(10, 0));
