// Matrix Multiplication Benchmark
// Multiplies two NxN matrices using naive O(n^3) algorithm

function matmul(a, b, n) {
    const c = [];
    for (let i = 0; i < n; i++) {
        c[i] = [];
        for (let j = 0; j < n; j++) {
            let s = 0;
            for (let k = 0; k < n; k++) {
                s += a[i][k] * b[k][j];
            }
            c[i][j] = s;
        }
    }
    return c;
}

function generateMatrix(n, seed) {
    const matrix = [];
    let x = seed;
    for (let i = 0; i < n; i++) {
        matrix[i] = [];
        for (let j = 0; j < n; j++) {
            x = (x * 1103515245 + 12345) % (1 << 31);
            matrix[i][j] = x % 100;
        }
    }
    return matrix;
}

function checksum(m, n) {
    let result = 0;
    for (let i = 0; i < n; i++) {
        for (let j = 0; j < n; j++) {
            result = (result + m[i][j] * (i * n + j + 1)) % 1000000007;
        }
    }
    return result;
}

// Use 100x100 matrices for reasonable runtime
const n = 100;
const a = generateMatrix(n, 42);
const b = generateMatrix(n, 123);
const c = matmul(a, b, n);
console.log(checksum(c, n));
