// Spectral Norm Benchmark
// From: Computer Language Benchmarks Game

function A(i, j) {
    return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1);
}

function Av(n, v, out) {
    for (let i = 0; i < n; i++) {
        let s = 0.0;
        for (let j = 0; j < n; j++) {
            s += A(i, j) * v[j];
        }
        out[i] = s;
    }
}

function Atv(n, v, out) {
    for (let i = 0; i < n; i++) {
        let s = 0.0;
        for (let j = 0; j < n; j++) {
            s += A(j, i) * v[j];
        }
        out[i] = s;
    }
}

function AtAv(n, v, out, tmp) {
    Av(n, v, tmp);
    Atv(n, tmp, out);
}

function spectralNorm(n) {
    const u = new Array(n).fill(1.0);
    const v = new Array(n).fill(0.0);
    const tmp = new Array(n).fill(0.0);

    for (let i = 0; i < 10; i++) {
        AtAv(n, u, v, tmp);
        AtAv(n, v, u, tmp);
    }

    let vBv = 0.0;
    let vv = 0.0;
    for (let i = 0; i < n; i++) {
        vBv += u[i] * v[i];
        vv += v[i] * v[i];
    }

    return Math.sqrt(vBv / vv);
}

// n = 100 is a reasonable size
const result = spectralNorm(100);
console.log(Math.floor(result * 1000000000));
