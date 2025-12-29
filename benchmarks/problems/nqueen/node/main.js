// N-Queens Benchmark
// Counts solutions to the N-queens problem using bit manipulation

function nqueen(n) {
    const allOnes = (1 << n) - 1;

    function solve(cols, diag1, diag2) {
        if (cols === allOnes) {
            return 1;
        }
        let count = 0;
        let avail = allOnes & ~(cols | diag1 | diag2);
        while (avail !== 0) {
            const pos = avail & (-avail);
            avail -= pos;
            count += solve(cols | pos, (diag1 | pos) << 1, (diag2 | pos) >>> 1);
        }
        return count;
    }

    return solve(0, 0, 0);
}

// N=13 gives reasonable runtime
console.log(nqueen(13));
