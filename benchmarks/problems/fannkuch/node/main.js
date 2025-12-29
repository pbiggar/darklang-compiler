// Fannkuch Benchmark
// From: Computer Language Benchmarks Game

function fannkuch(n) {
    const perm = [];
    const count = [];
    for (let i = 0; i < n; i++) {
        perm[i] = i;
        count[i] = i;
    }
    let maxFlips = 0;

    while (true) {
        // Count flips for current permutation
        if (perm[0] !== 0) {
            const p = perm.slice();
            let flips = 0;
            while (p[0] !== 0) {
                const k = p[0];
                // Reverse first k+1 elements
                for (let i = 0; i <= k >> 1; i++) {
                    const tmp = p[i];
                    p[i] = p[k - i];
                    p[k - i] = tmp;
                }
                flips++;
            }
            if (flips > maxFlips) {
                maxFlips = flips;
            }
        }

        // Generate next permutation
        let i = 1;
        while (i < n) {
            // Rotate first i+1 elements left by 1
            const t = perm[0];
            for (let j = 0; j < i; j++) {
                perm[j] = perm[j + 1];
            }
            perm[i] = t;
            count[i]--;
            if (count[i] > 0) {
                break;
            }
            count[i] = i;
            i++;
        }
        if (i >= n) {
            break;
        }
    }

    return maxFlips;
}

// n=9 gives reasonable runtime
console.log(fannkuch(9));
