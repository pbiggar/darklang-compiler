// Binary Trees Benchmark
// Counts nodes in complete binary trees

function countTree(depth) {
    if (depth <= 0) {
        return 1;
    }
    return 1 + countTree(depth - 1) + countTree(depth - 1);
}

function stressTest(depth, iterations, acc) {
    if (iterations <= 0) {
        return acc;
    }
    const count = countTree(depth);
    return stressTest(depth, iterations - 1, acc + count);
}

// Run stress test: create many trees of depth 15
// Each complete binary tree of depth 15 has 2^16 - 1 = 65535 nodes
// Do 100 iterations
console.log(stressTest(15, 100, 0));
