// Quicksort Benchmark
// Sorts a list and returns a checksum

function quicksort(arr) {
    if (arr.length <= 1) {
        return arr;
    }
    const pivot = arr[Math.floor(arr.length / 2)];
    const left = arr.filter(x => x < pivot);
    const middle = arr.filter(x => x === pivot);
    const right = arr.filter(x => x > pivot);
    return [...quicksort(left), ...middle, ...quicksort(right)];
}

function generateList(n, seed) {
    const result = [];
    let x = seed;
    for (let i = 0; i < n; i++) {
        x = (x * 1103515245 + 12345) % (1 << 31);
        result.push(x % 10000);
    }
    return result;
}

function checksum(arr) {
    let result = 0;
    for (let i = 0; i < arr.length; i++) {
        result = (result + arr[i] * (i + 1)) % 1000000007;
    }
    return result;
}

// Sort 100 elements (reduced to match Dark heap constraints)
const arr = generateList(100, 42);
const sortedArr = quicksort(arr);
console.log(checksum(sortedArr));
