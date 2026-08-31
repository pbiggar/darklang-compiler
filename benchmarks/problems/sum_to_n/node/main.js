// main.js - Parameterized Node.js benchmark implementation.
function argument(index) {
    const value = Number.parseInt(process.argv[index + 2], 10);
    if (!Number.isFinite(value)) throw new Error(`invalid benchmark argument ${index}`);
    return value;
}

function sumTo(n, acc) {
    if (n <= 0) {
        return acc;
    }
    return sumTo(n - 1, acc + n);
}

function repeat(n, sumInput, acc) {
    if (n <= 0) {
        return acc;
    }
    return repeat(n - 1, sumInput, sumTo(sumInput, 0));
}

console.log(repeat(argument(0), argument(1), 0));
