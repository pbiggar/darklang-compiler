// main.js - Parameterized Node.js benchmark implementation.
function argument(index) {
    const value = Number.parseInt(process.argv[index + 2], 10);
    if (!Number.isFinite(value)) throw new Error(`invalid benchmark argument ${index}`);
    return value;
}

function factorial(n) {
    if (n <= 1) {
        return 1n;
    }
    return BigInt(n) * factorial(n - 1);
}

function repeat(n, acc) {
    if (n <= 0) {
        return acc;
    }
    return repeat(n - 1, factorial(argument(1)));
}

console.log(repeat(argument(0), 0n).toString());
