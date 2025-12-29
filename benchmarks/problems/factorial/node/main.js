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
    return repeat(n - 1, factorial(20));
}

console.log(repeat(10000, 0n).toString());
