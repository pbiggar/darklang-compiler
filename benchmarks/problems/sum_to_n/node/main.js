function sumTo(n, acc) {
    if (n <= 0) {
        return acc;
    }
    return sumTo(n - 1, acc + n);
}

function repeat(n, acc) {
    if (n <= 0) {
        return acc;
    }
    return repeat(n - 1, sumTo(10000, 0));
}

console.log(repeat(100, 0));
