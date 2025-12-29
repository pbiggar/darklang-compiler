function ackermann(m, n) {
    if (m === 0) {
        return n + 1;
    } else if (n === 0) {
        return ackermann(m - 1, 1);
    } else {
        return ackermann(m - 1, ackermann(m, n - 1));
    }
}

// A(3, 12) = 32765
console.log(ackermann(3, 12));
