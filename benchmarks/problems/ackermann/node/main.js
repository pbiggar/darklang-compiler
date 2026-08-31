// main.js - Parameterized Node.js benchmark implementation.
function argument(index) {
    const value = Number.parseInt(process.argv[index + 2], 10);
    if (!Number.isFinite(value)) throw new Error(`invalid benchmark argument ${index}`);
    return value;
}

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
console.log(ackermann(argument(0), argument(1)));
