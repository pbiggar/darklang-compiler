// Edigits Benchmark - Computing digits of e
// Uses series expansion: e = sum(1/n!) for n=0 to infinity
// Computes checksum of first N digits

function computeEDigits(numDigits) {
    // We need extra precision for computation
    const precision = numDigits + 10;
    const base = 10;

    // Work with scaled integers
    // Start with e = 1 (scaled by 10^precision)
    const eScaled = new Array(precision + 1).fill(0);
    eScaled[0] = 1; // represents 1.0

    // Add 1/n! for n = 1, 2, 3, ...
    // 1/n! = (1/(n-1)!) / n
    const term = new Array(precision + 1).fill(0);
    term[0] = 1; // start with 1

    for (let n = 1; n <= 50; n++) {
        // term = term / n
        let carry = 0;
        for (let i = 0; i <= precision; i++) {
            const current = carry * base + term[i];
            term[i] = Math.floor(current / n);
            carry = current % n;
        }

        // eScaled += term
        let carry2 = 0;
        for (let i = precision; i >= 0; i--) {
            const sum = eScaled[i] + term[i] + carry2;
            eScaled[i] = sum % base;
            carry2 = Math.floor(sum / base);
        }
    }

    // Extract digits
    return eScaled.slice(0, numDigits);
}

// Compute first 1000 digits of e multiple times
let checksum = 0;
for (let iter = 0; iter < 10; iter++) {
    const digits = computeEDigits(1000);
    checksum = 0;
    for (let i = 0; i < digits.length; i++) {
        checksum = (checksum + digits[i] * (i + 1)) % 1000000007;
    }
}
console.log(checksum);
