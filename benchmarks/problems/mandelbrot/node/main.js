// Mandelbrot Benchmark

function mandelbrot(cr, ci, maxIter) {
    let zr = 0.0;
    let zi = 0.0;
    for (let i = 0; i < maxIter; i++) {
        if (zr * zr + zi * zi > 4.0) {
            return 0;
        }
        const newZr = zr * zr - zi * zi + cr;
        const newZi = 2.0 * zr * zi + ci;
        zr = newZr;
        zi = newZi;
    }
    return 1;
}

function countMandelbrot(size, maxIter) {
    let count = 0;
    for (let y = 0; y < size; y++) {
        const ci = 2.0 * y / size - 1.0;
        for (let x = 0; x < size; x++) {
            const cr = 2.0 * x / size - 1.5;
            count += mandelbrot(cr, ci, maxIter);
        }
    }
    return count;
}

// Count points in 200x200 grid with 50 iterations
console.log(countMandelbrot(200, 50));
