// main.rs - Parameterized Rust benchmark implementation.
// Mandelbrot Benchmark - Rust reference implementation

fn mandelbrot(cr: f64, ci: f64, max_iter: i32) -> i32 {
    let mut zr = 0.0;
    let mut zi = 0.0;
    for _ in 0..max_iter {
        if zr * zr + zi * zi > 4.0 {
            return 0;
        }
        let new_zr = zr * zr - zi * zi + cr;
        let new_zi = 2.0 * zr * zi + ci;
        zr = new_zr;
        zi = new_zi;
    }
    1
}

fn count_mandelbrot(size: i32, max_iter: i32) -> i32 {
    let mut count = 0;
    for y in 0..size {
        let ci = 2.0 * (y as f64) / (size as f64) - 1.0;
        for x in 0..size {
            let cr = 2.0 * (x as f64) / (size as f64) - 1.5;
            count += mandelbrot(cr, ci, max_iter);
        }
    }
    count
}

fn argument(index: usize) -> i64 {
    std::env::args().nth(index + 1)
        .expect("missing benchmark argument")
        .parse::<i64>()
        .expect("benchmark argument must be an i64")
}

fn main() {
    let result = count_mandelbrot(argument(0) as i32, argument(1) as i32);
    println!("{}", result);
}
