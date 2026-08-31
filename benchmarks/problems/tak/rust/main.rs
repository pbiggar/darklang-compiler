// main.rs - Parameterized Rust benchmark implementation.
// Tak (Takeuchi) Benchmark - Rust reference implementation

fn tak(x: i64, y: i64, z: i64) -> i64 {
    if x <= y {
        z
    } else {
        tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
    }
}

fn argument(index: usize) -> i64 {
    std::env::args().nth(index + 1)
        .expect("missing benchmark argument")
        .parse::<i64>()
        .expect("benchmark argument must be an i64")
}

fn main() {
    let mut result = 0;
    for _ in 0..argument(0) {
        result = tak(argument(1), argument(2), argument(3));
    }
    println!("{}", result);
}
