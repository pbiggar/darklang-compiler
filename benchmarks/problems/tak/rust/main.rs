// Tak (Takeuchi) Benchmark - Rust reference implementation

fn tak(x: i64, y: i64, z: i64) -> i64 {
    if x <= y {
        z
    } else {
        tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
    }
}

fn main() {
    // Repeat multiple times for meaningful measurement
    let mut result = 0;
    for _ in 0..10 {
        result = tak(24, 16, 8);
    }
    println!("{}", result);
}
