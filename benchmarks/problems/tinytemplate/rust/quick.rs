// Reduced-workload driver for the full vendored TinyTemplate implementation.

mod full {
    include!("main.rs");
}

fn main() {
    full::benchmark(1, 3);
}
