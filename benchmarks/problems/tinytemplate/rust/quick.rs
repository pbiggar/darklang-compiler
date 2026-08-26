// Reduced TinyTemplate 1.2.1 compiler/interpreter benchmark.
mod engine { include!("main.rs"); }

fn main() { engine::benchmark(40); }
