// Reduced-workload driver for the full vendored TinyTemplate implementation.

mod full {
    include!("main.rs");
}

fn main() {
    let arguments = std::env::args().skip(1).collect::<Vec<_>>();
    let row_count = arguments[0].parse::<usize>().expect("row count must be a usize");
    let runs = arguments[1].parse::<usize>().expect("run count must be a usize");
    full::benchmark(runs, row_count);
}
