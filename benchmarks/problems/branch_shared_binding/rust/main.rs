// Exercises shared pure leading work across conditional branches.

#[inline(never)]
fn select(value: i64) -> i64 {
    if value > 0 {
        let shared = value * 3;
        shared + 1
    } else {
        let shared = value * 3;
        shared - 1
    }
}

fn main() {
    let mut n = 1_000_000_i64;
    let mut acc = 0_i64;
    while n > 0 {
        acc += select(n);
        n -= 1;
    }
    println!("{acc}");
}
