// Exercises elimination of an identical-arm value selection in a hot loop.

#[inline(never)]
fn select_same(value: i64) -> i64 {
    let condition = value % 2 == 0;
    (if condition { value } else { value }) + 1
}

fn main() {
    let mut n = 1_000_000_i64;
    let mut total = 0_i64;
    while n > 0 {
        total += select_same(n);
        n -= 1;
    }
    println!("{total}");
}
