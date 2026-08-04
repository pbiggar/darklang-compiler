// Exercises repeated successor conditions in a hot loop.

#[inline(never)]
fn choose(condition: bool) -> i64 {
    if condition {
        if condition {
            return 1;
        }
        return 0;
    }
    0
}

fn main() {
    let mut result = 0_i64;
    for n in (1_i64..=1_000_000).rev() {
        result += choose((n & 1) == 0);
    }
    println!("{result}");
}
