// Exercises multiply-subtract fusion in a hot loop.

#[inline(never)]
fn multiply_subtract(minuend: i64, left: i64, right: i64) -> i64 {
    minuend - (left * right)
}

fn main() {
    let mut n = 1_000_000_i64;
    let mut result = 0_i64;
    while n > 0 {
        result = multiply_subtract(1_000_000, n, n);
        n -= 1;
    }
    println!("{result}");
}
