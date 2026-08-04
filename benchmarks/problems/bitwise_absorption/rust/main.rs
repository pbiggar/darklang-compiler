// Exercises bitwise absorption in a hot loop.

fn main() {
    let mut n = 1_000_000_i64;
    let mut result = 0_i64;
    while n > 0 {
        let absorbed_and = n & (n | result);
        let absorbed_or = n | (n & result);
        result = absorbed_and + absorbed_or;
        n -= 1;
    }
    println!("{result}");
}
