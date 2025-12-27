fn sum_to(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        sum_to(n - 1, acc + n)
    }
}

fn repeat(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        repeat(n - 1, sum_to(10000, 0))
    }
}

fn main() {
    println!("{}", repeat(100, 0));
}
