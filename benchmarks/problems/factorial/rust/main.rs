fn factorial(n: i64) -> i64 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn repeat(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        repeat(n - 1, factorial(20))
    }
}

fn main() {
    println!("{}", repeat(10000, 0));
}
