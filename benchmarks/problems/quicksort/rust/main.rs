// Quicksort Benchmark - Rust reference implementation
// Sorts a list and returns a checksum

fn quicksort(arr: Vec<i64>) -> Vec<i64> {
    if arr.len() <= 1 {
        return arr;
    }
    let pivot = arr[arr.len() / 2];
    let left: Vec<i64> = arr.iter().filter(|&&x| x < pivot).copied().collect();
    let middle: Vec<i64> = arr.iter().filter(|&&x| x == pivot).copied().collect();
    let right: Vec<i64> = arr.iter().filter(|&&x| x > pivot).copied().collect();

    let mut result = quicksort(left);
    result.extend(middle);
    result.extend(quicksort(right));
    result
}

fn generate_list(n: usize, seed: u64) -> Vec<i64> {
    let mut result = Vec::with_capacity(n);
    let mut x = seed;
    for _ in 0..n {
        x = (x.wrapping_mul(1103515245).wrapping_add(12345)) % (1u64 << 31);
        result.push((x % 10000) as i64);
    }
    result
}

fn checksum(arr: &[i64]) -> i64 {
    let mut result: i64 = 0;
    for (i, &x) in arr.iter().enumerate() {
        result = (result + x * (i as i64 + 1)) % 1000000007;
    }
    result
}

fn main() {
    // Sort 5000 elements
    let arr = generate_list(5000, 42);
    let sorted_arr = quicksort(arr);
    println!("{}", checksum(&sorted_arr));
}
