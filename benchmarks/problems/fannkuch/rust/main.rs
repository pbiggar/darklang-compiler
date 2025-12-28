// Fannkuch Benchmark - Rust reference implementation
// From: Computer Language Benchmarks Game

fn fannkuch(n: usize) -> i32 {
    let mut perm: Vec<usize> = (0..n).collect();
    let mut count: Vec<usize> = (0..n).collect();
    let mut max_flips = 0;
    let mut nperm = 0;

    loop {
        // Count flips for current permutation
        if perm[0] != 0 {
            let mut p = perm.clone();
            let mut flips = 0;
            while p[0] != 0 {
                let k = p[0];
                // Reverse first k+1 elements
                p[..=k].reverse();
                flips += 1;
            }
            if flips > max_flips {
                max_flips = flips;
            }
        }

        // Generate next permutation
        let mut i = 1;
        while i < n {
            // Rotate first i+1 elements left by 1
            let t = perm[0];
            for j in 0..i {
                perm[j] = perm[j + 1];
            }
            perm[i] = t;
            count[i] -= 1;
            if count[i] > 0 {
                break;
            }
            count[i] = i;
            i += 1;
        }
        if i >= n {
            break;
        }
        nperm += 1;
    }

    max_flips
}

fn main() {
    // n=9 gives reasonable runtime
    let result = fannkuch(9);
    println!("{}", result);
}
