// Binary Trees Benchmark - Rust reference implementation

enum Tree {
    Leaf,
    Node(Box<Tree>, Box<Tree>),
}

fn make_tree(depth: i64) -> Tree {
    if depth <= 0 {
        Tree::Leaf
    } else {
        Tree::Node(
            Box::new(make_tree(depth - 1)),
            Box::new(make_tree(depth - 1)),
        )
    }
}

fn count_nodes(tree: &Tree) -> i64 {
    match tree {
        Tree::Leaf => 1,
        Tree::Node(left, right) => 1 + count_nodes(left) + count_nodes(right),
    }
}

fn stress_test(depth: i64, iterations: i64) -> i64 {
    let mut total = 0;
    for _ in 0..iterations {
        let tree = make_tree(depth);
        total += count_nodes(&tree);
    }
    total
}

fn main() {
    // Reduced from depth 15 and 100 iterations to depth 10 and 5 iterations.
    let result = stress_test(10, 5);
    println!("{}", result);
}
