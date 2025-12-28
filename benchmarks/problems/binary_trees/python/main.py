#!/usr/bin/env python3
# Binary Trees Benchmark - Python reference implementation

class Tree:
    pass

class Leaf(Tree):
    pass

class Node(Tree):
    def __init__(self, left, right):
        self.left = left
        self.right = right

def make_tree(depth):
    if depth <= 0:
        return Leaf()
    else:
        return Node(make_tree(depth - 1), make_tree(depth - 1))

def count_nodes(tree):
    if isinstance(tree, Leaf):
        return 1
    else:
        return 1 + count_nodes(tree.left) + count_nodes(tree.right)

def stress_test(depth, iterations):
    total = 0
    for _ in range(iterations):
        tree = make_tree(depth)
        total += count_nodes(tree)
    return total

# Same parameters as Dark version
result = stress_test(15, 100)
print(result)
