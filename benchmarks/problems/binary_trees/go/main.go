// main.go - Parameterized Go benchmark implementation.
package main

import (
	"fmt"
	"os"
	"strconv"
)

func argument(index int) int64 {
	value, err := strconv.ParseInt(os.Args[index+1], 10, 64)
	if err != nil {
		panic("benchmark argument must be an int64")
	}
	return value
}




type Tree struct {
	left  *Tree
	right *Tree
}

func makeTree(depth int64) *Tree {
	if depth <= 0 {
		return &Tree{}
	}
	return &Tree{
		left:  makeTree(depth - 1),
		right: makeTree(depth - 1),
	}
}

func countNodes(tree *Tree) int64 {
	if tree.left == nil {
		return 1
	}
	return 1 + countNodes(tree.left) + countNodes(tree.right)
}

func stressTest(depth, iterations int64) int64 {
	var total int64
	for i := int64(0); i < iterations; i++ {
		tree := makeTree(depth)
		total += countNodes(tree)
	}
	return total
}

func main() {
	fmt.Println(stressTest(argument(0), argument(1)))
}
