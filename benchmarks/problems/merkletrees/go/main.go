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




func hash(data uint64) uint64 {
	h := uint64(14695981039346656037)
	for i := 0; i < 8; i++ {
		h ^= data & 0xFF
		h *= 1099511628211
	}
	return h
}

func hashPair(left, right uint64) uint64 {
	combined := left + right*31
	return hash(combined)
}

func buildTree(depth uint32, leafStart uint64) uint64 {
	if depth == 0 {
		return hash(leafStart)
	}

	leftSize := uint64(1) << (depth - 1)
	left := buildTree(depth-1, leafStart)
	right := buildTree(depth-1, leafStart+leftSize)
	return hashPair(left, right)
}

func verifyTree(depth uint32, leafStart, expectedRoot uint64) bool {
	return buildTree(depth, leafStart) == expectedRoot
}

func main() {
	depth := uint32(argument(0))
	iterations := int(argument(1))

	var totalHash uint64
	for i := 0; i < iterations; i++ {
		root := buildTree(depth, uint64(i*1000))
		if verifyTree(depth, uint64(i*1000), root) {
			totalHash ^= root
		}
	}
	fmt.Println(int64(totalHash % 1000000000))
}
