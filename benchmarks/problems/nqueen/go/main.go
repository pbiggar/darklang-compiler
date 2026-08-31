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




func nqueen(n uint32) uint64 {
	allOnes := uint32((1 << n) - 1)

	var solve func(cols, diag1, diag2 uint32) uint64
	solve = func(cols, diag1, diag2 uint32) uint64 {
		if cols == allOnes {
			return 1
		}
		var count uint64
		avail := allOnes &^ (cols | diag1 | diag2)
		for avail != 0 {
			pos := avail & (^avail + 1) // lowest set bit
			avail -= pos
			count += solve(cols|pos, (diag1|pos)<<1, (diag2|pos)>>1)
		}
		return count
	}

	return solve(0, 0, 0)
}

func main() {
	fmt.Println(nqueen(uint(argument(0))))
}
