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




func sumTo(n, acc int64) int64 {
	if n <= 0 {
		return acc
	}
	return sumTo(n-1, acc+n)
}

func repeat(n, sumInput, acc int64) int64 {
	if n <= 0 {
		return acc
	}
	return repeat(n-1, sumInput, sumTo(sumInput, 0))
}

func main() {
	fmt.Println(repeat(argument(0), argument(1), 0))
}
