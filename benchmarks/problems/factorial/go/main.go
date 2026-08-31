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




func factorial(n int64) int64 {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

func repeat(n, acc int64) int64 {
	if n <= 0 {
		return acc
	}
	return repeat(n-1, factorial(argument(1)))
}

func main() {
	fmt.Println(repeat(argument(0), 0))
}
