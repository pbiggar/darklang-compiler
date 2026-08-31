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




func fib(n int64) int64 {
	if n <= 1 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	fmt.Println(fib(argument(0)))
}
