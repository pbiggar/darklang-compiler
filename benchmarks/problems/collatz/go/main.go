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




func collatzSteps(n int64) int64 {
	var steps int64
	for n != 1 {
		if n%2 == 0 {
			n = n / 2
		} else {
			n = 3*n + 1
		}
		steps++
	}
	return steps
}

func sumCollatz(limit int64) int64 {
	var total int64
	for i := int64(1); i <= limit; i++ {
		total += collatzSteps(i)
	}
	return total
}

func main() {
	fmt.Println(sumCollatz(argument(0)))
}
