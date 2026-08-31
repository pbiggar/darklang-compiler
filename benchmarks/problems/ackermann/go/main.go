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




func ackermann(m, n int64) int64 {
	if m == 0 {
		return n + 1
	} else if n == 0 {
		return ackermann(m-1, 1)
	}
	return ackermann(m-1, ackermann(m, n-1))
}

func main() {
	fmt.Println(ackermann(argument(0), argument(1)))
}
