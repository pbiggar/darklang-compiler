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




func leibnizPi(n int64) float64 {
	s := 0.0
	sign := 1.0
	for i := int64(0); i < n; i++ {
		s += sign / float64(2*i+1)
		sign = -sign
	}
	return s * 4.0
}

func main() {
	result := leibnizPi(argument(0))
	fmt.Println(int64(result * 100000000.0))
}
