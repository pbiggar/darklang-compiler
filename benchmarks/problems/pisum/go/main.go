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




func pisum(rounds, n int64) float64 {
	var s float64
	for i := 0; int64(i) < rounds; i++ {
		s = 0.0
		for k := int64(1); k <= n; k++ {
			s += 1.0 / float64(k*k)
		}
	}
	return s
}

func main() {
	result := pisum(argument(0), argument(1))
	fmt.Println(int64(result * 1000000000000.0))
}
