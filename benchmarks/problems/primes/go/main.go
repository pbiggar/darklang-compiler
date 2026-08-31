// main.go - Parameterized Go benchmark implementation.
package main

import (
	"fmt"
	"os"
	"strconv"
	"math"
)

func argument(index int) int64 {
	value, err := strconv.ParseInt(os.Args[index+1], 10, 64)
	if err != nil {
		panic("benchmark argument must be an int64")
	}
	return value
}




func isPrime(n int64) bool {
	if n < 2 {
		return false
	}
	if n == 2 {
		return true
	}
	if n%2 == 0 {
		return false
	}
	limit := int64(math.Sqrt(float64(n)))
	for d := int64(3); d <= limit; d++ {
		if n%d == 0 {
			return false
		}
	}
	return true
}

func countPrimes(n int64) int64 {
	var count int64
	for i := int64(2); i <= n; i++ {
		if isPrime(i) {
			count++
		}
	}
	return count
}

func main() {
	fmt.Println(countPrimes(argument(0)))
}
