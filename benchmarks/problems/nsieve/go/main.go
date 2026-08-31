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




func nsieve(n int) int {
	isPrime := make([]bool, n+1)
	for i := range isPrime {
		isPrime[i] = true
	}
	count := 0

	for i := 2; i <= n; i++ {
		if isPrime[i] {
			count++
			for j := i + i; j <= n; j += i {
				isPrime[j] = false
			}
		}
	}
	return count
}

func main() {
	var total int
	for i := 0; int64(i) < argument(1); i++ {
		total = nsieve(int(argument(0)))
	}
	fmt.Println(total)
}
