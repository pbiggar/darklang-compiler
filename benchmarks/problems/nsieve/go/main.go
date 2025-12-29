package main

import "fmt"

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
	for i := 0; i < 100; i++ {
		total = nsieve(100000)
	}
	fmt.Println(total)
}
