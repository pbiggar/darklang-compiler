package main

import "fmt"

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
	fmt.Println(sumCollatz(100000))
}
