package main

import "fmt"

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
	return repeat(n-1, factorial(20))
}

func main() {
	fmt.Println(repeat(10000, 0))
}
