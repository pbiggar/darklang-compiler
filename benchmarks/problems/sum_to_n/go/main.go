package main

import "fmt"

func sumTo(n, acc int64) int64 {
	if n <= 0 {
		return acc
	}
	return sumTo(n-1, acc+n)
}

func repeat(n, acc int64) int64 {
	if n <= 0 {
		return acc
	}
	return repeat(n-1, sumTo(10000, 0))
}

func main() {
	fmt.Println(repeat(100, 0))
}
