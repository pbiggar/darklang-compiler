package main

import "fmt"

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
	result := leibnizPi(100000000)
	fmt.Println(int64(result * 100000000.0))
}
