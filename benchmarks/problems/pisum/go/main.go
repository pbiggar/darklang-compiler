package main

import "fmt"

func pisum(n int64) float64 {
	var s float64
	for i := 0; i < 500; i++ {
		s = 0.0
		for k := int64(1); k <= n; k++ {
			s += 1.0 / float64(k*k)
		}
	}
	return s
}

func main() {
	result := pisum(10000)
	fmt.Println(int64(result * 1000000000000.0))
}
