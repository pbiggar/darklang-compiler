package main

import (
	"fmt"
	"math"
)

func a(i, j int) float64 {
	return 1.0 / float64((i+j)*(i+j+1)/2+i+1)
}

func av(n int, v, out []float64) {
	for i := 0; i < n; i++ {
		s := 0.0
		for j := 0; j < n; j++ {
			s += a(i, j) * v[j]
		}
		out[i] = s
	}
}

func atv(n int, v, out []float64) {
	for i := 0; i < n; i++ {
		s := 0.0
		for j := 0; j < n; j++ {
			s += a(j, i) * v[j]
		}
		out[i] = s
	}
}

func atav(n int, v, out, tmp []float64) {
	av(n, v, tmp)
	atv(n, tmp, out)
}

func spectralNorm(n int) float64 {
	u := make([]float64, n)
	v := make([]float64, n)
	tmp := make([]float64, n)

	for i := range u {
		u[i] = 1.0
	}

	for i := 0; i < 10; i++ {
		atav(n, u, v, tmp)
		atav(n, v, u, tmp)
	}

	vBv := 0.0
	vv := 0.0
	for i := 0; i < n; i++ {
		vBv += u[i] * v[i]
		vv += v[i] * v[i]
	}

	return math.Sqrt(vBv / vv)
}

func main() {
	result := spectralNorm(100)
	fmt.Println(int64(result * 1000000000.0))
}
