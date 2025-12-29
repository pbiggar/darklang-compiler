package main

import "fmt"

func matmul(a, b [][]int64, n int) [][]int64 {
	c := make([][]int64, n)
	for i := range c {
		c[i] = make([]int64, n)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			var s int64
			for k := 0; k < n; k++ {
				s += a[i][k] * b[k][j]
			}
			c[i][j] = s
		}
	}
	return c
}

func generateMatrix(n int, seed uint64) [][]int64 {
	matrix := make([][]int64, n)
	x := seed
	for i := 0; i < n; i++ {
		matrix[i] = make([]int64, n)
		for j := 0; j < n; j++ {
			x = (x*1103515245 + 12345) % (1 << 31)
			matrix[i][j] = int64(x % 100)
		}
	}
	return matrix
}

func checksum(m [][]int64, n int) int64 {
	var result int64
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			result = (result + m[i][j]*int64(i*n+j+1)) % 1000000007
		}
	}
	return result
}

func main() {
	n := 100
	a := generateMatrix(n, 1)
	b := generateMatrix(n, 2)
	c := matmul(a, b, n)
	fmt.Println(checksum(c, n))
}
