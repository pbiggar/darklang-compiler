package main

import "fmt"

func tak(x, y, z int64) int64 {
	if x <= y {
		return z
	}
	return tak(tak(x-1, y, z), tak(y-1, z, x), tak(z-1, x, y))
}

func main() {
	var result int64
	for i := 0; i < 10; i++ {
		result = tak(24, 16, 8)
	}
	fmt.Println(result)
}
