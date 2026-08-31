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




func tak(x, y, z int64) int64 {
	if x <= y {
		return z
	}
	return tak(tak(x-1, y, z), tak(y-1, z, x), tak(z-1, x, y))
}

func main() {
	var result int64
	for i := 0; int64(i) < argument(0); i++ {
		result = tak(argument(1), argument(2), argument(3))
	}
	fmt.Println(result)
}
