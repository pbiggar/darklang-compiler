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




func mandelbrot(cr, ci float64, maxIter int) int {
	zr := 0.0
	zi := 0.0
	for i := 0; i < maxIter; i++ {
		if zr*zr+zi*zi > 4.0 {
			return 0
		}
		newZr := zr*zr - zi*zi + cr
		newZi := 2.0*zr*zi + ci
		zr = newZr
		zi = newZi
	}
	return 1
}

func countMandelbrot(size, maxIter int) int {
	count := 0
	for y := 0; y < size; y++ {
		ci := 2.0*float64(y)/float64(size) - 1.0
		for x := 0; x < size; x++ {
			cr := 2.0*float64(x)/float64(size) - 1.5
			count += mandelbrot(cr, ci, maxIter)
		}
	}
	return count
}

func main() {
	fmt.Println(countMandelbrot(int(argument(0)), int(argument(1))))
}
