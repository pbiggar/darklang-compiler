package main

import "fmt"

func quicksort(arr []int64) []int64 {
	if len(arr) <= 1 {
		return arr
	}
	pivot := arr[len(arr)/2]

	var left, middle, right []int64
	for _, x := range arr {
		if x < pivot {
			left = append(left, x)
		} else if x == pivot {
			middle = append(middle, x)
		} else {
			right = append(right, x)
		}
	}

	result := quicksort(left)
	result = append(result, middle...)
	result = append(result, quicksort(right)...)
	return result
}

func generateList(n int, seed uint64) []int64 {
	result := make([]int64, n)
	x := seed
	for i := 0; i < n; i++ {
		x = (x*1103515245 + 12345) % (1 << 31)
		result[i] = int64(x % 10000)
	}
	return result
}

func checksum(arr []int64) int64 {
	var result int64
	for i, x := range arr {
		result = (result + x*int64(i+1)) % 1000000007
	}
	return result
}

func main() {
	arr := generateList(5000, 42)
	sorted := quicksort(arr)
	fmt.Println(checksum(sorted))
}
