package main

import "fmt"

func fannkuch(n int) int {
	perm := make([]int, n)
	count := make([]int, n)
	for i := 0; i < n; i++ {
		perm[i] = i
		count[i] = i
	}

	maxFlips := 0

	for {
		// Count flips for current permutation
		if perm[0] != 0 {
			p := make([]int, n)
			copy(p, perm)
			flips := 0
			for p[0] != 0 {
				k := p[0]
				// Reverse first k+1 elements
				for i, j := 0, k; i < j; i, j = i+1, j-1 {
					p[i], p[j] = p[j], p[i]
				}
				flips++
			}
			if flips > maxFlips {
				maxFlips = flips
			}
		}

		// Generate next permutation
		i := 1
		for i < n {
			// Rotate first i+1 elements left by 1
			t := perm[0]
			for j := 0; j < i; j++ {
				perm[j] = perm[j+1]
			}
			perm[i] = t
			count[i]--
			if count[i] > 0 {
				break
			}
			count[i] = i
			i++
		}
		if i >= n {
			break
		}
	}
	return maxFlips
}

func main() {
	fmt.Println(fannkuch(10))
}
