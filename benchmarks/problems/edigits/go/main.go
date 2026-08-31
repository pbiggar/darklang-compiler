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




func computeEDigits(numDigits int) []uint8 {
	precision := numDigits + 10
	base := uint64(10)

	eScaled := make([]uint64, precision+1)
	eScaled[0] = 1

	term := make([]uint64, precision+1)
	term[0] = 1

	for n := uint64(1); n <= 50; n++ {
		var carry uint64
		for i := 0; i <= precision; i++ {
			current := carry*base + term[i]
			term[i] = current / n
			carry = current % n
		}

		var carry2 uint64
		for i := precision; i >= 0; i-- {
			sum := eScaled[i] + term[i] + carry2
			eScaled[i] = sum % base
			carry2 = sum / base
		}
	}

	digits := make([]uint8, numDigits)
	for i := 0; i < numDigits; i++ {
		digits[i] = uint8(eScaled[i])
	}
	return digits
}

func checksumDigits(digits []uint8) int64 {
	var checksum int64
	for i, d := range digits {
		checksum += int64(d) * int64(i+1)
		checksum %= 1000000000
	}
	return checksum
}

func main() {
	var digits []byte
	for i := int64(0); i < argument(0); i++ {
		digits = computeEDigits(int(argument(1)))
	}
	checksum := checksumDigits(digits)
	fmt.Println(checksum)
}
