package main

import "fmt"

const (
	IM = 139968
	IA = 3877
	IC = 29573
)

var last uint64 = 42

func random(max float64) float64 {
	last = (last*IA + IC) % IM
	return max * float64(last) / float64(IM)
}

var ALU = []byte("GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA")

type IUB struct {
	c byte
	p float64
}

func makeCumulative(table []IUB) {
	cp := 0.0
	for i := range table {
		cp += table[i].p
		table[i].p = cp
	}
}

func selectRandom(table []IUB, r float64) byte {
	for _, item := range table {
		if r < item.p {
			return item.c
		}
	}
	return table[len(table)-1].c
}

func repeatFasta(seq []byte, n int) int64 {
	var checksum int64
	seqLen := len(seq)
	for i := 0; i < n; i++ {
		checksum += int64(seq[i%seqLen])
	}
	return checksum
}

func randomFasta(table []IUB, n int) int64 {
	var checksum int64
	for i := 0; i < n; i++ {
		r := random(1.0)
		c := selectRandom(table, r)
		checksum += int64(c)
	}
	return checksum
}

func main() {
	n := 50000

	iub := []IUB{
		{'a', 0.27},
		{'c', 0.12},
		{'g', 0.12},
		{'t', 0.27},
		{'B', 0.02},
		{'D', 0.02},
		{'H', 0.02},
		{'K', 0.02},
		{'M', 0.02},
		{'N', 0.02},
		{'R', 0.02},
		{'S', 0.02},
		{'V', 0.02},
		{'W', 0.02},
		{'Y', 0.02},
	}
	makeCumulative(iub)

	homosapiens := []IUB{
		{'a', 0.3029549426680},
		{'c', 0.1979883004921},
		{'g', 0.1975473066391},
		{'t', 0.3015094502008},
	}
	makeCumulative(homosapiens)

	var checksum int64
	checksum += repeatFasta(ALU, n*2)
	checksum += randomFasta(iub, n*3)
	checksum += randomFasta(homosapiens, n*5)

	fmt.Println(checksum)
}
