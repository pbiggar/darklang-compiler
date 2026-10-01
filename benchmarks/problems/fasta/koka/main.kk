// Parameterized reference workload; provenance in benchmarks/IMPLEMENTATIONS.md.
import std/os/env
import std/num/float64
import std/core/vector

fun argument(i : int) : <ndet,exn> int
  match get-args()[i]
    Just(s) -> match s.parse-int
      Just(n) -> n
      Nothing -> throw("invalid benchmark argument")
    Nothing -> throw("missing benchmark argument")

val alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA".list.map(fn(c) c.int).vector
fun cumulative(xs : list<float64>) : vector<float64>
  var s := 0.0
  xs.map(fn(x) { s := s+x; s }).vector
fun select(r : float64, chars : vector<int>, probs : vector<float64>) : exn int
  var index := chars.length - 1
  var found := False
  for(0,chars.length - 1) fn(i)
    if !found && r<probs[i] then
      index := i
      found := True
  chars[index]
fun random-fasta(n : int, chars : vector<int>, probs : vector<float64>, seed : int) : exn (int,int)
  var state := seed
  var checksum := 0
  for(1,n) fn(i)
    state := (state*3877+29573)%139968
    checksum := (checksum+select(state.float64 / 139968.0,chars,probs)*i)%1000000007
  (checksum,state)
pub fun main()
  val n = argument(0)
  val iub = "acgtBDHKMNRSVWY".list.map(fn(c) c.int).vector
  val human = "acgt".list.map(fn(c) c.int).vector
  val ip = cumulative([0.27,0.12,0.12,0.27]++list(1,11,fn(_) 0.02))
  val hp = cumulative([0.3029549426680,0.1979883004921,0.1975473066391,0.3015094502008])
  var c1 := 0
  for(1,2*n) fn(i)
    c1 := (c1+alu[(i - 1)%alu.length]*i)%1000000007
  val (c2,seed) = random-fasta(3*n,iub,ip,42)
  val (c3,_) = random-fasta(5*n,human,hp,seed)
  ((c1+c2+c3)%1000000007).println
