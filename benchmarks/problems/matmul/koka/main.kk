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

fun matrix(n : int, seed : int) : vector<int>
  var state := seed
  vector-init(n*n) fn(_)
    state := (state*1103515245+12345)%2147483648
    state%100
pub fun main()
  val n = argument(0)
  val a = matrix(n,42)
  val b = matrix(n,123)
  val c = vector-init(n*n) fn(index)
    val i = index / n
    val j = index%n
    var s := 0
    for(0,n - 1) fn(k)
      s := s + a[i*n+k]*b[k*n+j]
    s
  var checksum := 0
  c.foreach-indexed fn(i,x)
    checksum := (checksum+x*(i+1))%1000000007
  checksum.println
