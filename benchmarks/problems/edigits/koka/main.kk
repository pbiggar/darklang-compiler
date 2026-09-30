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

fun divide(term : vector<int>, k : int) : exn vector<int>
  var carry := 0
  vector-init(term.length) fn(i)
    val current = carry*10+term[i]
    carry := current%k
    current / k
fun add(total : vector<int>, term : vector<int>) : exn vector<int>
  var output := total
  var carry := 0
  for(0,total.length - 1) fn(j)
    val i = total.length - 1 - j
    val current = total[i]+term[i]+carry
    output[i] := current%10
    carry := current / 10
  output
fun checksum(n : int) : exn int
  val initial = vector-init(n+11,fn(i) if i==0 then 1 else 0)
  var term := initial
  var total := initial
  for(1,50) fn(k)
    term := divide(term,k)
    total := add(total,term)
  var s := 0
  for(0,n - 1) fn(i)
    s := (s+total[i]*(i+1))%1000000007
  s
pub fun main()
  var result := 0
  for(1,argument(0)) fn(_)
    result := checksum(argument(1))
  result.println
