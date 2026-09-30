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

// Middle - pivot filtering variant matching the existing Rust/Python references.
fun qsort(xs : list<int>) : <div,exn> list<int>
  if xs.is-empty then []
  else
    val pivot = xs[xs.length / 2].default(0)
    qsort(xs.filter(fn(x) x<pivot)) ++ xs.filter(fn(x) x==pivot) ++ qsort(xs.filter(fn(x) x>pivot))
fun generate(n : int, seed : int, acc : list<int>) : div list<int>
  if n <= 0 then acc.reverse
  else
    val next = (seed*1103515245+12345)%2147483648
    generate(n - 1,next,Cons(next%10000,acc))
pub fun main()
  var checksum := 0
  qsort(generate(argument(0),argument(1),[])).foreach-indexed fn(i,x)
    checksum := (checksum + x*(i+1))%1000000007
  checksum.println
