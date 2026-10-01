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

fun sieve(n : int) : <exn,div> int
  var flags := vector(n+1,True)
  var count := 0
  for(2,n) fn(i)
    if flags[i] then
      count := count+1
      for(2,n / i) fn(k)
        flags[k*i] := False
  count
pub fun main()
  var result := 0
  for(1,argument(1)) fn(_)
    result := sieve(argument(0))
  result.println
