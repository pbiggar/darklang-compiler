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

pub fun main()
  var s := 0.0
  for(0,argument(0) - 1) fn(i)
    s := s + (if i%2==0 then 1.0 else -1.0) / (2*i+1).float64
  (s*4.0*100000000.0).truncate.int.println
