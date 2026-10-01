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
  for(1,argument(0)) fn(_)
    s := 0.0
    for(1,argument(1)) fn(k)
      s := s + 1.0 / (k*k).float64
  (s*1000000000000.0).truncate.int.println
