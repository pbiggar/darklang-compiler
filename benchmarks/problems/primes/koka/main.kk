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

fun prime(n : int) : bool
  if n < 2 then False
  elif n == 2 then True
  elif n%2==0 then False
  else
    var valid := True
    for(3,n.float64.sqrt.truncate.int) fn(d)
      if n%d==0 then valid := False
    valid
pub fun main()
  var count := 0
  for(2,argument(0)) fn(i)
    if prime(i) then count := count+1
  count.println
