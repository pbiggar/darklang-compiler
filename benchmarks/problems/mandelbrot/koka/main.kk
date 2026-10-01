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

fun inside(limit : int, cr : float64, ci : float64, i : int, zr : float64, zi : float64) : div int
  if i >= limit then 1
  elif zr*zr+zi*zi > 4.0 then 0
  else inside(limit,cr,ci,i+1,zr*zr - zi*zi+cr,2.0*zr*zi+ci)
pub fun main()
  val n = argument(0)
  val limit = argument(1)
  var count := 0
  for(0,n - 1) fn(y)
    for(0,n - 1) fn(x)
      count := count + inside(limit,2.0*x.float64 / n.float64 - 1.5,2.0*y.float64 / n.float64 - 1.0,0,0.0,0.0)
  count.println
