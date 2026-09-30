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

fun a(i : int, j : int) : float64
  1.0 / ((i+j)*(i+j+1) / 2+i+1).float64
fun product(n : int, v : vector<float64>, transpose : bool) : exn vector<float64>
  vector-init(n) fn(i)
    var s := 0.0
    for(0,n - 1) fn(j)
      s := s + (if transpose then a(j,i) else a(i,j))*v[j]
    s
fun atav(n : int, v : vector<float64>) : exn vector<float64>
  product(n,product(n,v,False),True)
pub fun main()
  val n = argument(0)
  var u := vector(n,1.0)
  var v := vector(n,0.0)
  for(1,argument(1)) fn(_)
    v := atav(n,u)
    u := atav(n,v)
  var vbv := 0.0
  var vv := 0.0
  for(0,n - 1) fn(i)
    vbv := vbv+u[i]*v[i]
    vv := vv+v[i]*v[i]
  ((vbv / vv).sqrt*1000000000.0).truncate.int.println
