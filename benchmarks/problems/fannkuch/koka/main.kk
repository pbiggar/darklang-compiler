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

// Permutation rotations and prefix flips adapted from the Game-derived reference.
fun flips(p : vector<int>, count : int) : <exn,div> int
  val k = p[0]
  if k==0 then count
  else flips(vector-init(p.length,fn(i) if i<=k then p[k - i] else p[i]),count+1)
fun next(perm : vector<int>, counts : vector<int>, i : int) : <exn,div> maybe<(vector<int>,vector<int>)>
  if i>=perm.length then Nothing
  else
    val rotated = vector-init(perm.length,fn(j) if j<i then perm[j+1] elif j==i then perm[0] else perm[j])
    val c = counts[i] - 1
    val updated = vector-init(counts.length,fn(j) if j==i then (if c>0 then c else i+1) else counts[j])
    if c>0 then Just((rotated,updated)) else next(rotated,updated,i+1)
fun run(perm : vector<int>, counts : vector<int>, best : int) : <exn,div> int
  val current = max(best,flips(perm,0))
  match next(perm,counts,1)
    Nothing -> current
    Just((p,c)) -> run(p,c,current)
pub fun main()
  val n = argument(0)
  run(vector-init(n,fn(i) i),vector-init(n,fn(i) i+1),0).println
