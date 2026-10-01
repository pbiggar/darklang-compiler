// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import std/os/env
import std/num/float64
import std/core/vector
fun argument(i : int) : <ndet,exn> int
  match get-args()[i]
    Just(s) -> match s.parse-int
      Just(n) -> n
      Nothing -> throw("invalid argument")
    Nothing -> throw("missing argument")
val modulus = 1000000007
fun modulo(n : int) : <div,exn> int
  ((n % modulus) + modulus) % modulus
fun snake(left : vector<char>, right : vector<char>, x : int, y : int) : <div,exn> (int,int)
  if x < left.length && y < right.length && left[x] == right[y] then snake(left,right,x + 1,y + 1) else (x,y)
fun layer(left : vector<char>, right : vector<char>, d : int, previous : vector<int>, work : int) : <div,exn> int
  val offset = left.length + right.length
  var frontier := vector-init(offset * 2 + 1, fn(_) 0)
  var sum := work
  var reached := False
  for(0,d) fn(i)
    val k = (0 - d) + i * 2
    val start = if k == (0 - d) || (k != d && previous[offset + k - 1] < previous[offset + k + 1]) then previous[offset + k + 1] else previous[offset + k - 1] + 1
    val (x,y) = snake(left,right,start,start - k)
    sum := modulo(sum + (x + 1) * (y + 3) + (k + d + 1) * 17)
    frontier[offset + k] := x
    if x >= left.length && y >= right.length then reached := True
  if reached then modulo(d * 1000003 + sum) else layer(left,right,d + 1,frontier,sum)
fun diff(left : vector<char>, right : vector<char>) : <div,exn> int
  val (x,y) = snake(left,right,0,0)
  val work = (x + 1) * (y + 3)
  val offset = left.length + right.length
  val previous = vector-init(offset * 2 + 1, fn(i) if i == offset then x else 0)
  if x == left.length && y == right.length then work else layer(left,right,1,previous,work)
pub fun main()
  val unit = "darklang compiler benchmark: persistent values and recursive paths.\n"
  val before = unit.repeat(argument(0))
  val suffix = unit.repeat(argument(0) + 1)
  val left = (before ++ suffix).vector
  val right = (before ++ "<changed-block>".repeat(argument(1)) ++ suffix).vector
  var total := 0
  for(1,argument(2)) fn(_)
    total := modulo(total + diff(left,right))
  total.println
