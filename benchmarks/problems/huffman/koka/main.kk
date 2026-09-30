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
type tree
  Leaf(symbol : int)
  Branch(left : tree, right : tree)
type entry
  Entry(weight : int, minimum : int, tree : tree)
fun generate(n : int, seed : int) : <div,exn> vector<int>
  var state := seed
  vector-init(n, fn(_)
    state := (state * 1103515245 + 12345) % 2147483648
    val r = state % 1000
    if r < 300 then 0 elif r < 480 then 1 elif r < 610 then 2 elif r < 710 then 3 elif r < 790 then 4 elif r < 850 then 5 elif r < 900 then 6 elif r < 940 then 7 else 8 + state % 24
  )
fun insert(queue : list<entry>, entry : entry) : <div,exn> list<entry>
  match queue
    Nil -> [entry]
    Cons(x,rest) -> if entry.weight < x.weight || (entry.weight == x.weight && entry.minimum < x.minimum) then Cons(entry,queue) else Cons(x,insert(rest,entry))
fun codec(data : vector<int>) : <div,exn> tree
  var counts := vector-init(32, fn(_) 0)
  data.foreach fn(s)
    counts[s] := counts[s] + 1
  var queue := []
  for(0,31) fn(s)
    if counts[s] > 0 then queue := insert(queue,Entry(counts[s],s,Leaf(s)))
  fun combine(q : list<entry>) : <div,exn> tree
    match q
      Cons(a,Nil) -> a.tree
      Cons(a,Cons(b,rest)) -> combine(insert(rest,Entry(a.weight + b.weight,min(a.minimum,b.minimum),Branch(a.tree,b.tree))))
      _ -> throw("empty codec")
  combine(queue)
fun codes(tree : tree) : <div,exn> vector<(int,int)>
  fun walk(t : tree, bits : int, length : int, table : vector<(int,int)>) : <div,exn> vector<(int,int)>
    match t
      Leaf(s) ->
        var result := table
        result[s] := (bits,max(1,length))
        result
      Branch(a,b) -> walk(b,bits * 2 + 1,length + 1,walk(a,bits * 2,length + 1,table))
  walk(tree,0,0,vector-init(32, fn(_) (0,0)))
fun encode(data : vector<int>, table : vector<(int,int)>) : <div,exn> vector<int>
  var reversed := []
  data.foreach fn(s)
    val (bits,length) = table[s]
    for(0,length - 1) fn(i)
      reversed := Cons((bits / 2.pow(length - i - 1)) % 2,reversed)
  reversed.reverse.vector
fun decode(tree : tree, bits : vector<int>) : <div,exn> vector<int>
  match tree
    Leaf(s) -> vector-init(bits.length, fn(_) s)
    Branch(_,_) ->
      var current := tree
      var output := []
      bits.foreach fn(bit)
        val next = match current
          Branch(a,b) -> if bit == 0 then a else b
          _ -> throw("invalid codec")
        match next
          Leaf(s) ->
            output := Cons(s,output)
            current := tree
          _ -> current := next
      output.reverse.vector
fun checksum(xs : vector<int>) : <div,exn> int
  var sum := 0
  xs.foreach-indexed fn(i,v)
    sum := modulo(sum + v * (i + 1))
  sum
fun roundtrip(data : vector<int>, tree : tree, table : vector<(int,int)>) : <div,exn> int
  val encoded = encode(data,table)
  val decoded = decode(tree,encoded)
  if decoded.length != data.length || !(decoded.list.zip(data.list).all(fn(pair) pair.fst == pair.snd)) then throw("Huffman roundtrip failed")
  modulo(encoded.length * 17 + checksum(encoded) + checksum(decoded))
pub fun main()
  val data = generate(argument(0),argument(1))
  val tree = codec(data)
  val table = codes(tree)
  var total := 0
  for(1,argument(2)) fn(_)
    total := modulo(total + roundtrip(data,tree,table))
  total.println
