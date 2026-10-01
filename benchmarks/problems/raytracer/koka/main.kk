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
type vec
  Vec(x : float64, y : float64, z : float64)
type sphere
  Sphere(center : vec, radius : float64, color : vec)
fun add(a : vec, b : vec) : <div,exn> vec
  Vec(a.x + b.x,a.y + b.y,a.z + b.z)
fun sub(a : vec, b : vec) : <div,exn> vec
  Vec(a.x - b.x,a.y - b.y,a.z - b.z)
fun scale(a : vec, s : float64) : <div,exn> vec
  Vec(a.x * s,a.y * s,a.z * s)
fun dot(a : vec, b : vec) : <div,exn> float64
  a.x * b.x + a.y * b.y + a.z * b.z
fun length(a : vec) : <div,exn> float64
  dot(a,a).sqrt
fun normalize(a : vec) : <div,exn> vec
  scale(a,1.0 / length(a))
fun intersect(origin : vec, direction : vec, sphere : sphere) : <div,exn> maybe<float64>
  val offset = sub(origin,sphere.center)
  val b = dot(offset,direction)
  val c = dot(offset,offset) - sphere.radius * sphere.radius
  val d = b * b - c
  if d < 0.0 then Nothing else
    val root = d.sqrt
    val near = (0.0 - b) - root
    val far = (0.0 - b) + root
    if near > 0.001 then Just(near) elif far > 0.001 then Just(far) else Nothing
val spheres = [Sphere(Vec(0.0,0.0,0.0),1.0,Vec(0.9,0.22,0.18)),Sphere(Vec(-1.45,-0.35,1.3),0.65,Vec(0.18,0.72,0.30)),Sphere(Vec(1.35,0.15,1.0),0.8,Vec(0.18,0.38,0.92)),Sphere(Vec(0.0,-101.0,1.5),100.0,Vec(0.72,0.70,0.62))]
val light = Vec(-4.0,5.0,-3.0)
fun closest(origin : vec, direction : vec, maximum : float64) : <div,exn> maybe<(float64,sphere)>
  fun search(xs : list<sphere>, limit : float64, hit : maybe<(float64,sphere)>) : <div,exn> maybe<(float64,sphere)>
    match xs
      Nil -> hit
      Cons(s,rest) -> match intersect(origin,direction,s)
        Just(d) -> if d < limit then search(rest,d,Just((d,s))) else search(rest,limit,hit)
        Nothing -> search(rest,limit,hit)
  search(spheres,maximum,Nothing)
fun trace(origin : vec, direction : vec) : <div,exn> vec
  match closest(origin,direction,1000000.0)
    Nothing ->
      val blend = 0.5 * (direction.y + 1.0)
      Vec(0.08 + 0.12 * blend,0.10 + 0.18 * blend,0.16 + 0.30 * blend)
    Just((distance,s)) ->
      val point = add(origin,scale(direction,distance))
      val normal = normalize(sub(point,s.center))
      val surface = add(point,scale(normal,0.001))
      val toward = sub(light,surface)
      val ld = normalize(toward)
      val diffuse = max(0.0,dot(normal,ld))
      val intensity = match closest(surface,ld,length(toward))
        Just(_) -> 0.12
        Nothing -> 0.12 + 0.88 * diffuse
      scale(s.color,intensity)
fun render(size : int) : <div,exn> int
  var total := 0
  for(0,size - 1) fn(py)
    for(0,size - 1) fn(px)
      val denominator = (size - 1).float64
      val direction = normalize(Vec(2.0 * px.float64 / denominator - 1.0,1.0 - 2.0 * py.float64 / denominator,1.5))
      val color = trace(Vec(0.0,0.0,-5.0),direction)
      val value = (color.x * 1000000.0).truncate.int * 3 + (color.y * 1000000.0).truncate.int * 5 + (color.z * 1000000.0).truncate.int * 7
      total := modulo(total + value * (py * size + px + 1))
  total
pub fun main()
  val size = argument(0)
  var total := 0
  for(1,argument(1)) fn(_)
    total := modulo(total + render(size))
  total.println
