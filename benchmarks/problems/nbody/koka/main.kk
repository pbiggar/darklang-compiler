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

type body
  Body(x : float64, y : float64, z : float64, vx : float64, vy : float64, vz : float64, mass : float64)
val initial = [Body(0.0,0.0,0.0,0.0,0.0,0.0,39.47841760435743),Body(4.841431442464721, -1.1603200440274284, -0.10362204447112311,0.606326392995832,2.81198684491626, -0.02521836165988763,0.03769367487038949),Body(8.34336671824458,4.124798564124305, -0.4035234171143214, -1.0107743461787924,1.8256623712304119,0.008415761376584154,0.011286326131968767),Body(12.894369562139131, -15.111151401698631, -0.22330757889265573,1.0827910064415354,0.8687130181696082, -0.010832637401363636,0.0017237240570597112),Body(15.379697114850917, -25.919314609987964,0.17925877295037118,0.979090732243898,0.5946989986476762, -0.034755955504078104,0.0020336868699246304)].vector
fun square(x : float64, y : float64, z : float64) : float64
  x*x+y*y+z*z
pub fun main()
  var bodies := initial
  var px := 0.0
  var py := 0.0
  var pz := 0.0
  bodies.foreach fn(b)
    px := px - b.vx*b.mass
    py := py - b.vy*b.mass
    pz := pz - b.vz*b.mass
  val sun = bodies[0]
  bodies[0] := Body(0.0,0.0,0.0,px / sun.mass,py / sun.mass,pz / sun.mass,sun.mass)
  for(1,argument(0)) fn(_)
    for(0,4) fn(i)
      for(i+1,4) fn(j)
        val b = bodies[i]
        val c = bodies[j]
        val dx = b.x - c.x
        val dy = b.y - c.y
        val dz = b.z - c.z
        val dist = square(dx,dy,dz).sqrt
        val mag = 0.01 / (dist*dist*dist)
        bodies[i] := Body(b.x,b.y,b.z,b.vx - dx*c.mass*mag,b.vy - dy*c.mass*mag,b.vz - dz*c.mass*mag,b.mass)
        bodies[j] := Body(c.x,c.y,c.z,c.vx+dx*b.mass*mag,c.vy+dy*b.mass*mag,c.vz+dz*b.mass*mag,c.mass)
    for(0,4) fn(i)
      val b = bodies[i]
      bodies[i] := Body(b.x+0.01*b.vx,b.y+0.01*b.vy,b.z+0.01*b.vz,b.vx,b.vy,b.vz,b.mass)
  var e := 0.0
  for(0,4) fn(i)
    val b = bodies[i]
    e := e+0.5*b.mass*square(b.vx,b.vy,b.vz)
    for(i+1,4) fn(j)
      val c = bodies[j]
      e := e - b.mass*c.mass / square(b.x - c.x,b.y - c.y,b.z - c.z).sqrt
  (e*1000000.0).truncate.int.println
