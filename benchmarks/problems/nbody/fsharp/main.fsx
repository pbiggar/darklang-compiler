// N-Body Benchmark
// From: Computer Language Benchmarks Game

let pi = System.Math.PI
let solarMass = 4.0 * pi * pi
let daysPerYear = 365.24

type Body = {
    mutable x: float; mutable y: float; mutable z: float
    mutable vx: float; mutable vy: float; mutable vz: float
    mass: float
}

let advance (bodies: Body[]) dt n =
    for _ in 1 .. n do
        let len = bodies.Length
        for i in 0 .. len - 1 do
            for j in i + 1 .. len - 1 do
                let dx = bodies.[i].x - bodies.[j].x
                let dy = bodies.[i].y - bodies.[j].y
                let dz = bodies.[i].z - bodies.[j].z

                let dist = sqrt (dx * dx + dy * dy + dz * dz)
                let mag = dt / (dist * dist * dist)

                bodies.[i].vx <- bodies.[i].vx - dx * bodies.[j].mass * mag
                bodies.[i].vy <- bodies.[i].vy - dy * bodies.[j].mass * mag
                bodies.[i].vz <- bodies.[i].vz - dz * bodies.[j].mass * mag

                bodies.[j].vx <- bodies.[j].vx + dx * bodies.[i].mass * mag
                bodies.[j].vy <- bodies.[j].vy + dy * bodies.[i].mass * mag
                bodies.[j].vz <- bodies.[j].vz + dz * bodies.[i].mass * mag

        for body in bodies do
            body.x <- body.x + dt * body.vx
            body.y <- body.y + dt * body.vy
            body.z <- body.z + dt * body.vz

let energy (bodies: Body[]) =
    let mutable e = 0.0
    let len = bodies.Length
    for i in 0 .. len - 1 do
        e <- e + 0.5 * bodies.[i].mass * (
            bodies.[i].vx * bodies.[i].vx +
            bodies.[i].vy * bodies.[i].vy +
            bodies.[i].vz * bodies.[i].vz)
        for j in i + 1 .. len - 1 do
            let dx = bodies.[i].x - bodies.[j].x
            let dy = bodies.[i].y - bodies.[j].y
            let dz = bodies.[i].z - bodies.[j].z
            let dist = sqrt (dx * dx + dy * dy + dz * dz)
            e <- e - (bodies.[i].mass * bodies.[j].mass) / dist
    e

let offsetMomentum (bodies: Body[]) =
    let mutable px = 0.0
    let mutable py = 0.0
    let mutable pz = 0.0
    for body in bodies do
        px <- px - body.vx * body.mass
        py <- py - body.vy * body.mass
        pz <- pz - body.vz * body.mass
    bodies.[0].vx <- px / solarMass
    bodies.[0].vy <- py / solarMass
    bodies.[0].vz <- pz / solarMass

let bodies = [|
    // Sun
    { x = 0.0; y = 0.0; z = 0.0; vx = 0.0; vy = 0.0; vz = 0.0; mass = solarMass }
    // Jupiter
    { x = 4.84143144246472090e+00; y = -1.16032004402742839e+00; z = -1.03622044471123109e-01
      vx = 1.66007664274403694e-03 * daysPerYear; vy = 7.69901118419740425e-03 * daysPerYear
      vz = -6.90460016972063023e-05 * daysPerYear; mass = 9.54791938424326609e-04 * solarMass }
    // Saturn
    { x = 8.34336671824457987e+00; y = 4.12479856412430479e+00; z = -4.03523417114321381e-01
      vx = -2.76742510726862411e-03 * daysPerYear; vy = 4.99852801234917238e-03 * daysPerYear
      vz = 2.30417297573763929e-05 * daysPerYear; mass = 2.85885980666130812e-04 * solarMass }
    // Uranus
    { x = 1.28943695621391310e+01; y = -1.51111514016986312e+01; z = -2.23307578892655734e-01
      vx = 2.96460137564761618e-03 * daysPerYear; vy = 2.37847173959480950e-03 * daysPerYear
      vz = -2.96589568540237556e-05 * daysPerYear; mass = 4.36624404335156298e-05 * solarMass }
    // Neptune
    { x = 1.53796971148509165e+01; y = -2.59193146099879641e+01; z = 1.79258772950371181e-01
      vx = 2.68067772490389322e-03 * daysPerYear; vy = 1.62824170038242295e-03 * daysPerYear
      vz = -9.51592254519715870e-05 * daysPerYear; mass = 5.15138902046611451e-05 * solarMass }
|]

offsetMomentum bodies
advance bodies 0.01 500000
printfn "%d" (int64 (energy bodies * 1000000.0))
