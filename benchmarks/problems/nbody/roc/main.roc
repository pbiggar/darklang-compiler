# Parameterized reference workload; provenance in benchmarks/IMPLEMENTATIONS.md.
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br" }
import pf.Arg exposing [Arg]
import pf.Stdout


argument : List Arg, U64 -> I64
argument = \args, index ->
    when List.get(args, index + 1) is
        Ok(arg) ->
            when Str.to_i64(Arg.display(arg)) is
                Ok(n) -> n
                Err(_) -> crash("invalid benchmark argument")
        Err(_) -> crash("missing benchmark argument")

at : List a, U64 -> a
at = \xs, index ->
    when List.get(xs, index) is
        Ok(x) -> x
        Err(_) -> crash("benchmark index out of bounds")

range : I64 -> List I64
range = \n -> List.range({ start: At(0), end: Before(n) })

Body : { x: F64, y: F64, z: F64, vx: F64, vy: F64, vz: F64, mass: F64 }
initial : List Body
initial = [{ x: 0.0, y: 0.0, z: 0.0, vx: 0.0, vy: 0.0, vz: 0.0, mass: 39.47841760435743 }, { x: 4.841431442464721, y: -1.1603200440274284, z: -0.10362204447112311, vx: 0.606326392995832, vy: 2.81198684491626, vz: -0.02521836165988763, mass: 0.03769367487038949 }, { x: 8.34336671824458, y: 4.124798564124305, z: -0.4035234171143214, vx: -1.0107743461787924, vy: 1.8256623712304119, vz: 0.008415761376584154, mass: 0.011286326131968767 }, { x: 12.894369562139131, y: -15.111151401698631, z: -0.22330757889265573, vx: 1.0827910064415354, vy: 0.8687130181696082, vz: -0.010832637401363636, mass: 0.0017237240570597112 }, { x: 15.379697114850917, y: -25.919314609987964, z: 0.17925877295037118, vx: 0.979090732243898, vy: 0.5946989986476762, vz: -0.034755955504078104, mass: 0.0020336868699246304 }]
square = \x, y, z -> x * x + y * y + z * z
advance : List Body -> List Body
advance = \bodies ->
    velocities = List.walk(range(5), bodies, \bs, ii ->
        i = Num.to_u64(ii)
        List.walk(range(4 - ii), bs, \cs, jj ->
            j = Num.to_u64(ii + jj + 1)
            b = at(cs, i)
            c = at(cs, j)
            dx = b.x - c.x
            dy = b.y - c.y
            dz = b.z - c.z
            dist = Num.sqrt(square(dx, dy, dz))
            mag = 0.01 / (dist * dist * dist)
            first = { b & vx: b.vx - dx * c.mass * mag, vy: b.vy - dy * c.mass * mag, vz: b.vz - dz * c.mass * mag }
            second = { c & vx: c.vx + dx * b.mass * mag, vy: c.vy + dy * b.mass * mag, vz: c.vz + dz * b.mass * mag }
            List.set(List.set(cs, i, first), j, second)))
    List.map(velocities, \b -> { b & x: b.x + 0.01 * b.vx, y: b.y + 0.01 * b.vy, z: b.z + 0.01 * b.vz })
energy : List Body -> F64
energy = \bodies -> List.walk(range(5), 0, \s, ii ->
    b = at(bodies, Num.to_u64(ii))
    kinetic = s + 0.5 * b.mass * square(b.vx, b.vy, b.vz)
    List.walk(range(4 - ii), kinetic, \e, jj ->
        c = at(bodies, Num.to_u64(ii + jj + 1))
        e - b.mass * c.mass / Num.sqrt(square(b.x - c.x, b.y - c.y, b.z - c.z))))
solve : I64 -> I64
solve = \n ->
    momentum = List.walk(initial, { x: 0, y: 0, z: 0 }, \p, b -> { x: p.x - b.vx * b.mass, y: p.y - b.vy * b.mass, z: p.z - b.vz * b.mass })
    sun = at(initial, 0)
    offset = List.set(initial, 0, { sun & vx: momentum.x / sun.mass, vy: momentum.y / sun.mass, vz: momentum.z / sun.mass })
    final = List.walk(range(n), offset, \bs, _ -> advance(bs))
    Num.ceiling(energy(final) * 1000000)

main! = \args ->
    result = solve(argument(args, 0))
    Stdout.line!(Num.to_str(result))
