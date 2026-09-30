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

range : I64 -> List I64
range = \n -> List.range({ start: At(0), end: Before(n) })

Vec : { x: F64, y: F64, z: F64 }
Sphere : { center: Vec, radius: F64, color: Vec }

add = \a, b -> { x: a.x + b.x, y: a.y + b.y, z: a.z + b.z }
sub = \a, b -> { x: a.x - b.x, y: a.y - b.y, z: a.z - b.z }
scale = \a, s -> { x: a.x * s, y: a.y * s, z: a.z * s }
dot = \a, b -> a.x * b.x + a.y * b.y + a.z * b.z
length = \a -> Num.sqrt(dot(a, a))
normalize = \a -> scale(a, 1 / length(a))
intersect = \origin, direction, sphere ->
    offset = sub(origin, sphere.center)
    b = dot(offset, direction)
    c = dot(offset, offset) - sphere.radius * sphere.radius
    d = b * b - c
    if d < 0 then Nothing else
        root = Num.sqrt(d)
        near = -b - root
        far = -b + root
        if near > 0.001 then Just(near) else if far > 0.001 then Just(far) else Nothing
spheres : List Sphere
spheres = [{ center: { x: 0, y: 0, z: 0 }, radius: 1, color: { x: 0.9, y: 0.22, z: 0.18 } }, { center: { x: -1.45, y: -0.35, z: 1.3 }, radius: 0.65, color: { x: 0.18, y: 0.72, z: 0.30 } }, { center: { x: 1.35, y: 0.15, z: 1 }, radius: 0.8, color: { x: 0.18, y: 0.38, z: 0.92 } }, { center: { x: 0, y: -101, z: 1.5 }, radius: 100, color: { x: 0.72, y: 0.70, z: 0.62 } }]
light : Vec
light = { x: -4, y: 5, z: -3 }
closest = \origin, direction, maximum ->
    state = List.walk(spheres, { limit: maximum, hit: Nothing }, \s, sphere -> when intersect(origin, direction, sphere) is
        Just(distance) -> if distance < s.limit then { limit: distance, hit: Just((distance, sphere)) } else s
        Nothing -> s)
    state.hit
trace = \origin, direction -> when closest(origin, direction, 1000000) is
    Nothing ->
        blend = 0.5 * (direction.y + 1)
        { x: 0.08 + 0.12 * blend, y: 0.10 + 0.18 * blend, z: 0.16 + 0.30 * blend }
    Just((distance, sphere)) ->
        point = add(origin, scale(direction, distance))
        normal = normalize(sub(point, sphere.center))
        surface = add(point, scale(normal, 0.001))
        toward = sub(light, surface)
        ld = normalize(toward)
        diffuse = Num.max(0, dot(normal, ld))
        intensity = when closest(surface, ld, length(toward)) is
            Just(_) -> 0.12
            Nothing -> 0.12 + 0.88 * diffuse
        scale(sphere.color, intensity)
render = \size ->
    List.walk(range(size), 0, \sum, y -> List.walk(range(size), sum, \s, x ->
        denominator = Num.to_f64(size - 1)
        direction = normalize({ x: 2 * Num.to_f64(x) / denominator - 1, y: 1 - 2 * Num.to_f64(y) / denominator, z: 1.5 })
        color = trace({ x: 0, y: 0, z: -5 }, direction)
        value = Num.floor(color.x * 1000000) * 3 + Num.floor(color.y * 1000000) * 5 + Num.floor(color.z * 1000000) * 7
        Num.rem(s + value * (y * size + x + 1), 1000000007)))
main! = \args ->
    total = List.walk(range(argument(args, 1)), 0, \s, _ -> Num.rem(s + render(argument(args, 0)), 1000000007))
    Stdout.line!(Num.to_str(total))
