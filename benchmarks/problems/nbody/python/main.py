#!/usr/bin/env python3
# N-Body Benchmark - Python reference implementation
# Simulates the orbits of Jovian planets using a symplectic integrator
# From: Computer Language Benchmarks Game

import math

PI = 3.141592653589793
SOLAR_MASS = 4 * PI * PI
DAYS_PER_YEAR = 365.24

# Bodies: Sun, Jupiter, Saturn, Uranus, Neptune
BODIES = {
    'sun': ([0.0, 0.0, 0.0], [0.0, 0.0, 0.0], SOLAR_MASS),
    'jupiter': (
        [4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01],
        [1.66007664274403694e-03 * DAYS_PER_YEAR, 7.69901118419740425e-03 * DAYS_PER_YEAR, -6.90460016972063023e-05 * DAYS_PER_YEAR],
        9.54791938424326609e-04 * SOLAR_MASS
    ),
    'saturn': (
        [8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01],
        [-2.76742510726862411e-03 * DAYS_PER_YEAR, 4.99852801234917238e-03 * DAYS_PER_YEAR, 2.30417297573763929e-05 * DAYS_PER_YEAR],
        2.85885980666130812e-04 * SOLAR_MASS
    ),
    'uranus': (
        [1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01],
        [2.96460137564761618e-03 * DAYS_PER_YEAR, 2.37847173959480950e-03 * DAYS_PER_YEAR, -2.96589568540237556e-05 * DAYS_PER_YEAR],
        4.36624404335156298e-05 * SOLAR_MASS
    ),
    'neptune': (
        [1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01],
        [2.68067772490389322e-03 * DAYS_PER_YEAR, 1.62824170038242295e-03 * DAYS_PER_YEAR, -9.51592254519715870e-05 * DAYS_PER_YEAR],
        5.15138902046611451e-05 * SOLAR_MASS
    ),
}

def advance(bodies, dt, n):
    for _ in range(n):
        for i, (name1, (pos1, vel1, mass1)) in enumerate(bodies.items()):
            for name2, (pos2, vel2, mass2) in list(bodies.items())[i+1:]:
                dx = pos1[0] - pos2[0]
                dy = pos1[1] - pos2[1]
                dz = pos1[2] - pos2[2]

                dist = math.sqrt(dx*dx + dy*dy + dz*dz)
                mag = dt / (dist * dist * dist)

                vel1[0] -= dx * mass2 * mag
                vel1[1] -= dy * mass2 * mag
                vel1[2] -= dz * mass2 * mag

                vel2[0] += dx * mass1 * mag
                vel2[1] += dy * mass1 * mag
                vel2[2] += dz * mass1 * mag

        for pos, vel, mass in bodies.values():
            pos[0] += dt * vel[0]
            pos[1] += dt * vel[1]
            pos[2] += dt * vel[2]

def energy(bodies):
    e = 0.0
    for i, (name1, (pos1, vel1, mass1)) in enumerate(bodies.items()):
        e += 0.5 * mass1 * (vel1[0]*vel1[0] + vel1[1]*vel1[1] + vel1[2]*vel1[2])
        for name2, (pos2, vel2, mass2) in list(bodies.items())[i+1:]:
            dx = pos1[0] - pos2[0]
            dy = pos1[1] - pos2[1]
            dz = pos1[2] - pos2[2]
            dist = math.sqrt(dx*dx + dy*dy + dz*dz)
            e -= (mass1 * mass2) / dist
    return e

def offset_momentum(bodies):
    px = py = pz = 0.0
    for pos, vel, mass in bodies.values():
        px -= vel[0] * mass
        py -= vel[1] * mass
        pz -= vel[2] * mass
    bodies['sun'][1][0] = px / SOLAR_MASS
    bodies['sun'][1][1] = py / SOLAR_MASS
    bodies['sun'][1][2] = pz / SOLAR_MASS

# Deep copy bodies for mutation
import copy
bodies = copy.deepcopy(BODIES)
offset_momentum(bodies)

# Run simulation
n = 500000
advance(bodies, 0.01, n)

# Output final energy (truncated to int for comparison)
print(int(energy(bodies) * 1000000))
