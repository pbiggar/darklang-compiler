# Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
from dataclasses import dataclass
import math
import sys

@dataclass(frozen=True,slots=True)
class Vec:
    x: float
    y: float
    z: float
    def __add__(self,b): return Vec(self.x+b.x,self.y+b.y,self.z+b.z)
    def __sub__(self,b): return Vec(self.x-b.x,self.y-b.y,self.z-b.z)
    def scale(self,s): return Vec(self.x*s,self.y*s,self.z*s)
    def dot(self,b): return self.x*b.x+self.y*b.y+self.z*b.z
    def length(self): return math.sqrt(self.dot(self))
    def normalize(self): return self.scale(1/self.length())

@dataclass(frozen=True,slots=True)
class Sphere:
    center: Vec
    radius: float
    color: Vec

def intersect(origin,direction,sphere):
    offset = origin-sphere.center
    b = offset.dot(direction)
    c = offset.dot(offset)-sphere.radius*sphere.radius
    discriminant = b*b-c
    if discriminant < 0: return None
    root = math.sqrt(discriminant)
    near,far = -b-root,-b+root
    return near if near > .001 else far if far > .001 else None

def closest(origin,direction,maximum):
    hit = None
    for sphere in spheres:
        distance = intersect(origin,direction,sphere)
        if distance is not None and distance < maximum:
            maximum = distance; hit = distance,sphere
    return hit

def trace(origin,direction):
    hit = closest(origin,direction,1e6)
    if hit is None:
        blend = .5*(direction.y+1)
        return Vec(.08+.12*blend,.10+.18*blend,.16+.30*blend)
    distance,sphere = hit
    point = origin+direction.scale(distance)
    normal = (point-sphere.center).normalize()
    surface = point+normal.scale(.001)
    toward = light-surface
    light_direction = toward.normalize()
    diffuse = max(0,normal.dot(light_direction))
    intensity = .12 if closest(surface,light_direction,toward.length()) else .12+.88*diffuse
    return sphere.color.scale(intensity)

spheres = [Sphere(Vec(0,0,0),1,Vec(.9,.22,.18)),
           Sphere(Vec(-1.45,-.35,1.3),.65,Vec(.18,.72,.30)),
           Sphere(Vec(1.35,.15,1),.8,Vec(.18,.38,.92)),
           Sphere(Vec(0,-101,1.5),100,Vec(.72,.70,.62))]
light,camera = Vec(-4,5,-3),Vec(0,0,-5)
size,runs = map(int,sys.argv[1:])
total = 0
for _ in range(runs):
    for y in range(size):
        for x in range(size):
            direction = Vec(2*x/(size-1)-1,1-2*y/(size-1),1.5).normalize()
            color = trace(camera,direction)
            value = int(color.x*1e6)*3+int(color.y*1e6)*5+int(color.z*1e6)*7
            total = (total+value*(y*size+x+1)) % 1_000_000_007
print(total)
