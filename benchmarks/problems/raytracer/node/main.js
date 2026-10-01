// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
"use strict";
const args = process.argv.slice(2).map(Number);
const MOD = 1000000007;
class Vec {
  constructor(x,y,z) {this.x=x;this.y=y;this.z=z;}
  add(b) {return new Vec(this.x+b.x,this.y+b.y,this.z+b.z);}
  sub(b) {return new Vec(this.x-b.x,this.y-b.y,this.z-b.z);}
  scale(s) {return new Vec(this.x*s,this.y*s,this.z*s);}
  dot(b) {return this.x*b.x+this.y*b.y+this.z*b.z;}
  length() {return Math.sqrt(this.dot(this));}
  normalize() {return this.scale(1/this.length());}
}
function intersect(origin,direction,sphere) {
  const offset=origin.sub(sphere.center), b=offset.dot(direction), c=offset.dot(offset)-sphere.radius*sphere.radius;
  const discriminant=b*b-c;
  if(discriminant<0) return null;
  const root=Math.sqrt(discriminant), near=-b-root, far=-b+root;
  return near>.001 ? near : far>.001 ? far : null;
}
function closest(origin,direction,maximum) {
  let hit=null;
  for(const sphere of spheres) {
    const distance=intersect(origin,direction,sphere);
    if(distance!==null && distance<maximum) {maximum=distance;hit={distance,sphere};}
  }
  return hit;
}
function trace(origin,direction) {
  const hit=closest(origin,direction,1e6);
  if(!hit) {const blend=.5*(direction.y+1);return new Vec(.08+.12*blend,.10+.18*blend,.16+.30*blend);}
  const point=origin.add(direction.scale(hit.distance)),normal=point.sub(hit.sphere.center).normalize(),surface=point.add(normal.scale(.001));
  const toward=light.sub(surface),ld=toward.normalize(),diffuse=Math.max(0,normal.dot(ld));
  const intensity=closest(surface,ld,toward.length()) ? .12 : .12+.88*diffuse;
  return hit.sphere.color.scale(intensity);
}
const spheres=[[0,0,0,1,.9,.22,.18],[-1.45,-.35,1.3,.65,.18,.72,.30],[1.35,.15,1,.8,.18,.38,.92],[0,-101,1.5,100,.72,.70,.62]].map(([x,y,z,r,red,green,blue]) => ({center:new Vec(x,y,z),radius:r,color:new Vec(red,green,blue)}));
const light=new Vec(-4,5,-3),camera=new Vec(0,0,-5);
const [size,runs]=args;
let total=0;
for(let run=0;run<runs;run++) for(let y=0;y<size;y++) for(let x=0;x<size;x++) {
  const direction=new Vec(2*x/(size-1)-1,1-2*y/(size-1),1.5).normalize(),color=trace(camera,direction);
  const value=Math.trunc(color.x*1e6)*3+Math.trunc(color.y*1e6)*5+Math.trunc(color.z*1e6)*7;
  total=(total+value*(y*size+x+1))%MOD;
}
console.log(total);
