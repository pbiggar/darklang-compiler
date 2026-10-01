// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
"use strict";
const args = process.argv.slice(2).map(Number);
const MOD = 1000000007;
function fft(values) {
  const n = values.length;
  if (n <= 1) return values;
  const even = fft(values.filter((_,i) => i%2 === 0));
  const odd = fft(values.filter((_,i) => i%2 === 1));
  const lower = [], upper = [];
  for (let i=0; i<n/2; i++) {
    const angle = -2*Math.PI*i/n, c = Math.cos(angle), s = Math.sin(angle);
    const [r,j] = odd[i], [a,b] = even[i];
    const tr=c*r-s*j, ti=c*j+s*r;
    lower.push([a+tr,b+ti]); upper.push([a-tr,b-ti]);
  }
  return lower.concat(upper);
}
const [n,runs] = args;
const input = Array.from({length:n},(_,i) => [Math.sin(i*.017)+Math.cos(i*.031),Math.cos(i*.013)-Math.sin(i*.007)]);
let total=0;
for (let run=0;run<runs;run++) {
  fft(input).forEach(([r,j],i) => { total=(total+Math.trunc((r*3+j*5)*1e6)*(i+1))%MOD; });
}
console.log((total+MOD)%MOD);
