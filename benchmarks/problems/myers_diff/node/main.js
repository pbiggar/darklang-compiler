// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
"use strict";
const args = process.argv.slice(2).map(Number);
const MOD = 1000000007;
function diff(left,right) {
  function snake(x,y) {
    while(x<left.length && y<right.length && left[x]===right[y]) {x++;y++;}
    return [x,y];
  }
  let [x,y]=snake(0,0), work=(x+1)*(y+3);
  if(x===left.length && y===right.length) return [0,work];
  let previous=new Map([[0,x]]);
  for(let d=1;d<=left.length+right.length;d++) {
    const frontier=new Map(); let reached=false;
    for(let k=-d;k<=d;k+=2) {
      const start=k===-d || (k!==d && previous.get(k-1)<previous.get(k+1)) ? previous.get(k+1) : previous.get(k-1)+1;
      [x,y]=snake(start,start-k);
      work=(work+(x+1)*(y+3)+(k+d+1)*17)%MOD;
      frontier.set(k,x); reached ||= x>=left.length && y>=right.length;
    }
    if(reached) return [d,work];
    previous=frontier;
  }
  throw new Error("unreachable diff");
}
const [blocks,insertions,runs]=args;
const unit="darklang compiler benchmark: persistent values and recursive paths.\n";
const prefix=unit.repeat(blocks), suffix=unit.repeat(blocks+1);
const left=Buffer.from(prefix+suffix), right=Buffer.from(prefix+"<changed-block>".repeat(insertions)+suffix);
let total=0;
for(let run=0;run<runs;run++) {const [d,w]=diff(left,right); total=(total+d*1000003+w)%MOD;}
console.log(total);
