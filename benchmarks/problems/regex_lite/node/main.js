// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
"use strict";
const args = process.argv.slice(2).map(Number);
const MOD = 1000000007;
const [blocks,runs]=args;
const text="darklang darkxxlang compiler42 compiler ab ab7 nope DARKlang compilerx\n".repeat(blocks);
const pattern=/dark[a-z]*lang|compiler[0-9]+|ab[0-9]?/y;
let total=0;
for(let run=0;run<runs;run++) {
  let count=0, checksum=0;
  for(let position=0;position<text.length;position++) {
    pattern.lastIndex=position;
    if(pattern.test(text)) {checksum=(checksum+(position+1)*(count+3))%MOD;count++;}
  }
  total=(total+count*1000003+checksum)%MOD;
}
console.log(total);
