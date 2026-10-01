// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
"use strict";
const args = process.argv.slice(2).map(Number);
const MOD = 1000000007;
const [n,seed,runs]=args;
let state=seed;
const thresholds=[300,480,610,710,790,850,900,940];
const data=Array.from({length:n},() => {
  // BigInt preserves the exact LCG product before reduction to 31 bits.
  state=Number((BigInt(state)*1103515245n+12345n)%2147483648n);
  const symbol=thresholds.findIndex(t => state%1000<t);
  return symbol<0 ? 8+state%24 : symbol;
});
const frequencies=new Map();
for(const symbol of data) frequencies.set(symbol,(frequencies.get(symbol)||0)+1);
let queue=Array.from(frequencies,([symbol,weight]) => ({symbol,weight,minimum:symbol}));
const order=(a,b) => a.weight-b.weight || a.minimum-b.minimum;
queue.sort(order);
while(queue.length>1) {
  const left=queue.shift(), right=queue.shift();
  queue.push({weight:left.weight+right.weight,minimum:Math.min(left.minimum,right.minimum),left,right});
  queue.sort(order);
}
const tree=queue[0], codes=new Map();
function visit(node,bits,length) {
  if(node.symbol!==undefined) codes.set(node.symbol,{bits,length:Math.max(1,length)});
  else {visit(node.left,bits*2,length+1);visit(node.right,bits*2+1,length+1);}
}
visit(tree,0,0);
const checksum=values => values.reduce((sum,v,i) => (sum+v*(i+1))%MOD,0);
let total=0;
for(let run=0;run<runs;run++) {
  const encoded=[];
  for(const symbol of data) {const {bits,length}=codes.get(symbol); for(let i=length-1;i>=0;i--) encoded.push(Math.floor(bits/2**i)%2);}
  const decoded=[]; let current=tree;
  for(const bit of encoded) {
    if(tree.symbol!==undefined) {decoded.push(tree.symbol);continue;}
    current=bit===0 ? current.left : current.right;
    if(current.symbol!==undefined) {decoded.push(current.symbol);current=tree;}
  }
  if(decoded.length!==data.length || decoded.some((v,i) => v!==data[i])) throw new Error("Huffman roundtrip failed");
  total=(total+encoded.length*17+checksum(encoded)+checksum(decoded))%MOD;
}
console.log(total);
