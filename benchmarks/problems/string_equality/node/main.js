// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
"use strict";
const args = process.argv.slice(2).map(Number);
const MOD = 1000000007;
const [rounds] = args, token=process.argv[3];
const middle="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".repeat(2);
const short="ab"+token+"cd", long="prefix:"+token+":"+middle+":suffix";
const shortCases=[short,["a","b",token,"cd"].join(""),short+"x","xb"+token+"cd","ab"+token+"ce"];
const longCases=[long,["pre","fix:",token,":",middle,":suffix"].join(""),long+"!","xrefix:"+token+":"+middle+":suffix","prefix:"+token+":"+middle+":suffiy"];
let total=0;
for(let run=0;run<rounds;run++) {
  shortCases.forEach((other,i) => {if(short===other) total+=1<<i;});
  longCases.forEach((other,i) => {if(long===other) total+=32<<i;});
}
console.log(total);
