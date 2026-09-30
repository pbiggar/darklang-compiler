// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
"use strict";
const args = process.argv.slice(2).map(Number);
const MOD = 1000000007;
const precedence={"<":1,"+":2,"-":2,"*":3,"/":3};
function lex(source) {return source.match(/[0-9]+|[a-zA-Z]+|[^\s]/g).map(t => /^\d+$/.test(t) ? Number(t) : t);}
class Parser {
  constructor(tokens) {this.tokens=tokens;this.index=0;}
  primary() {
    const token=this.tokens[this.index++];
    if(typeof token==="number") return token;
    if(token==="x") return this.x;
    if(token==="y") return this.y;
    if(token==="(") {const value=this.expression(0);if(this.tokens[this.index++]!==")") throw new Error("expected )");return value;}
    throw new Error("invalid primary");
  }
  expression(minimum) {
    let left=this.primary();
    while(this.index<this.tokens.length) {
      const op=this.tokens[this.index], p=precedence[op]||0;
      if(!p || p<minimum) break;
      this.index++; const right=this.expression(p+1);
      switch(op) {case "+":left+=right;break;case "-":left-=right;break;case "*":left*=right;break;case "/":left=Math.trunc(left/right);break;case "<":left=Number(left<right);break;}
    }
    return left;
  }
  evaluate(iteration) {
    let result=0,index=0;
    while(this.index<this.tokens.length) {
      this.x=(iteration*17+index*13)%97+3;this.y=(iteration*29+index*7)%89+5;
      const value=this.expression(0);
      if(this.tokens[this.index++]!==";") throw new Error("expected ;");
      result=(result+value*(index+1))%MOD;index++;
    }
    return (result+MOD)%MOD;
  }
}
const [n,runs]=args;
const tokens=lex("x * x + y * 3 + (x + y) * (x - y) + x / 2;\n".repeat(n));
let total=0;
for(let i=0;i<runs;i++) total=(total+new Parser(tokens).evaluate(i))%MOD;
console.log(total);
