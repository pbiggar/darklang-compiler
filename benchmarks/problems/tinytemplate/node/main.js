// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
"use strict";
const args = process.argv.slice(2).map(Number);
const MOD = 1000000007;
function parse(source) {
  const tokens=[];let offset=0,trimNext=false;
  for(const match of source.matchAll(/{#.*?#}|{{.*?}}|{[^{}]*}/gs)) {
    let literal=source.slice(offset,match.index),raw=match[0];
    if(trimNext) literal=literal.trimStart();
    if(raw.startsWith("{{-")) literal=literal.trimEnd();
    if(literal) tokens.push({kind:"text",text:literal});
    trimNext=raw.endsWith("-}}");
    if(raw.startsWith("{#")) {}
    else if(raw.startsWith("{{")) tokens.push({kind:"control",text:raw.slice(2,-2).trim().replace(/^-|-$/g,"").trim()});
    else tokens.push({kind:"value",text:raw.slice(1,-1).trim()});
    offset=match.index+raw.length;
  }
  let tail=source.slice(offset);if(trimNext) tail=tail.trimStart();if(tail) tokens.push({kind:"text",text:tail});
  let index=0;
  function block(stops=[]) {
    const nodes=[];
    while(index<tokens.length) {
      const token=tokens[index];
      if(token.kind==="control" && stops.includes(token.text)) return [nodes,token.text];
      index++;
      if(token.kind!=="control") {nodes.push(token);continue;}
      const [kind,...words]=token.text.split(/\s+/);
      switch(kind) {
        case "if": {
          const [body,stop]=block(["else","endif"]);index++;let other=[];
          if(stop==="else") {other=block(["endif"])[0];index++;}
          nodes.push({kind,words,body,other});break;
        }
        case "for":case "with": {
          const body=block([kind==="for"?"endfor":"endwith"])[0];index++;
          nodes.push({kind,words,body});break;
        }
        case "call":nodes.push({kind,words});break;
        default:throw new Error("unknown directive "+kind);
      }
    }
    if(stops.length) throw new Error("unclosed directive");
    return [nodes,null];
  }
  return block()[0];
}
function lookup(path,root,scope) {
  const [head,...tail]=path.split(".");
  let value=head==="@root" ? root : Object.hasOwn(scope,head) ? scope[head] : root[head];
  for(const key of tail) value=value[key];
  if(value===undefined) throw new Error("missing path "+path);
  return value;
}
function escape(text) {return text.replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;").replaceAll('"',"&quot;").replaceAll("'","&#39;");}
class Engine {
  constructor() {this.templates=new Map();this.formatters=new Map([["unescaped",String]]);}
  add(name,text) {this.templates.set(name,parse(text));}
  render(name,root) {return this.nodes(this.templates.get(name),root,{});}
  nodes(nodes,root,scope) {
    const output=[];
    for(const node of nodes) {
      const {kind}=node;
      if(kind==="text") output.push(node.text);
      else if(kind==="value") {
        const [path,formatter]=node.text.split("|").map(s=>s.trim()),value=lookup(path,root,scope);
        output.push(formatter ? this.formatters.get(formatter)(value) : escape(String(value)));
      } else if(kind==="if") {
        const value=Boolean(lookup(node.words.at(-1),root,scope)),test=node.words[0]==="not" ? !value : value;
        output.push(this.nodes(test?node.body:node.other,root,scope));
      } else if(kind==="for") {
        const values=lookup(node.words[2],root,scope);
        values.forEach((value,i) => output.push(this.nodes(node.body,root,{...scope,[node.words[0]]:value,"@index":i,"@first":i===0,"@last":i===values.length-1})));
      } else if(kind==="with") output.push(this.nodes(node.body,root,{...scope,[node.words[2]]:lookup(node.words[0],root,scope)}));
      else if(kind==="call") output.push(this.render(node.words[0],lookup(node.words[2],root,scope)));
    }
    return output.join("");
  }
}
const PAGE="{# TinyTemplate application benchmark #}<main>\n<h1>{ title }</h1>\n{{ if not empty }}<section>{{ for row in rows -}}\n{{ call row with row }}\n{{- endfor }}</section>{{ else }}<p>No inventory.</p>{{ endif }}\n{{ call footer with footer }}\n</main>";
const ROW="<article class=\"{{ if featured }}featured{{ else }}standard{{ endif }}\">\n<h2>{ name }</h2>\n{{ with details as detail }}<p>{ detail.category }: { detail.price | currency }</p>{{ endwith }}\n<ul>{{ for tag in tags }}<li data-first=\"{ @first }\" data-last=\"{ @last }\">{ @index }:{ tag }</li>{{ endfor }}</ul>\n<div>{ raw_html | unescaped }</div>\n</article>";
const [n,runs]=args;
const engine=new Engine();engine.formatters.set("currency",value => `$${value}.00`);
engine.add("page",PAGE);engine.add("row",ROW);engine.add("footer","<footer>{ @root }</footer>");
const data={title:"Inventory <nightly>",empty:n===0,rows:Array.from({length:n},(_,i) => ({name:`Item <${i}>`,featured:i%3===0,details:{category:i%2===0?"hardware":"software",price:(i+1)*7},tags:["stable",`batch-${i%4}`,"ready & tested"],raw_html:`<span>SKU-${String(i).padStart(3,"0")}</span>`})),footer:"Generated & checked"};
let total=0;
for(let run=0;run<runs;run++) {
  let checksum=0;for(const byte of Buffer.from(engine.render("page",data))) checksum=(checksum*31+byte)%MOD;
  total=(total+checksum)%MOD;
}
console.log(total);
