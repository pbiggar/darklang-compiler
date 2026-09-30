# Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import re
import sys

# Parse the benchmark-used TinyTemplate grammar to an AST; render with scoped values.
TOKEN = re.compile(r"{#.*?#}|{{.*?}}|{[^{}]*}", re.S)

def parse(source):
    tokens = []
    offset = 0
    trim_next = False
    for match in TOKEN.finditer(source):
        literal = source[offset:match.start()]
        if trim_next: literal = literal.lstrip()
        raw = match.group()
        if raw.startswith("{{-"): literal = literal.rstrip()
        if literal: tokens.append(("text",literal))
        trim_next = raw.endswith("-}}")
        if raw.startswith("{#"): pass
        elif raw.startswith("{{"):
            tokens.append(("control",raw[2:-2].strip().strip("-").strip()))
        else: tokens.append(("value",raw[1:-1].strip()))
        offset = match.end()
    tail = source[offset:]
    if trim_next: tail = tail.lstrip()
    if tail: tokens.append(("text",tail))
    index = 0
    def block(stops=()):
        nonlocal index
        result = []
        while index < len(tokens):
            kind,text = tokens[index]
            if kind == "control" and text in stops: return result,text
            index += 1
            if kind != "control": result.append((kind,text)); continue
            command,*args = text.split()
            if command == "if":
                body,stop = block(("else","endif")); index += 1
                other = []
                if stop == "else": other,_ = block(("endif",)); index += 1
                result.append(("if",args,body,other))
            elif command == "for":
                body,_ = block(("endfor",)); index += 1
                result.append(("for",args[0],args[2],body))
            elif command == "with":
                body,_ = block(("endwith",)); index += 1
                result.append(("with",args[0],args[2],body))
            elif command == "call": result.append(("call",args[0],args[2]))
            else: raise ValueError("unknown directive: "+command)
        if stops: raise ValueError("unclosed directive")
        return result,None
    return block()[0]

def lookup(path,root,scope):
    parts = path.split(".")
    if parts[0] == "@root": value = root
    elif parts[0] in scope: value = scope[parts[0]]
    else: value = root[parts[0]]
    for part in parts[1:]: value = value[part]
    return value

def escape(text):
    return text.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;").replace('"',"&quot;").replace("'","&#39;")

class Engine:
    def __init__(self): self.templates = {}; self.formatters = {"unescaped":str}
    def add(self,name,text): self.templates[name] = parse(text)
    def render(self,name,root): return self.nodes(self.templates[name],root,{})
    def nodes(self,nodes,root,scope):
        output = []
        for node in nodes:
            kind = node[0]
            if kind == "text": output.append(node[1])
            elif kind == "value":
                parts = [s.strip() for s in node[1].split("|")]
                value = lookup(parts[0],root,scope)
                if len(parts) == 2: text = self.formatters[parts[1]](value)
                else: text = escape(str(value).lower() if isinstance(value,bool) else str(value))
                output.append(text)
            elif kind == "if":
                args = node[1]
                value = bool(lookup(args[-1],root,scope))
                if args[0] == "not": value = not value
                output.append(self.nodes(node[2] if value else node[3],root,scope))
            elif kind == "for":
                values = lookup(node[2],root,scope)
                for i,value in enumerate(values):
                    local = dict(scope, **{node[1]:value,"@index":i,"@first":i==0,"@last":i==len(values)-1})
                    output.append(self.nodes(node[3],root,local))
            elif kind == "with":
                local = dict(scope, **{node[2]:lookup(node[1],root,scope)})
                output.append(self.nodes(node[3],root,local))
            elif kind == "call": output.append(self.render(node[1],lookup(node[2],root,scope)))
        return "".join(output)

PAGE = """{# TinyTemplate application benchmark #}<main>
<h1>{ title }</h1>
{{ if not empty }}<section>{{ for row in rows -}}
{{ call row with row }}
{{- endfor }}</section>{{ else }}<p>No inventory.</p>{{ endif }}
{{ call footer with footer }}
</main>"""
ROW = """<article class="{{ if featured }}featured{{ else }}standard{{ endif }}">
<h2>{ name }</h2>
{{ with details as detail }}<p>{ detail.category }: { detail.price | currency }</p>{{ endwith }}
<ul>{{ for tag in tags }}<li data-first="{ @first }" data-last="{ @last }">{ @index }:{ tag }</li>{{ endfor }}</ul>
<div>{ raw_html | unescaped }</div>
</article>"""

def report(n):
    return {"title":"Inventory <nightly>", "empty":n==0,
            "rows":[{"name":f"Item <{i}>","featured":i%3==0,
                     "details":{"category":"hardware" if i%2==0 else "software","price":(i+1)*7},
                     "tags":["stable",f"batch-{i%4}","ready & tested"],
                     "raw_html":f"<span>SKU-{i:03}</span>"} for i in range(n)],
            "footer":"Generated & checked"}

n,runs = map(int,sys.argv[1:])
engine = Engine()
engine.formatters["currency"] = lambda value: f"${value}.00"
engine.add("page",PAGE); engine.add("row",ROW); engine.add("footer","<footer>{ @root }</footer>")
data = report(n)
total = 0
for _ in range(runs):
    checksum = 0
    for byte in engine.render("page",data).encode(): checksum = (checksum*31+byte)%1_000_000_007
    total = (total+checksum)%1_000_000_007
print(total)
