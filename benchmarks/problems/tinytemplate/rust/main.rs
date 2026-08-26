// TinyTemplate 1.2.1-inspired application benchmark.
//
// This dependency-free port retains the exercised compiler/interpreter path:
// scalar values, if/else blocks, and for blocks compile to an instruction tree
// and are interpreted against a small application context.

#[derive(Clone)]
enum Value { Text(String), Bool(bool), TextList(Vec<String>) }

#[derive(Clone)]
enum Instruction { Literal(String), Value(String), If(String, Vec<Instruction>, Vec<Instruction>), For(String, String, Vec<Instruction>) }

fn value<'a>(context: &'a [(String, Value)], name: &str) -> &'a Value {
    context.iter().rev().find(|(key, _)| key == name).map(|(_, value)| value).expect("valid template path")
}

fn open_brace(source: &str, start: usize) -> Option<usize> { source.as_bytes()[start..].iter().position(|byte| *byte == b'{').map(|offset| start + offset) }
fn close_tag(source: &str, start: usize, double: bool) -> usize { let end = if double { "}}" } else { "}" }; source[start..].find(end).map(|offset| start + offset).expect("valid template tag") }

fn parse(source: &str, start: usize) -> (Vec<Instruction>, usize, Option<&'static str>) {
    let mut instructions = Vec::new();
    let mut position = start;
    loop {
        let Some(open) = open_brace(source, position) else { if position < source.len() { instructions.push(Instruction::Literal(source[position..].to_owned())); } return (instructions, source.len(), None); };
        if open > position { instructions.push(Instruction::Literal(source[position..open].to_owned())); }
        let double = source.as_bytes().get(open + 1) == Some(&b'{');
        let content_start = open + if double { 2 } else { 1 };
        let close = close_tag(source, content_start, double);
        let tag = source[content_start..close].trim();
        position = close + if double { 2 } else { 1 };
        if double && (tag == "else" || tag == "endif" || tag == "endfor") { return (instructions, position, Some(if tag == "else" { "else" } else if tag == "endif" { "endif" } else { "endfor" })); }
        if double && tag.starts_with("if ") {
            let (then_body, after_then, stop) = parse(source, position);
            let (else_body, after_else) = if stop == Some("else") { let (body, after, end) = parse(source, after_then); assert_eq!(end, Some("endif")); (body, after) } else { assert_eq!(stop, Some("endif")); (Vec::new(), after_then) };
            instructions.push(Instruction::If(tag[3..].trim().to_owned(), then_body, else_body)); position = after_else;
        } else if double && tag.starts_with("for ") {
            let parts: Vec<&str> = tag.split_whitespace().collect(); assert_eq!(parts.len(), 4); assert_eq!(parts[2], "in");
            let (body, after, stop) = parse(source, position); assert_eq!(stop, Some("endfor"));
            instructions.push(Instruction::For(parts[1].to_owned(), parts[3].to_owned(), body)); position = after;
        } else { instructions.push(Instruction::Value(tag.to_owned())); }
    }
}

fn render(instructions: &[Instruction], context: &[(String, Value)], output: &mut String) {
    for instruction in instructions { match instruction {
        Instruction::Literal(text) => output.push_str(text),
        Instruction::Value(name) => match value(context, name) { Value::Text(text) => output.push_str(text), Value::Bool(enabled) => output.push_str(if *enabled { "true" } else { "false" }), Value::TextList(_) => panic!("list cannot be interpolated") },
        Instruction::If(name, then_body, else_body) => match value(context, name) { Value::Bool(true) => render(then_body, context, output), Value::Bool(false) => render(else_body, context, output), _ => panic!("if requires boolean") },
        Instruction::For(name, collection, body) => match value(context, collection) { Value::TextList(items) => for item in items { let mut scoped = context.to_vec(); scoped.push((name.clone(), Value::Text(item.clone()))); render(body, &scoped, output); }, _ => panic!("for requires text list") },
    }}
}

fn checksum(text: &str) -> i64 { text.bytes().fold(0i64, |sum, byte| (sum * 31 + byte as i64) % 1_000_000_007) }

pub fn benchmark(runs: i64) {
    let template = "Report: { title }\n{{ if enabled }}Active{{ else }}Disabled{{ endif }}\nTags: {{ for item in items }}{ item },{{ endfor }}\n";
    let context = vec![("title".to_owned(), Value::Text("Release notes".to_owned())), ("enabled".to_owned(), Value::Bool(true)), ("items".to_owned(), Value::TextList(vec!["parser".to_owned(), "renderer".to_owned(), "cache".to_owned(), "compiler".to_owned(), "runtime".to_owned()]))];
    let mut result = 0i64;
    for _ in 0..runs { let (program, _, stop) = parse(template, 0); assert_eq!(stop, None); let mut rendered = String::new(); render(&program, &context, &mut rendered); result = (result + checksum(&rendered)) % 1_000_000_007; }
    println!("{}", result);
}

#[allow(dead_code)]
fn main() { benchmark(800); }
