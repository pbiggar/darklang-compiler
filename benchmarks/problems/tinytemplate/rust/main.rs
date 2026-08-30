// Full-application benchmark driver for the vendored TinyTemplate 1.2.1 crate.

use serde::Serialize;
use serde_json::Value;
use std::fmt::Write;
use tinytemplate::{error::Result, TinyTemplate};

static PAGE_TEMPLATE: &str = r#"{# TinyTemplate application benchmark #}<main>
<h1>{ title }</h1>
{{ if not empty }}<section>{{ for row in rows -}}
{{ call row with row }}
{{- endfor }}</section>{{ else }}<p>No inventory.</p>{{ endif }}
{{ call footer with footer }}
</main>"#;

static ROW_TEMPLATE: &str = r#"<article class="{{ if featured }}featured{{ else }}standard{{ endif }}">
<h2>{ name }</h2>
{{ with details as detail }}<p>{ detail.category }: { detail.price | currency }</p>{{ endwith }}
<ul>{{ for tag in tags }}<li data-first="{ @first }" data-last="{ @last }">{ @index }:{ tag }</li>{{ endfor }}</ul>
<div>{ raw_html | unescaped }</div>
</article>"#;

static FOOTER_TEMPLATE: &str = "<footer>{ @root }</footer>";

#[derive(Serialize)]
struct Details {
    category: String,
    price: i64,
}

#[derive(Serialize)]
struct Row {
    name: String,
    featured: bool,
    details: Details,
    tags: Vec<String>,
    raw_html: String,
}

#[derive(Serialize)]
struct Report {
    title: String,
    empty: bool,
    rows: Vec<Row>,
    footer: String,
}

fn currency(value: &Value, output: &mut String) -> Result<()> {
    match value {
        Value::Number(number) => {
            write!(output, "${}.00", number)?;
            Ok(())
        }
        _ => tinytemplate::format(value, output),
    }
}

fn make_report(row_count: usize) -> Report {
    let rows = (0..row_count)
        .map(|index| Row {
            name: format!("Item <{}>", index),
            featured: index % 3 == 0,
            details: Details {
                category: if index % 2 == 0 { "hardware" } else { "software" }.to_owned(),
                price: (index as i64 + 1) * 7,
            },
            tags: vec![
                "stable".to_owned(),
                format!("batch-{}", index % 4),
                "ready & tested".to_owned(),
            ],
            raw_html: format!("<span>SKU-{index:03}</span>"),
        })
        .collect();

    Report {
        title: "Inventory <nightly>".to_owned(),
        empty: row_count == 0,
        rows,
        footer: "Generated & checked".to_owned(),
    }
}

fn checksum(text: &str) -> i64 {
    text.bytes()
        .fold(0i64, |sum, byte| (sum * 31 + byte as i64) % 1_000_000_007)
}

pub fn benchmark(runs: usize, row_count: usize) {
    let mut engine = TinyTemplate::new();
    engine.add_formatter("currency", currency);
    engine.add_template("page", PAGE_TEMPLATE).unwrap();
    engine.add_template("row", ROW_TEMPLATE).unwrap();
    engine.add_template("footer", FOOTER_TEMPLATE).unwrap();
    let report = make_report(row_count);

    let mut total = 0i64;
    for _ in 0..runs {
        let rendered = engine.render("page", &report).unwrap();
        total = (total + checksum(&rendered)) % 1_000_000_007;
    }
    println!("{}", total);
}

#[allow(dead_code)]
fn main() {
    benchmark(1, 12);
}
