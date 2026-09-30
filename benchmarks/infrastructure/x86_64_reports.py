"""Render stored QEMU snapshots without invoking compilers or measurements."""

from benchmark_baseline import TRACKS, compare_suites, track_dict

TRACK = TRACKS["x86_64-quick-qemu"]


def render_results(dark, rust) -> tuple[dict[str, object], str]:
    rust_by_name = {row.name: row for row in rust.benchmarks}
    comparable_dark = tuple(row for row in dark.benchmarks if row.name in rust_by_name)
    comparison = compare_suites(
        comparable_dark, (rust_by_name[row.name] for row in comparable_dark)
    )
    rows_json = []
    rows_markdown = []
    for dark_row in dark.benchmarks:
        rust_row = rust_by_name.get(dark_row.name)
        ratio = dark_row.instructions / rust_row.instructions if rust_row else None
        rows_json.append(
            {
                "name": dark_row.name,
                "dark": dark_row.instructions,
                "rust": rust_row.instructions if rust_row else None,
                "dark_rust_ratio": ratio,
            }
        )
        rust_text = "—" if rust_row is None else f"{rust_row.instructions:,}"
        ratio_text = "—" if ratio is None else f"{ratio:.3f}×"
        rows_markdown.append(
            f"| {dark_row.name} | {dark_row.instructions:,} | {rust_text} | {ratio_text} |"
        )
    payload = {
        "schema_version": 1,
        "suite": "dark-compiler",
        "track": track_dict(TRACK),
        "contract_sha256": dark.contract_sha256,
        "compiler": {"commit": dark.compiler.commit, "subject": dark.compiler.subject},
        "generated_at": dark.generated_at,
        "overall_dark_rust_ratio": comparison.ratio,
        "benchmarks": rows_json,
    }
    markdown = "\n".join(
        [
            "# x86_64 QEMU Benchmark Results",
            "",
            "Canonical quick-profile guest instruction counts under pinned QEMU.",
            "",
            f"**Compiler:** `{dark.compiler.commit}` - {dark.compiler.subject}",
            f"**Generated:** {dark.generated_at}",
            f"**Track:** `{TRACK.id}`",
            f"**Measurement policy:** `{TRACK.measurement_policy}`",
            f"**Overall Dark/Rust:** `{comparison.ratio:.6f}×`",
            "",
            "| Benchmark | Dark instructions | Rust instructions | Dark/Rust |",
            "| --- | ---: | ---: | ---: |",
            *rows_markdown,
            "",
        ]
    )
    return payload, markdown


