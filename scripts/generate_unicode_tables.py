#!/usr/bin/env python3
"""Generate the compiler's deterministic Unicode 17 text tables.

The generator downloads the pinned UCD inputs into a local cache, verifies and
records their SHA-256 digests, and emits compressed Dark data.  No system UCD,
Python ``unicodedata`` database, or locale data participates in generation.

Use ``--check`` in CI to prove that the checked-in output is reproducible.
"""

from __future__ import annotations

import argparse
import hashlib
import os
from pathlib import Path
import sys
import urllib.request


UNICODE_VERSION = "17.0.0"
UCD_ROOT = f"https://www.unicode.org/Public/{UNICODE_VERSION}/ucd"
SOURCES = {
    "UnicodeData.txt": f"{UCD_ROOT}/UnicodeData.txt",
    "SpecialCasing.txt": f"{UCD_ROOT}/SpecialCasing.txt",
    "CompositionExclusions.txt": f"{UCD_ROOT}/CompositionExclusions.txt",
    "GraphemeBreakProperty.txt": f"{UCD_ROOT}/auxiliary/GraphemeBreakProperty.txt",
    "DerivedCoreProperties.txt": f"{UCD_ROOT}/DerivedCoreProperties.txt",
    "emoji-data.txt": f"{UCD_ROOT}/emoji/emoji-data.txt",
    "PropList.txt": f"{UCD_ROOT}/PropList.txt",
}

SCRIPT_DIR = Path(__file__).resolve().parent
CACHE_DIR = SCRIPT_DIR / ".unicode_cache" / UNICODE_VERSION
OUTPUT = SCRIPT_DIR.parent / "src" / "DarkCompiler" / "unicode_data.dark"
SHARD_DIR = SCRIPT_DIR.parent / "src" / "DarkCompiler" / "unicode_data"
SHARD_COUNT = 64
INDEX_SHARD_DIR = SCRIPT_DIR.parent / "src" / "DarkCompiler" / "unicode_data_index"
INDEX_SHARD_COUNT = 8

GRAPHEME_CASES = [
    "GBOther", "GBCR", "GBLF", "GBControl", "GBExtend", "GBZWJ",
    "GBRegionalIndicator", "GBPrepend", "GBSpacingMark", "GBL", "GBV", "GBT", "GBLV", "GBLVT",
]
GRAPHEME_NAMES = {
    "Other": "GBOther",
    "CR": "GBCR",
    "LF": "GBLF",
    "Control": "GBControl",
    "Extend": "GBExtend",
    "ZWJ": "GBZWJ",
    "Regional_Indicator": "GBRegionalIndicator",
    "Prepend": "GBPrepend",
    "SpacingMark": "GBSpacingMark",
    "L": "GBL",
    "V": "GBV",
    "T": "GBT",
    "LV": "GBLV",
    "LVT": "GBLVT",
}
GENERAL_CATEGORIES = [
    "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No",
    "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po", "Sm", "Sc", "Sk", "So",
    "Zs", "Zl", "Zp", "Cc", "Cf", "Cs", "Co", "Cn",
]
INCB_CASES = {"Consonant": "Consonant", "Extend": "InCBExtend", "Linker": "Linker"}

COMPACT_LOOKUP_SOURCE = [
    "let __hexDigit(byte: Int64) : Int64 =",
    "    if byte >= 48 && byte <= 57 then byte - 48",
    "    else if byte >= 65 && byte <= 70 then byte - 55",
    '    else Builtin.testRuntimeError("Invalid generated Unicode data")',
    "",
    "let __hex(data: String, index: Int64, remaining: Int64, acc: Int64) : Int64 =",
    "    if remaining == 0 then acc",
    "    else Unicode.Data.__hex(data, index + 1, remaining - 1, acc * 16 + Unicode.Data.__hexDigit(Stdlib.String.getByteAt(data, index)))",
    "",
    "let __lookupMappingValueIn(data: String, index: Int64, key: Int64, keyWidth: Int64, recordWidth: Int64, valueIndex: Int64) : Stdlib.Option.Option<Int64> =",
    "    if index >= Stdlib.String.__byteLength(data) then None",
    "    else",
    "        let found = Unicode.Data.__hex(data, index, keyWidth, 0) in",
    "        if found == key then",
    "            let length = Unicode.Data.__hex(data, index + keyWidth, 1, 0) in",
    "            if valueIndex < length then Some(Unicode.Data.__hex(data, index + keyWidth + 1 + valueIndex * 6, 6, 0)) else None",
    "        else if found > key then None",
    "        else Unicode.Data.__lookupMappingValueIn(data, index + recordWidth, key, keyWidth, recordWidth, valueIndex)",
    "",
    "let __lookupRangeIn(data: String, index: Int64, codepoint: Int64, recordWidth: Int64) : Stdlib.Option.Option<Int64> =",
    "    if index >= Stdlib.String.__byteLength(data) then None",
    "    else",
    "        let start_ = Unicode.Data.__hex(data, index, 6, 0) in",
    "        let end_ = Unicode.Data.__hex(data, index + 6, 6, 0) in",
    "        if codepoint < start_ then None",
    "        else if codepoint <= end_ then Some(Unicode.Data.__hex(data, index + 12, 2, 0))",
    "        else Unicode.Data.__lookupRangeIn(data, index + recordWidth, codepoint, recordWidth)",
    "",
    "let __graphemeFromCode(value: Int64) : Unicode.Data.GraphemeBreak =",
    "    if value == 1 then Unicode.Data.GraphemeBreak.GBCR",
    "    else if value == 2 then Unicode.Data.GraphemeBreak.GBLF",
    "    else if value == 3 then Unicode.Data.GraphemeBreak.GBControl",
    "    else if value == 4 then Unicode.Data.GraphemeBreak.GBExtend",
    "    else if value == 5 then Unicode.Data.GraphemeBreak.GBZWJ",
    "    else if value == 6 then Unicode.Data.GraphemeBreak.GBRegionalIndicator",
    "    else if value == 7 then Unicode.Data.GraphemeBreak.GBPrepend",
    "    else if value == 8 then Unicode.Data.GraphemeBreak.GBSpacingMark",
    "    else if value == 9 then Unicode.Data.GraphemeBreak.GBL",
    "    else if value == 10 then Unicode.Data.GraphemeBreak.GBV",
    "    else if value == 11 then Unicode.Data.GraphemeBreak.GBT",
    "    else if value == 12 then Unicode.Data.GraphemeBreak.GBLV",
    "    else if value == 13 then Unicode.Data.GraphemeBreak.GBLVT",
    "    else Unicode.Data.GraphemeBreak.GBOther",
    "",
    "let __indicFromCode(value: Int64) : Unicode.Data.IndicConjunctBreak =",
    "    if value == 1 then Unicode.Data.IndicConjunctBreak.InCBConsonant",
    "    else if value == 2 then Unicode.Data.IndicConjunctBreak.InCBExtend",
    "    else if value == 3 then Unicode.Data.IndicConjunctBreak.InCBLinker",
    "    else Unicode.Data.IndicConjunctBreak.InCBNone",
    "",
    "let __categoryFromCode(value: Int64) : Unicode.Data.GeneralCategory =",
] + [
    f"    {'if' if index == 0 else 'else if'} value == {index} then Unicode.Data.GeneralCategory.{name}"
    for index, name in enumerate(GENERAL_CATEGORIES[:-1])
] + [
    "    else Unicode.Data.GeneralCategory.Cn",
    "",
]


def digest(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def obtain(name: str, url: str) -> bytes:
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    path = CACHE_DIR / name
    if path.exists():
        return path.read_bytes()
    print(f"Downloading {url}")
    with urllib.request.urlopen(url) as response:
        content = response.read()
    path.write_bytes(content)
    return content


def useful_lines(content: str):
    for original in content.splitlines():
        body = original.split("#", 1)[0].strip()
        if body:
            yield body


def codepoint_range(text: str) -> tuple[int, int]:
    if ".." in text:
        start, end = text.split("..", 1)
        return int(start, 16), int(end, 16)
    value = int(text, 16)
    return value, value


def parse_property(content: str, selected: set[str] | None = None):
    result: list[tuple[int, int, str]] = []
    for line in useful_lines(content):
        left, prop = (part.strip() for part in line.split(";", 1))
        prop = prop.split()[0]
        if selected is None or prop in selected:
            start, end = codepoint_range(left)
            result.append((start, end, prop))
    return result


def compress(values: list[tuple[int, int, str]]) -> list[tuple[int, int, str]]:
    ordered = sorted(values)
    result: list[tuple[int, int, str]] = []
    for start, end, value in ordered:
        if result and result[-1][2] == value and result[-1][1] + 1 == start:
            previous = result[-1]
            result[-1] = (previous[0], end, value)
        else:
            result.append((start, end, value))
    return result


def parse_unicode_data(content: str):
    rows: list[tuple[int, list[str]]] = []
    pending: tuple[int, list[str]] | None = None
    categories: list[tuple[int, int, str]] = []
    combining: list[tuple[int, int, str]] = []
    decomposition: dict[int, list[int]] = {}
    upper: dict[int, list[int]] = {}
    lower: dict[int, list[int]] = {}

    for line in useful_lines(content):
        fields = line.split(";")
        cp = int(fields[0], 16)
        name = fields[1]
        if name.endswith(", First>"):
            pending = (cp, fields)
            continue
        if name.endswith(", Last>"):
            if pending is None:
                raise ValueError(f"range end without start at U+{cp:04X}")
            start, first_fields = pending
            categories.append((start, cp, first_fields[2]))
            ccc = int(first_fields[3])
            if ccc:
                combining.append((start, cp, str(ccc)))
            pending = None
            continue

        rows.append((cp, fields))
        categories.append((cp, cp, fields[2]))
        ccc = int(fields[3])
        if ccc:
            combining.append((cp, cp, str(ccc)))
        raw_decomposition = fields[5].strip()
        if raw_decomposition and not raw_decomposition.startswith("<"):
            decomposition[cp] = [int(value, 16) for value in raw_decomposition.split()]
        if fields[12]:
            upper[cp] = [int(fields[12], 16)]
        if fields[13]:
            lower[cp] = [int(fields[13], 16)]

    if pending is not None:
        raise ValueError("unterminated UnicodeData range")
    return compress(categories), compress(combining), decomposition, upper, lower


def parse_special_casing(content: str):
    unconditional_upper: dict[int, list[int]] = {}
    unconditional_lower: dict[int, list[int]] = {}
    contextual: list[tuple[int, list[int], list[int], list[int], str]] = []
    for line in useful_lines(content):
        fields = [field.strip() for field in line.split(";")]
        cp = int(fields[0], 16)
        lower = [int(value, 16) for value in fields[1].split()]
        title = [int(value, 16) for value in fields[2].split()]
        upper = [int(value, 16) for value in fields[3].split()]
        condition = fields[4] if len(fields) > 4 else ""
        if condition:
            contextual.append((cp, lower, title, upper, condition))
        else:
            if lower != [cp]:
                unconditional_lower[cp] = lower
            if upper != [cp]:
                unconditional_upper[cp] = upper
    return unconditional_upper, unconditional_lower, contextual


def parse_exclusions(content: str) -> set[int]:
    result: set[int] = set()
    for line in useful_lines(content):
        start, end = codepoint_range(line)
        result.update(range(start, end + 1))
    return result


def encoded_chunks(records: list[str], records_per_chunk: int = 128) -> list[str]:
    return [
        "".join(records[index:index + records_per_chunk])
        for index in range(0, len(records), records_per_chunk)
    ]


def encode_mapping(entries: dict[int, list[int]], key_width: int) -> tuple[list[str], int, int]:
    slots = max(len(values) for values in entries.values())
    if slots > 15:
        raise ValueError("mapping length does not fit one hexadecimal digit")
    records = [
        f"{key:0{key_width}X}{len(values):X}"
        + "".join(f"{value:06X}" for value in values)
        + "000000" * (slots - len(values))
        for key, values in sorted(entries.items())
    ]
    return encoded_chunks(records), key_width + 1 + 6 * slots, slots


def encode_ranges(
    entries: list[tuple[int, int, object]], values: dict[object, int]
) -> tuple[list[str], int]:
    records = [
        f"{start:06X}{end:06X}{values[value]:02X}"
        for start, end, value in entries
    ]
    return encoded_chunks(records), 14


def emit_string_data(name: str, chunks: list[str]) -> list[str]:
    result = [f"let __{name}Data() : List<String> =", "    ["]
    result.extend(
        f'        "{chunk}"{"," if index + 1 < len(chunks) else ""}'
        for index, chunk in enumerate(chunks)
    )
    return result + ["    ]", ""]


def generate_compact_data(
    source_hashes,
    counts,
    decomposition,
    composition,
    combining,
    grapheme,
    pictographic,
    incb,
    cased,
    case_ignorable,
    upper,
    lower,
    contextual,
    categories,
    whitespace,
):
    decomp_data, decomp_width, decomp_slots = encode_mapping(decomposition, 6)
    compose_data, compose_width, compose_slots = encode_mapping(composition, 12)
    upper_data, upper_width, upper_slots = encode_mapping(upper, 6)
    lower_data, lower_width, lower_slots = encode_mapping(lower, 6)
    combining_data, range_width = encode_ranges(combining, {str(i): i for i in range(256)})
    grapheme_values = {name: index for index, name in enumerate(GRAPHEME_CASES)}
    grapheme_data, _ = encode_ranges(
        grapheme, {name: grapheme_values[GRAPHEME_NAMES[name]] for name in GRAPHEME_NAMES}
    )
    pictographic_data, _ = encode_ranges(
        [(start, end, True) for start, end in pictographic], {True: 1}
    )
    incb_data, _ = encode_ranges(
        incb,
        {
            "Consonant": 1, "Extend": 2, "Linker": 3,
            "InCB=Consonant": 1, "InCB=Extend": 2, "InCB=Linker": 3,
        },
    )
    cased_data, _ = encode_ranges(
        [(start, end, True) for start, end, _ in cased], {True: 1}
    )
    case_ignorable_data, _ = encode_ranges(
        [(start, end, True) for start, end, _ in case_ignorable], {True: 1}
    )
    category_data, _ = encode_ranges(
        categories, {name: index for index, name in enumerate(GENERAL_CATEGORIES)}
    )
    whitespace_data, _ = encode_ranges(
        [(start, end, True) for start, end in whitespace], {True: 1}
    )

    lines = [
        "// Unicode.Data - generated Unicode text data for the native compiler.",
        "//",
        f"// Unicode version: {UNICODE_VERSION}",
        "// Fixed-width hexadecimal records keep the complete tables compact.",
        "// DO NOT EDIT. Regenerate with scripts/generate_unicode_tables.py.",
        "",
        "module Unicode.Data",
        "",
        "type GraphemeBreak = " + " | ".join(GRAPHEME_CASES),
        "type IndicConjunctBreak = InCBNone | InCBConsonant | InCBExtend | InCBLinker",
        "type GeneralCategory = " + " | ".join(GENERAL_CATEGORIES),
        "",
        f'let unicodeVersion() : String = "{UNICODE_VERSION}"',
        "",
        "let sourceHashes() : List<(String, String)> =",
        "    [",
    ]
    lines.extend(
        f'        ("{name}", "{sha}"){"," if i + 1 < len(source_hashes) else ""}'
        for i, (name, sha) in enumerate(source_hashes)
    )
    lines.extend(["    ]", "", "let tableCounts() : List<(String, Int64)> =", "    ["])
    count_items = sorted(counts.items())
    lines.extend(
        f'        ("{name}", {count}){"," if i + 1 < len(count_items) else ""}'
        for i, (name, count) in enumerate(count_items)
    )
    lines.extend(["    ]", ""])
    data_tables = [
        ("canonicalDecomposition", decomp_data),
        ("canonicalComposition", compose_data),
        ("combiningClass", combining_data),
        ("graphemeBreak", grapheme_data),
        ("extendedPictographic", pictographic_data),
        ("indicConjunctBreak", incb_data),
        ("cased", cased_data),
        ("caseIgnorable", case_ignorable_data),
        ("uppercase", upper_data),
        ("lowercase", lower_data),
        ("generalCategory", category_data),
        ("whiteSpace", whitespace_data),
    ]
    shard_lines = [
        [
            "// Unicode.Data generated compact table shard.",
            "",
            f"module Unicode.Data.Table{index:02d}",
            "",
        ]
        for index in range(SHARD_COUNT)
    ]
    next_shard = 0
    table_references: dict[str, list[str]] = {}
    for name, data in data_tables:
        references: list[str] = []
        for chunk_index, chunk in enumerate(data):
            shard_index = next_shard % SHARD_COUNT
            next_shard += 1
            function_name = f"__{name}Chunk{chunk_index}"
            shard_lines[shard_index].extend([
                f"let {function_name}() : String = \"{chunk}\"",
                "",
            ])
            references.append(
                f"Unicode.Data.Table{shard_index:02d}.{function_name}()"
            )
        table_references[name] = references
    shards = ["\n".join(shard) for shard in shard_lines]

    lines.extend([
        "let contextualCasing() : List<(Int64, List<Int64>, List<Int64>, List<Int64>, String)> =",
        "    [",
    ])
    lines.extend(
        f'        ({cp}, {dark_list(lo)}, {dark_list(title)}, {dark_list(up)}, "{condition}"){"," if i + 1 < len(contextual) else ""}'
        for i, (cp, lo, title, up, condition) in enumerate(contextual)
    )
    lines.extend(["    ]", ""])
    lines.extend(COMPACT_LOOKUP_SOURCE)

    def emit_option_chain(calls: list[str], indent: str) -> list[str]:
        first, *rest = calls
        result = [
            f"{indent}match {first} with",
            f"{indent}| Some(value) -> Some(value)",
            f"{indent}| None ->",
        ]
        if rest:
            return result + emit_option_chain(rest, indent + "    ")
        return result + [f"{indent}    None"]

    def emit_direct_lookup(
        name: str,
        references: list[str],
        call_for_reference,
        parameters: str,
        arguments: str,
    ) -> None:
        group_names: list[str] = []
        for group_index in range(0, len(references), 4):
            group_name = f"__{name}LookupGroup{group_index // 4}"
            group_names.append(group_name)
            lines.extend([
                f"let {group_name}({parameters}) : Stdlib.Option.Option<Int64> =",
                *emit_option_chain(
                    [call_for_reference(reference) for reference in references[group_index:group_index + 4]],
                    "    ",
                ),
                "",
            ])
        lines.extend([
            f"let __{name}Lookup({parameters}) : Stdlib.Option.Option<Int64> =",
            *emit_option_chain(
                [f"Unicode.Data.{group_name}({arguments})" for group_name in group_names],
                "    ",
            ),
            "",
        ])

    mapping_specs = {
        "canonicalDecomposition": (6, decomp_width),
        "canonicalComposition": (12, compose_width),
        "uppercase": (6, upper_width),
        "lowercase": (6, lower_width),
    }
    for name, (key_width, record_width) in mapping_specs.items():
        emit_direct_lookup(
            name,
            table_references[name],
            lambda reference, key_width=key_width, record_width=record_width:
                f"Unicode.Data.__lookupMappingValueIn({reference}, 0, key, {key_width}, {record_width}, valueIndex)",
            "key: Int64, valueIndex: Int64",
            "key, valueIndex",
        )

    for name in [
        "combiningClass", "graphemeBreak", "extendedPictographic",
        "indicConjunctBreak", "cased", "caseIgnorable",
        "generalCategory", "whiteSpace",
    ]:
        emit_direct_lookup(
            name,
            table_references[name],
            lambda reference:
                f"Unicode.Data.__lookupRangeIn({reference}, 0, codepoint, 14)",
            "codepoint: Int64",
            "codepoint",
        )

    lines.extend([
        "let lookupCanonicalDecompositionAt(codepoint: Int64, index: Int64) : Stdlib.Option.Option<Int64> = Unicode.Data.__canonicalDecompositionLookup(codepoint, index)",
        "let lookupCanonicalComposition(key: Int64) : Stdlib.Option.Option<Int64> = Unicode.Data.__canonicalCompositionLookup(key, 0)",
        "let lookupCombiningClass(codepoint: Int64) : Int64 = Stdlib.Option.withDefault<Int64>(Unicode.Data.__combiningClassLookup(codepoint), 0)",
        "let lookupGraphemeBreak(codepoint: Int64) : Unicode.Data.GraphemeBreak = Unicode.Data.__graphemeFromCode(Stdlib.Option.withDefault<Int64>(Unicode.Data.__graphemeBreakLookup(codepoint), 0))",
        "let isExtendedPictographic(codepoint: Int64) : Bool = Stdlib.Option.isSome<Int64>(Unicode.Data.__extendedPictographicLookup(codepoint))",
        "let lookupIndicConjunctBreak(codepoint: Int64) : Unicode.Data.IndicConjunctBreak = Unicode.Data.__indicFromCode(Stdlib.Option.withDefault<Int64>(Unicode.Data.__indicConjunctBreakLookup(codepoint), 0))",
        "let isCased(codepoint: Int64) : Bool = Stdlib.Option.isSome<Int64>(Unicode.Data.__casedLookup(codepoint))",
        "let isCaseIgnorable(codepoint: Int64) : Bool = Stdlib.Option.isSome<Int64>(Unicode.Data.__caseIgnorableLookup(codepoint))",
        "let lookupUppercaseAt(codepoint: Int64, index: Int64) : Stdlib.Option.Option<Int64> = Unicode.Data.__uppercaseLookup(codepoint, index)",
        "let lookupLowercaseAt(codepoint: Int64, index: Int64) : Stdlib.Option.Option<Int64> = Unicode.Data.__lowercaseLookup(codepoint, index)",
        "let lookupGeneralCategory(codepoint: Int64) : Unicode.Data.GeneralCategory = Unicode.Data.__categoryFromCode(Stdlib.Option.withDefault<Int64>(Unicode.Data.__generalCategoryLookup(codepoint), 29))",
        "let isWhiteSpace(codepoint: Int64) : Bool = Stdlib.Option.isSome<Int64>(Unicode.Data.__whiteSpaceLookup(codepoint))",
        "",
    ])
    declaration_starts = [
        index for index, line in enumerate(lines)
        if line.startswith("type ") or line.startswith("let ")
    ]
    declaration_starts.append(len(lines))
    declarations = [
        lines[start:end]
        for start, end in zip(declaration_starts, declaration_starts[1:])
    ]
    index_bins: list[list[list[str]]] = [[] for _ in range(INDEX_SHARD_COUNT)]
    index_sizes = [0 for _ in range(INDEX_SHARD_COUNT)]
    # The Dark lexer is intentionally simple and recursive. Keep even the
    # generated index code in small deterministic files so loading complete
    # Unicode data does not depend on the host thread's stack size.
    for declaration in sorted(
        declarations,
        key=lambda item: -sum(len(line) + 1 for line in item),
    ):
        bin_index = min(range(INDEX_SHARD_COUNT), key=lambda index: index_sizes[index])
        index_bins[bin_index].append(declaration)
        index_sizes[bin_index] += sum(len(line) + 1 for line in declaration)

    index_outputs: list[str] = []
    for index, declarations_for_file in enumerate(index_bins):
        header = [
            "// Unicode.Data generated lookup index shard.",
            "// DO NOT EDIT. Regenerate with scripts/generate_unicode_tables.py.",
            "",
            "module Unicode.Data",
            "",
        ]
        body = [line for declaration in declarations_for_file for line in declaration]
        index_outputs.append("\n".join(header + body))

    return index_outputs[0], index_outputs[1:], shards


def dark_list(values: list[int]) -> str:
    return "[" + ", ".join(str(value) for value in values) + "]"


def generate(contents: dict[str, bytes]) -> tuple[str, list[str], list[str]]:
    decoded = {name: value.decode("utf-8") for name, value in contents.items()}
    categories, combining, decomposition, upper, lower = parse_unicode_data(decoded["UnicodeData.txt"])
    special_upper, special_lower, contextual = parse_special_casing(decoded["SpecialCasing.txt"])
    upper.update(special_upper)
    lower.update(special_lower)
    exclusions = parse_exclusions(decoded["CompositionExclusions.txt"])

    composition: dict[int, list[int]] = {}
    for composed, parts in decomposition.items():
        if len(parts) == 2 and composed not in exclusions:
            composition[parts[0] * 0x110000 + parts[1]] = [composed]

    grapheme = compress(parse_property(decoded["GraphemeBreakProperty.txt"]))
    emoji = parse_property(decoded["emoji-data.txt"], {"Extended_Pictographic"})
    pictographic = [(start, end) for start, end, _ in emoji]
    derived = parse_property(
        decoded["DerivedCoreProperties.txt"],
        {"InCB=Consonant", "InCB=Extend", "InCB=Linker"},
    )
    # UCD property files spell these as "InCB ; Consonant" in some releases.
    if not derived:
        derived = []
        for line in useful_lines(decoded["DerivedCoreProperties.txt"]):
            fields = [part.strip() for part in line.split(";")]
            if len(fields) >= 3 and fields[1] == "InCB" and fields[2] in INCB_CASES:
                start, end = codepoint_range(fields[0])
                derived.append((start, end, fields[2]))
    incb = compress(derived)
    cased = compress(parse_property(decoded["DerivedCoreProperties.txt"], {"Cased"}))
    case_ignorable = compress(
        parse_property(decoded["DerivedCoreProperties.txt"], {"Case_Ignorable"})
    )
    whitespace = [
        (start, end)
        for start, end, _ in parse_property(decoded["PropList.txt"], {"White_Space"})
    ]

    source_hashes = [(name, digest(contents[name])) for name in sorted(contents)]
    counts = {
        "canonicalDecompositions": len(decomposition),
        "canonicalCompositions": len(composition),
        "combiningClassRanges": len(combining),
        "graphemeBreakRanges": len(grapheme),
        "extendedPictographicRanges": len(pictographic),
        "indicConjunctRanges": len(incb),
        "casedRanges": len(cased),
        "caseIgnorableRanges": len(case_ignorable),
        "uppercaseMappings": len(upper),
        "lowercaseMappings": len(lower),
        "contextualCasingMappings": len(contextual),
        "generalCategoryRanges": len(categories),
        "whiteSpaceRanges": len(whitespace),
        "compositionExclusions": len(exclusions),
    }

    return generate_compact_data(
        source_hashes,
        counts,
        decomposition,
        composition,
        combining,
        grapheme,
        pictographic,
        incb,
        cased,
        case_ignorable,
        upper,
        lower,
        contextual,
        categories,
        whitespace,
    )

def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true", help="fail if generated output differs")
    args = parser.parse_args()

    contents = {name: obtain(name, url) for name, url in SOURCES.items()}
    output, index_shards, shards = generate(contents)
    expected_files = {SHARD_DIR / f"{index:02d}.dark": shard for index, shard in enumerate(shards)}
    expected_index_files = {
        INDEX_SHARD_DIR / f"{index:02d}.dark": shard
        for index, shard in enumerate(index_shards)
    }
    if args.check:
        if not OUTPUT.exists() or OUTPUT.read_text(encoding="utf-8") != output:
            print(f"{OUTPUT} is not up to date", file=sys.stderr)
            return 1
        for path, shard in expected_files.items():
            if not path.exists() or path.read_text(encoding="utf-8") != shard:
                print(f"{path} is not up to date", file=sys.stderr)
                return 1
        for path, shard in expected_index_files.items():
            if not path.exists() or path.read_text(encoding="utf-8") != shard:
                print(f"{path} is not up to date", file=sys.stderr)
                return 1
        print(f"{OUTPUT} is reproducible for Unicode {UNICODE_VERSION}")
        return 0

    SHARD_DIR.mkdir(parents=True, exist_ok=True)
    INDEX_SHARD_DIR.mkdir(parents=True, exist_ok=True)
    for old_path in SHARD_DIR.glob("*.dark"):
        old_path.unlink()
    for old_path in INDEX_SHARD_DIR.glob("*.dark"):
        old_path.unlink()
    OUTPUT.write_text(output, encoding="utf-8")
    for path, shard in expected_files.items():
        path.write_text(shard, encoding="utf-8")
    for path, shard in expected_index_files.items():
        path.write_text(shard, encoding="utf-8")
    print(
        f"Wrote {OUTPUT}, {len(index_shards)} lookup index shards, "
        f"and {len(shards)} compact table shards"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
