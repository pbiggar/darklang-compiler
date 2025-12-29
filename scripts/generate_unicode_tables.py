#!/usr/bin/env python3
"""
Generate Unicode data tables for Darklang.

Downloads and parses Unicode data files, then generates Dark code
with Dict structures for case mappings and grapheme break properties.
"""

import urllib.request
import os
import sys

UNICODE_VERSION = "15.0.0"
UNICODE_DATA_URL = f"https://www.unicode.org/Public/{UNICODE_VERSION}/ucd/UnicodeData.txt"
SPECIAL_CASING_URL = f"https://www.unicode.org/Public/{UNICODE_VERSION}/ucd/SpecialCasing.txt"
GRAPHEME_BREAK_URL = f"https://www.unicode.org/Public/{UNICODE_VERSION}/ucd/auxiliary/GraphemeBreakProperty.txt"

CACHE_DIR = os.path.join(os.path.dirname(__file__), ".unicode_cache")

def download_file(url: str, filename: str) -> str:
    """Download a file, caching it locally."""
    os.makedirs(CACHE_DIR, exist_ok=True)
    cache_path = os.path.join(CACHE_DIR, filename)

    if os.path.exists(cache_path):
        print(f"Using cached {filename}")
        with open(cache_path, "r") as f:
            return f.read()

    print(f"Downloading {filename}...")
    with urllib.request.urlopen(url) as response:
        content = response.read().decode("utf-8")

    with open(cache_path, "w") as f:
        f.write(content)

    return content

def parse_unicode_data(content: str) -> dict:
    """Parse UnicodeData.txt for simple case mappings."""
    upper_map = {}  # codepoint -> [codepoints]
    lower_map = {}  # codepoint -> [codepoints]

    for line in content.strip().split("\n"):
        if not line or line.startswith("#"):
            continue

        fields = line.split(";")
        if len(fields) < 14:
            continue

        cp = int(fields[0], 16)
        upper = fields[12].strip()  # Simple uppercase mapping
        lower = fields[13].strip()  # Simple lowercase mapping

        if upper:
            upper_map[cp] = [int(upper, 16)]
        if lower:
            lower_map[cp] = [int(lower, 16)]

    return upper_map, lower_map

def parse_special_casing(content: str) -> tuple:
    """Parse SpecialCasing.txt for multi-character case mappings."""
    upper_map = {}
    lower_map = {}

    for line in content.strip().split("\n"):
        if not line or line.startswith("#"):
            continue

        # Remove comments
        line = line.split("#")[0].strip()
        if not line:
            continue

        fields = [f.strip() for f in line.split(";")]
        if len(fields) < 4:
            continue

        # Skip conditional mappings (have a 5th field with conditions)
        if len(fields) > 4 and fields[4].strip():
            continue

        cp = int(fields[0], 16)
        lower_cps = [int(x, 16) for x in fields[1].split() if x]
        # fields[2] is title case, skip for now
        upper_cps = [int(x, 16) for x in fields[3].split() if x]

        # Only include if it's different from identity mapping
        if upper_cps and upper_cps != [cp]:
            upper_map[cp] = upper_cps
        if lower_cps and lower_cps != [cp]:
            lower_map[cp] = lower_cps

    return upper_map, lower_map

def parse_grapheme_break(content: str) -> dict:
    """Parse GraphemeBreakProperty.txt for grapheme categories."""
    # Category name to number mapping
    CATEGORIES = {
        "Other": 0, "CR": 1, "LF": 2, "Control": 3, "Extend": 4,
        "ZWJ": 5, "Regional_Indicator": 6, "Prepend": 7, "SpacingMark": 8,
        "L": 9, "V": 10, "T": 11, "LV": 12, "LVT": 13
    }

    grapheme_map = {}

    for line in content.strip().split("\n"):
        if not line or line.startswith("#"):
            continue

        # Remove comments
        line = line.split("#")[0].strip()
        if not line:
            continue

        parts = line.split(";")
        if len(parts) < 2:
            continue

        range_part = parts[0].strip()
        category = parts[1].strip()

        if category not in CATEGORIES:
            continue

        cat_num = CATEGORIES[category]

        if ".." in range_part:
            start, end = range_part.split("..")
            for cp in range(int(start, 16), int(end, 16) + 1):
                grapheme_map[cp] = cat_num
        else:
            cp = int(range_part, 16)
            grapheme_map[cp] = cat_num

    return grapheme_map

def generate_dark_code(upper_map: dict, lower_map: dict, grapheme_map: dict) -> str:
    """Generate Dark code for Unicode tables."""
    lines = []
    lines.append("// Auto-generated Unicode data tables")
    lines.append(f"// Unicode version: {UNICODE_VERSION}")
    lines.append("// DO NOT EDIT - regenerate with scripts/generate_unicode_tables.py")
    lines.append("")

    # Generate uppercase mapping
    lines.append("// Uppercase mapping: codepoint -> list of codepoints")
    lines.append("def Unicode.Data.__upperCase() : Dict<Int64, List<Int64>> =")
    lines.append("    Stdlib.Dict.fromList<Int64, List<Int64>>([")

    sorted_upper = sorted(upper_map.items())
    for i, (cp, upper_cps) in enumerate(sorted_upper):
        cps_str = ", ".join(str(c) for c in upper_cps)
        comma = "," if i < len(sorted_upper) - 1 else ""
        lines.append(f"        ({cp}, [{cps_str}]){comma}")

    lines.append("    ])")
    lines.append("")

    # Generate lowercase mapping
    lines.append("// Lowercase mapping: codepoint -> list of codepoints")
    lines.append("def Unicode.Data.__lowerCase() : Dict<Int64, List<Int64>> =")
    lines.append("    Stdlib.Dict.fromList<Int64, List<Int64>>([")

    sorted_lower = sorted(lower_map.items())
    for i, (cp, lower_cps) in enumerate(sorted_lower):
        cps_str = ", ".join(str(c) for c in lower_cps)
        comma = "," if i < len(sorted_lower) - 1 else ""
        lines.append(f"        ({cp}, [{cps_str}]){comma}")

    lines.append("    ])")
    lines.append("")

    # Generate grapheme break property
    lines.append("// Grapheme break property: codepoint -> category (0-13)")
    lines.append("// Categories: 0=Other, 1=CR, 2=LF, 3=Control, 4=Extend, 5=ZWJ,")
    lines.append("//             6=Regional_Indicator, 7=Prepend, 8=SpacingMark,")
    lines.append("//             9=L, 10=V, 11=T, 12=LV, 13=LVT")
    lines.append("def Unicode.Data.__graphemeBreak() : Dict<Int64, Int64> =")
    lines.append("    Stdlib.Dict.fromList<Int64, Int64>([")

    sorted_grapheme = sorted(grapheme_map.items())
    for i, (cp, cat) in enumerate(sorted_grapheme):
        comma = "," if i < len(sorted_grapheme) - 1 else ""
        lines.append(f"        ({cp}, {cat}){comma}")

    lines.append("    ])")
    lines.append("")

    return "\n".join(lines)

def main():
    # Download Unicode data files
    unicode_data = download_file(UNICODE_DATA_URL, "UnicodeData.txt")
    special_casing = download_file(SPECIAL_CASING_URL, "SpecialCasing.txt")
    grapheme_break = download_file(GRAPHEME_BREAK_URL, "GraphemeBreakProperty.txt")

    # Parse the data
    print("Parsing UnicodeData.txt...")
    simple_upper, simple_lower = parse_unicode_data(unicode_data)

    print("Parsing SpecialCasing.txt...")
    special_upper, special_lower = parse_special_casing(special_casing)

    # Merge mappings (special casing takes precedence)
    upper_map = {**simple_upper, **special_upper}
    lower_map = {**simple_lower, **special_lower}

    print("Parsing GraphemeBreakProperty.txt...")
    grapheme_map = parse_grapheme_break(grapheme_break)

    print(f"Found {len(upper_map)} uppercase mappings")
    print(f"Found {len(lower_map)} lowercase mappings")
    print(f"Found {len(grapheme_map)} grapheme break properties")

    # Generate Dark code
    print("Generating Dark code...")
    dark_code = generate_dark_code(upper_map, lower_map, grapheme_map)

    # Write output file
    output_path = os.path.join(os.path.dirname(__file__), "..", "src", "DarkCompiler", "unicode_data.dark")
    with open(output_path, "w") as f:
        f.write(dark_code)

    print(f"Written to {output_path}")
    print(f"Total lines: {len(dark_code.split(chr(10)))}")

if __name__ == "__main__":
    main()
