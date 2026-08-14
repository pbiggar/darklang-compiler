#!/usr/bin/env python3
"""Focused tests for scripts/validate-darklang.py."""

import importlib.util
from pathlib import Path


def load_validate_darklang_module():
    script_path = Path(__file__).with_name("validate-darklang.py")
    spec = importlib.util.spec_from_file_location("validate_darklang", script_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_public_lambda_syntax_is_unchanged():
    module = load_validate_darklang_module()
    converter = module.SyntaxConverter()
    actual = converter.convert("fun x y -> x + y")
    expected = "fun x y -> x + y"
    assert actual == expected, f"expected {expected!r}, got {actual!r}"


def main():
    test_public_lambda_syntax_is_unchanged()


if __name__ == "__main__":
    main()
