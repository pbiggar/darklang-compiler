#!/bin/bash
# test-common.sh - Shared helpers for test runner scripts
#
# Provides build_tests, find_test_exe, and run_tests helpers.

build_tests() {
    local script_dir="$1"
    local configfile="$2"
    local build_label="${3:-Building...}"

    echo "$build_label"
    if [ -n "$configfile" ]; then
        if ! timeout 120 dotnet build --verbosity quiet --configfile "$configfile" 2>&1; then
            echo "Build failed!"
            return 1
        fi
    else
        if ! timeout 120 dotnet build --verbosity quiet 2>&1; then
            echo "Build failed!"
            return 1
        fi
    fi
    echo "Build complete."
    echo ""
}

find_test_exe() {
    local script_dir="$1"
    local missing_hint="$2"
    local test_exe="$script_dir/bin/Tests/Debug/net9.0/Tests"

    if [ ! -f "$test_exe" ]; then
        echo "Error: Test executable not found at $test_exe"
        if [ -n "$missing_hint" ]; then
            echo "$missing_hint"
        fi
        return 1
    fi
    echo "$test_exe"
}

run_tests() {
    local test_exe="$1"
    shift
    exec "$test_exe" "$@"
}
