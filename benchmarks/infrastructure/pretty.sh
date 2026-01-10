#!/bin/bash
# Shared colorized output helpers for benchmark scripts.

if [ -t 1 ] && [ -z "${NO_COLOR:-}" ]; then
    COLOR_RESET="\033[0m"
    COLOR_BOLD="\033[1m"
    COLOR_DIM="\033[2m"
    COLOR_RED="\033[31m"
    COLOR_GREEN="\033[32m"
    COLOR_YELLOW="\033[33m"
    COLOR_BLUE="\033[34m"
    COLOR_CYAN="\033[36m"
else
    COLOR_RESET=""
    COLOR_BOLD=""
    COLOR_DIM=""
    COLOR_RED=""
    COLOR_GREEN=""
    COLOR_YELLOW=""
    COLOR_BLUE=""
    COLOR_CYAN=""
fi

pretty_header() {
    printf "%b\n" "${COLOR_BOLD}${COLOR_BLUE}$*${COLOR_RESET}"
}

pretty_section() {
    printf "%b\n" "${COLOR_BOLD}${COLOR_CYAN}$*${COLOR_RESET}"
}

pretty_info() {
    printf "%b\n" "${COLOR_DIM}-${COLOR_RESET} $*"
}

pretty_ok() {
    printf "%b\n" "${COLOR_GREEN}OK${COLOR_RESET} $*"
}

pretty_warn() {
    printf "%b\n" "${COLOR_YELLOW}WARN${COLOR_RESET} $*"
}

pretty_fail() {
    printf "%b\n" "${COLOR_RED}FAIL${COLOR_RESET} $*"
}
