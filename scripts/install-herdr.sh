#!/usr/bin/env bash
# install-herdr.sh - Install a Herdr binary for the image architecture.

set -euo pipefail

architecture="${1:?usage: install-herdr.sh <amd64|arm64> <version> [install-dir]}"
version="${2:?usage: install-herdr.sh <amd64|arm64> <version> [install-dir]}"
install_dir="${3:-${HOME}/.local/bin}"

case "$architecture" in
  amd64)
    release_arch="x86_64"
    ;;
  arm64)
    release_arch="aarch64"
    ;;
  *)
    printf 'Unsupported image architecture: %s\n' "$architecture" >&2
    exit 1
    ;;
esac

download_dir="$(mktemp -d)"
trap 'rm -rf "$download_dir"' EXIT
download_path="$download_dir/herdr"
download_url="https://github.com/herdrdev/herdr/releases/download/v${version}/herdr-linux-${release_arch}"

curl -fsSL --retry 3 --connect-timeout 10 --max-time 120 \
  "$download_url" \
  -o "$download_path"
install -d "$install_dir"
install -m 0755 "$download_path" "$install_dir/herdr"
"$install_dir/herdr" --version
