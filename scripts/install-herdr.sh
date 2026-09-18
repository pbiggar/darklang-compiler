#!/usr/bin/env bash
# install-herdr.sh - Install a checksum-pinned Herdr binary for the image architecture.

set -euo pipefail

architecture="${1:?usage: install-herdr.sh <amd64|arm64> <version> [install-dir]}"
version="${2:?usage: install-herdr.sh <amd64|arm64> <version> [install-dir]}"
install_dir="${3:-${HOME}/.local/bin}"

case "$architecture" in
  amd64)
    release_arch="x86_64"
    expected_sha256="4fa1a01158dd8043da92d31b270780b0dcc10603038d9b61cac4d81ab63fb71f"
    ;;
  arm64)
    release_arch="aarch64"
    expected_sha256="9c8db20fb7e7427b138d5367113f1621ffd319f2f65d6f009e2594029115f0d2"
    ;;
  *)
    printf 'Unsupported image architecture: %s\n' "$architecture" >&2
    exit 1
    ;;
esac

if [[ "$version" != "0.9.0" ]]; then
  printf 'No verified Herdr checksums are recorded for version %s\n' "$version" >&2
  exit 1
fi

download_dir="$(mktemp -d)"
trap 'rm -rf "$download_dir"' EXIT
download_path="$download_dir/herdr"
download_url="https://github.com/herdrdev/herdr/releases/download/v${version}/herdr-linux-${release_arch}"

curl -fsSL --retry 3 --connect-timeout 10 --max-time 120 \
  "$download_url" \
  -o "$download_path"
printf '%s  %s\n' "$expected_sha256" "$download_path" | sha256sum --check --status
install -d "$install_dir"
install -m 0755 "$download_path" "$install_dir/herdr"
"$install_dir/herdr" --version
