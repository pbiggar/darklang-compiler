#!/usr/bin/env bash
# install-darklang-interpreter.sh - Install the latest darklang interpreter binary from GitHub releases.

set -euo pipefail

if [[ $# -ne 0 ]]; then
  echo "Error: install-darklang-interpreter.sh accepts no arguments." >&2
  exit 1
fi

command -v jq >/dev/null 2>&1 || { echo "Error: jq is required" >&2; exit 1; }
command -v curl >/dev/null 2>&1 || { echo "Error: curl is required" >&2; exit 1; }
command -v gunzip >/dev/null 2>&1 || { echo "Error: gunzip is required" >&2; exit 1; }

arch="$(uname -m)"
asset_regex="$(
  case "$arch" in
    aarch64 | arm64)
      echo 'darklang-alpha-.*-linux-(arm|arm64|aarch64)\.gz$'
      ;;
    x86_64 | amd64)
      echo 'darklang-alpha-.*-linux-(amd64|x64|x86_64)\.gz$'
      ;;
    *)
      echo "Error: Unsupported architecture '$arch'" >&2
      exit 1
      ;;
  esac
)"
output_path="$HOME/.local/bin/darklang-interpreter"
release_json="$(curl -fsSL \
  -H "Accept: application/vnd.github+json" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  "https://api.github.com/repos/darklang/dark/releases?per_page=100")"

asset_info="$(
  jq -r --arg re "$asset_regex" '
    def releases:
      if type == "array" then . else [.] end;
    limit(1;
      releases
      | sort_by(.published_at // .created_at // "") | reverse
      | .[]
      | . as $release
      | .assets[]?
      | select(.name | test($re))
      | [$release.tag_name // "", .name, .browser_download_url]
      | @tsv
    )
  ' <<< "$release_json" \
)"

if [[ -z "$asset_info" ]]; then
  available_assets="$(
    jq -r '
      def releases:
        if type == "array" then . else [.] end;
      [releases[] | .assets[]?.name] | unique[]?
    ' <<< "$release_json" \
    | paste -sd ', ' -
  )"
  echo "Error: No release asset matched regex '${asset_regex}'. Available assets: ${available_assets:-none}" >&2
  exit 1
fi

tag_name="${asset_info%%$'\t'*}"
asset_rest="${asset_info#*$'\t'}"
asset_name="${asset_rest%%$'\t'*}"
asset_url="${asset_rest#*$'\t'}"

echo "Release: ${tag_name:-unknown}"
echo "Asset: $asset_name"
echo "URL: $asset_url"

output_dir="$(dirname "$output_path")"
mkdir -p "$output_dir"

tmp_output="$(mktemp "${output_path}.tmp.XXXXXX")"
cleanup() {
  rm -f "$tmp_output"
}
trap cleanup EXIT

curl -fsSL "$asset_url" | gunzip > "$tmp_output"
chmod +x "$tmp_output"
mv "$tmp_output" "$output_path"

trap - EXIT
echo "Installed darklang-interpreter to $output_path"
