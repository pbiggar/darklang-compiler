#!/bin/bash
# check-shell.sh
# Run shellcheck error checks against each tracked bash script in the repository.

set -euo pipefail

if ! command -v shellcheck >/dev/null 2>&1; then
  echo "shellcheck is required but was not found on PATH."
  exit 1
fi

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

mapfile -d '' tracked_files < <(git ls-files -z)
bash_files=()

for file in "${tracked_files[@]}"; do
  if [[ -f "$file" ]]; then
    IFS= read -r first_line < "$file" || first_line=""
    if [[ "$first_line" == '#!'*bash* ]]; then
      bash_files+=("$file")
    fi
  fi
done

if [[ ${#bash_files[@]} -eq 0 ]]; then
  echo "No bash scripts found."
  exit 0
fi

shellcheck --severity=error "${bash_files[@]}"
