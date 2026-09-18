#!/usr/bin/env bash
# start-codex.sh - Start Codex after Herdr creates a workspace and its root pane.

set -euo pipefail

herdr_bin="${HERDR_BIN_PATH:-herdr}"
workspace_id="${HERDR_WORKSPACE_ID:-}"

if [[ -z "$workspace_id" ]]; then
  workspace_id="$(
    printf '%s' "${HERDR_PLUGIN_EVENT_JSON:-{}}" |
      jq -r '.data.workspace.workspace_id // empty'
  )"
fi

if [[ -z "$workspace_id" ]]; then
  printf 'workspace.created did not provide a workspace ID\n' >&2
  exit 1
fi

# The workspace event precedes root-pane creation, so wait briefly for the pane.
pane_id=""
for _attempt in {1..50}; do
  pane_id="$(
    "$herdr_bin" pane list --workspace "$workspace_id" |
      jq -r '.result.panes[0].pane_id // empty'
  )"

  if [[ -n "$pane_id" ]]; then
    break
  fi

  sleep 0.1
done

if [[ -z "$pane_id" ]]; then
  printf 'root pane for workspace %s did not become available\n' "$workspace_id" >&2
  exit 1
fi

agent_name="$(
  printf 'codex-%s' "$workspace_id" |
    tr '[:upper:]' '[:lower:]' |
    tr -cd 'a-z0-9_-'
)"

"$herdr_bin" agent start "$agent_name" \
  --kind codex \
  --pane "$pane_id" \
  --timeout 60000
