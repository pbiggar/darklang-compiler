#!/usr/bin/env bash
# run-mergetrain-integrator.sh - Land auto-approved trains and repair recoverable failures.

set -euo pipefail

integrator_source_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
repo_root="$integrator_source_root"
interval_seconds=15
attempt_dir="/tmp/dark-compiler-mergetrain-codex-attempts"
run_once=false
color_mode=auto

usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Continuously validate and deploy auto-approved merge-train jobs. Recover
transient gate failures, dismiss patch-equivalent work, and ask Codex to repair
semantic conflicts or reproducible gates in a fresh worktree. Independently
verify each committed repair before replacing the blocked queue row.

Options:
  --repo PATH          Repository whose merge-train queue is processed.
                       Default: $repo_root
  --interval SECONDS   Delay between completed queue passes.
                       Default: $interval_seconds
  --attempt-dir PATH   Directory for Codex attempt markers and final messages.
                       Default: $attempt_dir
  --color MODE         Colorize status output: auto, always, or never.
                       Default: $color_mode
  --once               Run one daemon/status/repair pass, then exit.
  -h, --help           Show this help and exit.

The integrator processes only jobs enqueued with --auto. Problems are reported
without stopping the loop so an operator can intervene while monitoring stays
active.
Daemon and Codex output stays in log files. The console reports readable phase
changes and bounded failure summaries, with color when attached to a terminal.

Example:
  $0 --repo /Users/paulbiggar/projects/c4d-for-dcb
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo)
      repo_root="$2"
      shift 2
      ;;
    --interval)
      interval_seconds="$2"
      shift 2
      ;;
    --attempt-dir)
      attempt_dir="$2"
      shift 2
      ;;
    --color)
      color_mode="$2"
      shift 2
      ;;
    --once)
      run_once=true
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      usage >&2
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
done

if [[ ! "$interval_seconds" =~ ^[1-9][0-9]*$ ]]; then
  echo "--interval must be a positive integer" >&2
  exit 2
fi

if [[ "$color_mode" != auto && "$color_mode" != always && "$color_mode" != never ]]; then
  echo "--color must be auto, always, or never" >&2
  exit 2
fi

for required_command in codex git mergetrain python3; do
  if ! command -v "$required_command" >/dev/null 2>&1; then
    echo "Required command not found: $required_command" >&2
    exit 1
  fi
done

repo_root="$(cd "$repo_root" && pwd -P)"
mkdir -p "$attempt_dir"
attempt_dir="$(cd "$attempt_dir" && pwd -P)"

color_enabled=false
if [[ "$color_mode" == always ]] ||
  [[ "$color_mode" == auto && -t 2 && -z "${NO_COLOR:-}" && "${TERM:-}" != dumb ]]; then
  color_enabled=true
fi

if [[ "$color_enabled" == true ]]; then
  color_reset=$'\033[0m'
  color_dim=$'\033[2m'
  color_blue=$'\033[36m'
  color_green=$'\033[32m'
  color_yellow=$'\033[33m'
  color_red=$'\033[31m'
else
  color_reset=""
  color_dim=""
  color_blue=""
  color_green=""
  color_yellow=""
  color_red=""
fi

log_event() {
  local label="$1"
  local color="$2"
  shift 2
  printf '%s[%s]%s %s%-5s%s %s\n' \
    "$color_dim" "$(date '+%H:%M:%S')" "$color_reset" \
    "$color" "$label" "$color_reset" "$*" >&2
}

log_info() {
  log_event INFO "$color_blue" "$@"
}

log_run() {
  log_event RUN "$color_yellow" "$@"
}

log_ok() {
  log_event OK "$color_green" "$@"
}

log_warn() {
  log_event WARN "$color_yellow" "$@"
}

log_error() {
  log_event ERROR "$color_red" "$@"
}

log_job() {
  local logger="$1"
  local job_id="$2"
  shift 2
  "$logger" "Job #$job_id: $*"
}

json_value() {
  local path="$1"
  python3 -c '
import json
import sys

value = json.load(sys.stdin)
for component in sys.argv[1].split("."):
    value = value.get(component) if isinstance(value, dict) else None
if value is None:
    print("")
elif isinstance(value, bool):
    print("true" if value else "false")
else:
    print(value)
' "$path"
}

print_log_excerpt() {
  local log_file="$1"
  local line_count="${2:-8}"
  local job_id="${3:-}"

  if [[ -s "$log_file" ]]; then
    if [[ -n "$job_id" ]]; then
      log_job log_warn "$job_id" "Last $line_count log line(s):"
      tail -n "$line_count" "$log_file" | sed "s/^/  Job #$job_id: /" >&2
    else
      log_warn "Last $line_count log line(s):"
      tail -n "$line_count" "$log_file" | sed 's/^/  /' >&2
    fi
  fi
}

attention_job_ids() {
  python3 -c '
import json
import sys

payload = json.load(sys.stdin)
seen = set()
for job in payload.get("attention_jobs", []):
    job_id = job.get("id")
    if isinstance(job_id, int) and job_id not in seen:
        seen.add(job_id)
        print(job_id)
target = payload.get("next_action", {}).get("target_job_id")
if isinstance(target, int) and target not in seen:
    print(target)
'
}

repair_attention_jobs() {
  local snapshot="$1"
  local daemon_output="$2"
  local job_id failed=false processed=false daemon_log
  daemon_log="$attempt_dir/attention-$(date -u +%Y%m%dT%H%M%SZ)-$$.daemon.log"
  mv "$daemon_output" "$daemon_log"
  while read -r job_id; do
    [[ -n "$job_id" ]] || continue
    processed=true
    if [[ " $failed_recovery_job_ids " == *" $job_id "* ]]; then
      continue
    fi
    if ! python3 "$integrator_source_root/scripts/mergetrain_recovery.py" \
      --repo "$repo_root" \
      --attempt-dir "$attempt_dir" \
      --job-id "$job_id"; then
      failed=true
      failed_recovery_job_ids="$failed_recovery_job_ids $job_id"
    fi
  done < <(attention_job_ids <<<"$snapshot")
  if [[ "$processed" == false ]]; then
    log_error "Mergetrain requested recovery without identifying an attention job"
    log_info "Daemon log: $daemon_log"
    return 1
  fi
  if [[ "$failed" == true ]]; then
    log_warn "One or more merge-train problems remain; other jobs will continue"
    log_info "Daemon log: $daemon_log"
    return 1
  fi
  rm -f "$daemon_log"
}

last_queue_signature=""
last_progress_event_id=0
active_progress_job_ids=""
failed_recovery_job_ids=""

report_queue_status() {
  local snapshot="$1"
  local state summary health attention running waiting ready signature message

  state="$(json_value state <<<"$snapshot")"
  summary="$(json_value summary <<<"$snapshot")"
  health="$(json_value health <<<"$snapshot")"
  attention="$(json_value counts.attention <<<"$snapshot")"
  running="$(json_value counts.running <<<"$snapshot")"
  waiting="$(json_value counts.waiting <<<"$snapshot")"
  ready="$(json_value counts.ready <<<"$snapshot")"
  signature="$state|$summary|$health|$attention|$running|$waiting|$ready"

  if [[ "$signature" == "$last_queue_signature" ]]; then
    return
  fi
  last_queue_signature="$signature"

  if [[ -n "$attention" && -n "$running" && -n "$waiting" && -n "$ready" ]]; then
    message="Queue: $attention attention, $running running, $waiting waiting, $ready ready"
  else
    message="Queue state: ${state:-unknown}"
  fi
  if [[ -n "$summary" ]]; then
    message="$message — $summary"
  fi

  if [[ "$state" == attention || "$health" != healthy ]]; then
    log_warn "$message"
  else
    log_info "$message"
  fi
}

running_job_ids() {
  python3 -c '
import json
import sys

payload = json.load(sys.stdin)
for job in payload.get("recent_jobs", []):
    if job.get("state") == "running" and isinstance(job.get("id"), int):
        print(job["id"])
'
}

report_train_progress() {
  local snapshot="$1"
  local running_ids job_ids final_poll=false details_file inspect_log job_id
  local event_id event_job_id event_state message detail

  running_ids="$(running_job_ids <<<"$snapshot")"
  if [[ -n "$running_ids" ]]; then
    active_progress_job_ids="$running_ids"
    job_ids="$running_ids"
  elif [[ -n "$active_progress_job_ids" ]]; then
    job_ids="$active_progress_job_ids"
    final_poll=true
  else
    return 0
  fi

  details_file="$(mktemp "$attempt_dir/.progress.XXXXXX.jsonl")"
  for job_id in $job_ids; do
    inspect_log="$(mktemp "$attempt_dir/.inspect.XXXXXX.log")"
    if mergetrain --repo "$repo_root" inspect "$job_id" --json \
      >>"$details_file" 2>"$inspect_log"; then
      printf '\n' >>"$details_file"
    fi
    rm -f "$inspect_log"
  done

  while IFS=$'\t' read -r event_id event_job_id event_state message detail; do
    [[ -n "$event_id" ]] || continue
    last_progress_event_id="$event_id"
    if [[ -n "$detail" && "$message" != *"$detail"* ]]; then
      message="$message — $detail"
    fi
    if [[ "$event_job_id" != "-" ]]; then
      message="Job #$event_job_id: $message"
    fi
    case "$event_state" in
      success)
        log_ok "$message"
        ;;
      failure|failed|error)
        log_error "$message"
        ;;
      *)
        log_run "$message"
        ;;
    esac
  done < <(
    python3 - "$last_progress_event_id" "$details_file" <<'PY'
import json
import pathlib
import sys

minimum_id = int(sys.argv[1])
events = {}
text = pathlib.Path(sys.argv[2]).read_text(encoding="utf-8")
decoder = json.JSONDecoder()
position = 0
while position < len(text):
    while position < len(text) and text[position].isspace():
        position += 1
    if position == len(text):
        break
    payload, position = decoder.raw_decode(text, position)
    for event in payload.get("events", []):
        event_id = event.get("id")
        if isinstance(event_id, int) and event_id > minimum_id:
            events[event_id] = event

for event_id in sorted(events):
    event = events[event_id]
    fields = [
        str(event_id),
        str(event.get("job_id")) if isinstance(event.get("job_id"), int) else "-",
        str(event.get("state", "active")),
        str(event.get("message") or "Mergetrain progress"),
        str(event.get("detail") or ""),
    ]
    print("\t".join(field.replace("\t", " ").replace("\n", " ") for field in fields))
PY
  )
  rm -f "$details_file"

  if [[ "$final_poll" == true ]]; then
    active_progress_job_ids=""
  fi
  return 0
}

log_info "Integrator started • repo $repo_root • interval ${interval_seconds}s • color $color_mode"

while true; do
  # The native one-shot daemon owns queue locking, validation, and deployment.
  daemon_output="$(mktemp "$attempt_dir/.daemon.XXXXXX.log")"
  mergetrain --repo "$repo_root" daemon --once >"$daemon_output" 2>&1 &
  daemon_pid=$!
  while kill -0 "$daemon_pid" 2>/dev/null; do
    progress_status_log="$(mktemp "$attempt_dir/.status-progress.XXXXXX.log")"
    if live_snapshot="$(
      mergetrain --repo "$repo_root" status --json 2>"$progress_status_log"
    )"; then
      live_contract_version="$(json_value contract_version <<<"$live_snapshot")"
      if [[ "$live_contract_version" == "4" ]]; then
        report_queue_status "$live_snapshot"
        report_train_progress "$live_snapshot"
      fi
    fi
    rm -f "$progress_status_log"
    sleep 0.25
  done
  if ! wait "$daemon_pid"; then
    daemon_log="$attempt_dir/daemon-failed-$(date -u +%Y%m%dT%H%M%SZ)-$$.log"
    mv "$daemon_output" "$daemon_log"
    log_error "Mergetrain daemon command failed"
    print_log_excerpt "$daemon_log"
    log_info "Full daemon log: $daemon_log"
    if [[ "$run_once" == true ]]; then
      exit 0
    fi
    sleep "$interval_seconds"
    continue
  fi
  status_log="$(mktemp "$attempt_dir/.status.XXXXXX.log")"
  if ! snapshot="$(mergetrain --repo "$repo_root" status --json 2>"$status_log")"; then
    failed_status_log="$attempt_dir/status-failed-$(date -u +%Y%m%dT%H%M%SZ)-$$.log"
    daemon_log="$attempt_dir/status-failed-$(date -u +%Y%m%dT%H%M%SZ)-$$.daemon.log"
    mv "$status_log" "$failed_status_log"
    mv "$daemon_output" "$daemon_log"
    log_error "Mergetrain status command failed"
    print_log_excerpt "$failed_status_log"
    log_info "Full status log: $failed_status_log"
    log_info "Daemon log: $daemon_log"
    if [[ "$run_once" == true ]]; then
      exit 0
    fi
    sleep "$interval_seconds"
    continue
  fi
  rm -f "$status_log"
  contract_version="$(json_value contract_version <<<"$snapshot")"
  next_action="$(json_value next_action.code <<<"$snapshot")"

  if [[ "$contract_version" != "4" ]]; then
    log_error "Unsupported mergetrain contract version: $contract_version"
    rm -f "$daemon_output"
    if [[ "$run_once" == true ]]; then
      exit 0
    fi
    sleep "$interval_seconds"
    continue
  fi
  report_queue_status "$snapshot"
  report_train_progress "$snapshot"

  case "$next_action" in
    fix_blocked_job)
      repair_attention_jobs "$snapshot" "$daemon_output" || true
      ;;
    enqueue_clean_branch|gc_available|run_daemon_when_approved|validate_queued_jobs)
      rm -f "$daemon_output"
      ;;
    wait_for_runner)
      rm -f "$daemon_output"
      ;;
    *)
      rm -f "$daemon_output"
      log_error "Mergetrain requires operator action: $next_action"
      ;;
  esac

  if [[ "$run_once" == true ]]; then
    exit 0
  fi
  sleep "$interval_seconds"
done
