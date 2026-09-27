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
changes on one updating line per job, with color when attached to a terminal.

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
activity_file="$attempt_dir/integrator-activity-$$.txt"
trap 'rm -f -- "$activity_file"' EXIT

set_activity() {
  local temporary="$activity_file.tmp"
  printf '%s\n%s\n' "$repo_root" "$1" > "$temporary"
  mv -f -- "$temporary" "$activity_file"
}

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

job_line_ids=()
job_lines=()
declare -A final_job_ids=()
jobs_seen=false
job_display_tty=false
if [[ -t 2 && "${TERM:-}" != dumb ]]; then
  job_display_tty=true
fi

job_display_width="$(tput cols 2>/dev/null || true)"
if [[ ! "$job_display_width" =~ ^[1-9][0-9]*$ ]]; then
  job_display_width=80
fi

fit_job_line() {
  python3 - "$job_display_width" "$1" <<'PY'
import sys
import unicodedata

limit = max(0, int(sys.argv[1]) - 1)
width = 0
output = []
for char in sys.argv[2]:
    cells = 0 if unicodedata.combining(char) else (
        2 if unicodedata.east_asian_width(char) in {"F", "W"} else 1
    )
    if width + cells > limit:
        break
    output.append(char)
    width += cells
print("".join(output))
PY
}

display_job() {
  local job_id="$1" state="$2" final="$3" message="$4"
  local index=-1 position line step state_color
  if [[ -n "${final_job_ids[$job_id]:-}" ]]; then
    return
  fi
  for position in "${!job_line_ids[@]}"; do
    if [[ "${job_line_ids[$position]}" == "$job_id" ]]; then
      index="$position"
      break
    fi
  done
  message="${message//$'\n'/ }"
  set_activity "Job #$job_id $state: $message"
  line="[$(date '+%H:%M:%S')] Job #$job_id $state: $message"
  if [[ "$job_display_tty" == true ]]; then
    line="$(fit_job_line "$line")"
  fi
  case "$state" in
    OK|READY) state_color="$color_green" ;;
    ERROR) state_color="$color_red" ;;
    WARN|WAIT) state_color="$color_yellow" ;;
    *) state_color="$color_blue" ;;
  esac
  line="${color_dim}${line:0:10}${color_reset}${line:10}"
  line="${line/ $state:/ ${state_color}$state${color_reset}:}"
  jobs_seen=true
  if [[ "$final" == true ]]; then
    final_job_ids[$job_id]=true
    if [[ "$job_display_tty" == true && ${#job_line_ids[@]} -gt 0 ]]; then
      printf '\033[%dA\r\033[J' "${#job_line_ids[@]}" >&2
    fi
    printf '%s\n' "$line" >&2
    if ((index >= 0)); then
      job_line_ids=("${job_line_ids[@]:0:index}" "${job_line_ids[@]:index+1}")
      job_lines=("${job_lines[@]:0:index}" "${job_lines[@]:index+1}")
    fi
    if [[ "$job_display_tty" == true && ${#job_lines[@]} -gt 0 ]]; then
      printf '%s\n' "${job_lines[@]}" >&2
    fi
  elif ((index < 0)); then
    job_line_ids+=("$job_id")
    job_lines+=("$line")
    if [[ "$job_display_tty" == true ]]; then
      printf '%s\n' "$line" >&2
    fi
  else
    job_lines[$index]="$line"
    if [[ "$job_display_tty" == true ]]; then
      step=$((${#job_line_ids[@]} - index))
      printf '\033[%dA\r\033[2K%s\033[%dB\r' "$step" "$line" "$step" >&2
    fi
  fi
}

log_event() {
  local label="$1"
  local color="$2"
  shift 2
  if [[ "$job_display_tty" == true && ${#job_line_ids[@]} -gt 0 ]]; then
    printf '\033[%dA\r\033[J' "${#job_line_ids[@]}" >&2
  fi
  printf '%s[%s]%s %s%-5s%s %s\n' \
    "$color_dim" "$(date '+%H:%M:%S')" "$color_reset" \
    "$color" "$label" "$color_reset" "$*" >&2
  if [[ "$job_display_tty" == true && ${#job_line_ids[@]} -gt 0 ]]; then
    printf '%s\n' "${job_lines[@]}" >&2
  fi
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
  local line

  if [[ -s "$log_file" ]]; then
    log_warn "Last $line_count log line(s):"
    while IFS= read -r line; do
      log_warn "  $line"
    done < <(tail -n "$line_count" "$log_file")
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
  local job_id failed=false processed=false daemon_log recovery_log
  daemon_log="$attempt_dir/attention-$(date -u +%Y%m%dT%H%M%SZ)-$$.daemon.log"
  mv "$daemon_output" "$daemon_log"
  while read -r job_id; do
    [[ -n "$job_id" ]] || continue
    processed=true
    if [[ " $failed_recovery_job_ids " == *" $job_id "* ]]; then
      continue
    fi
    recovery_log="$attempt_dir/recovery-$job_id-$(date -u +%Y%m%dT%H%M%SZ)-$$.log"
    display_job "$job_id" RUN false "recovering attention job"
    if ! python3 "$integrator_source_root/scripts/mergetrain_recovery.py" \
      --repo "$repo_root" \
      --attempt-dir "$attempt_dir" \
      --job-id "$job_id" >"$recovery_log" 2>&1; then
      failed=true
      failed_recovery_job_ids="$failed_recovery_job_ids $job_id"
      display_job "$job_id" ERROR true "recovery needs attention; log: $recovery_log"
    else
      display_job "$job_id" OK true "recovery completed; log: $recovery_log"
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
  if [[ "$jobs_seen" == true ]]; then
    return
  fi

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

report_new_jobs() {
  local snapshot="$1" job_id state task label existing known_id
  while IFS=$'\t' read -r job_id state task; do
    [[ -n "$job_id" ]] || continue
    existing=false
    if [[ -n "${final_job_ids[$job_id]:-}" ]]; then
      existing=true
    fi
    for known_id in "${job_line_ids[@]}"; do
      if [[ "$known_id" == "$job_id" ]]; then
        existing=true
        break
      fi
    done
    if [[ "$existing" == false ]]; then
      case "$state" in
        waiting) label=WAIT ;;
        ready) label=READY ;;
        attention) label=WARN ;;
        *) label=RUN ;;
      esac
      display_job "$job_id" "$label" false "${task:-job} ($state)"
    fi
  done < <(python3 -c '
import json, sys
payload = json.load(sys.stdin)
seen = set()
for job in payload.get("recent_jobs", []) + payload.get("attention_jobs", []):
    job_id = job.get("id")
    state = job.get("state")
    if isinstance(job_id, int) and state in {"waiting", "running", "ready", "attention"} and job_id not in seen:
        seen.add(job_id)
        task = str(job.get("task") or "job").replace("\t", " ").replace("\n", " ")
        print(f"{job_id}\t{state}\t{task}")
' <<<"$snapshot")
}

report_terminal_jobs() {
  local snapshot="$1" job_id state known_id
  while IFS=$'\t' read -r job_id state; do
    [[ -n "$job_id" ]] || continue
    for known_id in "${job_line_ids[@]}"; do
      if [[ "$known_id" == "$job_id" ]]; then
        case "$state" in
          done|deployed) display_job "$job_id" OK true "deployed" ;;
          canceled) display_job "$job_id" ERROR true "canceled" ;;
        esac
        break
      fi
    done
  done < <(python3 -c '
import json, sys
for job in json.load(sys.stdin).get("recent_jobs", []):
    if isinstance(job.get("id"), int) and job.get("state") in {"done", "deployed", "canceled"}:
        print("{}\t{}".format(job["id"], job["state"]))
' <<<"$snapshot")
}

job_snapshot_state() {
  local job_id="$1"
  python3 -c '
import json, sys
payload = json.load(sys.stdin)
target = int(sys.argv[1])
for job in payload.get("recent_jobs", []) + payload.get("attention_jobs", []):
    if job.get("id") == target:
        print(job.get("state") or "unknown")
        break
' "$job_id"
}

report_train_progress() {
  local snapshot="$1"
  local running_ids job_ids final_poll=false details_file inspect_log job_id
  local event_id event_job_id event_state message detail display_state final

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
    final=false
    case "$event_state" in
      success)
        display_state=OK
        ;;
      failure|failed|error)
        display_state=ERROR
        ;;
      *)
        display_state=RUN
        ;;
    esac
    if [[ "$event_job_id" == "-" ]]; then
      for job_id in $job_ids; do
        display_job "$job_id" "$display_state" "$final" "$message"
      done
    else
      display_job "$event_job_id" "$display_state" "$final" "$message"
    fi
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
    for job_id in $job_ids; do
      case "$(job_snapshot_state "$job_id" <<<"$snapshot")" in
        done|deployed) display_job "$job_id" OK true "deployed" ;;
        attention) display_job "$job_id" WARN false "needs attention" ;;
        canceled|failed) display_job "$job_id" ERROR true "stopped" ;;
        ready) display_job "$job_id" READY false "validated; awaiting deployment" ;;
        waiting) display_job "$job_id" WAIT false "waiting" ;;
        *) display_job "$job_id" WARN false "waiting for final state" ;;
      esac
    done
    active_progress_job_ids=""
  fi
  return 0
}

set_activity "Starting integrator"
log_info "Integrator started • repo $repo_root • interval ${interval_seconds}s • color $color_mode"

while true; do
  pre_status_log="$(mktemp "$attempt_dir/.status-before.XXXXXX.log")"
  if pre_snapshot="$(mergetrain --repo "$repo_root" status --json 2>"$pre_status_log")"; then
    if [[ "$(json_value contract_version <<<"$pre_snapshot")" == "4" ]]; then
      report_queue_status "$pre_snapshot"
      report_new_jobs "$pre_snapshot"
    fi
  fi
  rm -f "$pre_status_log"
  # The native one-shot daemon owns queue locking, validation, and deployment.
  daemon_output="$(mktemp "$attempt_dir/.daemon.XXXXXX.log")"
  set_activity "Running merge-train daemon"
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
        report_new_jobs "$live_snapshot"
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
  set_activity "Checking queue outcome"
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
  report_new_jobs "$snapshot"
  report_train_progress "$snapshot"
  report_terminal_jobs "$snapshot"

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
      while read -r job_id; do
        [[ -n "$job_id" ]] || continue
        display_job "$job_id" WARN true "operator action required: $next_action"
      done < <(attention_job_ids <<<"$snapshot")
      log_error "Mergetrain requires operator action: $next_action"
      ;;
  esac

  if [[ "$run_once" == true ]]; then
    exit 0
  fi
  set_activity "Waiting for next queue pass"
  sleep "$interval_seconds"
done
