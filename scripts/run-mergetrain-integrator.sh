#!/usr/bin/env bash
# run-mergetrain-integrator.sh - Land auto-approved trains and repair recoverable failures.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
interval_seconds=15
attempt_dir="/tmp/dark-compiler-mergetrain-codex-attempts"
run_once=false
color_mode=auto

usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Continuously validate and deploy auto-approved merge-train jobs. When a job is
blocked by a merge conflict, local non-fast-forward update, or an unrecorded
benchmark improvement, invoke Codex once for that exact job revision, verify
its committed repair, and retry the job.

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

The integrator processes only jobs enqueued with --auto. It stops for manual
jobs, unknown states, non-recoverable failures, or a repeated Codex repair
attempt. Daemon and Codex output stays in log files. The console reports
readable phase changes and bounded failure summaries, with color when attached
to a terminal.

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

last_message_summary() {
  local message_file="$1"

  python3 - "$message_file" <<'PY'
import pathlib
import sys

text = pathlib.Path(sys.argv[1]).read_text(encoding="utf-8", errors="replace")
summary = " ".join(text.split())
print(summary[:500] + ("…" if len(summary) > 500 else ""))
PY
}

failed_gate_name() {
  python3 -c '
import json
import re
import sys

payload = json.load(sys.stdin)
for event in reversed(payload.get("events", [])):
    match = re.fullmatch(r"Failed gate [0-9]+/[0-9]+: (.+)", str(event.get("message", "")))
    if event.get("state") == "failure" and match:
        print(match.group(1))
        break
'
}

repair_job() {
  local snapshot="$1"
  local daemon_output="$2"
  local job_id details category reason worktree branch old_head attempt_marker output_file
  local current_branch new_head dirty git_common_dir codex_log daemon_log inspect_log
  local summary retry_log failed_gate repair_instructions failure_label

  job_id="$(json_value next_action.target_job_id <<<"$snapshot")"
  if [[ -z "$job_id" ]]; then
    daemon_log="$attempt_dir/unknown-job-$(date -u +%Y%m%dT%H%M%SZ)-$$.daemon.log"
    mv "$daemon_output" "$daemon_log"
    log_error "Mergetrain requested conflict repair without a target job"
    log_info "Daemon log: $daemon_log"
    exit 1
  fi

  inspect_log="$attempt_dir/$job_id.inspect.log"
  if ! details="$(
    mergetrain --repo "$repo_root" inspect "$job_id" --json 2>"$inspect_log"
  )"; then
    daemon_log="$attempt_dir/$job_id-unknown.daemon.log"
    mv "$daemon_output" "$daemon_log"
    log_job log_error "$job_id" "Mergetrain inspection failed"
    print_log_excerpt "$inspect_log" 8 "$job_id"
    log_job log_info "$job_id" "Full inspection log: $inspect_log"
    log_job log_info "$job_id" "Daemon log: $daemon_log"
    exit 1
  fi
  rm -f "$inspect_log"
  category="$(json_value outcome.failure_category <<<"$details")"
  reason="$(json_value outcome.message <<<"$details")"
  old_head="$(json_value job.head_sha <<<"$details")"
  daemon_log="$attempt_dir/$job_id-${old_head:-unknown}.daemon.log"
  mv "$daemon_output" "$daemon_log"
  branch="$(json_value job.branch <<<"$details")"
  failure_label="${category//_/ }"
  case "$category" in
    merge_conflict|semantic_conflict)
      repair_instructions="$(cat <<'EOF'
Resolve the rebase without discarding either source change.

Special generated-benchmark rule: if the rebase conflicts in
benchmarks/RESULTS.md, do not hand-merge it, choose ours/theirs, or edit its
conflict markers. First resolve the source changes, then run
./benchmarks/run_benchmarks.sh full in recording mode from the rebased tree.
That run must prove an aggregate improvement, advance the canonical Dark
snapshot, and regenerate benchmarks/RESULTS.md; stage the regenerated benchmark
files. If it fails or does not replace the conflicted RESULTS.md, abort the
rebase so failed recording artifacts are not committed, and explain that the
required improvement was not established.
EOF
)"
      ;;
    gate_failed)
      failed_gate="$(failed_gate_name <<<"$details")"
      if [[ "$failed_gate" != benchmarks ]]; then
        log_error "Job #$job_id needs operator attention ($category); refusing an automatic repair"
        log_info "Daemon log: $daemon_log"
        exit 1
      fi
      failure_label="benchmark gate failure"
      repair_instructions="$(cat <<'EOF'
The read-only integration benchmark gate reran the complete suite and found
that the canonical benchmark files do not describe this candidate. Run
./benchmarks/run_benchmarks.sh full in recording mode from the rebased tree.
The recording command is the decision boundary: it must compare with the
snapshot from the integration parent and must not advance it on a regression.

If the run improves the aggregate result, commit the regenerated benchmark files,
including benchmarks/RESULTS.md. If the recording run reports a regression,
produces no tracked benchmark change, or cannot complete, do not
commit or retry it; restore the branch to its original clean commit and explain
the blocker. Never manufacture, hand-edit, or select an older generated result.
EOF
)"
      ;;
    push_rejected)
      if [[ "$reason" != *non-fast-forward* ]]; then
        log_job log_error "$job_id" "Non-recoverable push rejection: $reason"
        log_job log_info "$job_id" "Daemon log: $daemon_log"
        exit 1
      fi
      repair_instructions="$(cat <<'EOF'
Rebase the task branch onto the current configured integration ref, preserving
both sides of the change, then run the repository's required verification and
commit the repaired result.
EOF
)"
      ;;
    *)
      log_job log_error "$job_id" "Needs operator attention ($category); refusing an automatic repair"
      log_job log_info "$job_id" "Daemon log: $daemon_log"
      exit 1
      ;;
  esac
  log_job log_run "$job_id" "Repairing $branch after $failure_label"

  worktree="$(json_value job.worktree_path <<<"$details")"
  if [[ -z "$worktree" || -z "$branch" || -z "$old_head" || ! -d "$worktree" ]]; then
    log_job log_error "$job_id" "Does not identify a usable owning worktree"
    log_job log_info "$job_id" "Daemon log: $daemon_log"
    exit 1
  fi

  attempt_marker="$attempt_dir/$job_id-$old_head.attempted"
  output_file="$attempt_dir/$job_id-$old_head.last-message.txt"
  codex_log="$attempt_dir/$job_id-$old_head.codex.log"
  if [[ -e "$attempt_marker" ]]; then
    log_job log_error "$job_id" "Codex already attempted revision $old_head; operator review required"
    log_job log_info "$job_id" "Daemon log: $daemon_log"
    exit 1
  fi
  touch "$attempt_marker"

  git_common_dir="$(git -C "$worktree" rev-parse --path-format=absolute --git-common-dir)"
  log_job log_run "$job_id" "Starting Codex repair; full output: $codex_log"

  if ! printf '%s\n' "$details" |
    codex exec \
      -C "$worktree" \
      --sandbox workspace-write \
      --add-dir "$git_common_dir" \
      --ephemeral \
      --output-last-message "$output_file" \
      "Repair mergetrain job #$job_id on branch $branch after a $category failure.

Read and follow AGENTS.md and the repository documentation. The mergetrain
inspection JSON is provided on stdin. Fetch the configured integration ref,
rebase this task branch onto it, understand both sides of any conflict, and
follow the repair instructions below. Work only in this job's owning worktree.
Run all relevant verification and commit the repair.

$repair_instructions

For this recovery run, do not invoke ./land. Do not push, deploy, enqueue,
retry, reconcile, cancel, dismiss, or modify mergetrain queue state; the
integrator owns the retry. If a confident repair is not possible, leave the
branch unchanged and explain the blocker." >"$codex_log" 2>&1; then
    log_job log_error "$job_id" "Codex repair failed ($category)"
    if [[ -s "$output_file" ]]; then
      summary="$(last_message_summary "$output_file")"
      if [[ -n "$summary" ]]; then
        log_job log_warn "$job_id" "Codex summary: $summary"
      fi
      log_job log_info "$job_id" "Final message: $output_file"
    else
      print_log_excerpt "$codex_log" 8 "$job_id"
    fi
    log_job log_info "$job_id" "Full execution log: $codex_log"
    log_job log_info "$job_id" "Daemon log: $daemon_log"
    exit 1
  fi

  log_job log_run "$job_id" "Codex finished; verifying the committed repair"
  current_branch="$(git -C "$worktree" branch --show-current)"
  new_head="$(git -C "$worktree" rev-parse HEAD)"
  dirty="$(git -C "$worktree" status --porcelain)"
  if [[ "$current_branch" != "$branch" || "$new_head" == "$old_head" || -n "$dirty" ]]; then
    log_job log_error "$job_id" "Codex did not leave a clean, newly committed $branch"
    log_job log_info "$job_id" "Final message: $output_file"
    log_job log_info "$job_id" "Full execution log: $codex_log"
    log_job log_info "$job_id" "Daemon log: $daemon_log"
    exit 1
  fi

  retry_log="$attempt_dir/$job_id-$new_head.retry.log"
  log_job log_run "$job_id" "Retrying at ${new_head:0:10}"
  if ! mergetrain --repo "$repo_root" retry "$job_id" --json >"$retry_log" 2>&1; then
    log_job log_error "$job_id" "Mergetrain retry failed"
    print_log_excerpt "$retry_log" 8 "$job_id"
    log_job log_info "$job_id" "Full retry log: $retry_log"
    exit 1
  fi
  rm -f "$retry_log"
  log_job log_ok "$job_id" "Retried after Codex committed a repair"
}

last_queue_signature=""
last_progress_event_id=0
active_progress_job_ids=""

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
    exit 1
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
    exit 1
  fi
  rm -f "$status_log"
  contract_version="$(json_value contract_version <<<"$snapshot")"
  next_action="$(json_value next_action.code <<<"$snapshot")"

  if [[ "$contract_version" != "4" ]]; then
    log_error "Unsupported mergetrain contract version: $contract_version"
    exit 1
  fi
  report_queue_status "$snapshot"
  report_train_progress "$snapshot"

  case "$next_action" in
    fix_blocked_job)
      repair_job "$snapshot" "$daemon_output"
      ;;
    enqueue_clean_branch|gc_available|run_daemon_when_approved)
      rm -f "$daemon_output"
      ;;
    wait_for_runner)
      rm -f "$daemon_output"
      ;;
    *)
      rm -f "$daemon_output"
      log_error "Mergetrain requires operator action: $next_action"
      exit 1
      ;;
  esac

  if [[ "$run_once" == true ]]; then
    exit 0
  fi
  sleep "$interval_seconds"
done
