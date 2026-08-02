#!/bin/bash
# land-on-main.sh
# Rebase the current branch onto main, run tests/benchmarks, and fast-forward merge into main when possible.

set -euo pipefail

start_time="$(date +%s)"

repo_root="$(git rev-parse --show-toplevel)"
current_branch="$(git rev-parse --abbrev-ref HEAD)"
if [[ "$current_branch" == "HEAD" ]]; then
  echo "Detached HEAD; please check out a branch first."
  exit 1
fi
if [[ "$current_branch" == "main" ]]; then
  echo "Already on main; nothing to land."
  exit 0
fi

main_worktree="$(
  git worktree list --porcelain | awk '
    $1 == "worktree" { wt = $2 }
    $1 == "branch" && $2 == "refs/heads/main" && !found { print wt; found = 1 }
  '
)"
if [[ -z "$main_worktree" ]]; then
  echo "Could not find a main worktree."
  exit 1
fi

ensure_clean() {
  local worktree="$1"
  if [[ -n "$(git -C "$worktree" status --porcelain)" ]]; then
    echo "Worktree has uncommitted changes: $worktree"
    exit 1
  fi
}

rebase_in_progress() {
  local worktree="$1"
  local rebase_apply
  local rebase_merge
  rebase_apply="$(git -C "$worktree" rev-parse --git-path rebase-apply)"
  rebase_merge="$(git -C "$worktree" rev-parse --git-path rebase-merge)"
  [[ -d "$rebase_apply" || -d "$rebase_merge" ]]
}

resolve_conflicts_with_claude() {
  local worktree="$1"
  local prompt
  prompt=$(
    cat <<'EOF'
You are resolving a git rebase conflict in progress.

Requirements:
- Resolve all conflicts in the repo.
- Prefer changes from the feature branch being rebased onto main.
- Run `git status` to find conflicts.
- Edit files to resolve conflicts.
- `git add` resolved files.
- Run `git rebase --continue` until the rebase finishes.
- Do this non-interactively.
EOF
  )

  (cd "$worktree" && claude --dangerously-skip-permissions -p "$prompt") || true
}

ensure_clean "$repo_root"

while true; do
  main_head_before="$(git -C "$repo_root" rev-parse main)"
  if ! git -C "$repo_root" rebase main; then
  if ! rebase_in_progress "$repo_root"; then
      echo "Rebase failed without conflicts; manual intervention required."
      exit 1
    fi
    resolve_conflicts_with_claude "$repo_root"
    while rebase_in_progress "$repo_root"; do
      if git -C "$repo_root" rebase --continue; then
        break
      fi
      if git -C "$repo_root" diff --name-only --diff-filter=U | grep -q .; then
        resolve_conflicts_with_claude "$repo_root"
      else
        echo "Rebase stopped without conflicts; manual intervention required."
        exit 1
      fi
    done
  fi
  rebase_end="$(date +%s)"

  main_head="$(git -C "$repo_root" rev-parse main)"
  feature_head="$(git -C "$repo_root" rev-parse "$current_branch")"

  if [[ "$main_head" != "$feature_head" ]]; then
    (cd "$repo_root" && ./run-tests --ai)

    # Verify deterministic full results without dirtying the branch being landed.
    (cd "$repo_root" && ./benchmarks/run_benchmarks.sh --verify all)
  else
    echo "No changes to land; main already matches $current_branch."
  fi

  elapsed=$(( $(date +%s) - rebase_end ))
  if (( elapsed > 60 )); then
    echo "More than 60s since rebase; repeating."
    continue
  fi

  if [[ -n "$(git -C "$main_worktree" status --porcelain)" ]]; then
    echo "Main worktree has uncommitted changes; skipping fast-forward."
    exit 1
  fi
  main_head_after="$(git -C "$main_worktree" rev-parse main)"
  if [[ "$main_head_before" != "$main_head_after" ]]; then
    echo "Main moved since rebase; skipping fast-forward."
    exit 1
  fi
  if ! git -C "$main_worktree" merge-base --is-ancestor main "$current_branch"; then
    echo "Branch is not up to date with main; fast-forward not possible."
    exit 1
  fi
  git -C "$main_worktree" checkout main
  if [[ "$main_head" != "$feature_head" ]]; then
    git -C "$main_worktree" merge --ff-only "$current_branch"
  fi
  break
done

elapsed=$(( $(date +%s) - start_time ))
echo "Landed in ${elapsed}s"
