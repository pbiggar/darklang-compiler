#!/bin/bash
# merge-worktree.sh
# Fast-forward merge a numbered worktree branch into the current branch.

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <worktree-number>"
  exit 1
fi

worktree_number="$1"
if [[ ! "$worktree_number" =~ ^[1-9][0-9]*$ ]]; then
  echo "Worktree number must be a positive integer."
  exit 1
fi

branch_name="wt-$worktree_number"
if ! git show-ref --verify --quiet "refs/heads/$branch_name"; then
  echo "Branch not found: $branch_name"
  exit 1
fi

git merge --ff-only "$branch_name"
