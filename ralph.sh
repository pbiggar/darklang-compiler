# ralph.sh
# Usage: ./ralph.sh [iterations]

set -e

iterations="${1:-1}"

# For each iteration, run Codex with the following prompt.
# This prompt is basic, we'll expand it later.
for ((i=1; i<=$iterations; i++)); do
  result=$(codex exec --dangerously-bypass-approvals-and-sandbox -m gpt-5.2-codex \
"Use Beads for planning and progress tracking. \
1. Decide which issue to work on next using bd ready/bd list/bd show. \
This should be the one YOU decide has the highest priority, \
- not necessarily the first in the list. \
2. Update the issue to in_progress before starting work. \
3. Check any feedback loops, such as types and tests. Verify code compiles, tests pass, and benchmarks are not slower. NEVER commit anything not hitting these quality thresholds. \
4. Record progress with bd comments add <issue-id> \"...\" and close the issue with bd close when complete. \
5. Make a git commit of that feature. \
ONLY WORK ON A SINGLE ISSUE. \
If, while implementing the issue, you notice that all work \
is complete, output <promise>COMPLETE</promise>. \
")

  echo "$result"

  if [[ "$result" == *"<promise>COMPLETE</promise>"* ]]; then
    echo "PRD complete, exiting."
    exit 0
  fi
done
