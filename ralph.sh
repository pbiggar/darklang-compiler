# ralph.sh
# Usage: ./ralph.sh <plan.md> [iterations]

set -e

if [ -z "$1" ]; then
  echo "Usage: $0 <plan.md> [iterations]"
  exit 1
fi

plan_file="$1"
iterations="${2:-1}"

# For each iteration, run Codex with the following prompt.
# This prompt is basic, we'll expand it later.
for ((i=1; i<=$iterations; i++)); do
  result=$(codex exec \
"@$plan_file @progress.txt \
1. Decide which task to work on next. \
This should be the one YOU decide has the highest priority, \
- not necessarily the first in the list. \
2. Check any feedback loops, such as types and tests. Verify code compiles, tests pass, and benchmarks are not slower. NEVER commit anything not hitting these quality thresholds.  \
3. Append your progress to the progress.txt file. \
4. Make a git commit of that feature. \
ONLY WORK ON A SINGLE FEATURE. \
If, while implementing the feature, you notice that all work \
is complete, output <promise>COMPLETE</promise>. \
")

  echo "$result"

  if [[ "$result" == *"<promise>COMPLETE</promise>"* ]]; then
    echo "PRD complete, exiting."
    exit 0
  fi
done
