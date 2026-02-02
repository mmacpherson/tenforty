#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 <STATE1> [STATE2 ...] [-- MODEL]"
  echo "  e.g. $0 IL NJ VA          (defaults to opus)"
  echo "  e.g. $0 OH GA -- sonnet"
  exit 1
}

[ $# -ge 1 ] || usage

STATES=()
MODEL="opus"
while [ $# -gt 0 ]; do
  case "$1" in
    --) MODEL="${2:?model required after --}"; shift 2 ;;
    *)  STATES+=("$1"); shift ;;
  esac
done

[ ${#STATES[@]} -ge 1 ] || usage

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

MAX_ITERATIONS=$(( ${#STATES[@]} + 1 ))
STATES_STR="${STATES[*]}"

TASK_FILE=$(mktemp "/tmp/add-states-XXXX.md")
sed \
  -e "s/^model: .*/model: ${MODEL}/" \
  -e "s/^max_iterations: .*/max_iterations: ${MAX_ITERATIONS}/" \
  -e "s/{STATES}/${STATES_STR}/g" \
  "$SCRIPT_DIR/add-state.md" >"$TASK_FILE"
trap 'rm -f "$TASK_FILE"' EXIT

echo "States: ${STATES_STR}  Model: $MODEL  Iterations: $MAX_ITERATIONS"
echo "Task file: $TASK_FILE"

claude-runner run --no-firewall --base-ref main-2025 -v -f "$TASK_FILE" "$REPO_DIR"
