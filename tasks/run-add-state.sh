#!/usr/bin/env bash
set -euo pipefail

STATE="${1:?Usage: $0 <STATE> [MODEL] (e.g., $0 WI sonnet)}"
MODEL="${2:-opus}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

TASK_FILE=$(mktemp "/tmp/add-state-${STATE}-XXXX.md")
sed "s/{STATE}/${STATE}/g; s/{ST}/${STATE}/g; s/^model: .*/model: ${MODEL}/" \
  "$SCRIPT_DIR/add-state.md" >"$TASK_FILE"
trap 'rm -f "$TASK_FILE"' EXIT

echo "Adding state: $STATE (model: $MODEL)"
echo "Task file: $TASK_FILE"

claude-runner run --no-firewall --base-ref main-2025 -v -f "$TASK_FILE" "$REPO_DIR"
