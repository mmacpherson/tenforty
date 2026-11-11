"""Run explicitly with `uv run python scripts/check_runtime_wasm.py`.

Requires wasm-pack and Node; compares built exports captured at 613b625.
"""

import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "tests/fixtures/runtime-api"


def test_wasm_exported_names():
    """Build without JIT and preserve the pre-removal WASM API."""
    subprocess.run(["make", "wasm-dev"], cwd=ROOT, check=True, capture_output=True)
    probe = """
import * as m from './crates/tenforty-graph/pkg/graphlib.js';
const result = Object.fromEntries(Object.keys(m).sort().map(k => [k,
  typeof m[k] === 'function' && m[k].prototype ? {
    static: Object.getOwnPropertyNames(m[k]).filter(n=>!['length','name','prototype'].includes(n)).sort(),
    methods: Object.getOwnPropertyNames(m[k].prototype).filter(n=>n!=='constructor').sort()
  } : null]));
console.log(JSON.stringify(result));
"""
    result = subprocess.run(
        ["node", "--input-type=module", "-e", probe],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    assert json.loads(result.stdout) == json.loads((FIXTURES / "wasm.json").read_text())


if __name__ == "__main__":
    test_wasm_exported_names()
