"""Run explicitly with `uv run python scripts/check_runtime_wasm.py`.

Requires wasm-pack and Node. The fixture is the public API contract; update it
deliberately when changing the API. Pass --skip-build to inspect an existing build.
"""

import argparse
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "tests/fixtures/runtime-api"


def check_wasm_exported_names(*, skip_build: bool = False):
    """Compare built WASM exports with the public API contract."""
    if not skip_build:
        subprocess.run(["make", "wasm-dev"], cwd=ROOT, check=True, capture_output=True)
    probe = """
import * as m from './crates/tenforty-graph/pkg/graphlib.js';
const result = Object.fromEntries(Object.keys(m).filter(k=>!k.startsWith("__")).sort().map(k => [k,
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
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--skip-build", action="store_true")
    check_wasm_exported_names(skip_build=parser.parse_args().skip_build)
