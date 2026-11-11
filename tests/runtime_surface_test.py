"""Shipped API names captured from pre-removal builds at 613b625.

These are interface snapshots, not expected tax values. The WASM probe inspects
wasm-bindgen's built JS exports, including class methods and static factories.
"""

import json
import re
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "tests/fixtures/runtime-api"


def python_surface():
    """Inspect the compiled Python module and its public class members."""
    from tenforty.graphlib import graphlib

    return {
        name: sorted(
            member
            for member in dir(getattr(graphlib, name))
            if not member.startswith("_")
        )
        for name in dir(graphlib)
        if not name.startswith("_")
    }


def test_python_exported_names():
    """Preserve the pre-removal Python API."""
    assert python_surface() == json.loads((FIXTURES / "python.json").read_text())


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


@pytest.mark.parametrize(
    "path", [ROOT / "Makefile", *sorted((ROOT / ".github/workflows").glob("*.yml"))]
)
def test_build_targets_do_not_require_jit(path):
    """Reject Makefile and CI feature requests that need the removed feature."""
    for features in re.findall(r'--features[= ]+["\']?([^\n]+)', path.read_text()):
        assert "jit" not in re.split(r'[\s,"\']+', features), path
