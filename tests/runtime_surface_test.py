"""Public API contract, initially captured from builds at 613b625.

Update these fixtures deliberately when changing the public API.

These are interface snapshots, not expected tax values. The WASM probe inspects
wasm-bindgen's built JS exports, including class methods and static factories.
"""

import json
import re
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


@pytest.mark.requires_graph
def test_python_exported_names():
    """Preserve the public Python API contract."""
    assert python_surface() == json.loads((FIXTURES / "python.json").read_text())


@pytest.mark.parametrize(
    "path", [ROOT / "Makefile", *sorted((ROOT / ".github/workflows").glob("*.yml"))]
)
def test_build_targets_do_not_require_jit(path):
    """Reject Makefile and CI feature requests that need the removed feature."""
    for features in re.findall(r'--features[= ]+["\']?([^\n]+)', path.read_text()):
        assert "jit" not in re.split(r'[\s,"\']+', features), path


def test_runtime_has_no_jit_feature_or_dependency():
    """Keep JIT and Cranelift out of the runtime dependency surface."""
    manifest = (ROOT / "crates/tenforty-graph/Cargo.toml").read_text()
    assert not re.search(r"^jit\s*=", manifest, re.MULTILINE)
    assert "cranelift" not in manifest
    assert "cranelift" not in (ROOT / "Cargo.lock").read_text()
