"""Tax-table planning quantization must originate in its DSL combinator."""

import re
from pathlib import Path


def _production_source_directories(spec):
    directories = set()
    in_sources = False
    for line in (spec / "tenforty-spec.cabal").read_text().splitlines():
        stripped = line.strip()
        if stripped.startswith("hs-source-dirs:"):
            in_sources = True
            directories.update(stripped.split(":", 1)[1].split())
        elif ":" in stripped or (line and not line[0].isspace()):
            in_sources = False
        elif in_sources:
            directories.update(stripped.split())
    return [spec / directory for directory in sorted(directories - {"test"})]


def test_specs_use_table_combinator_not_raw_quantize():
    """Keep raw planning quantization out of production specs and helpers."""
    spec = Path(__file__).resolve().parents[1] / "tenforty-spec"
    constructor_modules = {
        spec / "src/TenForty/Expr.hs",
        spec / "src/TenForty/Compile/JSON.hs",
        spec / "src/TenForty/TaxTable.hs",
    }
    forbidden = re.compile(r"\b(?:TaxTableQuantize|OpTaxTableQuantize)\b")
    violations = [
        str(path.relative_to(spec))
        for directory in _production_source_directories(spec)
        for path in sorted(directory.rglob("*.hs"))
        if path not in constructor_modules and forbidden.search(path.read_text())
    ]
    assert not violations, (
        f"Use taxTableBandTax instead of raw quantization: {violations}"
    )


def test_lint_covers_every_current_production_source_directory():
    """Include executable sources alongside form and library sources."""
    spec = Path(__file__).resolve().parents[1] / "tenforty-spec"
    assert {path.name for path in _production_source_directories(spec)} == {
        "app",
        "forms",
        "src",
    }
