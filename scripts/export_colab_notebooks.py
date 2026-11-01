#!/usr/bin/env python3
"""Export the marimo notebooks that back a Colab badge to Jupyter notebooks.

The marimo source is the one we edit; the exported .ipynb exists only so Colab
can open it. marimo writes markdown cells as Jupyter markdown, so the exported
notebook never needs marimo itself, and its `import marimo as mo` cell is
dropped. `make notebooks` writes it and `make notebooks-check` verifies it, with
marimo pinned in the Makefile so the export format cannot drift.
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
COLAB_NOTEBOOKS = {
    ROOT / "notebooks/tenforty_demo.py": ROOT / "notebooks/tenforty_Package_Demo.ipynb",
}


def _is_marimo_import(cell: dict) -> bool:
    return cell["cell_type"] == "code" and "".join(cell["source"]).strip() == (
        "import marimo as mo"
    )


def export(source: Path) -> dict:
    with tempfile.TemporaryDirectory() as scratch:
        exported = Path(scratch) / "notebook.ipynb"
        subprocess.run(
            [
                sys.executable,
                "-m",
                "marimo",
                "export",
                "ipynb",
                str(source),
                "-o",
                str(exported),
            ],
            check=True,
            capture_output=True,
        )
        notebook = json.loads(exported.read_text())
    notebook["cells"] = [
        cell for cell in notebook["cells"] if not _is_marimo_import(cell)
    ]
    return notebook


def main() -> int:
    parser = argparse.ArgumentParser()
    action = parser.add_mutually_exclusive_group(required=True)
    action.add_argument("--check", action="store_true")
    action.add_argument("--write", action="store_true")
    args = parser.parse_args()

    stale = []
    for source, target in COLAB_NOTEBOOKS.items():
        notebook = export(source)
        if args.write:
            target.write_text(json.dumps(notebook, indent=1, ensure_ascii=False) + "\n")
        elif not target.exists() or json.loads(target.read_text()) != notebook:
            stale.append(target.relative_to(ROOT))

    for path in stale:
        print(f"{path} is stale; run this script with --write")
    return 1 if stale else 0


if __name__ == "__main__":
    raise SystemExit(main())
