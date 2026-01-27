"""Sync generated JSON graphs from `tenforty-spec/` into `src/tenforty/forms/`."""

from __future__ import annotations

from pathlib import Path


def main() -> None:
    """Replace `src/tenforty/forms/*.json` with `tenforty-spec/*.json`."""
    repo_root = Path(__file__).resolve().parent.parent
    spec_dir = repo_root / "tenforty-spec"
    forms_dir = repo_root / "src" / "tenforty" / "forms"

    if not forms_dir.exists():
        raise SystemExit(f"forms dir not found: {forms_dir}")

    dist_dir = spec_dir / "dist"
    generated = sorted(dist_dir.glob("*.json"))
    if not generated:
        raise SystemExit(
            "No generated graphs found in tenforty-spec/dist/*.json.\n"
            "Run `make spec-graphs` first."
        )

    for p in forms_dir.glob("*.json"):
        p.unlink()

    for src in generated:
        (forms_dir / src.name).write_text(src.read_text())

    print(f"Synced {len(generated)} graphs into {forms_dir}")


if __name__ == "__main__":
    main()
