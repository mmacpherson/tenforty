"""Policy checks for generated form graph artifacts."""

import glob
import json
import os


def _iter_form_json_paths() -> list[str]:
    repo_root = os.path.dirname(os.path.dirname(__file__))
    return sorted(
        glob.glob(os.path.join(repo_root, "src", "tenforty", "forms", "*.json"))
    )


def test_compiled_form_graphs_have_expected_metadata() -> None:
    """Assert shipped graphs are DSL-generated and year-consistent."""
    paths = _iter_form_json_paths()
    assert paths, "Expected at least one compiled form graph under src/tenforty/forms/"

    for path in paths:
        filename = os.path.basename(path)
        stem = filename.removesuffix(".json")
        form_id, year_str = stem.rsplit("_", 1)
        year = int(year_str)

        with open(path) as f:
            data = json.load(f)

        assert data.get("meta"), f"{filename}: missing meta"
        meta = data["meta"]

        assert meta.get("generated_by") == "tenforty-dsl", (
            f"{filename}: expected meta.generated_by == 'tenforty-dsl', got {meta.get('generated_by')!r}"
        )
        assert meta.get("form_id") == form_id, (
            f"{filename}: expected meta.form_id == {form_id!r}, got {meta.get('form_id')!r}"
        )
        assert meta.get("year") == year, (
            f"{filename}: expected meta.year == {year}, got {meta.get('year')!r}"
        )

        for imp in data.get("imports", []):
            assert imp.get("year") == year, (
                f"{filename}: import year mismatch: expected {year}, got {imp.get('year')!r}"
            )
