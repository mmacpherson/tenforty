"""Policy checks for generated form graph artifacts."""

import glob
import json
import os


def _iter_form_json_paths() -> list[str]:
    repo_root = os.path.dirname(os.path.dirname(__file__))
    return sorted(
        glob.glob(os.path.join(repo_root, "src", "tenforty", "forms", "*.json"))
    )


def test_compiled_form_graphs_policy_and_integrity() -> None:
    """Assert shipped graphs are DSL-generated, year-consistent, and structurally valid."""
    paths = _iter_form_json_paths()
    assert paths, "Expected at least one compiled form graph under src/tenforty/forms/"

    for path in paths:
        filename = os.path.basename(path)
        stem = filename.removesuffix(".json")
        form_id, year_str = stem.rsplit("_", 1)
        year = int(year_str)

        with open(path) as f:
            data = json.load(f)

        # 1. Policy: Metadata checks
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

        # 2. Integrity: Node consistency
        nodes = data.get("nodes", {})
        node_ids = set()
        for key_id, node in nodes.items():
            assert str(node.get("id")) == key_id, (
                f"{filename}: node key {key_id} does not match id {node.get('id')}"
            )
            node_ids.add(node.get("id"))

        # 3. Integrity: Table references
        tables = data.get("tables", {})
        for node in nodes.values():
            op = node.get("op", {})
            if op.get("type") == "bracket_tax":
                table_id = op.get("table")
                assert table_id in tables, (
                    f"{filename}: node {node.get('name')} references missing table {table_id!r}"
                )
            elif op.get("type") == "ordering":
                # Assuming 'ordering' invariant might reference tables if implemented as op?
                # Actually Invariants are separate. Let's check Ops only for now.
                pass

        # 4. Integrity: Invariant references
        invariants = data.get("invariants", [])
        for inv in invariants:
            if inv.get("type") == "ordering":
                table_id = inv.get("table")
                assert table_id in tables, (
                    f"{filename}: invariant references missing table {table_id!r}"
                )
