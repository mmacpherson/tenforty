"""Policy checks for generated form graph artifacts."""

import glob
import json
import os


def _forms_dir() -> str:
    return os.path.join(
        os.path.dirname(os.path.dirname(__file__)), "src", "tenforty", "forms"
    )


def _iter_form_json_paths() -> list[str]:
    # Per-form compiled graphs only; the resolved per-year graph (us_tax_graph_*)
    # is a different artifact with its own integrity test below.
    return sorted(
        p
        for p in glob.glob(os.path.join(_forms_dir(), "*.json"))
        if not os.path.basename(p).startswith("us_tax_graph_")
    )


def _operand_refs(op: dict) -> list:
    t = op.get("type")
    if t in ("add", "sub", "mul", "div", "max", "min"):
        return [op.get("left"), op.get("right")]
    if t in ("floor", "neg", "abs", "clamp"):
        return [op.get("arg")]
    if t == "bracket_tax":
        return [op.get("income")]
    if t == "phase_out":
        return [op.get("agi")]
    if t == "by_status":
        v = op.get("values", {})
        return [
            v.get("single"),
            v.get("married_joint"),
            v.get("married_separate"),
            v.get("head_of_household"),
            v.get("qualifying_widow"),
        ]
    if t == "if_positive":
        return [op.get("cond"), op.get("then"), op.get("otherwise")]
    return []


def test_compiled_form_graphs_policy_and_integrity() -> None:
    """Assert shipped graphs are DSL-generated, year-consistent, and structurally valid."""
    paths = _iter_form_json_paths()
    assert paths, "Expected at least one compiled form graph under src/tenforty/forms/"

    for path in paths:
        filename = os.path.basename(path)
        stem = filename.removesuffix(".json")
        form_id, year_str = stem.rsplit("_", 1)
        year = int(year_str)

        with open(path, encoding="utf-8") as f:
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
        assert isinstance(nodes, dict), (
            f"{filename}: expected nodes to be a dict, got {type(nodes)!r}"
        )
        node_ids = set()
        for key_id, node in nodes.items():
            assert str(node.get("id")) == key_id, (
                f"{filename}: node key {key_id} does not match id {node.get('id')}"
            )
            node_ids.add(node.get("id"))
        assert len(node_ids) == len(nodes), f"{filename}: duplicate node ids detected"

        # 3. Integrity: Table references
        tables = data.get("tables", {})
        assert isinstance(tables, dict), (
            f"{filename}: expected tables to be a dict, got {type(tables)!r}"
        )
        for node in nodes.values():
            op = node.get("op", {})
            if op.get("type") == "bracket_tax":
                table_id = op.get("table")
                assert table_id in tables, (
                    f"{filename}: node {node.get('name')} references missing table {table_id!r}"
                )

        # 4. Integrity: Invariant references
        invariants = data.get("invariants", [])
        assert isinstance(invariants, list), (
            f"{filename}: expected invariants to be a list, got {type(invariants)!r}"
        )
        for inv in invariants:
            if inv.get("type") == "ordering":
                table_id = inv.get("table")
                assert table_id in tables, (
                    f"{filename}: invariant references missing table {table_id!r}"
                )

        # 5. Integrity: Node references
        for node in nodes.values():
            op = node.get("op", {})

            for ref in _operand_refs(op):
                if ref is not None:
                    assert str(ref) in nodes, (
                        f"{filename}: node {node.get('name')} references non-existent node {ref}"
                    )


def test_resolved_tax_graph_integrity() -> None:
    """The resolved per-year graph (us_tax_graph_<year>) has no imports, no dangling edges."""
    paths = sorted(glob.glob(os.path.join(_forms_dir(), "us_tax_graph_*.json")))
    assert paths, "Expected resolved per-year graphs (us_tax_graph_<year>.json)"

    for path in paths:
        filename = os.path.basename(path)
        with open(path, encoding="utf-8") as f:
            data = json.load(f)

        assert data["meta"]["generated_by"] == "resolveForms", filename
        assert data.get("imports") == [], (
            f"{filename}: resolved graph must carry no imports"
        )

        nodes = data["nodes"]
        for key_id, node in nodes.items():
            assert str(node.get("id")) == key_id, (
                f"{filename}: node id/key mismatch at {key_id}"
            )
            for ref in _operand_refs(node.get("op", {})):
                if ref is not None:
                    assert str(ref) in nodes, f"{filename}: dangling edge -> {ref}"
