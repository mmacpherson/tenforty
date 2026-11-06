"""Every Python block in README.md runs, and every output it shows matches.

This is a documentation check, not tax evidence: it pins what the README shows
to what the code returns, so the two cannot drift. Shown outputs are marked in
the README with an HTML comment directly above them:

- ``<!-- readme-output: value -->`` above a Python literal: the value of the
  last expression of the preceding code block.
- ``<!-- readme-output: table -->`` above a Markdown table: the polars frame
  that block evaluates to, compared at the precision each cell shows.

The blocks run in order in one namespace, as a reader running them would. A
block that ends in a seaborn plot is rendered, so a plot that cannot draw
fails here.
"""

import ast
import math
import pathlib
import re
import warnings

import matplotlib
import polars as pl
import pytest
import seaborn.objects as so

README = pathlib.Path(__file__).parent.parent / "README.md"
FENCE = re.compile(r"^``` ?(\w*)\s*$")
MARKER = re.compile(r"^<!-- readme-output: (value|table) -->$")
IMAGE = re.compile(r"!\[[^\]]*\]\(([^)]+)\)")


def _segments() -> list[tuple[str, str]]:
    """Split the README into (kind, text): python, value, table or image."""
    lines = README.read_text().splitlines()
    segments: list[tuple[str, str]] = []
    pending: str | None = None
    index = 0
    while index < len(lines):
        line = lines[index]
        marker = MARKER.match(line)
        fence = FENCE.match(line)
        if marker:
            pending = marker.group(1)
        elif fence:
            language = fence.group(1)
            end = index + 1
            while not lines[end].startswith("```"):
                end += 1
            body = "\n".join(lines[index + 1 : end])
            if pending == "value":
                segments.append(("value", body))
            elif language == "python":
                segments.append(("python", body))
            pending = None
            index = end
        elif pending == "table" and line.startswith("|"):
            end = index
            while end < len(lines) and lines[end].startswith("|"):
                end += 1
            segments.append(("table", "\n".join(lines[index:end])))
            pending = None
            index = end - 1
        else:
            for target in IMAGE.findall(line):
                segments.append(("image", target))
        index += 1
    return segments


def _run(source: str, namespace: dict) -> object:
    """Execute a block; return the value of its last expression, if any."""
    tree = ast.parse(source)
    last = tree.body[-1] if tree.body and isinstance(tree.body[-1], ast.Expr) else None
    if last is not None:
        tree.body = tree.body[:-1]
    exec(compile(tree, str(README), "exec"), namespace)
    if last is None:
        return None
    return eval(compile(ast.Expression(last.value), str(README), "eval"), namespace)


def _parse_table(text: str) -> tuple[list[str], list[list[str]]]:
    rows = [
        [cell.strip() for cell in line.strip().strip("|").split("|")]
        for line in text.splitlines()
    ]
    return rows[0], rows[2:]


def _assert_cell_matches(shown: str, actual: object, where: str) -> None:
    decimals = len(shown.split(".")[1]) if "." in shown else 0
    assert actual is not None, where
    assert not math.isnan(actual), where
    assert abs(float(shown) - actual) <= 0.5 * 10**-decimals + 1e-9, (
        where,
        shown,
        actual,
    )


def _assert_table_matches(text: str, frame: object, block: int) -> None:
    assert isinstance(frame, pl.DataFrame), f"table after block {block}"
    header, rows = _parse_table(text)
    assert header == frame.columns, (header, frame.columns)
    assert len(rows) == frame.height, (len(rows), frame.height)
    for row_index, (shown_row, actual_row) in enumerate(
        zip(rows, frame.iter_rows(), strict=True)
    ):
        for column, shown, actual in zip(header, shown_row, actual_row, strict=True):
            _assert_cell_matches(shown, actual, f"row {row_index} {column}")


def _assert_value_matches(text: str, actual: object, block: int) -> None:
    shown = ast.literal_eval(text)
    assert actual == shown, f"value after block {block}"


def test_readme_has_examples_and_shown_outputs():
    """Guard: the parser finds the README's code and its marked outputs."""
    kinds = [kind for kind, _ in _segments()]
    assert kinds.count("python") >= 8
    assert kinds.count("value") >= 1
    assert kinds.count("table") >= 1


@pytest.mark.requires_graph
def test_every_readme_block_runs_and_shows_its_output():
    """Run the README top to bottom; check each shown output and plot."""
    matplotlib.use("Agg")
    namespace: dict = {"__name__": "readme"}
    last_value: object = None
    block = 0
    checked = 0
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        for kind, text in _segments():
            if kind == "python":
                block += 1
                last_value = _run(text, namespace)
                if isinstance(last_value, so.Plot):
                    last_value.plot()
            elif kind == "value":
                _assert_value_matches(text, last_value, block)
                checked += 1
            elif kind == "table":
                _assert_table_matches(text, last_value, block)
                checked += 1
            elif kind == "image" and not text.startswith("http"):
                assert (README.parent / text).is_file(), text
    assert checked >= 2
