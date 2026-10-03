r"""Transcribe the IRS Form 1040 Tax Table from its published PDF into a CSV.

The CSV is the independent legal oracle for the federal Tax Table tests: every
row comes from the IRS PDF, never from tenforty, OTS, or any repo table.

Usage:
    uv run --no-sync python scripts/parse_irs_tax_table.py \
        --pdf i1040tt--2024.pdf --year 2024 \
        --url https://www.irs.gov/pub/irs-prior/i1040tt--2024.pdf \
        --pages 3-14 --out tests/fixtures/federal_tax_table/irs_tax_table_2024.csv

Requires poppler's ``pdftotext``. ``-raw`` keeps the content-stream order, in
which each published row is one line: at-least, but-less-than, then the Single,
Married filing jointly, Married filing separately and Head of household amounts.
The transcription is rejected unless the rows tile $0 to $100,000 without a gap
or overlap and every column is nondecreasing. As an independent cross-check, the
``-layout`` extraction (three printed columns of rows per line) must not contain
any row that disagrees with the ``-raw`` transcription.
"""

import argparse
import csv
import datetime
import hashlib
import itertools
import re
import subprocess
from pathlib import Path

ROW = re.compile(r"^([\d,]+) ([\d,]+) ([\d,]+) ([\d,]+) ([\d,]+) ([\d,]+)$")
COLUMNS = (
    "at_least",
    "less_than",
    "single",
    "married_joint",
    "married_separate",
    "head_of_household",
)
TABLE_CEILING = 100_000


def _integer(token: str) -> int:
    return int(token.replace(",", ""))


def _pdftotext(pdf: Path, first_page: int, last_page: int, mode: str) -> str:
    return subprocess.run(
        ["pdftotext", mode, "-f", str(first_page), "-l", str(last_page), str(pdf), "-"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout


def extract_rows(pdf: Path, first_page: int, last_page: int) -> list[tuple[int, ...]]:
    text = _pdftotext(pdf, first_page, last_page, "-raw")
    return [
        tuple(_integer(token) for token in match.groups())
        for line in text.splitlines()
        if (match := ROW.match(line.strip()))
    ]


def layout_rows(pdf: Path, first_page: int, last_page: int) -> set[tuple[int, ...]]:
    rows = set()
    for line in _pdftotext(pdf, first_page, last_page, "-layout").splitlines():
        tokens = line.split()
        if (
            tokens
            and len(tokens) % 6 == 0
            and all(re.fullmatch(r"[\d,]+", token) for token in tokens)
        ):
            rows.update(
                tuple(_integer(token) for token in tokens[start : start + 6])
                for start in range(0, len(tokens), 6)
            )
    return rows


def cross_check(rows: list[tuple[int, ...]], layout: set[tuple[int, ...]]) -> None:
    disagreements = layout - set(rows)
    if disagreements:
        raise ValueError(
            f"-layout rows disagree with -raw transcription: {sorted(disagreements)[:5]}"
        )


def validate(rows: list[tuple[int, ...]]) -> None:
    if not rows:
        raise ValueError("no table rows found")
    ordered = sorted(rows)
    if ordered[0][0] != 0 or ordered[-1][1] != TABLE_CEILING:
        raise ValueError(
            f"rows span {ordered[0][0]}..{ordered[-1][1]}, not 0..{TABLE_CEILING}"
        )
    for previous, current in itertools.pairwise(ordered):
        if previous[1] != current[0]:
            raise ValueError(f"gap or overlap between {previous} and {current}")
        if any(
            now < before for before, now in zip(previous[2:], current[2:], strict=True)
        ):
            raise ValueError(f"tax decreases between {previous} and {current}")
    for row in ordered:
        if row[1] <= row[0]:
            raise ValueError(f"empty band {row}")


def write_csv(
    out: Path,
    rows: list[tuple[int, ...]],
    *,
    year: int,
    url: str,
    pdf: Path,
    pages: str,
) -> None:
    digest = hashlib.sha256(pdf.read_bytes()).hexdigest()
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="") as handle:
        handle.write(
            f"# {year} IRS Tax Table (Form 1040 line 16), transcribed by scripts/parse_irs_tax_table.py\n"
        )
        handle.write(f"# source_url: {url}\n")
        handle.write(f"# source_sha256: {digest}\n")
        handle.write(f"# source_pdf_pages: {pages}\n")
        handle.write(f"# transcribed_on: {datetime.date.today().isoformat()}\n")
        handle.write(f"# rows: {len(rows)}\n")
        writer = csv.writer(handle, lineterminator="\n")
        writer.writerow(COLUMNS)
        writer.writerows(sorted(rows))


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("--pdf", type=Path, required=True)
    parser.add_argument("--year", type=int, required=True)
    parser.add_argument("--url", required=True)
    parser.add_argument(
        "--pages", required=True, help="PDF page range holding the table, e.g. 3-14"
    )
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    first_page, last_page = (int(page) for page in args.pages.split("-"))
    rows = extract_rows(args.pdf, first_page, last_page)
    validate(rows)
    layout = layout_rows(args.pdf, first_page, last_page)
    cross_check(rows, layout)
    write_csv(
        args.out, rows, year=args.year, url=args.url, pdf=args.pdf, pages=args.pages
    )
    print(
        f"{args.out}: {len(rows)} rows; {len(layout)} independently confirmed by -layout"
    )


if __name__ == "__main__":
    main()
