"""Variant implementations of parse_ots_return for benchmarking and comparison.

Each variant adds cumulative optimizations:
- v0: Original implementation (verbatim copy)
- v1: Pre-compiled regexes at module level
- v2: v1 + _parse_value moved to module level
- v3: v2 + string-based fast path for assignment lines
- v4: v3 + early blank-line skip + '.' shortcut in _parse_value
- v5: Minimal-diff version — all perf wins from v4 but keeps parse_value
      as inner function and preserves all logger.debug calls
"""

import logging
import re
from typing import Any

from .models import OTSParseError

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# v0: Original implementation (verbatim copy from core.py)
# ---------------------------------------------------------------------------
def parse_ots_return_v0(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    def parse_value(val: str) -> int | float | str:
        cleaned = val.replace(",", "")
        try:
            return int(cleaned)
        except ValueError:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val

    fields = {}
    for line in text.split("\n"):
        if amt_match := re.search(
            r"Your Alternative Minimum Tax =\s*(\d+(\.\d+)?)", line
        ):
            fields["amt"] = parse_value(amt_match.group(1))
        elif assignment_match := re.search(r"\s*(\S+)\s*=\s*(\S+)", line):
            identifier, rhs = assignment_match.groups()
            fields[identifier] = parse_value(rhs)
        elif bracket_match := re.search(
            r"You are in the (\d+(\.\d+)?)% marginal tax bracket", line
        ):
            fields["tax_bracket"] = parse_value(bracket_match.group(1))
        elif effective_match := re.search(
            r"you are paying an effective (\d+(\.\d+)?)% tax", line
        ):
            fields["effective_tax_rate"] = parse_value(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


# ---------------------------------------------------------------------------
# v1: Pre-compiled regexes at module level
# ---------------------------------------------------------------------------
_V1_AMT_RE = re.compile(r"Your Alternative Minimum Tax =\s*(\d+(\.\d+)?)")
_V1_ASSIGNMENT_RE = re.compile(r"\s*(\S+)\s*=\s*(\S+)")
_V1_BRACKET_RE = re.compile(r"You are in the (\d+(\.\d+)?)% marginal tax bracket")
_V1_EFFECTIVE_RE = re.compile(r"you are paying an effective (\d+(\.\d+)?)% tax")


def parse_ots_return_v1(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    def parse_value(val: str) -> int | float | str:
        cleaned = val.replace(",", "")
        try:
            return int(cleaned)
        except ValueError:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val

    fields = {}
    for line in text.split("\n"):
        if amt_match := _V1_AMT_RE.search(line):
            fields["amt"] = parse_value(amt_match.group(1))
        elif assignment_match := _V1_ASSIGNMENT_RE.search(line):
            identifier, rhs = assignment_match.groups()
            fields[identifier] = parse_value(rhs)
        elif bracket_match := _V1_BRACKET_RE.search(line):
            fields["tax_bracket"] = parse_value(bracket_match.group(1))
        elif effective_match := _V1_EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = parse_value(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


# ---------------------------------------------------------------------------
# v2: v1 + _parse_value moved to module level
# ---------------------------------------------------------------------------
def _parse_value_v2(val: str) -> int | float | str:
    cleaned = val.replace(",", "")
    try:
        return int(cleaned)
    except ValueError:
        try:
            return float(cleaned)
        except ValueError:
            logger.debug(f"Couldn't parse value as number, leaving as string: [{val}]")
            return val


def parse_ots_return_v2(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    fields = {}
    for line in text.split("\n"):
        if amt_match := _V1_AMT_RE.search(line):
            fields["amt"] = _parse_value_v2(amt_match.group(1))
        elif assignment_match := _V1_ASSIGNMENT_RE.search(line):
            identifier, rhs = assignment_match.groups()
            fields[identifier] = _parse_value_v2(rhs)
        elif bracket_match := _V1_BRACKET_RE.search(line):
            fields["tax_bracket"] = _parse_value_v2(bracket_match.group(1))
        elif effective_match := _V1_EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = _parse_value_v2(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


# ---------------------------------------------------------------------------
# v3: v2 + string-based fast path for assignment lines
#
# Instead of regex for every line, check for '=' with str ops.
# The AMT line contains '=' so we guard it with a prefix check.
# Lines without '=' only need bracket/effective regex checks.
# ---------------------------------------------------------------------------
_AMT_PREFIX = "Your Alternative Minimum Tax"


def parse_ots_return_v3(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    fields = {}
    for line in text.split("\n"):
        if "=" in line:
            if _AMT_PREFIX in line:
                amt_match = _V1_AMT_RE.search(line)
                if amt_match:
                    fields["amt"] = _parse_value_v2(amt_match.group(1))
                    continue
            before, _, after = line.partition("=")
            before_stripped = before.strip()
            after_stripped = after.strip()
            if before_stripped and after_stripped:
                identifier = before_stripped.rsplit(None, 1)[-1]
                rhs = after_stripped.split(None, 1)[0]
                fields[identifier] = _parse_value_v2(rhs)
        elif bracket_match := _V1_BRACKET_RE.search(line):
            fields["tax_bracket"] = _parse_value_v2(bracket_match.group(1))
        elif effective_match := _V1_EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = _parse_value_v2(effective_match.group(1))

    return fields


# ---------------------------------------------------------------------------
# v4: v3 + early blank-line skip + '.' shortcut in _parse_value
# ---------------------------------------------------------------------------
def _parse_value_v4(val: str) -> int | float | str:
    cleaned = val.replace(",", "")
    if "." in cleaned:
        try:
            return float(cleaned)
        except ValueError:
            return val
    try:
        return int(cleaned)
    except ValueError:
        try:
            return float(cleaned)
        except ValueError:
            return val


def parse_ots_return_v4(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    fields = {}
    for line in text.split("\n"):
        if not line or line.isspace():
            continue
        if "=" in line:
            if _AMT_PREFIX in line:
                amt_match = _V1_AMT_RE.search(line)
                if amt_match:
                    fields["amt"] = _parse_value_v4(amt_match.group(1))
                    continue
            before, _, after = line.partition("=")
            before_stripped = before.strip()
            after_stripped = after.strip()
            if before_stripped and after_stripped:
                identifier = before_stripped.rsplit(None, 1)[-1]
                rhs = after_stripped.split(None, 1)[0]
                fields[identifier] = _parse_value_v4(rhs)
        elif bracket_match := _V1_BRACKET_RE.search(line):
            fields["tax_bracket"] = _parse_value_v4(bracket_match.group(1))
        elif effective_match := _V1_EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = _parse_value_v4(effective_match.group(1))

    return fields


# ---------------------------------------------------------------------------
# v5: Minimal-diff — all perf wins, but keeps parse_value as inner function
#     and preserves all logger.debug calls from the original.
# ---------------------------------------------------------------------------
def parse_ots_return_v5(
    text: str,
    year: int | None = None,
    form_id: str | None = None,
) -> dict[str, Any]:
    if not text or not text.strip():
        context = f" for {year}/{form_id}" if year and form_id else ""
        raise OTSParseError(f"OTS output is empty{context}", raw_output=text)

    def parse_value(val: str) -> int | float | str:
        cleaned = val.replace(",", "")
        if "." in cleaned:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val
        try:
            return int(cleaned)
        except ValueError:
            try:
                return float(cleaned)
            except ValueError:
                logger.debug(
                    f"Couldn't parse value as number, leaving as string: [{val}]"
                )
                return val

    fields = {}
    for line in text.split("\n"):
        if not line or line.isspace():
            continue
        if "=" in line:
            if _AMT_PREFIX in line:
                amt_match = _V1_AMT_RE.search(line)
                if amt_match:
                    fields["amt"] = parse_value(amt_match.group(1))
                    continue
            before, _, after = line.partition("=")
            before_stripped = before.strip()
            after_stripped = after.strip()
            if before_stripped and after_stripped:
                identifier = before_stripped.rsplit(None, 1)[-1]
                rhs = after_stripped.split(None, 1)[0]
                fields[identifier] = parse_value(rhs)
            else:
                logger.debug(f"Uninterpreted line: [{line}]")
        elif bracket_match := _V1_BRACKET_RE.search(line):
            fields["tax_bracket"] = parse_value(bracket_match.group(1))
        elif effective_match := _V1_EFFECTIVE_RE.search(line):
            fields["effective_tax_rate"] = parse_value(effective_match.group(1))
        else:
            logger.debug(f"Uninterpreted line: [{line}]")

    return fields


ALL_VARIANTS = {
    "v0": parse_ots_return_v0,
    "v1": parse_ots_return_v1,
    "v2": parse_ots_return_v2,
    "v3": parse_ots_return_v3,
    "v4": parse_ots_return_v4,
    "v5": parse_ots_return_v5,
}
