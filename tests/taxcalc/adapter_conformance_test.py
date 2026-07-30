"""The taxcalc adapter must carry every case field the suite claims to compare.

A field the adapter silently drops does not produce a loud failure — it
produces a quiet agreement, because taxcalc is answering a different
question from the one tenforty was asked. `iso` was dropped for exactly this
reason: `taxcalc_batch` built its record without `cmbtp`, so taxcalc saw no
AMT preference and returned zero AMT, and `iso=200_000` and `iso=0` gave
identical output.

Nothing caught it because `_case_strategy()` generates no `iso`, so no running
case exercised the path. These tests assert the adapter's behaviour directly
rather than waiting for the strategy to cover it.
"""

import os

import pytest

if not os.environ.get("TENFORTY_TAXCALC"):
    pytest.skip(
        "taxcalc adapter tests require the taxcalc dependency group; "
        "set TENFORTY_TAXCALC=1 to run",
        allow_module_level=True,
    )

pytest.importorskip("taxcalc")
pytest.importorskip("pandas")

from .taxcalc_differential_test import taxcalc_batch
from .taxcalc_policy import evaluate_components

# The F14 case: Single 2024, $150k wages, $200k ISO exercise spread, standard
# deduction. Chosen because the AMT preference is the whole point of the case,
# so any adapter that drops it reports AMT of zero.
F14_CASE = {
    "year": 2024,
    "status": "Single",
    "w2": 150_000.0,
    "se": 0.0,
    "stcg": 0.0,
    "ltcg": 0.0,
    "interest": 0.0,
    "ord_div": 0.0,
    "qual_div": 0.0,
    "qual_frac": 0.0,
    "itemized": 0.0,
    "std_or_item": "Standard",
    "iso": 200_000.0,
}


def test_iso_reaches_taxcalc_as_amt_preference():
    """An ISO spread must produce AMT in taxcalc, not silence."""
    amt = taxcalc_batch([F14_CASE])[0]["amt"]
    assert amt > 0.0, (
        "taxcalc reported no AMT for a $200k ISO exercise spread — the adapter "
        "is not carrying `iso` through to `cmbtp`, so taxcalc is being asked "
        "a different question than tenforty."
    )
    assert amt == pytest.approx(39_725.50, abs=1.0)


def test_iso_changes_the_taxcalc_result():
    """Guard against the field being accepted and then ignored."""
    with_iso = taxcalc_batch([F14_CASE])[0]["amt"]
    without_iso = taxcalc_batch([{**F14_CASE, "iso": 0.0}])[0]["amt"]
    assert without_iso == pytest.approx(0.0, abs=1.0)
    assert with_iso != pytest.approx(without_iso, abs=1.0), (
        "iso=200,000 and iso=0 produced the same taxcalc result"
    )


# Case keys that are legitimately not carried to taxcalc, with the reason.
# Everything else in a case is swept, so a field added later comes under test
# without anyone remembering to extend a list — which is how `iso` was missed.
NOT_CARRIED = {
    "year": "selects the tax year, not an amount to carry",
    "status": "non-numeric; becomes MARS",
    "std_or_item": "non-numeric; taxcalc always takes best-of",
    "qual_frac": "consumed by _normalize_case into qual_div before the adapter",
}


def test_case_fields_all_reach_the_adapter():
    """Every numeric field of a case must change some taxcalc output.

    A silently dropped field is invisible until someone happens to write a case
    that depends on it, so this sweeps them rather than trusting review. The
    sweep is derived from the case itself rather than a hand-kept list: a list
    is exactly what let `iso` go unmapped, and later let `qual_div` and
    `itemized` sit outside a loop whose docstring claimed to cover them.
    """
    swept = sorted(set(F14_CASE) - set(NOT_CARRIED))
    assert swept, "no fields to sweep — did the case shape change?"

    baseline = taxcalc_batch([F14_CASE])[0]
    inert = []
    for field in swept:
        bumped = {**F14_CASE, field: F14_CASE[field] + 25_000.0}
        if taxcalc_batch([bumped])[0] == baseline:
            inert.append(field)
    assert not inert, (
        f"case fields the adapter ignores: {', '.join(inert)}. Either map them "
        f"in taxcalc_batch or add them to NOT_CARRIED with a reason."
    )


@pytest.mark.xfail(
    reason="F14: taxcalc leaves the standard deduction in AMTI for non-itemizers "
    "(no Form 6251 line 2a add-back), so its AMT is short by 28% x 14,600. "
    "Adjudicated in docs/taxcalc-differential-audit.md; upstream question "
    "drafted in docs/upstream-taxcalc-reports.md",
    strict=True,
)
def test_taxcalc_amt_matches_form_6251():
    """Taxcalc's AMT on an ISO case, against a hand-worked Form 6251.

    AMTI = taxable income $135,400 + standard deduction $14,600 + preference
    $200,000 = $350,000. Less the $85,700 exemption leaves $264,300, so the
    tentative minimum tax is 26% x $232,600 + 28% x $31,700 = $69,352 against
    regular tax of $25,538.50.

    This is the burn-in for F14 on the taxcalc side: it flips if Tax-Calculator
    adopts the add-back.
    """
    assert taxcalc_batch([F14_CASE])[0]["amt"] == pytest.approx(43_813.50, abs=1.0)


def test_ots_matches_form_6251_on_the_same_case():
    """OTS is the engine that agrees with the hand-worked form here."""
    assert evaluate_components(F14_CASE, "ots")["amt"] == pytest.approx(
        43_813.50, abs=1.0
    )


def test_itemized_aggregate_is_not_charity_capped():
    """The generic aggregate must survive taxcalc without a charitable ceiling."""
    case = {
        **F14_CASE,
        "status": "Widow(er)",
        "w2": 0.0,
        "stcg": 74_196.0,
        "itemized": 58_661.0,
        "iso": 0.0,
    }

    result = taxcalc_batch([case])[0]

    assert result["taxable_income"] == pytest.approx(15_535.0, abs=0.01)
    assert result["income_tax"] == pytest.approx(1_553.5, abs=0.01)


def test_taxcalc_keeps_the_standard_deduction_when_itemizing_is_free():
    """Deduction choice: taxcalc takes the cheaper, tenforty takes the larger.

    taxcalc computes the return both ways and keeps the itemized total only when
    it strictly lowers tax (`calculator.py`, `_calc_one_year`). OTS and the graph
    spec take whichever deduction is bigger. A deduction landing on income already
    in the 0% long-term-gain bracket lowers nothing, so the two rules report
    different taxable income and identical tax. F19 excuses taxable_income for
    exactly this, and only taxable_income -- this test is what says the tax really
    does agree, so that excusing it stays honest.
    """
    case = {
        **F14_CASE,
        "year": 2025,
        "status": "Head_of_House",
        "w2": 0.0,
        "ltcg": 58_509.0,
        "itemized": 56_482.0,
        "iso": 0.0,
    }

    result = taxcalc_batch([case])[0]
    ours = evaluate_components(case, "graph")

    # AGI 58,509 less the 2025 Head-of-Household standard deduction of 23,625.
    assert result["taxable_income"] == pytest.approx(34_884.0, abs=0.01)
    # tenforty takes the larger deduction instead: 58,509 less 56,482.
    assert ours["taxable_income"] == pytest.approx(2_027.0, abs=0.01)
    # ... and it costs nothing, which is the whole reason taxable_income is excused.
    assert result["income_tax"] == pytest.approx(ours["income_tax"], abs=0.01)
    assert result["total_tax"] == pytest.approx(ours["total_tax"], abs=0.01)
