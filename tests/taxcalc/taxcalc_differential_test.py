"""Hypothesis-driven differential testing against PSL Tax-Calculator.

Property: for any generated federal return, every tenforty component quantity
matches taxcalc within the shared tolerance policy, except disagreements
attributable by signature to a known, tracked defect (taxcalc_policy.py) —
so known bugs are excused by name and any novel disagreement fails the run.

Structural choices:

- **Batched taxcalc calls.** taxcalc has ~13s of fixed per-invocation overhead
  (policy deepcopy and parameter expansion) but is vectorized over records, so
  each hypothesis example is a *list* of cases evaluated in one taxcalc call.
  Shrinking collapses a failing list to the single minimal failing case.
- **target()-guided search.** Each quantity's worst UNEXCUSED disagreement is
  fed to hypothesis as an optimization target, so generation climbs toward
  novel divergence instead of re-finding tracked defects.
- **@example anchors.** The audit's known counterexamples are pinned so every
  run exercises them deterministically regardless of search luck.
- **MFJ attribution bounds.** taxcalc requires per-spouse wages; tenforty's
  w2_income is a household aggregate. MFJ cases run taxcalc under both
  attributions and assert tenforty falls within the bounds. Bounds can
  false-pass attribution-sensitive quantities — a known limit of the method,
  not a guarantee.

Requires the taxcalc dependency group: uv sync --group taxcalc
Slow: gated behind TENFORTY_TAXCALC=1.
"""

import os

import pytest

if not os.environ.get("TENFORTY_TAXCALC"):
    pytest.skip(
        "taxcalc differential suite is slow; set TENFORTY_TAXCALC=1 to run",
        allow_module_level=True,
    )

taxcalc = pytest.importorskip("taxcalc")
pd = pytest.importorskip("pandas")

from hypothesis import HealthCheck, example, given, settings, target  # noqa: E402
from hypothesis import strategies as st  # noqa: E402

import tenforty  # noqa: E402

from .taxcalc_policy import excused_quantities, tolerance  # noqa: E402

MARS = {
    "Single": 1,
    "Married/Joint": 2,
    "Married/Sep": 3,
    "Head_of_House": 4,
    "Widow(er)": 5,
}

QUANTITIES = (
    "agi",
    "taxable_income",
    "se_tax",
    "niit",
    "addl_medicare",
    "amt",
    "income_tax",
    "total_tax",
)


def _normalize_case(case):
    case = dict(case)
    case["qual_div"] = round(case.pop("qual_frac") * case["ord_div"], 2)
    return case


def _case_strategy():
    dollars = st.one_of(
        st.sampled_from((0, 125_000, 168_600, 176_100, 200_000, 250_000)),
        st.integers(min_value=0, max_value=400_000),
    ).map(float)
    small_dollars = st.one_of(st.just(0), st.integers(0, 60_000)).map(float)
    return st.fixed_dictionaries(
        {
            "year": st.sampled_from((2024, 2025)),
            "status": st.sampled_from(sorted(MARS)),
            "w2": dollars,
            "se": dollars,
            "stcg": st.one_of(st.just(0), st.integers(0, 250_000)).map(float),
            "ltcg": st.one_of(st.just(0), st.integers(0, 250_000)).map(float),
            "interest": small_dollars,
            "ord_div": small_dollars,
            "qual_frac": st.sampled_from((0.0, 0.5, 1.0)),
            "itemized": small_dollars,
            "std_or_item": st.sampled_from(("Standard", "Itemized")),
            # No "iso" yet, deliberately. The adapter now carries it (see
            # cmbtp above), but generating AMT-preference cases surfaces F14 on
            # both engines at once, and the _f14 signature currently excuses
            # the backend that turned out to be RIGHT. Flip that signature and
            # fix the graph spec first (tenforty-8ik), then add iso here as
            # part of the AMT coverage in tenforty-y90.
        }
    ).map(_normalize_case)


def _anchor(**kw):
    base = {
        "year": 2024,
        "status": "Single",
        "w2": 0.0,
        "se": 0.0,
        "stcg": 0.0,
        "ltcg": 0.0,
        "interest": 0.0,
        "ord_div": 0.0,
        "qual_div": 0.0,
        "itemized": 0.0,
        "std_or_item": "Standard",
    }
    base.update(kw)
    return base


ANCHOR_NIIT_STCG = _anchor(w2=300_000.0, stcg=50_000.0)
ANCHOR_SE_WAGE_BASE = _anchor(w2=168_600.0, se=60_000.0)
ANCHOR_8959_NO_WAGES = _anchor(se=300_000.0)


def taxcalc_batch(cases, wage_attribution="primary"):
    """Evaluate a batch of cases in one vectorized taxcalc call per year."""
    out = [None] * len(cases)
    for year in sorted({c["year"] for c in cases}):
        idx = [i for i, c in enumerate(cases) if c["year"] == year]
        recs = []
        for i in idx:
            c = cases[i]
            mars = MARS[c["status"]]
            wages_on_spouse = wage_attribution == "spouse" and mars == 2
            recs.append(
                {
                    "RECID": i + 1,
                    "MARS": mars,
                    "XTOT": 2 if mars == 2 else 1,
                    "age_head": 40,
                    "age_spouse": 40 if mars == 2 else 0,
                    "e00200": c["w2"],
                    "e00200p": 0.0 if wages_on_spouse else c["w2"],
                    "e00200s": c["w2"] if wages_on_spouse else 0.0,
                    "e00900": c["se"],
                    "e00900p": c["se"],
                    "e00900s": 0.0,
                    "e00300": c["interest"],
                    "e00600": max(c["ord_div"], c["qual_div"]),
                    "e00650": c["qual_div"],
                    "p22250": c["stcg"],
                    "p23250": c["ltcg"],
                    "e19800": c["itemized"],
                    # AMT preference income (ISO exercise spread). Without this
                    # taxcalc is handed no preference at all and reports zero
                    # AMT, so every AMT case would compare tenforty with the
                    # preference against taxcalc without it.
                    "cmbtp": c.get("iso", 0.0),
                }
            )
        df = pd.DataFrame(recs)
        records = taxcalc.Records(data=df, start_year=year, gfactors=None, weights=None)
        calc = taxcalc.Calculator(policy=taxcalc.Policy(), records=records)
        calc.advance_to_year(year)
        calc.calc_all()
        arr = calc.array
        for row, i in enumerate(idx):
            iitax = float(arr("iitax")[row])
            setax = float(arr("setax")[row])
            amc = float(arr("ptax_amc")[row])
            niit = float(arr("niit")[row])
            out[i] = {
                "agi": float(arr("c00100")[row]),
                "taxable_income": float(arr("c04800")[row]),
                "se_tax": setax,
                "niit": niit,
                "addl_medicare": amc,
                "amt": float(arr("c09600")[row]),
                # taxcalc iitax includes NIIT; tenforty federal_income_tax
                # excludes it.
                "income_tax": iitax - niit,
                "total_tax": iitax + setax + amc,
            }
    return out


def tenforty_components(case, backend):
    """Evaluate one case on a tenforty backend, returning compared quantities."""
    r = tenforty.evaluate_return(
        year=case["year"],
        filing_status=case["status"],
        backend=backend,
        w2_income=case["w2"],
        self_employment_income=case["se"],
        short_term_capital_gains=case["stcg"],
        long_term_capital_gains=case["ltcg"],
        taxable_interest=case["interest"],
        ordinary_dividends=case["ord_div"],
        qualified_dividends=case["qual_div"],
        itemized_deductions=case["itemized"],
        standard_or_itemized=case["std_or_item"],
    )
    return {
        "agi": r.federal_adjusted_gross_income,
        "taxable_income": r.federal_taxable_income,
        "se_tax": r.federal_se_tax,
        "niit": r.federal_niit,
        "addl_medicare": r.federal_additional_medicare_tax,
        "amt": r.federal_amt,
        "income_tax": r.federal_income_tax,
        "total_tax": r.federal_total_tax,
    }


@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.data_too_large],
)
@given(cases=st.lists(_case_strategy(), min_size=1, max_size=40))
@example(cases=[ANCHOR_NIIT_STCG])
@example(cases=[ANCHOR_SE_WAGE_BASE])
@example(cases=[ANCHOR_8959_NO_WAGES])
@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_components_match_taxcalc(backend, cases):
    """Every quantity matches taxcalc within tolerance, unless excused by name."""
    expected = taxcalc_batch(cases)
    mfj_present = any(c["status"] == "Married/Joint" for c in cases)
    expected_alt = taxcalc_batch(cases, "spouse") if mfj_present else expected

    failures = []
    worst = dict.fromkeys(QUANTITIES, 0.0)
    for case, exp, exp_alt in zip(cases, expected, expected_alt, strict=True):
        ours = tenforty_components(case, backend)
        excused = excused_quantities(backend, case)
        for quantity in QUANTITIES:
            lo = min(exp[quantity], exp_alt[quantity])
            hi = max(exp[quantity], exp_alt[quantity])
            tol = tolerance(backend, quantity, exp["taxable_income"], case)
            diff = max(lo - ours[quantity], ours[quantity] - hi, 0.0)
            if quantity in excused:
                continue
            worst[quantity] = max(worst[quantity], diff)
            if diff > tol:
                failures.append(
                    f"{case}: {quantity} got={ours[quantity]:,.2f} "
                    f"expected=[{lo:,.2f}, {hi:,.2f}] (diff {diff:,.2f} > {tol})"
                )
    # target() accepts one observation per label per example, so feed it the
    # batch maximum of unexcused disagreement: hypothesis steers toward novel
    # divergence, not the already-tracked defects.
    for quantity, diff in worst.items():
        target(diff, label=quantity)
    assert not failures, "\n".join(failures[:5])
