"""Hypothesis-driven differential testing against PSL Tax-Calculator.

Property: for any generated federal return, every tenforty component quantity
matches taxcalc within tolerance.

Two structural choices worth knowing about:

- **Batched oracle calls.** taxcalc has ~13s of fixed per-invocation overhead
  (policy deepcopy and parameter expansion) but is vectorized over records, so
  each hypothesis example is a *list* of cases evaluated in one taxcalc call.
  Shrinking naturally collapses a failing list to the single minimal failing
  case, so counterexample quality is unchanged.
- **target()-guided search.** Each quantity's absolute disagreement is fed to
  hypothesis as an optimization target, so generation actively climbs toward
  the largest divergence instead of sampling blindly.

Requires the oracle dependency group: uv sync --group oracle

Married/Joint is excluded: taxcalc requires per-spouse wage attribution and
tenforty's w2_income is a household aggregate, so exact comparison is only
defined for single-person statuses (see docs/taxcalc-differential-audit.md).

The module is a non-strict xfail while the known mapping bugs stand (audit
findings F1-F6). Once those are fixed, drop the marker: this becomes the
standing adversarial guard against regressions and shared omissions.
"""

import os

import pytest

if not os.environ.get("TENFORTY_ORACLE"):
    pytest.skip(
        "oracle differential suite is slow (~5 min); set TENFORTY_ORACLE=1 to run",
        allow_module_level=True,
    )

taxcalc = pytest.importorskip("taxcalc")
pd = pytest.importorskip("pandas")

from hypothesis import HealthCheck, given, settings, target  # noqa: E402
from hypothesis import strategies as st  # noqa: E402

import tenforty  # noqa: E402

pytestmark = pytest.mark.xfail(
    reason="known mapping bugs, docs/taxcalc-differential-audit.md F1-F6",
    strict=False,
)

MARS = {"Single": 1, "Married/Sep": 3, "Head_of_House": 4, "Widow(er)": 5}

SS_WAGE_BASE_2024 = 168_600
BOUNDARY_DOLLARS = (0, 125_000, SS_WAGE_BASE_2024, 200_000, 250_000)

dollars = st.one_of(
    st.sampled_from(BOUNDARY_DOLLARS),
    st.integers(min_value=0, max_value=400_000),
).map(float)

case_strategy = st.fixed_dictionaries(
    {
        "status": st.sampled_from(sorted(MARS)),
        "w2": dollars,
        "se": dollars,
        "stcg": dollars,
        "ltcg": dollars,
    }
)

TAX_TABLE_TOL = 15.0  # OTS uses the $50-step 1040 tax tables; taxcalc is exact
COMPONENT_TOL = 2.0


def taxcalc_batch(cases):
    """Evaluate a batch of cases in one vectorized taxcalc call."""
    df = pd.DataFrame(
        [
            {
                "RECID": i + 1,
                "MARS": MARS[c["status"]],
                "XTOT": 1,
                "age_head": 40,
                "e00200": c["w2"],
                "e00200p": c["w2"],
                "e00900": c["se"],
                "e00900p": c["se"],
                "p22250": c["stcg"],
                "p23250": c["ltcg"],
            }
            for i, c in enumerate(cases)
        ]
    )
    records = taxcalc.Records(data=df, start_year=2024, gfactors=None, weights=None)
    calc = taxcalc.Calculator(policy=taxcalc.Policy(), records=records)
    calc.calc_all()
    arr = calc.array
    return [
        {
            "agi": (float(arr("c00100")[i]), COMPONENT_TOL),
            "taxable_income": (float(arr("c04800")[i]), COMPONENT_TOL),
            "se_tax": (float(arr("setax")[i]), COMPONENT_TOL),
            "niit": (float(arr("niit")[i]), COMPONENT_TOL),
            "addl_medicare": (float(arr("ptax_amc")[i]), COMPONENT_TOL),
            "total_tax": (
                float(arr("iitax")[i] + arr("setax")[i] + arr("ptax_amc")[i]),
                TAX_TABLE_TOL,
            ),
        }
        for i in range(len(cases))
    ]


@settings(
    max_examples=10,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.data_too_large],
)
@given(cases=st.lists(case_strategy, min_size=1, max_size=40))
@pytest.mark.parametrize("backend", ["ots", "graph"])
def test_components_match_taxcalc(backend, cases):
    """Every tenforty component quantity matches taxcalc within tolerance."""
    oracle = taxcalc_batch(cases)
    failures = []
    worst = {}
    for case, expected_row in zip(cases, oracle, strict=True):
        r = tenforty.evaluate_return(
            year=2024,
            filing_status=case["status"],
            w2_income=case["w2"],
            self_employment_income=case["se"],
            short_term_capital_gains=case["stcg"],
            long_term_capital_gains=case["ltcg"],
            backend=backend,
        )
        ours = {
            "agi": r.federal_adjusted_gross_income,
            "taxable_income": r.federal_taxable_income,
            "se_tax": r.federal_se_tax,
            "niit": r.federal_niit,
            "addl_medicare": r.federal_additional_medicare_tax,
            "total_tax": r.federal_total_tax,
        }
        for quantity, (expected, tol) in expected_row.items():
            diff = abs(ours[quantity] - expected)
            worst[quantity] = max(worst.get(quantity, 0.0), diff)
            if diff > tol:
                failures.append(f"{case}: {quantity} diff {diff:,.2f} > {tol}")
    # target() accepts one observation per label per example, so feed it the
    # batch maximum: hypothesis then steers toward batches containing the
    # largest disagreement.
    for quantity, diff in worst.items():
        target(diff, label=quantity)
    assert not failures, "\n".join(failures[:5])
