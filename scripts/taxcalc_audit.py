"""Differential audit: tenforty (OTS + graph) vs PSL Tax-Calculator.

Modes:
    python scripts/taxcalc_audit.py results.csv
        Full three-way sweep, long-format CSV (as in the original audit).
    python scripts/taxcalc_audit.py --goldens tests/oracle/fixtures/goldens.json
        Compute oracle expectations only and write a committed golden fixture
        (taxcalc version recorded; regenerate deliberately on oracle upgrades).
    python scripts/taxcalc_audit.py --check
        Three-way sweep with a verdict: disagreements are measured against the
        shared tolerance policy, classified against known-defect signatures,
        summarized, and any UNCLASSIFIED disagreement exits nonzero.

Scope notes:
- Federal only (taxcalc has no state model).
- No dependents, no ISO gains, no rental/schedule-1 income (taxcalc mapping
  for those is ambiguous or absent).
- Itemized deductions are mapped to taxcalc cash charity (e19800): no AGI
  floor below 60%, so it's the cleanest single-aggregate carrier.
- MFJ wage attribution: taxcalc requires per-spouse wages; tenforty's
  w2_income is a household aggregate. taxcalc runs under both attributions
  (all wages on the self-employed primary / all on the other spouse); a
  tenforty MFJ value passes if it falls within the bounds.
- Ages set to 40 to avoid 65+ standard deduction and EITC age edges.
"""

import argparse
import importlib.util
import json
import sys
import warnings
from datetime import date
from pathlib import Path

import pandas as pd

warnings.filterwarnings("ignore")

_POLICY_PATH = Path(__file__).parent.parent / "tests" / "oracle" / "oracle_policy.py"
_spec = importlib.util.spec_from_file_location("oracle_policy", _POLICY_PATH)
oracle_policy = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(oracle_policy)

STATUSES = ["Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow(er)"]
MARS = {
    "Single": 1,
    "Married/Joint": 2,
    "Married/Sep": 3,
    "Head_of_House": 4,
    "Widow(er)": 5,
}

SS_WAGE_BASE = {2024: 168_600, 2025: 176_100}
YEARS = (2024, 2025)

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


def build_cases() -> list[dict]:
    """Boundary-focused federal case grid over all supported years."""
    cases = []

    def add(year, tag, **kw):
        cases.append(
            {
                "year": year,
                "tag": tag,
                "status": kw.pop("status"),
                "w2": float(kw.pop("w2", 0.0)),
                "se": float(kw.pop("se", 0.0)),
                "stcg": float(kw.pop("stcg", 0.0)),
                "ltcg": float(kw.pop("ltcg", 0.0)),
                "interest": float(kw.pop("interest", 0.0)),
                "ord_div": float(kw.pop("ord_div", 0.0)),
                "qual_div": float(kw.pop("qual_div", 0.0)),
                "itemized": float(kw.pop("itemized", 0.0)),
                "std_or_item": kw.pop("std_or_item", "Standard"),
            }
        )
        assert not kw, kw

    for year in YEARS:
        base = SS_WAGE_BASE[year]
        for st in STATUSES:
            for w2 in (0, 50_000, 120_000, base, base + 31_400, 260_000, 400_000):
                for se in (0, 30_000, 60_000, 150_000, 300_000):
                    if w2 == 0 and se == 0:
                        continue
                    add(year, "A_se", status=st, w2=w2, se=se)

        for st in ("Single", "Married/Joint", "Head_of_House"):
            for w2 in (50_000, 150_000, 250_000, 400_000):
                for stcg in (0, 25_000, 60_000):
                    for ltcg in (0, 25_000, 60_000, 200_000):
                        if stcg == 0 and ltcg == 0:
                            continue
                        add(year, "B_gains", status=st, w2=w2, stcg=stcg, ltcg=ltcg)

        for st in ("Single", "Married/Joint"):
            for w2 in (60_000, 200_000, 300_000):
                for interest in (0, 15_000):
                    for od, qd in (
                        (20_000, 0),
                        (20_000, 12_000),
                        (20_000, 20_000),
                        (0, 12_000),
                    ):
                        add(
                            year,
                            "C_div",
                            status=st,
                            w2=w2,
                            interest=interest,
                            ord_div=od,
                            qual_div=qd,
                        )

        for st in ("Single", "Married/Joint"):
            for w2 in (100_000, 300_000):
                for item in (10_000, 40_000):
                    add(
                        year,
                        "D_item",
                        status=st,
                        w2=w2,
                        itemized=item,
                        std_or_item="Itemized",
                    )

        for st in STATUSES:
            add(
                year,
                "E_mix",
                status=st,
                w2=180_000,
                se=80_000,
                ltcg=50_000,
                ord_div=15_000,
                qual_div=10_000,
                interest=5_000,
            )

    for i, c in enumerate(cases):
        c["case_id"] = i
    return cases


def run_tenforty(cases, backend):
    """Evaluate every case on one tenforty backend; errors recorded, not raised."""
    import tenforty

    rows = {}
    for c in cases:
        try:
            r = tenforty.evaluate_return(
                year=c["year"],
                filing_status=c["status"],
                backend=backend,
                w2_income=c["w2"],
                self_employment_income=c["se"],
                short_term_capital_gains=c["stcg"],
                long_term_capital_gains=c["ltcg"],
                taxable_interest=c["interest"],
                ordinary_dividends=c["ord_div"],
                qualified_dividends=c["qual_div"],
                itemized_deductions=c["itemized"],
                standard_or_itemized=c["std_or_item"],
            )
            rows[c["case_id"]] = {
                "agi": r.federal_adjusted_gross_income,
                "taxable_income": r.federal_taxable_income,
                "se_tax": r.federal_se_tax,
                "niit": r.federal_niit,
                "addl_medicare": r.federal_additional_medicare_tax,
                "amt": r.federal_amt,
                "income_tax": r.federal_income_tax,
                "total_tax": r.federal_total_tax,
            }
        except Exception as exc:
            rows[c["case_id"]] = {"error": f"{type(exc).__name__}: {exc}"}
    return rows


def run_taxcalc(cases, wage_attribution="primary"):
    """Evaluate cases on taxcalc, one vectorized call per year."""
    import taxcalc as tc

    out = {}
    for year in sorted({c["year"] for c in cases}):
        year_cases = [c for c in cases if c["year"] == year]
        recs = []
        for c in year_cases:
            mars = MARS[c["status"]]
            qd = c["qual_div"]
            od = max(c["ord_div"], qd)
            wages_on_spouse = wage_attribution == "spouse" and mars == 2
            recs.append(
                {
                    "RECID": c["case_id"] + 1,
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
                    "e00600": od,
                    "e00650": qd,
                    "p22250": c["stcg"],
                    "p23250": c["ltcg"],
                    "e19800": c["itemized"],
                }
            )
        df = pd.DataFrame(recs)
        records = tc.Records(data=df, start_year=year, gfactors=None, weights=None)
        calc = tc.Calculator(policy=tc.Policy(), records=records)
        calc.advance_to_year(year)
        calc.calc_all()
        arr = calc.array
        for i, c in enumerate(year_cases):
            iitax = float(arr("iitax")[i])
            setax = float(arr("setax")[i])
            amc = float(arr("ptax_amc")[i])
            niit = float(arr("niit")[i])
            out[c["case_id"]] = {
                "agi": float(arr("c00100")[i]),
                "taxable_income": float(arr("c04800")[i]),
                "se_tax": setax,
                "niit": niit,
                "addl_medicare": amc,
                "amt": float(arr("c09600")[i]),
                # taxcalc iitax includes NIIT; tenforty's federal_income_tax
                # excludes it (and SE tax and additional Medicare).
                "income_tax": iitax - niit,
                "total_tax": iitax + setax + amc,
                "qbided": float(arr("qbided")[i]),
            }
    return out


def oracle_expectations(cases):
    """Primary-attribution expectations plus MFJ spouse-attribution bounds."""
    primary = run_taxcalc(cases, "primary")
    mfj = [c for c in cases if c["status"] == "Married/Joint"]
    spouse = run_taxcalc(mfj, "spouse") if mfj else {}
    return primary, spouse


def write_goldens(cases, path):
    """Write committed golden fixtures with oracle metadata."""
    import taxcalc as tc

    primary, spouse = oracle_expectations(cases)
    payload = {
        "meta": {
            "taxcalc_version": tc.__version__,
            "generated": date.today().isoformat(),
            "years": sorted({c["year"] for c in cases}),
            "n_cases": len(cases),
        },
        "cases": [
            {
                **{k: c[k] for k in c if k != "case_id"},
                "expected": {q: primary[c["case_id"]][q] for q in QUANTITIES},
                "expected_spouse_attr": (
                    {q: spouse[c["case_id"]][q] for q in QUANTITIES}
                    if c["case_id"] in spouse
                    else None
                ),
            }
            for c in cases
        ],
    }
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    Path(path).write_text(json.dumps(payload, indent=1) + "\n")
    print(f"wrote {len(cases)} golden cases to {path}", file=sys.stderr)


def check(cases):
    """Three-way verdict run. Exit nonzero on any unclassified disagreement."""
    primary, spouse = oracle_expectations(cases)
    ots = run_tenforty(cases, "ots")
    graph = run_tenforty(cases, "graph")

    unclassified, known, errors = [], [], []
    for c in cases:
        cid = c["case_id"]
        exp = primary[cid]
        exp_alt = spouse.get(cid)
        for backend, rows in (("ots", ots), ("graph", graph)):
            got = rows[cid]
            if "error" in got:
                errors.append((backend, c, got["error"]))
                continue
            excused = oracle_policy.excused_quantities(backend, c)
            for q in QUANTITIES:
                tol = oracle_policy.tolerance(backend, q, exp["taxable_income"], c)
                lo = hi = exp[q]
                if exp_alt is not None:
                    lo, hi = min(lo, exp_alt[q]), max(hi, exp_alt[q])
                if lo - tol <= got[q] <= hi + tol:
                    continue
                record = (backend, c, q, got[q], exp[q])
                (known if q in excused else unclassified).append(record)

    print(
        f"cases={len(cases)} known_disagreements={len(known)} "
        f"unclassified={len(unclassified)} backend_errors={len(errors)}"
    )
    for backend, c, err in errors:
        print(f"ERROR  {backend} case={c}: {err}")
    for backend, c, q, got, exp in unclassified[:25]:
        print(f"UNCLASSIFIED  {backend} {q} got={got:,.2f} expected={exp:,.2f} {c}")
    return 1 if (unclassified or errors) else 0


def main():
    """Entry point for the audit script's three modes."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("csv", nargs="?", default=None)
    parser.add_argument("--goldens", metavar="PATH")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()

    cases = build_cases()
    print(f"{len(cases)} cases", file=sys.stderr)

    if args.goldens:
        write_goldens(cases, args.goldens)
        return 0
    if args.check:
        return check(cases)

    primary, spouse = oracle_expectations(cases)
    ots = run_tenforty(cases, "ots")
    graph = run_tenforty(cases, "graph")
    rows = []
    for c in cases:
        cid = c["case_id"]
        base = {k: v for k, v in c.items() if k != "case_id"}
        base["taxcalc_qbided"] = primary[cid].get("qbided", 0.0)
        for q in QUANTITIES:
            row = dict(base)
            row["quantity"] = q
            row["taxcalc"] = primary[cid].get(q)
            row["taxcalc_spouse_attr"] = spouse.get(cid, {}).get(q)
            row["ots"] = ots[cid].get(q)
            row["graph"] = graph[cid].get(q)
            row["ots_error"] = ots[cid].get("error", "")
            row["graph_error"] = graph[cid].get("error", "")
            rows.append(row)
    out = pd.DataFrame(rows)
    out.to_csv(args.csv or "audit_results.csv", index=False)
    print("done", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
