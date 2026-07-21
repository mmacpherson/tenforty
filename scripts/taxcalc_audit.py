"""Differential audit: tenforty (OTS + graph) vs PSL Tax-Calculator.

Generates a boundary-focused federal case grid, evaluates every case on all
three engines, and writes a long-format CSV of per-quantity comparisons.

Scope notes (v1):
- Federal only (taxcalc has no state model).
- No dependents, no ISO gains, no rental/schedule-1 income (taxcalc mapping
  for those is ambiguous or absent).
- Itemized deductions are mapped to taxcalc cash charity (e19800): no AGI
  floor below 60%, so it's the cleanest single-aggregate carrier.
- MFJ wage attribution: taxcalc requires per-spouse wages; tenforty's
  w2_income is a household aggregate. We run taxcalc twice for MFJ SE cases:
  wages all on the self-employed primary ("primary") and wages all on the
  other spouse ("spouse"). These bracket the truth for tenforty's aggregate.
- Ages set to 40 to avoid 65+ standard deduction and EITC age edges.
"""

import sys
import warnings

import pandas as pd

warnings.filterwarnings("ignore")

STATUSES = ["Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow(er)"]
MARS = {
    "Single": 1,
    "Married/Joint": 2,
    "Married/Sep": 3,
    "Head_of_House": 4,
    "Widow(er)": 5,
}

SS_WAGE_BASE = 168_600  # 2024


def build_cases() -> list[dict]:
    cases = []

    def add(tag, **kw):
        cases.append({"tag": tag, **kw})

    # A: SE tax / SS wage base / additional Medicare / QBI interactions
    for st in STATUSES:
        for w2 in (0, 50_000, 120_000, SS_WAGE_BASE, 200_000, 260_000, 400_000):
            for se in (0, 30_000, 60_000, 150_000, 300_000):
                if w2 == 0 and se == 0:
                    continue
                add("A_se", filing_status=st, w2_income=w2, self_employment_income=se)

    # B: capital gains / NIIT thresholds
    for st in ("Single", "Married/Joint", "Head_of_House"):
        for w2 in (50_000, 150_000, 250_000, 400_000):
            for stcg in (0, 25_000, 60_000):
                for ltcg in (0, 25_000, 60_000, 200_000):
                    if stcg == 0 and ltcg == 0:
                        continue
                    add(
                        "B_gains",
                        filing_status=st,
                        w2_income=w2,
                        short_term_capital_gains=stcg,
                        long_term_capital_gains=ltcg,
                    )

    # C: interest and dividends (qualified subset of ordinary)
    for st in ("Single", "Married/Joint"):
        for w2 in (60_000, 200_000, 300_000):
            for intr in (0, 15_000):
                for od, qd in (
                    (20_000, 0),
                    (20_000, 12_000),
                    (20_000, 20_000),
                    (0, 12_000),
                ):
                    add(
                        "C_div",
                        filing_status=st,
                        w2_income=w2,
                        taxable_interest=intr,
                        ordinary_dividends=od,
                        qualified_dividends=qd,
                    )

    # D: forced itemization (mapped to charity on taxcalc side)
    for st in ("Single", "Married/Joint"):
        for w2 in (100_000, 300_000):
            for item in (10_000, 40_000):
                add(
                    "D_item",
                    filing_status=st,
                    w2_income=w2,
                    itemized_deductions=item,
                    standard_or_itemized="Itemized",
                )

    # E: kitchen sink
    for st in STATUSES:
        add(
            "E_mix",
            filing_status=st,
            w2_income=180_000,
            self_employment_income=80_000,
            long_term_capital_gains=50_000,
            ordinary_dividends=15_000,
            qualified_dividends=10_000,
            taxable_interest=5_000,
        )

    for i, c in enumerate(cases):
        c["case_id"] = i
    return cases


TENFORTY_FIELDS = (
    "w2_income",
    "taxable_interest",
    "ordinary_dividends",
    "qualified_dividends",
    "short_term_capital_gains",
    "long_term_capital_gains",
    "self_employment_income",
    "itemized_deductions",
)

QUANTITIES = (
    "agi",
    "taxable_income",
    "se_tax",
    "niit",
    "addl_medicare",
    "amt",
    "total_tax",
)


def run_tenforty(cases, backend):
    import tenforty

    rows = {}
    for c in cases:
        kwargs = {k: c.get(k, 0.0) for k in TENFORTY_FIELDS}
        if c.get("standard_or_itemized"):
            kwargs["standard_or_itemized"] = c["standard_or_itemized"]
        try:
            r = tenforty.evaluate_return(
                year=2024,
                filing_status=c["filing_status"],
                backend=backend,
                **kwargs,
            )
            rows[c["case_id"]] = {
                "agi": r.federal_adjusted_gross_income,
                "taxable_income": r.federal_taxable_income,
                "se_tax": r.federal_se_tax,
                "niit": r.federal_niit,
                "addl_medicare": r.federal_additional_medicare_tax,
                "amt": r.federal_amt,
                "total_tax": r.federal_total_tax,
            }
        except Exception as exc:
            rows[c["case_id"]] = {"error": f"{type(exc).__name__}: {exc}"}
    return rows


def run_taxcalc(cases, wage_attribution="primary"):
    import taxcalc as tc

    recs = []
    for c in cases:
        mars = MARS[c["filing_status"]]
        w2 = float(c.get("w2_income", 0.0))
        se = float(c.get("self_employment_income", 0.0))
        qd = float(c.get("qualified_dividends", 0.0))
        od = max(float(c.get("ordinary_dividends", 0.0)), qd)
        wages_on_spouse = wage_attribution == "spouse" and mars == 2
        recs.append(
            {
                "RECID": c["case_id"] + 1,
                "MARS": mars,
                "XTOT": 2 if mars == 2 else 1,
                "age_head": 40,
                "age_spouse": 40 if mars == 2 else 0,
                "e00200": w2,
                "e00200p": 0.0 if wages_on_spouse else w2,
                "e00200s": w2 if wages_on_spouse else 0.0,
                "e00900": se,
                "e00900p": se,
                "e00900s": 0.0,
                "e00300": float(c.get("taxable_interest", 0.0)),
                "e00600": od,
                "e00650": qd,
                "p22250": float(c.get("short_term_capital_gains", 0.0)),
                "p23250": float(c.get("long_term_capital_gains", 0.0)),
                "e19800": float(c.get("itemized_deductions", 0.0)),
            }
        )
    df = pd.DataFrame(recs)
    records = tc.Records(data=df, start_year=2024, gfactors=None, weights=None)
    calc = tc.Calculator(policy=tc.Policy(), records=records)
    calc.advance_to_year(2024)
    calc.calc_all()

    out = {}
    arr = calc.array
    for i, c in enumerate(cases):
        iitax = float(arr("iitax")[i])
        setax = float(arr("setax")[i])
        amc = float(arr("ptax_amc")[i])
        out[c["case_id"]] = {
            "agi": float(arr("c00100")[i]),
            "taxable_income": float(arr("c04800")[i]),
            "se_tax": setax,
            "niit": float(arr("niit")[i]),
            "addl_medicare": amc,
            "amt": float(arr("c09600")[i]),
            "total_tax": iitax + setax + amc,
            "qbided": float(arr("qbided")[i]),
        }
    return out


def main():
    cases = build_cases()
    print(f"{len(cases)} cases", file=sys.stderr)

    tc_primary = run_taxcalc(cases, "primary")
    mfj_cases = [c for c in cases if c["filing_status"] == "Married/Joint"]
    tc_spouse = run_taxcalc(mfj_cases, "spouse") if mfj_cases else {}
    ots = run_tenforty(cases, "ots")
    graph = run_tenforty(cases, "graph")

    rows = []
    for c in cases:
        cid = c["case_id"]
        base = {
            k: c.get(k, 0)
            for k in ("case_id", "tag", "filing_status", *TENFORTY_FIELDS)
        }
        base["taxcalc_qbided"] = tc_primary[cid].get("qbided", 0.0)
        for q in QUANTITIES:
            row = dict(base)
            row["quantity"] = q
            row["taxcalc"] = tc_primary[cid].get(q)
            row["taxcalc_spouse_attr"] = tc_spouse.get(cid, {}).get(q)
            row["ots"] = ots[cid].get(q)
            row["graph"] = graph[cid].get(q)
            row["ots_error"] = ots[cid].get("error", "")
            row["graph_error"] = graph[cid].get("error", "")
            rows.append(row)

    out = pd.DataFrame(rows)
    out.to_csv(sys.argv[1] if len(sys.argv) > 1 else "audit_results.csv", index=False)
    print("done", file=sys.stderr)


if __name__ == "__main__":
    main()
