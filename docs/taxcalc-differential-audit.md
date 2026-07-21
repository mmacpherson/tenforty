# Differential Audit: tenforty vs PSL Tax-Calculator

Branch: `audit/taxcalc-differential`, cut from main as of 2026-07-21,
immediately before PR #279 merged.
Update 2026-07-21: PR #279 has since merged, fixing F1/F2; the remaining
findings are burned into the suite as strict xfails in
`tests/known_defects_test.py`.
Oracle: [Tax-Calculator](https://github.com/PSLmodels/Tax-Calculator) (`taxcalc` 6.7.2, CC0),
federal only, tax year 2024.

## Findings ledger

Every disagreement class gets an ID here, a narrative section below, a
strict-xfail burn-in in `tests/known_defects_test.py`, and an excusing
signature in `tests/oracle/oracle_policy.py`. Fixes must flip the burn-in,
delete the signature, and update this table in the same PR.

| ID | Finding | At fault | Status | Found by |
|----|---------|----------|--------|----------|
| F1 | Schedule SE L8a never filled | mapping, both backends | fixed (#279, v2025.11) | @bg002h, #278 |
| F2 | SE-tax error propagates to AGI | consequence of F1 | fixed with F1 | @bg002h, #278 |
| F3 | QBI: missing (OTS) / gross base (graph) | OTS orchestration + graph spec | open | @bg002h, #278 |
| F4 | Form 8960 L5a omits short-term gains | mapping, both backends | open | mapping assessment + oracle sweep |
| F5 | Graph Form 8959 omits SE earnings | graph mapping | open | mapping assessment + oracle sweep |
| F6 | OTS 8959 never fires with zero wages | OTS activation semantics | open | oracle sweep |
| F7 | "Itemized" force vs best-of divergence | API contract | open (owner decision) | oracle sweep |
| F8 | Cross-mode batch grid explosion | graph batch path | fix in PR #287 | benchmark |
| F9 | Batch path bypasses TaxReturnInput | graph batch path | open | batch-conformance tests |
| F10 | Short-term gains taxed at preferential rates | graph spec | open | oracle grid |
| F11 | 2024 HoH 32% bracket starts \$191,150, not \$191,950 | upstream OpenTaxSolver | adjudicated vs IRS; upstream report pending | oracle grid |
| F12 | Itemized-deduction category changes AMT | API (input model v2) | open (design) | adversarial search |
| F13 | 2025 MFS long-term-gain thresholds high | graph spec (suspect) | open | oracle grid |

## Method

363 boundary-focused federal cases (SS wage base, NIIT/additional-Medicare
thresholds, QBI interactions, capital-gain mixes, dividend subsets, forced
itemization) across all five filing statuses, evaluated on three engines:
tenforty OTS, tenforty graph, and taxcalc. Seven quantities compared per case:
AGI, taxable income, SE tax, NIIT, additional Medicare tax, AMT, total tax.

- Tolerances: $2, except $15 for total tax (OTS uses the 1040 tax tables,
  which quantize taxable income in $50 steps; taxcalc uses exact formulas).
- MFJ wage attribution: taxcalc requires per-spouse wages; tenforty's
  `w2_income` is a household aggregate. taxcalc was run with wages attributed
  to the self-employed primary and separately to the other spouse; a tenforty
  MFJ value is flagged only if it falls outside both bounds.
- Out of scope: states (taxcalc has none), dependents/credits, ISO/AMT
  preference items, rental and schedule-1 income, batch evaluation paths.

Harness: `audit_harness.py` + `analyze*.py` (session scratchpad; candidates
for `scripts/` promotion).

## Findings

### F1. Schedule SE line 8a never filled — issue #278 recapitulated blind

Flagged exactly 22 cases in each of the four single-person statuses; zero
flags at `w2=0`; zero MFJ flags (MFJ falls inside the attribution bounds,
matching the wages-on-other-spouse reading). The harness independently
rediscovered not only the bug but the precise per-status fix boundary that
PR #279 implements. Max overcharge in grid: $20,906 of SE tax
(Married/Sep, w2 $400k, SE $300k... representative high case).

### F2. SE-tax error propagates to AGI

Across all flagged cases, AGI error = −0.5 × SE-tax error to five decimal
places (the §164(f) half-SE deduction). Fixing F1 also fixes AGI, MAGI, and
everything downstream of them.

### F3. §199A QBI — issue #278 part 2 recapitulated, both directions

- OTS: taxable income overstated by exactly taxcalc's `qbided` plus the F2
  AGI shift — the decomposition closes to within $2 in **all 145** SE-income
  cases. Pure missing orchestration: no Form 8995 config exists.
- Graph: the spec **does** implement the 20%-of-taxable-income limitation
  (a first-pass read of this audit said otherwise). Its sole defect is the
  base: Form 8995 L1 receives *gross* Schedule C profit instead of profit
  net of the §164(f) half-SE deduction. When the base term binds the error
  is 20% × the half-SE deduction (e.g., $1,130.36 at Single w2 $50k /
  SE $80k); when the cap binds (the grid's 20 `w2=0` cases) the graph is
  exactly correct. The 125-case "20% of gross" fit and the up-to-$70,453
  taxable understatement are the base-bound cases.
- Above the §199A thresholds the correct deduction additionally depends on
  business W-2 wages / UBIA / SSTB status — concepts absent from tenforty's
  API (taxcalc assumes zero business wages, phasing the deduction to zero
  above the upper threshold). Any fix must choose an assumption there and
  document it.

### F4. NIIT omits short-term capital gains — shared omission, both backends

All 56 NIIT flags have STCG > 0; the error is −3.8% × STCG (or MAGI-limited).
Root cause is symmetrical: Form 8960 line 5a ("net gain from disposition of
property") is mapped from `long_term_capital_gains` only —
`models.py` OTS input_map and `mappings.py` graph fan-out both lack
`short_term_capital_gains`. Backend parity can never catch this class.

### F5. Graph omits SE earnings from Form 8959 (additional Medicare tax)

145 graph flags, every one with SE income > 0. OTS maps SE earnings via
`_se_income_to_8959_l8`; the graph fan-out has no SE edge into
`us_form_8959`. Confirms the prior mapping-layer assessment finding.

### F6. NEW — OTS Form 8959 never fires with zero W-2 wages

`evaluate_return(year=2024, filing_status="Single", self_employment_income=300_000)`
returns additional Medicare tax $0.00; with `w2_income=1` it returns $693.46.
Cause: the Phase-1 activation check (`core.py:455`) counts only `int`/`float`
values, but `_se_income_to_8959_l8` returns its value as a *string*
(`("L8", str(round(...)))`), and `Status` is also a string. With `w2_income=0`
no numeric value survives and the form is skipped. Missed tax up to $1,368
(Married/Sep, SE $300k) in the grid. A formatting decision leaked into
activation semantics — exactly the "activation" contract dimension the
mapping-layer assessment called out.

### F8. NEW — Graph cross-mode batch returns an exploded, misaligned grid

Found incidentally while benchmarking; priority-critical.
`evaluate_returns(backend="graph", mode="cross")` — the default mode — returns
`python_grid × rust_axis` rows instead of the grid (`w2_income=[10k,20k,30k]`
→ 9 rows; a 2×3 grid → 18), and the input columns cycle at a different rate
than the results, so rows pair wrong inputs with wrong outputs — only the
diagonal is correct. OTS cross/zip and graph zip are all correct. Prior
audits used one-row batch cases, where 1×1 = 1 row masks the explosion
entirely — another instance of easy scenarios hiding a broken path.

### F9. NEW — Graph batch path bypasses TaxReturnInput normalization

Found by the oracle suite's batch-conformance tests (added post-audit).
`GraphBackend.evaluate_batch` consumes raw input columns, skipping pydantic
model validators and computed fields. Two confirmed symptoms: the
qualified>ordinary dividend lift is not applied (Single, w2 $60k, qualified
dividends $12k: scalar AGI $72,000, zip AGI $60,000; OTS zip correct), and
the `schedule_se_ss_wages` derivation is absent (the batch xfail shipped
with PR #279). Durable fix: route batch rows through `TaxReturnInput`, or
move the derivations into the graph spec.

### F10. NEW — Graph taxes short-term capital gains at preferential rates

Caught by the widened oracle grid (48 flagged cases). With pure short-term
gains the graph computes the long-term preferential rate: Single, $50k wages
+ $25k STCG gives income tax $6,022.25 vs the correct $8,341.00 (OTS and
taxcalc agree). Short-term gains are ordinary income; the spec's Schedule D
/ qualified-rate worksheet appears to treat all net gains as long-term.
Form-calculation defect in the graph spec, not a mapping edge.

### F11. NEW, ADJUDICATED — upstream OTS 2024 Head-of-House bracket typo

For 2024 Head-of-House filers, OTS begins the 32% bracket at taxable income
**$191,150**; the IRS (Rev. Proc. 2023-34) says **$191,950** — taxcalc and
the graph spec both carry the correct figure. Result: a flat $64.00
overcharge (8% x $800) for every 2024 HoH return with taxable income at or
above the true boundary. Bisection of OTS's marginal rate pinpoints the
boundary exactly; the 2025 table is correct ($197,300), so this is an
isolated one-digit transposition (191,150 vs 191,950) in the 2024 table —
an *upstream OpenTaxSolver* defect, to be reported upstream. First catch
for the three-way adjudication method: the oracle and the independent
in-house engine outvoted the incumbent, and the revenue procedure confirmed
the majority.

### F12. NEW — itemized_deductions category ambiguity changes AMT

Found by the adversarial hypothesis search (MFS, $250k of gains, $33,410
itemized). All three engines take the same deduction and agree on taxable
income — but OTS reports $1,634.46 of AMT where graph and taxcalc report
none. Cause: the aggregate rides in a different Schedule A category per
engine — OTS maps it to A6 ("other taxes", added back on Form 6251), graph
to L16 ("other deductions", not added back), the oracle adapter to charity
(not added back). None is wrong; the input model cannot say which kind of
deduction it is. This is the categorized-deductions API gap made concrete,
resolved by input model v2; until then the divergence is a documented
assumption, excused by signature.

### F13. NEW — graph 2025 Married/Sep long-term-gain thresholds diverge

Six grid cases, 2025 + MFS + LTCG only: graph income tax is $1,377–$1,665
above the OTS+taxcalc consensus. The pattern fits a preferential-rate
breakpoint error for MFS in the 2025 spec parameters (MFS thresholds are
not always half of Single). 2-vs-1 against the graph; fix and adjudicate in
the spec bundle.

### F7. Itemization semantics diverge between backends

With `standard_or_itemized="Itemized"` and deductions below the standard
deduction, OTS *forces* itemization (higher tax) while graph takes
best-of-both (agreeing with taxcalc, which cannot force). With deductions
above the standard deduction all three agree. The API contract for
"Itemized" is currently implemented two different ways.

### Clean areas

Dividends (including the qualified-subset normalization), interest, AMT (no
ISO cases in grid), and all capital-gains cases apart from the NIIT edge —
scalar paths only. The previously reported qualified-dividend and AMT-output
bugs are batch-path defects this scalar harness deliberately did not probe.

## Recommended changes, by layer

1. **Merge PR #279** (F1/F2 for single-person statuses; correct as far as it
   goes).
2. **Graph spec (Haskell)** — four edits, all of which then flow to every
   execution path (scalar, batch, gradient, solver) for free:
   - compute the filer's SS wages in-spec (retires PR #279's batch xfail);
   - Form 8995: feed QBI net of the half-SE deduction — the taxable-income
     limitation is already implemented in-spec (F3);
   - Form 8959: add the SE-earnings edge (F5);
   - Form 8960 line 5a: include short-term gains (F4).
3. **OTS backend (Python)**:
   - Form 8960 input_map: add `short_term_capital_gains` summed into L5a (F4);
   - activation: decide form firing from the *natural inputs consumed*, not
     the post-format values (F6);
   - Form 8995: new phase-2 subordinate config fed by
     taxable-income-before-QBI from the 1040 pass (F3), with the phase-out
     assumption documented.
4. **API decisions** (blocking full correctness, owner's call):
   - per-spouse wage/SE fields (completes F1 for MFJ; taxcalc's
     `e00200p`/`e00200s` is a working reference);
   - define "Itemized": force vs best-of, then make both backends match (F7);
   - §199A above-threshold assumption (F3).
5. **Keep the oracle**: promote the harness to `scripts/`, commit a pinned
   golden fixture set at the semantic boundaries, and run the three-way sweep
   as a scheduled/pre-release job. Parity catches divergence; only an
   independent oracle catches shared omissions (F4 was invisible to parity by
   construction).
