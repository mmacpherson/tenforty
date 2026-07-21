# Differential Audit: tenforty vs PSL Tax-Calculator

Branch: `audit/taxcalc-differential`, cut from main as of 2026-07-21,
immediately before PR #279 merged.
Update 2026-07-21: PR #279 has since merged, fixing F1/F2; the remaining
findings are burned into the suite as strict xfails in
`tests/known_defects_test.py`.
Oracle: [Tax-Calculator](https://github.com/PSLmodels/Tax-Calculator) (`taxcalc` 6.7.2, CC0),
federal only, tax year 2024.

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

Found incidentally while benchmarking, tracked as `tenforty-i7n` (P0).
`evaluate_returns(backend="graph", mode="cross")` — the default mode — returns
`python_grid × rust_axis` rows instead of the grid (`w2_income=[10k,20k,30k]`
→ 9 rows; a 2×3 grid → 18), and the input columns cycle at a different rate
than the results, so rows pair wrong inputs with wrong outputs — only the
diagonal is correct. OTS cross/zip and graph zip are all correct. Prior
audits used one-row batch cases, where 1×1 = 1 row masks the explosion
entirely — another instance of easy scenarios hiding a broken path.

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
