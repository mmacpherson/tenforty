# Differential Audit: tenforty vs PSL Tax-Calculator

Branch: `audit/taxcalc-differential`, cut from main as of 2026-07-21,
immediately before PR #279 merged.
Update 2026-07-21: PR #279 has since merged, fixing F1/F2; the remaining
findings are burned into the suite as strict xfails in
`tests/known_defects_test.py`.
Reference: [Tax-Calculator](https://github.com/PSLmodels/Tax-Calculator) (`taxcalc` 6.7.2, CC0),
federal only, tax years 2024 and 2025.

## Findings ledger

Every disagreement class gets an ID here, a narrative section below, a
strict-xfail burn-in, and a bounded-delta signature in
`tests/taxcalc/taxcalc_policy.py`. Fixes must flip the burn-in, delete the
signature, and update this table in the same PR.

| ID | Finding | At fault | Status | Found by |
|----|---------|----------|--------|----------|
| F1 | Schedule SE L8a never filled | mapping, both backends | fixed (#279, v2025.11) | @bg002h, #278 |
| F2 | SE-tax error propagates to AGI | consequence of F1 | fixed with F1 | @bg002h, #278 |
| F3 | QBI: missing (OTS) / above-threshold limit (graph) | OTS orchestration + graph spec | fixed (tenforty-6hr, tenforty-mhe, tenforty-2u6) | @bg002h, #278 |
| F4 | Form 8960 L5a omits short-term gains | mapping, both backends | fixed (OTS #296, graph: L5a imports Schedule D L16) | mapping assessment + differential sweep |
| F5 | Graph Form 8959 Part II drops SE earnings (line 12 used min, not subtract) | graph spec | fixed | mapping assessment + differential sweep |
| F6 | OTS 8959 never fires with zero wages | OTS activation semantics | fixed | differential sweep |
| F7 | TaxCalc cannot express forced below-standard itemization | TaxCalc capability gap | tenforty fixed (`tenforty-435`); TaxCalc gap open | differential sweep |
| F8 | Cross-mode batch grid explosion | graph batch path | fix in PR #287 | benchmark |
| F9 | Batch path bypasses TaxReturnInput | graph batch path | fixed (tenforty-tve) | batch-conformance tests |
| F10 | Short-term gains taxed at preferential rates (QCGWS line 3) | graph spec | fixed | differential grid |
| F11 | 2024 HoH 32% bracket starts \$191,150, not \$191,950 | upstream OpenTaxSolver | adjudicated vs IRS; upstream report pending — not patched locally, we vendor OTS unmodified | differential grid |
| F12 | Itemized-deduction category changes AMT | API (input model v2) | open (design) | adversarial search |
| F13 | 2025 MFS 15%-rate ceiling wrong (266,700 vs 300,000), inline in 1040 not Tables | graph spec | fixed | differential grid |
| F14 | AMT std-deduction add-back divergence (ISO cases) | taxcalc (and graph, now fixed) | graph fixed (tenforty-8ik); taxcalc omits add-back, upstream note pending | H_amt stratum |
| F16 | Suite adapter drops `iso`, so taxcalc sees no AMT preference | taxcalc harness | fixed (#295) | F14 adjudication |
| F17 | Graph charges SE tax below the \$400 de-minimis floor | graph spec | fixed (tenforty-dw0, #297) | differential sweep |
| F19 | Deduction choice: taxcalc minimizes tax; tenforty maximizes the deduction | API contract, both backends | open (documented signature; tenforty-z31) | differential sweep |
| F20 | Suite adapter compares post-refund `iitax` with pre-refund tenforty tax | taxcalc harness | fixed (tenforty-jte) | differential sweep |
| F21 | 199A QBI phase-in range is doubled for qualifying widow(er) | upstream TaxCalc parameter | open (tenforty-2jg; upstream report pending) | Form 8995-A adjudication |
| F22 | OTS cancels the AMT standard-deduction add-back when taxable income floors at zero | upstream OpenTaxSolver | open (tenforty-by2; upstream report pending) | randomized ISO differential |
| F23 | MFS high-income AMTI increase omitted | graph spec + upstream TaxCalc | graph fixed (tenforty-3bt); TaxCalc open (tenforty-doo) | randomized high-income ISO differential |
| F24 | OTS 2024 MFS AMTI increase uses stale 2023 constants | upstream OpenTaxSolver | open (tenforty-2jv; upstream report pending) | F23 adjudication |
| F25 | Graph Form 6251 uses stale 2025 MFS 15%-gain ceiling | graph spec | fixed (tenforty-c9a) | randomized MFS ISO/gain differential |
| F26 | TaxCalc lets unused itemization reduce AMTI below the taxable-income floor | upstream TaxCalc | open (tenforty-w7t; upstream report pending) | randomized itemized ISO/loss differential |
| F27 | OTS skips the AMT preferential-rate worksheet when regular taxable income is zero | upstream OpenTaxSolver | open (tenforty-909.1; upstream report pending) | randomized zero-taxable-income ISO/dividend differential |

## Method

1,179 boundary-focused federal cases (SS wage base, NIIT/additional-Medicare
thresholds, QBI interactions, capital-gain mixes, dividend subsets, forced
itemization) across all five filing statuses, evaluated on three engines:
tenforty OTS, tenforty graph, and taxcalc. Eight quantities compared per case:
AGI, taxable income, SE tax, NIIT, additional Medicare tax, AMT, income tax,
and total tax.

- Tolerances: $2, except $15 for total tax (OTS uses the 1040 tax tables,
  which quantize taxable income in $50 steps; taxcalc uses exact formulas).
- MFJ wage attribution: taxcalc requires per-spouse wages; tenforty's
  `w2_income` is a household aggregate. taxcalc was run with wages attributed
  to the self-employed primary and separately to the other spouse; a tenforty
  MFJ value is flagged only if it falls outside both bounds.
- Out of scope: states (taxcalc has none), dependents/credits, rental and
  schedule-1 income. ISO/AMT preference cases and scalar/batch identity are now
  covered; TaxCalc itself has no tenforty-style batch API to compare directly.

Harness: `scripts/taxcalc_audit.py`; the committed fixture records the exact
TaxCalc version and can be regenerated or checked byte-for-semantics from that
canonical case grid.

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
  (a first-pass read of this audit said otherwise). Its sole defect was the
  base: Form 8995 L1 received *gross* Schedule C profit instead of profit
  net of the §164(f) half-SE deduction. When the base term binds the error
  was 20% × the half-SE deduction (e.g., $1,130.36 at Single w2 $50k /
  SE $80k); when the cap binds (the grid's 20 `w2=0` cases) the graph was
  already exactly correct. **Fixed (tenforty-6hr):** Form 8995 now imports
  Schedule SE line 11 and nets it out before the 20%, so below the §199A
  threshold the graph agrees with taxcalc to the cent (verified at both the
  base-bound Single w2 $50k / SE $80k → taxable $94,878.54 and the cap-bound
  MFJ SE $80k → taxable $36,118.54).
- **Form 8995-A fixed (tenforty-mhe):** the graph now accepts business W-2
  wages, UBIA, and SSTB status for one trade or business (or a valid
  aggregation), and applies the statutory wage/UBIA limitation and SSTB
  applicable percentage through the phase-in range. The zero-wage default
  phases the component to zero above the range; nonzero wage, UBIA, and SSTB
  cases agree with TaxCalc where its parameters are correct. The previously
  dead threshold table is now live, with qualifying widow(er) corrected to the
  all-other-returns threshold and $50,000 phase-in range.
- **OTS fixed (tenforty-2u6):** a preliminary OTS 1040 now feeds OTS's own Form
  8995, preserving an independent QBI base and taxable-income/capital-gain
  ceiling. Tenforty's orchestration applies the Form 8995-A wage, UBIA, and SSTB
  phase calculation that OTS does not implement, injects the resulting deduction,
  and reruns the 1040. The F3 differential signature is gone for both backends.
  Deterministic anchors cover the net QBI base, capital-gain limitation,
  zero-wage phase-in, both wage/UBIA branches, and SSTB treatment. F21 separately
  records TaxCalc's qualifying-widow(er) phase-range defect; it is not attributed
  to F3 or copied into the graph.

### F4. NIIT omits short-term capital gains — shared omission, both backends

All 56 NIIT flags have STCG > 0; the error is −3.8% × STCG (or MAGI-limited).
Root cause is symmetrical: Form 8960 line 5a ("net gain from disposition of
property") is mapped from `long_term_capital_gains` only —
`models.py` OTS input_map and `mappings.py` graph fan-out both lack
`short_term_capital_gains`. Backend parity can never catch this class.

**Fixed on OTS, at a different layer than first proposed.** Line 5a is not
reconstructed from the gain naturals; it is imported from Form 1040 line 7 via
`fed_import_map`, which is what Form 8960 instructs. Line 7 is the Schedule D
result, so both holding periods are already netted and the section 1211(b)
$3,000 loss limitation is already applied — an earlier attempt that summed the
two gain naturals onto line 5a fixed the gains but silently dropped that
limitation, understating NIIT by up to $1,786 on a net-loss case.

Line 5a intentionally carries all of line 7. Gain from property held in an
active trade or business leaves net investment income on line 5b, not by
filtering 5a; we map no 5b because no input can produce business-property
gain, pinned by a strict xfail. The graph half is still open, so the
`_f4_niit_stcg` signature is narrowed to `backend == "graph"`.

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

**Fixed.** Activation is decided by `subordinate_form_applies`, which tests
the *natural* inputs a form consumes rather than the post-format OTS values, so
a formatting decision can no longer decide whether a form runs. Naturals that
reach a form indirectly through `fed_import_map` are declared in
`activation_naturals` and counted too — without that, moving Form 8960 line 5a
to the 1040 import would have stopped the form firing for a filer whose only
investment income is capital gains.

### F8. NEW — Graph cross-mode batch returns an exploded, misaligned grid

Found incidentally while benchmarking; priority-critical.
`evaluate_returns(backend="graph", mode="cross")` — the default mode — returns
`python_grid × rust_axis` rows instead of the grid (`w2_income=[10k,20k,30k]`
→ 9 rows; a 2×3 grid → 18), and the input columns cycle at a different rate
than the results, so rows pair wrong inputs with wrong outputs — only the
diagonal is correct. OTS cross/zip and graph zip are all correct. Prior
audits used one-row batch cases, where 1×1 = 1 row masks the explosion
entirely — another instance of easy scenarios hiding a broken path.

### F9. FIXED — Graph batch path bypasses TaxReturnInput normalization

Found by the differential suite's batch-conformance tests (added post-audit).
`GraphBackend.evaluate_batch` consumed raw input columns, skipping pydantic
model validators and computed fields. Two confirmed symptoms: the
qualified>ordinary dividend lift was not applied (Single, w2 $60k, qualified
dividends $12k: scalar AGI $72,000, zip AGI $60,000; OTS zip correct), and
the `schedule_se_ss_wages` derivation was absent (the batch xfail shipped
with PR #279).

Fixed by routing each materialized batch row through `TaxReturnInput` inside
`evaluate_batch`, right after cross mode expands to zip on the Python side —
so the status-dependent line-8a derivation is available per row — and taking
the same `model_dump` (excluding year/state/filing_status/standard_or_itemized)
the single-scenario path uses. Batch now reproduces scalar row-for-row; the
`batch_input_gap_quantities` excuser is deleted and batch conformance passes
unexcused. The two strict-xfail burn-ins
(`test_graph_zip_applies_dividend_normalization`,
`test_se_tax_graph_batch_matches_single`) flip to passing guards. Closed by
`tenforty-tve`.

### F10. NEW — Graph taxes short-term capital gains at preferential rates

Caught by the widened differential grid (48 flagged cases). With pure short-term
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
for the three-way adjudication method: taxcalc and the independent
in-house engine outvoted the incumbent, and the revenue procedure confirmed
the majority.

**Not patched locally.** We vendor OpenTaxSolver unmodified — no edits to the
release sources or to the generated amalgamation, and no correcting patch
function in `ots/amalgamate.py`. The fix belongs in an OTS release. Until one
carries it, the `_f11_ots_hoh_bracket` signature and its strict-xfail burn-in
are the record; the xfail flips on its own once upstream corrects the table.
(The same bad row is duplicated into Form 2210's copy of the 2024 rate table,
which the upstream report should mention.)

### F12. NEW — itemized_deductions category ambiguity changes AMT

Found by the adversarial hypothesis search (MFS, $250k of gains, $33,410
itemized). All three engines take the same deduction and agree on taxable
income — but OTS reports $1,634.46 of AMT where graph and taxcalc report
none. Cause: the aggregate rides in a different Schedule A category per
engine — OTS maps it to A6 ("other taxes", added back on Form 6251), graph
to L16 ("other deductions", not added back), and the taxcalc adapter to
interest paid through e19200 (not added back). None is wrong; the input model
cannot say which kind of deduction it is. This is the categorized-deductions
API gap made concrete, resolved by input model v2; until then the divergence
is a documented assumption, excused by signature.

### F19. NEW — deduction choice: taxcalc minimizes tax, tenforty maximizes the deduction

Retires F15, which named the wrong mechanism. F15 said OTS applied itemized
deductions the caller had not asked for; it does not — all three engines take
the greater of the standard deduction and the aggregate. The divergence F15 was
really written for was the 60%-of-AGI charitable ceiling, which went away when
the aggregate moved to the uncapped `e19200` (F12, #327).

The actual rule is in taxcalc's `calculator.py` (`_calc_one_year`): it computes
the return **both ways** and keeps the itemized total only when it strictly
lowers tax.

```python
self.array('standard', np.where(item_taxes < std_taxes, 0., std))
self.array('c04470',   np.where(item_taxes < std_taxes, item, 0.))
```

OTS and the graph spec take whichever deduction is larger. The rules agree
whenever the extra deduction displaces income taxed at a positive rate, and part
company when it does not — a deduction landing on income already in the 0%
long-term-gain bracket buys nothing, so taxcalc keeps the standard deduction and
reports higher taxable income for identical tax. 2025 Head of Household, $58,509
of long-term gain, $56,482 aggregate: taxcalc reports taxable income $34,884
(AGI less the $23,625 standard deduction), both tenforty backends report $2,027
(AGI less the aggregate), and income tax is $0 on all three.

None is wrong. taxcalc answers "what does this filer owe", tenforty's backends
answer "what does the larger deduction produce", and the two coincide except
where the deduction is free. **Only `taxable_income` is excused** — the tax
agreeing is the entire premise, so if it ever stops agreeing that must surface.
`test_taxcalc_keeps_the_standard_deduction_when_itemizing_is_free` pins the tax
agreement so the excuse stays honest.

The signature excuses **both** backends, as F14 does. F15 excused OTS only,
which is why `[graph]` failed the differential about half the times it was run:
the identical divergence was suppressed on one engine and unexcused on the
other. Over 8,000 randomized cases, F15 → F19 takes graph from 17 violating
cases to 2 and OTS from 16 to 1. Tracked as tenforty-z31.

### F13. NEW — graph 2025 Married/Sep long-term-gain thresholds diverge

Six grid cases, 2025 + MFS + LTCG only: graph income tax is $1,377–$1,665
above the OTS+taxcalc consensus. The pattern fits a preferential-rate
breakpoint error for MFS in the 2025 spec parameters (MFS thresholds are
not always half of Single). 2-vs-1 against the graph; fix and adjudicate in
the spec bundle.

### F14. FIXED (graph) — AMT standard-deduction add-back divergence

Found by the first AMT-positive stratum (H_amt: ISO exercise spread carried
to taxcalc as `cmbtp`). Single, $150k wages + $200k ISO: OTS computes AMT
$43,813.50; taxcalc and the graph spec both compute $39,725.50 — agreeing
to the penny — and the $4,088 gap is exactly 28% x $14,600, the standard
deduction. Form 6251 line 2a instructs non-itemizers to add the standard
deduction back into AMTI, which is what OTS does. If the form walkthrough
and a TAXSIM cross-check support that reading, this would be the first time
the suite has turned up something in a *reference implementation* — apparently
shared by our own graph spec — and the excusing signature flips from OTS to
graph, with a note raised upstream to PSL. The
suspects agreeing to the penny is itself evidence of a shared modeling
choice rather than independent correctness.

**ADJUDICATED — OTS appears to match Form 6251; taxcalc and our graph spec
both appear to diverge from it.** As best we can tell this is the first time
the suite has turned up something in a *reference implementation* rather than in us.

Four lines of evidence, which agree with each other:

1. **Form 6251 walkthrough.** AGI $150,000; standard deduction $14,600;
   regular taxable income $135,400. Line 2a adds the standard deduction back,
   so AMTI = $135,400 + $14,600 + $200,000 = **$350,000** — equivalently
   AGI + preference, which is the point of the add-back. Exemption $85,700
   (no phase-out below $609,350), base $264,300, TMT = 26% x $232,600 +
   28% x $31,700 = $69,352. Regular tax $25,538.50. AMT = **$43,813.50** —
   exactly OTS.
2. **IRS instructions for line 2a**, verbatim: *"If you aren't filing
   Schedule A (Form 1040), then enter the standard deduction amount that you
   reported on Form 1040 or 1040-SR, line 12e."* It is an addition in Part I.
   (Line references shift by form year; the rule is long-standing.)
3. **taxcalc source.** For non-itemizers `calcfunctions.py` computes
   `c62100 = c00100 - e00700 - qbided - standard` — AGI less the standard
   deduction, with no add-back that we could find. Reproduced directly:
   feeding `cmbtp=200000` with the standard deduction yields AMTI $335,400
   against the $350,000 we expected, short by exactly the $14,600 standard
   deduction, and AMT $39,725.50.
4. **The two taxcalc branches seem inconsistent with each other.** The
   *itemizer* branch subtracts deductions and then adds back the
   AMT-disallowed ones (SALT, misc, excess medical), which matches our
   reading of the form. The non-itemizer branch subtracts the standard
   deduction and adds back nothing. Holding AGI and the preference fixed and
   varying only the deduction shows the asymmetry: $30,000 of pure charity
   (allowed for AMT) gives $320,000, matching expectation, while the $14,600
   standard deduction gives $335,400 where we expected $350,000.

If our reading is right, the upstream change would be to drop the
`- standard` term: `c62100 = c00100 - e00700 - qbided`.

**What we have not established.** Whether any of this is unintended. The
formula was translated from an older SAS implementation, and taxcalc issue
#37 shows someone passing over this same line in 2014 and closing it as a
false alarm — so it may be long-standing inherited behaviour rather than an
oversight. More importantly, `cmbtp` is built in `taxdata` from PUF records;
if it is derived from reported AMTI it may already absorb this, in which case
the model and its data could be consistent in aggregate and a change to
`calcfunctions.py` alone might make published results worse. We have no
visibility into that. The upstream note is therefore framed as a question,
not a bug report (`docs/upstream-taxcalc-reports.md`).

**A correction to our own reasoning.** The original F14 entry treated taxcalc
and our graph spec agreeing to the penny as the tell — two engines against
one. On reflection that agreement is weak evidence: both appear to take the
same shortcut (start from taxable income, add preferences, skip line 2a), so
it looks like common-mode error rather than independent corroboration. The
statute and the form instructions are what carry the argument, not the vote.

**Fixed (tenforty-8ik).** Form 6251 line 2a now adds the taxes back
correctly: it imports the 1040's deduction actually taken (`L12Final`) and
compares it to the standard deduction for the status; when the filer itemized
(the 1040 took the larger itemized amount) it adds back SALT, otherwise it
adds back the standard deduction. Both backends now compute AMT $43,813.50 on
the walkthrough case, matching Form 6251. The `_f14` signature is not deleted
but **inverted**: it previously excused OTS's divergence from taxcalc; now that
both backends add the deduction back, it excuses **both** backends against
taxcalc on ISO + Standard cases (the 24 such golden fixtures), taxcalc being
the outlier. The upstream note to PSL (`docs/upstream-taxcalc-reports.md`) is
unchanged; we changed our own engine regardless of how that question lands,
since we claim to compute the law rather than match an aggregate.

### F16. NEW — the suite's taxcalc adapter drops `iso`, so taxcalc sees no AMT preference

Found while adjudicating F14. `scripts/taxcalc_audit.py` — the probe that
found F14 — correctly carries the ISO spread to taxcalc as `cmbtp`. The
adapter that shipped into the suite, `taxcalc_batch` in
`tests/taxcalc/taxcalc_differential_test.py`, does not: it builds its record
without `cmbtp`, so taxcalc is handed no AMT preference at all. Through the
suite, the F14 case returns taxcalc AMT `$0.00` rather than `$39,725.50`,
and feeding `iso=200_000` or `iso=0` produces identical output.

Currently **latent**: `_case_strategy()` does not generate `iso`, so no
running test compares an AMT-preference case. That is exactly why it is
dangerous — the moment AMT coverage is added (which `tenforty-y90` plans),
every such case would compare tenforty *with* the preference against taxcalc
*without* it, manufacturing large bogus divergences on both backends and
burying any real one. Fix the adapter before adding the coverage.

### F20. FIXED — the suite compares post-refund `iitax` with pre-refund tax

Found while validating the graph autodiff kink fix. Tax-Calculator defines
`iitax` as `c09200 - refund`: Form 1040 line 24 less its line-32 analogue of
refundable credits. Tenforty's `federal_income_tax` and `federal_total_tax`
are pre-refund line-24 quantities. The suite nevertheless mapped raw `iitax`
directly, so a low-income return with an EITC appeared to disagree even though
both engines reported zero pre-refund tax.

The apparent batch-cardinality defect had the same cause. Tax-Calculator
generates `credit_claim_urn` by row position for probabilistic EITC take-up.
TY2024 Head of Household with wages of $802 receives no credit alone, but as
the second row of a two-record batch it receives a $61.353 EITC: raw `iitax`
changes from $0 to -$61.353 while pre-refund tax remains $0 in both cases.
That microsimulation behavior is not part of the quantity this differential
claims to compare.

The adapter now adds `refund` back to `iitax` before applying its existing
NIIT, self-employment-tax, and Additional-Medicare bucketing. It retains raw
`iitax` and `refund` as diagnostics, and deterministic conformance tests pin
both the one-record and multi-record cases. No policy signature excuses the
difference.

### F7. TaxCalc cannot express forced below-standard itemization

With `standard_or_itemized="Itemized"` and deductions below the standard
deduction, both tenforty backends force itemization. TaxCalc always chooses
between the available deductions and exposes no equivalent force switch, so
the differential retains a bounded F7 delta for both backends. With deductions
above the standard deduction all three agree.

**Decision (`tenforty-ddj`, 2026-08-01):** preserve the historical default
without discarding a required tax election. Legacy `"Standard"` remains the
automatic greater-of mode; `"Itemized"` forces federal itemization even when
the supplied amount is smaller or zero. Forced itemization is necessary for,
among other cases, a married-filing-separately filer whose spouse itemizes and
who is therefore ineligible for the standard deduction. The graph backend was
brought into conformance by `tenforty-435`. Input model v2 will replace the
misleading legacy field with an explicit
`Auto / Standard / Itemized` choice. See
[`docs/deduction-choice-contract.md`](deduction-choice-contract.md).

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
   - Form 8995: completed in tenforty-2u6 with a preliminary OTS 1040 feeding
     OTS Form 8995, tenforty's Form 8995-A limitation, and a final 1040 pass
     carrying the resulting deduction (F3).
4. **API decisions**:
   - per-spouse wage/SE fields (completes F1 for MFJ; taxcalc's
     `e00200p`/`e00200s` is a working reference);
   - **implemented:** legacy `"Standard"` is automatic greater-of and
     `"Itemized"` forces itemization in both backends (F7, `tenforty-435`);
5. **Keep the reference**: promote the harness to `scripts/`, commit a pinned
   golden fixture set at the semantic boundaries, and run the three-way sweep
   as a scheduled/pre-release job. Parity catches divergence; only an
   independent reference catches shared omissions (F4 was invisible to parity by
   construction).


### F17. NEW — graph charges self-employment tax below the $400 floor

Found by the differential sweep while fixing the taxcalc adapter, on a
hypothesis draw of `se=128` alongside large other income. Small
self-employment amounts are an unusual draw, and every hand-written case had
used round, large figures — which is why this survived earlier passes.

Schedule SE line 4c stops the computation when net earnings from
self-employment are under $400 (IRC 1402(b)(2)). OTS and taxcalc both honour
the floor; the graph spec charges 15.3% from the first dollar. The threshold
applies to *adjusted* earnings, after the 92.35% factor:

| SE income | net earnings | OTS `se_tax` | graph `se_tax` |
|---|---|---|---|
| 128 | 118.21 | 0.00 | 18.09 |
| 400 | 369.40 | 0.00 | 56.52 |
| 500 | 461.75 | 70.65 | 70.65 |

Above the floor the two agree exactly, so the divergence is isolated to the
de-minimis rule. Two engines against one, with the form instruction agreeing
with the majority. The half-SE-tax adjustment reaches AGI, so `agi` diverged
by exactly half the SE tax as well.


### Graph spec bundle — corrected mechanisms (F5, F10, F13, F4-graph, F17)

Five graph-side findings fixed together in the Haskell spec. Two of the
mechanisms differed from what the tracking notes predicted; recording the
real ones.

**F5 was arithmetic, not a missing import.** Form 8959 line 8 already imported
self-employment income from Schedule SE. Line 12 computed
`smallerOf line8 line11` where the form subtracts (`line8 - line11`, not below
zero). With wages above the threshold, line 11 is zero, so `min(SE, 0)` was
zero and the whole SE Additional-Medicare charge vanished. Changed to
`subtractNotBelowZero`. Single, $250k wages + $50k SE: $450 → $865.57.

**F10 is the Qualified Dividends and Capital Gain Tax Worksheet, line 3.** It
read `ifPos L15 (min L15 L16) L16` — when there was no long-term gain it fell
through to L16 (the net total, including short-term), routing short-term gains
to preferential rates. Replaced with `max0 (min L15 L16)`, the worksheet's
"smaller of Schedule D line 15 or 16, else zero." Single, $50k wages + $25k
STCG: income tax $6,022 → $8,341.

**F13 was a single wrong constant, and not where expected.** The preferential
breakpoints are inlined in `US1040_2025.hs`, not read from `Tables2025.hs`
(whose `qualifiedDividendBrackets2025` is dead code). The 2025 MFS 15%-rate
ceiling read $266,700; Rev. Proc. 2024-40 puts it at $300,000. The wrong value
taxed $33,300 of gain at 20% instead of 15% — a flat $1,665 overcharge. 2024's
MFS breakpoints were already correct.

**F4-graph** now imports Form 8960 line 5a from Schedule D line 16 (both
holding periods netted), mirroring #296's OTS fix and dropping the
`long_term_capital_gains → L5a` mapping. Short-term gains reach NIIT; the
long-term path is unchanged. Single, $300k wages + $50k STCG: NIIT $0 →
$1,900.

**F17** adds the Schedule SE $400 de-minimis floor (IRC 1402(b)(2)): line 10
returns zero when line 4c is under $400. See the F17 entry above.

The five known-defect signatures (`_f4_niit_stcg`, `_f5_graph_8959`,
`_f10_graph_stcg_preferential`, `_f13_graph_2025_mfs_ltcg`,
`_f17_graph_se_deminimis`) and their strict-xfail burn-ins are deleted; the
differential sweep passes on the graph backend with none of them.

Two things found and left for follow-up, both since closed: the dead
`qualifiedDividendBrackets2025` table (and the 2024 twin) that duplicated the
inline 1040 breakpoints has been replaced by a single `qualifiedDividend0PctMax`
/ `qualifiedDividend15PctMax` `ByStatus` source the worksheet reads via
`byStatusE` — the F13 smell removed (`tenforty-db5`). And the F18 capital-loss
limitation below.

### F18. FIXED — graph omits the section 1211(b) $3,000 capital-loss limitation

Graph Schedule D line 16 summed the net gain or loss without the section
1211(b) $3,000 cap ($1,500 MFS). 1040 line 7 imported that uncapped figure, so
a net-loss return understated AGI and taxable income; and F4-graph imported
Form 8960 line 5a from the same node, so the uncapped loss also understated
NIIT — e.g. $300k wages + $100k interest + a $50k short-term loss yielded graph
NIIT on the full $50k offset rather than the $3,000 the statute allows ($3,686
correct). Harmless for the gain cases the sweep exercises; the sweep does not
appear to emit net-loss cases, which is why F18 was hand-found rather than
signature-surfaced.

The fix is structural, not a cap on line 16 (the QCGWS line-3 smaller-of test
genuinely wants the uncapped net): Schedule D now computes line 21 as
`maxE line16 (byStatus -3000/-1500-MFS)` — a gain passes through, a loss is
floored at the cap — and 1040 line 7 and 8960 line 5a import line 21 while the
worksheet stays on line 16. Verified against OTS to the penny across STCG/LTCG
losses, the MFS half-cap, sub-cap losses, and gain cases (unchanged); AGI on a
net-loss return now falls by at most the cap. The strict-xfail
`test_graph_niit_honors_the_capital_loss_limitation` flipped to a passing
guard. Closed by `tenforty-kf4`.

### F21. NEW, ADJUDICATED — TaxCalc doubles the QBI phase-in range for qualifying widow(er)

TaxCalc 6.7.2 gives qualifying widow(er) the correct all-other-returns
section 199A threshold, but the married-filing-jointly phase-in range. Its
`PT_qbid_taxinc_thd` parameter is $191,950 for 2024 qualifying widow(er), while
`PT_qbid_taxinc_gap` is $100,000. The 2024 Form 8995-A instructions specify a
$50,000 range for every return other than married filing jointly.

The difference is visible at the midpoint of the statutory range. For a 2024
qualifying widow(er) with $100,000 of self-employment profit and $153,214.775
of taxable interest, taxable income before the QBI deduction is $216,950.
After the deductible half of self-employment tax, QBI is $92,935.225 and its
20% component is $18,587.045. With zero business W-2 wages and UBIA, the
official 50% phase-in reduction leaves a $9,293.5225 deduction. TaxCalc applies
only a 25% reduction over its $100,000 range and returns $13,940.28375.

The graph follows the official range. The differential policy excuses only
qualifying-widow(er) cases in the affected phase-in band, and a strict-xfail
adapter test pins TaxCalc 6.7.2's result until a release corrects the parameter.
The signature covers taxable income, AMT, income tax, and total tax because the
incorrect deduction can propagate through both regular taxable income and AMTI.
The upstream report is staged in `docs/upstream-taxcalc-reports.md`; tracked by
`tenforty-2jg`.

### F22. NEW, ADJUDICATED — OTS loses the AMT taxable-income floor

The randomized ISO strategy found a case below the regular taxable-income
floor: 2024 Head of Household, no regular income, the standard deduction, and
a $200,000 ISO exercise spread. Form 1040 line 15 is zero. Form 6251 then adds
the $21,900 standard deduction on line 2a, so AMTI is $221,900. After the
$85,700 exemption, tentative minimum tax and AMT are **$35,412.00**.

The graph follows that form path. OTS reports **$29,718.00** because
`form6251_AlternativeMinimumTax` replaces line 1 with the unfloored
`L11 - L14` whenever Form 1040 line 15 is zero. Here that is -$21,900; line
2a adds $21,900 back and the two cancel, leaving AMTI at $200,000. This is an
upstream tax-logic defect, so the vendored source remains unchanged.

TaxCalc 6.7.2 reports **$24,024.00** for the same return. That is the already
adjudicated F14 defect: it starts AMTI from AGI less the standard deduction,
producing $178,100. Below the regular taxable-income floor, the full gap between
TaxCalc and the Form 6251 path is the standard deduction plus its unused part,
not merely one standard deduction. F14's bounded model now reflects that; F22
adds a separate negative OTS correction, so the two mechanisms compose without
a blanket waiver.

The strict-xfail compares OTS directly with the hand-worked graph value and
will flip when an OTS release preserves the Form 1040 line-15 floor. The
upstream report is staged in `docs/upstream-ots-reports.md`; tracked by
`tenforty-by2`.

### F23. ADJUDICATED — TaxCalc omits the MFS AMTI increase; graph fixed

The 2025 Form 6251 instructions have a special line-4 rule for married filing
separately: above $900,350 of AMTI, add 25% of the excess to line 4, capped at
$68,500. For 2024 the threshold is $875,950 and the cap is $66,650. OTS has the
mechanism. Before `tenforty-3bt`, the graph went straight from the ordinary AMTI
sum to the exemption; TaxCalc's `c62100` path still only zeros the exemption
above its terminal threshold.

A clean 2025 witness is MFS with $750,000 wages and a $300,000 ISO spread.
Before the special rule, AMTI is $1,050,000. The required addition is
25% x ($1,050,000 - $900,350) = $37,412.50, increasing AMT by $10,475.50.
OTS reports the official **$68,380.75**. Before `tenforty-3bt`, graph reported
**$57,905.25**; TaxCalc remains lower still because F14 independently omits the
standard-deduction add-back.

The graph now implements the increase from shared table constants and pins the
threshold, partial-increase, and cap regimes for both years. The F23
reference-delta model applies to graph and OTS because TaxCalc remains the
reference with the omission; only the TaxCalc strict-xfail remains. The graph
fix is tracked by `tenforty-3bt`, and the staged TaxCalc report remains tracked
by `tenforty-doo`.

### F24. NEW, ADJUDICATED — OTS 2024 uses stale MFS line-4 constants

OTS's 2024 routine implements the F23 mechanism with the prior-year threshold
and cap: $831,150 and $63,250. The official 2024 instructions use $875,950 and
$66,650. On the clean $750,000-wage / $300,000-ISO witness, OTS reports
**$71,832.75** while the official 2024 rule gives **$68,696.75**.

F24 models only the signed difference between OTS's stale addition and the
official F23 addition; the two ranges compose. The vendored source remains
unchanged, and the upstream report plus strict-xfail are tracked by
`tenforty-2jv`.

An independent golden witness places OTS AMTI at $850,000: above the stale
$831,150 threshold but below the official $875,950 threshold. F24 contributes
$1,649.38 of modeled AMT there while F23 contributes exactly zero, preventing
the overlapping high-income witness from masking an error in either model.
The effective rate is 35% in this interval: 28% tentative minimum tax plus the
7% effect of phasing out another 25 cents of exemption per added dollar.

### F25. FIXED — graph Form 6251 retained the stale MFS gain ceiling

F13 corrected the 2025 MFS 15%-rate ceiling in the 1040 qualified-dividend and
capital-gain worksheet from $266,700 to $300,000. Form 6251 Part III contains a
second hand-written copy and still uses $266,700, so an AMT-binding MFS return
can move up to $33,300 of preferential income from the 15% band to 20%.

The deterministic witness has $250,000 wages, $15,458 STCG, $6,032 LTCG,
$49,171 interest, $11,057 qualified dividends, $19,444 itemized deductions,
and a $50,000 ISO spread. Graph AMT is **$2,459.55** versus TaxCalc's
**$2,218.80**; the $240.75 difference is exactly 5% of the $4,815 that reaches
the stale band.

F25 permitted only the 5-point spread over at most the $33,300 bad interval, a
maximum $1,665 positive graph delta. `tenforty-c9a` replaced Form 6251's
duplicate table with `qualifiedDividend15PctMax2025`, the same source used by
the Form 1040 worksheet. The strict-xfail and differential signature were
retired together; the dedicated golden witness now passes without an
allowance.

### F26. NEW, ADJUDICATED — TaxCalc loses the itemized taxable-income floor

TaxCalc's itemizer AMTI branch reconstructs Form 6251 line 4 directly from AGI
less itemized deductions plus AMT add-backs. When allowed itemized deductions
exceed AGI, their unused portion therefore makes the base negative. Form 6251
line 1 is Form 1040 line 15, which has already floored taxable income at zero;
unused itemization cannot carry through that line.

The witness is 2024 MFS with a large net capital loss, $15,079 interest,
$33,112 ordinary dividends ($16,556 qualified), $50,061 itemized deductions,
and a $300,000 ISO spread. AGI is $46,691, so $3,370 of itemization is unused.
Graph starts line 1 at zero and reports AMT **$58,376.32**. TaxCalc carries the
-$3,370 through AMTI and reports **$57,432.72**, lower by $943.60 (28%).

F26 applies only to the positive graph-minus-TaxCalc effect of that unused
deduction, bounded by the same AMT/preferential slopes as F12. A TaxCalc
strict-xfail, upstream draft, and golden witness track `tenforty-w7t`.

### F27. NEW, ADJUDICATED — OTS skips the AMT preferential-rate worksheet at zero taxable income

The zero-taxable-income path exposes a second OTS defect independent of F22.
The 2024 1040 routine does not run its qualified-dividend or Schedule D tax
worksheet when line 15 is zero. Form 6251 Part III nevertheless reads the
worksheet arrays when qualified dividends or gains are present. Their
zero-initialized values make the AMT computation treat all AMT taxable income
as ordinary rather than preserving its preferential component. The 2025
routine has the same control flow.

The cleanest isolated witness is 2024 Head of Household with a $200,000 ISO
spread and ordinary dividends all reported as qualified. At exactly the
$21,900 standard deduction, F22's unused-deduction error is zero, but F27 makes
OTS tax the entire AMT base at 26%. One additional dollar activates the
regular-tax worksheet and makes the defect disappear:

| qualified dividends | taxable income | OTS AMT | graph AMT |
|---:|---:|---:|---:|
| $21,900 | $0 | $35,412.00 | $29,718.00 |
| $21,901 | $1 | $29,718.00 | $29,718.00 |

The $5,694 cliff is exactly 26% of $21,900. The same boundary reproduces in
2025: OTS falls from $35,236.50 to $29,094.00 when taxable income moves from
zero to one dollar, a $6,142.50 drop equal to 26% of the $23,625 standard
deduction.

The randomized differential originally exposed a composite 2024 witness with
$17,322 of qualified dividends. Its official path has AMTI $221,900 and AMT
taxable income $136,200. Removing the $17,322 preferential component leaves
$118,878 taxed at 26%, for AMT **$30,908.28**.

OTS also carries F22 on this return, putting AMT taxable income at $131,622.
With only F22, Part III would tax $114,300 of ordinary AMT income and report
**$29,718.00**. Instead the uninitialized worksheet makes OTS tax the full
$131,622 at 26%, reporting **$34,221.72**. Thus F27 contributes exactly
$4,503.72, while F22 separately contributes -$1,190.28 relative to the
official path. TaxCalc's **$24,024.00** remains the already-adjudicated F14
standard-deduction defect.

The F27 signature is a nonnegative OTS correction bounded by 28% of the
preferential income, the largest amount skipping Part III can add. It applies
only when TaxCalc regular taxable income is zero, preference income and an ISO
adjustment are present, and composes with F14 and F22. A deterministic
differential anchor prevents the randomized suite from rediscovering the case
as an unclassified failure. The strict-xfail uses the exact-standard-deduction
boundary where F22 is inert, while a passing companion pins the correct result
one dollar above it. Tracked by `tenforty-909.1`; the vendored source is
unchanged.
