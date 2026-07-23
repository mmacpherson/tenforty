# Differential Audit: tenforty vs PSL Tax-Calculator

Branch: `audit/taxcalc-differential`, cut from main as of 2026-07-21,
immediately before PR #279 merged.
Update 2026-07-21: PR #279 has since merged, fixing F1/F2; the remaining
findings are burned into the suite as strict xfails in
`tests/known_defects_test.py`.
Reference: [Tax-Calculator](https://github.com/PSLmodels/Tax-Calculator) (`taxcalc` 6.7.2, CC0),
federal only, tax year 2024.

## Findings ledger

Every disagreement class gets an ID here, a narrative section below, a
strict-xfail burn-in in `tests/known_defects_test.py`, and an excusing
signature in `tests/taxcalc/taxcalc_policy.py`. Fixes must flip the burn-in,
delete the signature, and update this table in the same PR.

| ID | Finding | At fault | Status | Found by |
|----|---------|----------|--------|----------|
| F1 | Schedule SE L8a never filled | mapping, both backends | fixed (#279, v2025.11) | @bg002h, #278 |
| F2 | SE-tax error propagates to AGI | consequence of F1 | fixed with F1 | @bg002h, #278 |
| F3 | QBI: missing (OTS) / gross base (graph) | OTS orchestration + graph spec | graph base fixed (tenforty-6hr); OTS omission + graph above-threshold open | @bg002h, #278 |
| F4 | Form 8960 L5a omits short-term gains | mapping, both backends | fixed (OTS #296, graph: L5a imports Schedule D L16) | mapping assessment + differential sweep |
| F5 | Graph Form 8959 Part II drops SE earnings (line 12 used min, not subtract) | graph spec | fixed | mapping assessment + differential sweep |
| F6 | OTS 8959 never fires with zero wages | OTS activation semantics | fixed | differential sweep |
| F7 | "Itemized" force vs best-of divergence | API contract | open (owner decision) | differential sweep |
| F8 | Cross-mode batch grid explosion | graph batch path | fix in PR #287 | benchmark |
| F9 | Batch path bypasses TaxReturnInput | graph batch path | fixed (tenforty-tve) | batch-conformance tests |
| F10 | Short-term gains taxed at preferential rates (QCGWS line 3) | graph spec | fixed | differential grid |
| F11 | 2024 HoH 32% bracket starts \$191,150, not \$191,950 | upstream OpenTaxSolver | adjudicated vs IRS; upstream report pending — not patched locally, we vendor OTS unmodified | differential grid |
| F12 | Itemized-deduction category changes AMT | API (input model v2) | open (design) | adversarial search |
| F13 | 2025 MFS 15%-rate ceiling wrong (266,700 vs 300,000), inline in 1040 not Tables | graph spec | fixed | differential grid |
| F14 | AMT std-deduction add-back divergence (ISO cases) | taxcalc (and graph, now fixed) | graph fixed (tenforty-8ik); taxcalc omits add-back, upstream note pending | H_amt stratum |
| F16 | Suite adapter drops `iso`, so taxcalc sees no AMT preference | taxcalc harness | fixed (#295) | F14 adjudication |
| F17 | Graph charges SE tax below the \$400 de-minimis floor | graph spec | open (tenforty-dw0) | differential sweep |

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
to L16 ("other deductions", not added back), the taxcalc adapter to charity
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
