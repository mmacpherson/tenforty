# Mississippi multiline entry rounding audit, 2026-11-05

Evidence label: non-blind source arithmetic, Sol, after implementation inspection.
This arithmetic was written before evaluating the example. It is not an
independent blind expected value.

Sources: official 2024 instructions
https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100241.pdf
printed p.10 (line38 wages), p.11 (line43 interest), pp.13-14 (total/AGI),
p.21 (all dollar amounts rounded), p.26 (tax computation).

Single, full-year resident, wages 18310.49 plus taxable bank interest .49,
no adjustments, additional exemptions or credits, standard deduction.
Line38 rounds18310.49 to18310. Line43 rounds .49 to0. Thus total/MS AGI18310;
2300 deduction plus6000 exemption leaves10010; taxed balance10; tax
10*.047=.47 rounds0. Rounding only their unrounded aggregate18310.98 instead
would produce18311, taxable10011, tax11*.047=.517 rounds1.

The current MS source uses federal AGI rather than reconstructing its page2
income/adjustment lines, so entry rounding of the aggregate alone may retain
this separate existing income-lowering gap. The example must be checked and
scoped with the conductor. The new spouse fact is line13 MS AGI, not a full
split of every page2 income category, which also matters for general cent
inputs across spouse columns. No expectation should be adjusted to graph output.


## Extended page-2 source mapping, SOL-33

This is non-blind source analysis, not a new blind derivation. Retrieved
2026-11-05. The official 2024/2025 MS instructions pp.10-14 provide these
entries: line38 wages;39 business profit/loss;40 capped net capital gain/loss
(same federal capital-loss limits);41 rental/Schedule E;43 taxable interest;
44 total dividends including the qualified subset;48 prepared other MS
income from Schedule N;49 sum of income;50 IRA;51 SEP/SIMPLE;52 savings
penalty;53 qualifying alimony paid;54 moving;55 National Guard/reserve pay;
56 MPACT;57 MACS;58 SE health;59 HSA;60 catastrophe savings;61 claimed federal
half-SE-tax deduction;62 first-home savings;63 agricultural disaster;64 ABLE;
65 total adjustments;66 income less adjustments. Each entered amount is
rounded before summing. The current natural API carries only the federal
half-SE-tax adjustment automatically; other named deductions may be prepared
raw inputs, but no new natural deduction fields are invented.

The API's generic schedule_1_income may include categories with distinct MS
treatment and cannot be placed in a single MS line safely. Generic
state_adjustment, dependent_exemptions, itemized_deductions and
state_and_local_taxes also do not identify a clean MS amount/category. Under
SOL-33 nonzero values are refused for MS; other states retain their contracts.
Federal below-AGI Schedule1-A/QBI/AMT inputs retain their federal effects and
are not MS AGI deductions. Ordinary MS-taxable wages and net business,
rental, interest/dividend and capital entries are assumed; special MS/federal
basis differences or excluded income require prepared state data.

Capital gains share ONE MS entry: combine holding periods and apply the
federal loss cap, then round that entry. Do not round each holding period as
a separate MS line. Qualified dividends are included in total ordinary
dividends and are never added a second time.

Federal half-SE-tax arithmetic below uses the cents method, which IRS
[Form1040 instructions](https://www.irs.gov/instructions/i1040gi),
“Rounding Off to Whole Dollars,” permit (whole-dollar rounding is optional
federally). [2024 ScheduleSE](https://www.irs.gov/pub/irs-prior/f1040sse--2024.pdf)
and [2025 ScheduleSE](https://www.irs.gov/pub/irs-prior/f1040sse--2025.pdf),
PartI lines4a/10/11/13, give 92.35% net earnings and 12.4%+2.9% below the SS
wage base. At business50000 and no wages: net46175; SE tax7064.775, reported
with cents7064.78; half3532.39, MS line61 rounds3532. At business10000 with
wages50000: net9235, SE tax1412.955, cents1412.96; half706.48, MS rounds706.
Both are below both years' SS wage caps. Negative business income has no SE
tax deduction. None of the cases lies on a federal/MS deduction tie.

## Worked page-2 cases (written before new graph comparisons)

All cases full-year Single unless MFS or J is specified; standard deductions,
no dependents or ages/credits. Data columns are the natural inputs. The MS
AGI column independently sums the rounded entries and subtracts the rounded
claimed adjustment. Both years use identical AGI/TI. Source labels are
non-blind source arithmetic throughout. Tax is the combined taxed balance
multiplied by .047 / .044, then rounded once.

| ID | Inputs | MS AGI | TI | Taxed balance | 2024 tax | 2025 tax |
| --- | --- | --- | --- | --- | --- | --- |
| interest-under-half | `{'w2_income': 18310.49, 'taxable_interest': 0.49}` | 18310 | 10010 | 10 | 0.470→0 | 0.440→0 |
| wage-at-half | `{'w2_income': 18310.5, 'taxable_interest': 0.49}` | 18311 | 10011 | 11 | 0.517→1 | 0.484→0 |
| interest-at-half | `{'w2_income': 18310.49, 'taxable_interest': 0.5}` | 18311 | 10011 | 11 | 0.517→1 | 0.484→0 |
| three-income-lines | `{'w2_income': 10000.49, 'taxable_interest': 10000.49, 'ordinary_dividends': 10000.49}` | 30000 | 21700 | 11700 | 549.900→550 | 514.800→515 |
| qualified-subset | `{'w2_income': 20000.49, 'ordinary_dividends': 5000.49, 'qualified_dividends': 3000}` | 25000 | 16700 | 6700 | 314.900→315 | 294.800→295 |
| qualified-only | `{'w2_income': 20000.49, 'qualified_dividends': 5000.49}` | 25000 | 16700 | 6700 | 314.900→315 | 294.800→295 |
| capital-one-entry | `{'w2_income': 20000.49, 'short_term_capital_gains': 1000.49, 'long_term_capital_gains': 1000.49}` | 22001 | 13701 | 3701 | 173.947→174 | 162.844→163 |
| capital-loss-limit | `{'w2_income': 50000, 'short_term_capital_gains': -5000, 'long_term_capital_gains': 1000}` | 47000 | 38700 | 28700 | 1348.900→1349 | 1262.800→1263 |
| capital-loss-mfs | `{'filing_status': 'Married/Sep', 'w2_income': 50000, 'short_term_capital_gains': -5000, 'long_term_capital_gains': 1000}` | 48500 | 40200 | 30200 | 1419.400→1419 | 1328.800→1329 |
| rental-entry | `{'w2_income': 20000.49, 'rental_income': 1000.49}` | 21000 | 12700 | 2700 | 126.900→127 | 118.800→119 |
| rental-loss | `{'w2_income': 20000, 'rental_income': -1000}` | 19000 | 10700 | 700 | 32.900→33 | 30.800→31 |
| business-deduction | `{'self_employment_income': 50000}` | 46468 | 38168 | 28168 | 1323.896→1324 | 1239.392→1239 |
| wages-business-deduction | `{'w2_income': 50000, 'self_employment_income': 10000}` | 59294 | 50994 | 40994 | 1926.718→1927 | 1803.736→1804 |
| business-loss | `{'w2_income': 50000, 'self_employment_income': -1000}` | 49000 | 40700 | 30700 | 1442.900→1443 | 1350.800→1351 |
| joint-multicategory | `{'filing_status': 'Married/Joint', 'w2_income': 20000.49, 'taxable_interest': 20000.49}` | 40000 | 23400 | 3400 | 159.800→160 | 149.600→150 |

For joint-multicategory, the supplied spouse line13 fact is20000 whole
dollars. Wages and interest each round20000, household MS AGI40000; each
spouse AGI is20000. Total allowances16600 leave23400, two10000 bands leave3400.
There is no per-category spouse split in the natural API. This fact already
represents a whole-dollar line13 entry; fractional facts are refused, not
rounded into an invented split. The two original fractional-spouse archive
cases remain raw pre-entry rounding tests only, with their outcomes intact.

A prepared raw IRA adjustment1000.49 on line50 rounds1000. With wages50000,
MS AGI49000, TI40700, balance30700, tax1442.90→1443 (2024) /1350.80→1351
(2025). Two prepared adjustments.49 on lines50/51 each round0 before the
sum, preserving the original wage-only result. These are prepared-component
checks, not new natural deduction inputs.


## Disposition, SOL-33

The initial reproduction describes checkpoint1c57430. This PR extends the
repair to page2 line-by-line lowering, not a tolerance/signature. The numeric
case records above were preserved unchanged before the extension's tests.
The unsupported aggregate list and whole-dollar fact requirement are explicit
contracts; no per-category spouse allocation is inferred.


## 2026-11-05 SOL-35 contract correction after independent review

The prior one-spouse-AGI natural contract and unconditional federal-QSS-to-HoF
inference are withdrawn. Both actual rounded MS line13 columns are required
for MFJ; QSS requires an explicit MS eligibility fact. See
[the new independent record](ms-pr447-review-independent.md). The existing
worked arithmetic assumes its stipulated actual columns/qualification and
is preserved unchanged. Household rounding is a validity reference only,
never a reconstruction of an actual column. MS MFJ household-income
state/total derivatives and solves are refused without perturbation ownership;
federal-only outputs remain supported.


## SOL-36 HoH baseline versus QSS requirement

Non-blind source/policy audit: IRS1040 Head of Household permits a dependent
parent who does not live with the taxpayer; MS80-100 p.6 requires a dependent
in the home for Head of Family. Mike's explicit default ruling treats the
ordinary resident-dependent HoH situation as an acceptable documented
baseline, with `head_of_family_eligible=False` selecting MS Single for the
known special circumstance. No assumption record is produced. QSS still
requires the fact. The $50k2025 parent-elsewhere override reuses the original
Single-with-one-dependent arithmetic40200/1329; the resident default reuses
HoF-with-dependent37100/1192. The six old zero-dependent natural silver
examples are repaired with the original blind archive's legal with-dependent
rows, and their literal-box component values remain asserted via raw controls.
Primary IRS source: https://www.irs.gov/instructions/i1040gi , Head of
Household / Special rule for parent, retrieved2026-11-05. The policy baseline
is a caller contract, not a claim that federal HoH logically entails MS HoF.
