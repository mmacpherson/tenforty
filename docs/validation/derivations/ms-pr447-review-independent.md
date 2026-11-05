# PR 447 independent source review

Reviewer identity: /root/ms447_source. Retrieved 2026-11-05. Source-only review: no repository files, implementation, generated graphs, or tenforty outputs consulted. Evidence kind throughout: independent derivation, not published worked examples.

Official sources:
- 2024 MS DOR Form 80-100: https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100241.pdf
- 2025 MS DOR Form 80-100: https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf

## Source rules

2024 pp. 5–7: Combined returns preserve each spouse's income ownership; deductions/exemptions may be allocated freely. Standard deduction totals $4,600, basic exemption $12,000. Line 13 carries line 66 AGI. Line 16 subtracts deductions/exemptions. Opposite-sign taxable incomes combine before taxation; two positive columns use separate tax bands. Page 21: nearest-dollar rounding, halves upward; 0% through $10,000, then 4.7%.

2025 pp. 5–7 repeat these allocation and signed-income rules; p. 27 supplies the 4.4% excess-$10,000 rate and adds column amounts before multiplying. Pages 5–6 and line 15 on p. 7 require MFS to halve total line-12 exemptions, including additions, not merely use half the basic exemption. Head of Family (p. 6) requires a qualifying dependent resident in the home all year; exemption $8,000 plus $1,500 per dependent, standard deduction $3,400. Spouse-died status (p. 5) applies to death in the current year. Neither booklet explicitly mentions federal QSS.

## Independent allocation derivation

Let a,b be signed whole-dollar MS line-13 AGIs; C>=0 the total allowed deductions plus exemptions; K=10000. Income ownership must remain fixed. Choose nonnegative cA+cB=C. Before final whole-dollar tax rounding, the minimum taxable excess is:

B = max(0, a+b-C-min(K,max(a,0))-min(K,max(b,0))).

Minimum tax is nearest-dollar(r*B), where r=.047 (2024) or .044 (2025). This assumes ordinary resident tax computation, no separate taxes/credits, and the freely allocable combined-return allowances described above.

Proof: For both AGIs nonnegative, allocating allowances first against dollars above K avoids wasting deductions within zero-rate bands. If allowances exhaust those dollars, tax is zero. Otherwise remaining excess is sum(max(a-K,0),max(b-K,0))-C. This is exactly B. Excess allocations can be placed anywhere after tax reaches zero. With one negative AGI and positive net income, the source explicitly nets signed taxable columns; all allowances reduce their sum and one K band applies. If the other positive AGI is below K, both the formula and direct netting yield zero. Both negative also yield zero. Therefore flooring each AGI before combining is wrong; subtracting one spouse's loss remains necessary. Separate deduction and exemption pools can realize any aggregate allocation cA in [0,C] by splitting each pool nonnegatively.

No unique optimum allocation exists generally. Equality of taxable columns is neither necessary nor always feasible because income cannot be transferred.

## Independent cents ambiguity

Assume wages are the only income, no adjustments, and both are MS residents. Household wages are identically $60,000.80 in both underlying cases, rounding to $60,001.

Case A: spouse wages $20,000.40 -> spouse line 13 $20,000; filer wages $40,000.40 -> filer line 13 $40,000.
Case B: spouse wages $20,000.10 -> spouse line 13 $20,000; filer wages $40,000.70 -> filer line 13 $40,001.

Both cases have identical exact household wages, identical rounded household category amount, and identical supplied spouse whole-dollar line-13 AGI. Yet the filer's whole-dollar AGI differs. Thus even exact household category totals plus spouse rounded AGI cannot uniquely reconstruct filer rounded AGI. Rounded-household-minus-rounded-spouse yields $40,001 for both and misrepresents case A. No adjustment, negative value, tie, or multiple-category summation convention is required. Necessary information includes either both independently rounded spouse AGIs or enough ownership-specific unrounded category detail to compute them.

## QSS limitation

For an unmarried surviving spouse with a qualifying dependent living at home all year, MS Head of Family follows directly from the stated state criteria; current-year spouse-died status does not apply to a prior-year death. However, a universal implication from the federal QSS label alone cannot be proved from these two MS booklets: neither defines federal QSS or explicitly maps it. In particular, any federal exception that allows QSS without actually claiming a dependent must be considered before treating the label alone as sufficient. This review intentionally did not consult federal instructions because its authorized source scope was MS DOR only.

## Follow-up: actual resident form page 2 decisively has TWO columns

Retrieved 2026-11-05:
- 2024 official 80-105 https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80105248.pdf
- 2025 official 80-105 https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80105258%201.pdf

Page 2 of BOTH forms explicitly labels Column A (Taxpayer) and Column B (Spouse), with distinct 38A/38B wages, 49A/49B total income, 65A/65B adjustments, and 66A/66B AGI. These carry to page 1, line 13A/13B. Thus there is no single household page-2 line 66 from which one may subtract the spouse. The booklet's instruction to separate beginning line 13 refers to page order, not a household-only income worksheet. The cents counterexample above uses precisely these ownership-specific whole-dollar form boxes.

## Follow-up: one-dollar tax ambiguity, independently derived

Evidence: independent derivation by /root/ms447_source, 2026-11-05, using 2025 80-105 page 2 wages/AGI columns; 2025 80-100 pp. 5–7 allowances, p. 22 nearest-dollar rule, p. 27 computation schedule. Assume married combined MS residents, under 65, not blind, no dependents, only wages, no adjustments or credits, standard deduction $4,600 and exemption $12,000. C=$16,600.

Identical household wages: $61,611.80 (rounded household category $61,612).

A: spouse $20,000.40 -> $20,000; filer $41,611.40 -> $41,611. Allocate all $16,600 allowances to filer: taxable columns $25,011 and $20,000. Schedule taxable excess = $15,011+$10,000=$25,011. Tax=$25,011*0.044=$1,100.484 -> $1,100.

B: spouse $20,000.10 -> $20,000; filer $41,611.70 -> $41,612. Same allocation: taxable columns $25,012 and $20,000. Schedule taxable excess=$15,012+$10,000=$25,012. Tax=$25,012*0.044=$1,100.528 -> $1,101.

Both allocations are optimal: both post-allowance columns exceed the $10,000 zero-rate band, so all $16,600 allowances reduce taxable excess. Same household exact wages and same integer spouse AGI produce different correct tax. Rates apply after combining the excess amounts; neither spouse tax is independently rounded. No backend outputs used to select or validate these expectations.

## Follow-up: federal QSS exception verified (expanded scope authorized)

Source retrieved 2026-11-05: IRS 2025 Form 1040 instructions, Filing Status / Qualifying Surviving Spouse, condition 2: https://www.irs.gov/instructions/i1040gi .

The IRS permits QSS even when the child cannot be claimed because the child's gross income is at least $5,200, the child files jointly, or the taxpayer can be claimed by another person. Therefore QSS does not necessarily establish an actual federal dependent. MS 80-100 p. 6 defines its dependent using federal dependency qualification and requires a dependent residing in the home for Head of Family. Inference: unconditional QSS -> MS Head of Family is unsupported and can grant improper status.

Concrete logical case: surviving parent whose spouse died in 2024, unmarried throughout 2025, maintains the home and provides over half support for a 30-year-old non-disabled son who lives there all year but earns $6,000. Assuming other federal QSS conditions hold, gross-income exception allows QSS; adult son is neither qualifying child nor qualifying relative because of gross income. With no other dependents, the MS instructions point to Single, not Head of Family.

Recommendation: determine MS Head of Family from actual state qualifying-dependent and residence facts. If those facts are not represented, reject unsupported QSS cases or require an explicit state-status/qualification input. A positive dependent count alone also does not establish that the actual dependent lived at home all year. Do not synthesize a dependency exemption merely from QSS status.
