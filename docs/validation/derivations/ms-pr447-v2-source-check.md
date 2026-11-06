# PR 447 v2 source-only consistency review

Reviewer /root/ms447_source; retrieved 2026-11-05. No repository, implementation, graph, or backend outputs consulted. No new expected tax values generated.

Sources:
- MS DOR 2024 80-100 https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100241.pdf
- MS DOR 2025 80-100 https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
- MS DOR FAQ https://www.dor.ms.gov/individual/individual-income-tax-frequently-asked-questions
- IRS 2025 Form 1040 instructions https://www.irs.gov/instructions/i1040gi

## Source rules and implications

Both MS booklets, p. 11 line 40: capital computations generally follow federal rules and capital-loss deductions have federal limitations; MS differences require reconciliation. Page 13 line 61: the SE deduction is based on federal SE taxes and what was claimed federally, with Schedule SE attached. These rules do not direct separate per-column application of a fresh capital-loss limit or recomputation of SE tax from household net business income. The FAQ describes a $3,000 annual capital-loss limit, but does not explain spousal allocation of it.

Consequently, use the actual federally claimed household SE deduction (assuming no state adjustment). Its ownership split is an allocation of that total, and independent whole-dollar rounding introduces at most $1 difference between sum of two rounded components and rounded total. Recomputing SE tax on aggregated household net business income does NOT have that guarantee: the underlying tax calculation can be nonlinear and individual-specific. The proposed wording explicitly says CLAIMED federal deduction, which avoids that issue if implemented literally.

For ordinary federal-conforming capital amounts on a joint return, the cited MS instructions support preserving the federal net amount and its loss limitation; they supply no authority to duplicate the joint capital-loss limit across two state columns. With that premise, the columns must allocate the recognized total, and the rounding-only difference is at most $1. However, the booklets do not explicitly prescribe the ownership allocation when one spouse has gains and another losses. I cannot prove a universal column-wise identity or produce a source-established counterexample merely by separately capping each spouse: separate caps would themselves assume an unestablished rule. Do not treat mathematical nonadditivity of a cap as evidence that Mississippi authorizes applying it independently to each column.

The $7 bound is therefore a sound arithmetic bound ONLY when all seven household quantities are the actual state-recognized totals being split across columns, not independently recomputed proxies. For seven additive quantities x_j=a_j+b_j, |sum_j(sign_j*(round(a_j)+round(b_j)-round(x_j)))|<=7. Signed losses do not invalidate that proof. Scope must exclude state/federal reconciliations, additional adjustments, exempt interest, state-exempt capital transactions, carryover differences, and any categories not represented. The MS instructions expressly permit substantive federal/state differences; their magnitudes are not bounded by $7. If the product already excludes those, no additional rounding objection was established by these sources. A universal assertion covering arbitrary actual MS AGIs is too broad.

## Head of Family qualification and override

MS booklets p. 6 require a dependent living in the taxpayer's home all year; a married taxpayer must also live apart from the spouse the entire year. Federal Head of Household instructions, Test 2, generally use more than half a year, allow a parent in a different home (Test 1), and can deem a still-married filer unmarried after six months apart. Thus a year-long resident-dependent assumption addresses only one of several differences.

Necessary supported exceptions/limitations:
- Resident dependent lives there more than half but less than all year: federal HoH does not by itself establish MS Head of Family. A qualification-false override handles this if documented.
- Dependent parent living elsewhere: federal HoH can apply; MS Head of Family fails the stated shared-home criterion. Same override.
- Still married, separated only the last six months: even with a year-long resident dependent, federal HoH can apply but MS Head of Family fails the full-year spouse-separation requirement. Falling back to MS Single is not generally justified: the taxpayer remains married. State married/separate status and spouse information are needed, or the case must be rejected as unsupported.
- Federal QSS does not establish an actual federally claimable dependent (gross-income/joint-return/dependent-taxpayer exceptions). An actual dependent eligibility requirement remains necessary independently of a residence assumption.
- Birth, death, adoption, foster placement, temporary absence, and kidnapping can receive federal residence exceptions. These MS booklets do not explicitly import those exceptions into the state full-year home rule. Do not automatically equate federal deemed residence with state qualification without further authority; allow explicit state qualification or mark these cases unsupported pending evidence.

An explicit, clearly documented baseline assumption of an actual dependent living at home all year is reasonable within a restricted model; a false override makes the nonresident-dependent cases representable. It does not establish unmarried status for federal HoH filers who are still married. An override must mean actual state eligibility, not merely a residence preference.
