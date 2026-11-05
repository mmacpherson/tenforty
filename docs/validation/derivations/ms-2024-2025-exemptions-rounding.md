# Mississippi 2024–2025 exemptions, rounding and spouse columns

Evidence kind: independent derivation, Sol source-only supplement, retrieved 2026-11-05. Written before opening any Mississippi specification, mapping, generated graph, fixture, or engine output. Astra's independent release record is archived verbatim in [ms-release-independent-source-astra.md](ms-release-independent-source-astra.md); this supplement independently re-adds its cases and covers additional boundaries. No backend was run to obtain or adjust any value below.

## Official sources and assumptions

- [2024 Form 80-100 instructions](https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100241.pdf), printed pp. 5–7, 21, 26. SHA256 `35d4d7f235cbe22ad05d02ec95845c07b75af714912d8bebf18cc60397972cb7`.
- [2025 Form 80-100 instructions](https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf), printed pp. 5–7, 22, 27. SHA256 `f0d2eb70d26f0934dba9d7dbceb21ee519d038061210f703c084cfcc85f85824`.

Full-year residents. Wages-only except the explicitly signed column-loss arithmetic examples, which are prepared MS-AGI examples rather than wages-only returns. No credits, other taxes, payments, blindness or claimable-filer adjustments. Standard deductions. S=Single; H=Head of Family (the household qualifies and has the listed dependent); MFS=Married Filing Separate; J=MFJ joint/combined. A dependent by itself does not establish Head of Family. No case equates federal qualifying-widow status with MS's spouse-died-in-current-year status.

The table on p.5 grants S/MFS a 2300 deduction and 6000 base exemption, H a 3400 deduction and 8000 base exemption, J a 4600 deduction and 12000 base exemption. Additional exemption on p.6, lines7–10 is 1500 for each federally qualifying dependent, independently of CTC eligibility, and 1500 per taxpayer/spouse age65 box (blindness is another separate box). Age65 begins the day before the 65th birthday. MFS line15 takes half of line12; its line11 is 12000 before that division, so each additional box/dependent contributes 750 on that separate return. The MFS examples state the complete line9 count for that return, not an invented transfer of unused allowances. Spouse age is meaningful only for married statuses.

Whole-dollar entries: 2024 p.21 / 2025 p.22 direct amounts below 50 cents downward and amounts at least 50 cents upward. All cases with cents here are nonnegative ties. No negative tie is used; official instructions do not settle negative ties, per the separately archived NY/OR negative-half record. Signed loss inputs are integral. For joint columns, split the unrounded household MS AGI into taxpayer `G−B` and spouse `B`, then round each column's line13 entry independently. Thus 50000.98 with spouse25000.49 gives 25000+25000, not a rounded household50001 followed by a second split.

## Exact allocation proof (approved SOL-31)

The instructions pp.5/7 permit allocating the joint standard/itemized deduction and exemptions in any manner and instruct selection of the least-tax method. Let rounded Column A/B line13 entries be `a,b`, total lawful deduction plus exemption be `K>=0`, and `T=a+b−K`. The minimum remaining balance taxed at 4.7% (2024) or 4.4% (2025) is exactly

`max(0, T − min(10000,max(0,a)) − min(10000,max(0,b)))`.

For two nonnegative AGIs, the two columns initially have untaxed capacity `min(10000,a)+min(10000,b)`. Allocate allowances to income above those bands first. If allowances are smaller than that excess, every allowance dollar removes one taxed dollar and neither column becomes negative. If allowances cover the excess, distribute the remainder within the bands; tax is zero. If allowances exceed all income, tax is also zero. Integer entries/allowances allow that construction in whole dollars, without fractional choices. This attains the formula and no allocation can enlarge either original column's band capacity, proving the lower bound is exact.

If either AGI is negative, the p.7 rule combines opposite-sign taxable columns before applying one 10000 band. Allocate allowances to the positive column; combining gives `max(0,T−10000)`, equal to the displayed formula when tax can be positive. If neither AGI is positive, tax is zero. An allocation making one of two initially nonnegative columns negative cannot improve the result: it forces the single-band rule and loses usable capacity. No search or numerical optimizer is required.

A prepared caller can choose a suboptimal division, but the natural API returns the lawful minimum. The required fact is `MS(spouse_adjusted_gross_income=...)`: finite signed dollars for Column B MS AGI (Form80-105 line13), before deductions/exemptions. Taxpayer AGI is household MS AGI minus the supplied fact. Explicit0 is a supplied sole-earner fact; missing is unsupported. It is not federal AGI, wages, taxable income or a percentage. There is no inferred split, default or assumption. Raw column-allocation controls may preserve a prepared return's chosen allocation.

Tax schedule: 2024 p.26 / 2025 p.27 first10000 in each applicable column is taxed at0%; add the remaining balances before multiplying by the year's rate, and round the resulting tax once. Do not round two separate spouse taxes and add them. A negative line16 is a literal form subtraction, not positive taxable income; its resulting tax is0.

## Worked cases

`n/f/s` gives total dependents / filer65 / spouse65. The arithmetic column shows rounded column AGIs, total standard deduction, exemption, and the signed combined line16 sum. For ordinary positive cases this is also aggregate taxable income. The spouse input column is rounded only for display; original fractional inputs are specified below the table. Zero-rate capacity and taxed balance use the exact proof above. Values are independently derived, not published worked examples.

| ID | Status | Household MS AGI | Spouse MS AGI | n/f/s | Column/allowance arithmetic | Taxed balance | 2024 tax | 2025 tax |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| S-base | S | 50000 | — | 0/0/0 | 50000+0−2300−6000=41700 | 31700 | 1489.900→1490 | 1394.800→1395 |
| S-dependent | S | 50000 | — | 1/0/0 | 50000+0−2300−7500=40200 | 30200 | 1419.400→1419 | 1328.800→1329 |
| S-age | S | 50000 | — | 0/1/0 | 50000+0−2300−7500=40200 | 30200 | 1419.400→1419 | 1328.800→1329 |
| S-dependent-age | S | 50000 | — | 1/1/0 | 50000+0−2300−9000=38700 | 28700 | 1348.900→1349 | 1262.800→1263 |
| S-two-dependents | S | 50000 | — | 2/0/0 | 50000+0−2300−9000=38700 | 28700 | 1348.900→1349 | 1262.800→1263 |
| H-dependent | H | 50000 | — | 1/0/0 | 50000+0−3400−9500=37100 | 27100 | 1273.700→1274 | 1192.400→1192 |
| H-dependent-age | H | 50000 | — | 1/1/0 | 50000+0−3400−11000=35600 | 25600 | 1203.200→1203 | 1126.400→1126 |
| MFS-base | MFS | 50000 | — | 0/0/0 | 50000+0−2300−6000=41700 | 31700 | 1489.900→1490 | 1394.800→1395 |
| MFS-age-filer | MFS | 50000 | — | 0/1/0 | 50000+0−2300−6750=40950 | 30950 | 1454.650→1455 | 1361.800→1362 |
| MFS-age-spouse | MFS | 50000 | — | 0/0/1 | 50000+0−2300−6750=40950 | 30950 | 1454.650→1455 | 1361.800→1362 |
| MFS-dependent | MFS | 50000 | — | 1/0/0 | 50000+0−2300−6750=40950 | 30950 | 1454.650→1455 | 1361.800→1362 |
| J-equal | J | 50000 | 25000 | 0/0/0 | 25000+25000−4600−12000=33400 | 13400 | 629.800→630 | 589.600→590 |
| J-one-earner | J | 50000 | 0 | 0/0/0 | 50000+0−4600−12000=33400 | 23400 | 1099.800→1100 | 1029.600→1030 |
| J-spouse-only | J | 50000 | 50000 | 0/0/0 | 0+50000−4600−12000=33400 | 23400 | 1099.800→1100 | 1029.600→1030 |
| J-small-spouse | J | 50000 | 1000 | 0/0/0 | 49000+1000−4600−12000=33400 | 22400 | 1052.800→1053 | 985.600→986 |
| J-below-band | J | 50000 | 9999 | 0/0/0 | 40001+9999−4600−12000=33400 | 13401 | 629.847→630 | 589.644→590 |
| J-at-band | J | 50000 | 10000 | 0/0/0 | 40000+10000−4600−12000=33400 | 13400 | 629.800→630 | 589.600→590 |
| J-above-band | J | 50000 | 10001 | 0/0/0 | 39999+10001−4600−12000=33400 | 13400 | 629.800→630 | 589.600→590 |
| J-both-age | J | 50000 | 25000 | 0/1/1 | 25000+25000−4600−15000=30400 | 10400 | 488.800→489 | 457.600→458 |
| J-dependents-age | J | 50000 | 25000 | 2/1/1 | 25000+25000−4600−18000=27400 | 7400 | 347.800→348 | 325.600→326 |
| J-spouse-loss | J | 50000 | -5000 | 0/0/0 | 55000+-5000−4600−12000=33400 | 23400 | 1099.800→1100 | 1029.600→1030 |
| J-filer-loss | J | 50000 | 50001 | 0/0/0 | -1+50001−4600−12000=33400 | 23400 | 1099.800→1100 | 1029.600→1030 |
| S-income-below-half | S | 18310.49 | — | 0/0/0 | 18310+0−2300−6000=10010 | 10 | 0.470→0 | 0.440→0 |
| S-income-at-half | S | 18310.50 | — | 0/0/0 | 18311+0−2300−6000=10011 | 11 | 0.517→1 | 0.484→0 |
| S-next-income-at-half | S | 18311.50 | — | 0/0/0 | 18312+0−2300−6000=10012 | 12 | 0.564→1 | 0.528→1 |
| S-tax-half-2024 | S | 18800 | — | 0/0/0 | 18800+0−2300−6000=10500 | 500 | 23.500→24 | 22.000→22 |
| S-tax-half-2025 | S | 18425 | — | 0/0/0 | 18425+0−2300−6000=10125 | 125 | 5.875→6 | 5.500→6 |
| J-columns-round-up | J | 50000.50 | 25001 | 0/0/0 | 25000+25001−4600−12000=33401 | 13401 | 629.847→630 | 589.644→590 |
| J-columns-round-down | J | 50000.98 | 25000 | 0/0/0 | 25000+25000−4600−12000=33400 | 13400 | 629.800→630 | 589.600→590 |
| S-band-18299 | S | 18299 | — | 0/0/0 | 18299+0−2300−6000=9999 | 0 | 0.000→0 | 0.000→0 |
| S-band-18300 | S | 18300 | — | 0/0/0 | 18300+0−2300−6000=10000 | 0 | 0.000→0 | 0.000→0 |
| S-band-18301 | S | 18301 | — | 0/0/0 | 18301+0−2300−6000=10001 | 1 | 0.047→0 | 0.044→0 |
| S-band-18310 | S | 18310 | — | 0/0/0 | 18310+0−2300−6000=10010 | 10 | 0.470→0 | 0.440→0 |
| S-band-18311 | S | 18311 | — | 0/0/0 | 18311+0−2300−6000=10011 | 11 | 0.517→1 | 0.484→0 |
| S-band-18312 | S | 18312 | — | 0/0/0 | 18312+0−2300−6000=10012 | 12 | 0.564→1 | 0.528→1 |
| J-band-36599 | J | 36599 | 18300 | 0/0/0 | 18299+18300−4600−12000=19999 | 0 | 0.000→0 | 0.000→0 |
| J-band-36600 | J | 36600 | 18300 | 0/0/0 | 18300+18300−4600−12000=20000 | 0 | 0.000→0 | 0.000→0 |
| J-band-36601 | J | 36601 | 18300 | 0/0/0 | 18301+18300−4600−12000=20001 | 1 | 0.047→0 | 0.044→0 |
| J-band-36610 | J | 36610 | 18300 | 0/0/0 | 18310+18300−4600−12000=20010 | 10 | 0.470→0 | 0.440→0 |
| J-band-36611 | J | 36611 | 18300 | 0/0/0 | 18311+18300−4600−12000=20011 | 11 | 0.517→1 | 0.484→0 |
| J-band-36612 | J | 36612 | 18300 | 0/0/0 | 18312+18300−4600−12000=20012 | 12 | 0.564→1 | 0.528→1 |

For `J-columns-round-up`, the supplied spouse AGI is25000.50 (rounds25001); taxpayer25000.00 stays25000. For `J-columns-round-down`, supplied spouse25000.49 and taxpayer25000.49 each round25000. The household input is preserved as shown.

The Single band begins after wages18300 (=2300+6000+10000); the zero-tax rounding region extends a few dollars farther and differs by year. The equal-earner joint band ends at household36600 (=4600+12000+2×10000), conditional on both columns having sufficient income. The 2024 positive half-tax case is500×.047=23.50→24; the 2025 case125×.044=5.50→6. Additional exemptions are additive: dependent plus age or two dependents each adds3000 before any MFS half division. No arithmetic is calibrated to implementation results.

## 2026-11-05 mapping note after implementation inspection

Evidence label: **non-blind source analysis**, approved in SOL-32. The existing
specification equated federal qualifying surviving spouse with Mississippi's
spouse-died-in-current-year category. Those are different years/statuses. The
MS instructions pp. 5/6 put a qualifying widowed household in Head of Family:
8,000 base exemption, 3,400 deduction, plus 1,500 per dependent. The archived
`H-dependent` case therefore also pins federal QW mapped to MS Head of Family:
50,000 - 3,400 - 8,000 - 1,500 = 37,100; balance 27,100; 2024 tax
27,100 × .047 = 1,273.70 → 1,274; 2025 tax 27,100 × .044 = 1,192.40 → 1,192.
The numeric Head of Family derivation predates the MS spec inspection; the
mapping rationale does not. Death-year returns remain federal MFJ and keep
the married allowances and explicit spouse-AGI requirement.


## 2026-11-05 prepared-allocation check after implementation inspection

Evidence label: **non-blind source arithmetic from the archived rules**.
At equal 25,000/25,000 AGIs, allocating all 4,600 deduction and 12,000
exemption to Column A gives signed line16 entries 8,400 and 25,000. Their
remaining balances are 0 and 15,000. The chosen, suboptimal prepared return
therefore has 2024 tax 15,000 × .047 = 705 and 2025 tax 15,000 × .044 = 660.
This contrasts with the already-blind equal-allocation/minimum case 630/590.
These extra arithmetic values were written before running the new prepared
allocation test, but after implementation inspection; they are not blind.
