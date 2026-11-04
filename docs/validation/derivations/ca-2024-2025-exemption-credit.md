# California 2024–2025 exemption-credit phase-out: source derivation

Evidence kind: **Independent derivation of new component cases**, including published instruction/form amounts. Deriver: Codex Sol,2026-11-03 Pacific time (retrieval UTC timestamps below). No CA spec, repository tax table, existing CA fixture, generated CA graph, or CA backend output was inspected before this record and its CSV were written. Prior NY/OR builds compiled the whole repository mechanically; this deriver did not inspect their CA implementation or numerical outputs. No subagent was used.

## Prior exposure and scope

The assigned bead disclosed the continuous-versus-stepped defect, questioned the2025 thresholds, and quoted a pre-existing2024 Single $250,000 witness (including credit131 and whole-return figures). That quoted witness is **not** claimed as a newly blind value and is excluded from the new case matrix. All new expectations below come from the official FTB sources, transcribed form amounts and standalone integer arithmetic, before CA implementation inspection. No tenforty code or output generated or adjusted any expected value.

This is a **Form540 line32 credit-component** derivation. Federal AGI at line13 and eligible exemption counts are stipulated. It does not derive federal AGI, dependent/age/blind/filing-status eligibility, CA gross tax, AMT, special tax methods or a whole-return net liability. Full-year resident, ordinary personal-income-tax regime; no RDP pro-forma-AGI adjustments. QSS/HOH eligibility is externally stipulated.

## Official sources and retrieval

| Source | Official URL | SHA256 | Retrieved UTC |
| --- | --- | --- | --- |
| 2024-540.pdf | [https://www.ftb.ca.gov/forms/2024/2024-540.pdf](https://www.ftb.ca.gov/forms/2024/2024-540.pdf) | `7729c31d62b474ccc9ae5d1351d5ff72cf5fe5ddcd647f3fb954c0703de49c05` | 2026-11-04T02:40:50.305749+00:00 |
| 2024-540-booklet.pdf | [https://www.ftb.ca.gov/forms/2024/2024-540-booklet.pdf](https://www.ftb.ca.gov/forms/2024/2024-540-booklet.pdf) | `446372719f6e1ce90132d550c268d7804fc44cefcf13514e09bf9ac9907ee374` | 2026-11-04T02:40:50.600451+00:00 |
| 2025-540.pdf | [https://www.ftb.ca.gov/forms/2025/2025-540.pdf](https://www.ftb.ca.gov/forms/2025/2025-540.pdf) | `3be81dcc5d2be3d303596f6fa528e8fdb7cda6644afac6ccf869fc5ebeb7bf1c` | 2026-11-04T02:40:50.303544+00:00 |
| 2025-540-booklet.pdf | [https://www.ftb.ca.gov/forms/2025/2025-540-booklet.pdf](https://www.ftb.ca.gov/forms/2025/2025-540-booklet.pdf) | `b48ee24bd2f9b9e74fe9a5bbc548d15bae0bd1ce53306bef1ff33fc76f8078c1` | 2026-11-04T02:40:50.610771+00:00 |

Booklets B24/B25: printed/PDF p.12, lines7–11 exemption eligibility and aggregation; p.13 line13 federal AGI; **p.14 line32 AGI Limitation Worksheet**, lines a–n. Both p.14 pages were separately rendered and visually checked against the extracted text. Forms F24/F25: Side1 lines6–9; Side2 lines10/11/13/32/33. The personal and dependent credit fields on both forms were separately rendered and visually checked. The web versions of the same booklets were also consulted at [2024](https://www.ftb.ca.gov/forms/2024/2024-540-booklet.html) and [2025](https://www.ftb.ca.gov/forms/2025/2025-540-booklet.html).

No published worked example for this worksheet was used. The supplied publications contain the rules and constants; the cases and arithmetic are this deriver’s work.

## Published constants and status counts

| Year | Personal/blind/senior credit per eligible box count (lines7/8/9) | Dependent credit per eligible dependent (line10) | Single/MFS threshold | MFJ/QSS threshold | HOH threshold |
| --- | ---: | ---: | ---: | ---: | ---: |
| 2024 |149|461|244,857|489,719|367,291|
| 2025 |153|475|252,203|504,411|378,310|

These2025 values are copied from B25 p.14, not obtained by multiplying rounded2024 values by an inflation factor. Form Side1 line7 grants count1 for Single/MFS/HOH and count2 for MFJ/QSS if not claimable. Blind and senior counts are additional eligible counts, generally0–2, determined externally. Claimable-filer rules can reduce the personal count; no eligibility is inferred here. The line10 count excludes the taxpayer/spouse and includes eligible dependents.

The phase-out key is **federal AGI on line13**, not California AGI on line17. State additions/subtractions alone do not move this worksheet’s phase-out band.

## Worksheet arithmetic and rounding

Let A be the already-entered whole-dollar line13 amount, T the published status threshold, B the interval width, F the sum of eligible **counts** in the boxes on lines7/8/9, and D the line10 dependent count. The credit amounts before limitation are P×F and C×D, with P/C from the table above.

- At A≤T, line32 is the unreduced exemption amount from line11.
- At A>T, worksheet lines c–e give excess=A−T and steps=ceil(excess/B), then reduction **per exemption**=6×steps.
- B is **1,250 only for MFS**; it is2,500 for Single, MFJ, HOH and QSS. Do not double the interval just because a joint return has two personal exemptions.
- Lines f–i: personal/age/blind group remaining=max(0,P×F−6×steps×F).
- Lines j–m: dependent group remaining=max(0,C×D−6×steps×D).
- Line n/line32 is the sum of those **separately floored** groups. Combining the two groups before flooring does not reproduce the worksheet when the personal group is exhausted first.

The worksheet and form specify whole dollars. All archived A values are stipulated whole-dollar entries; no fractional-AGI conversion convention or negative-half tie is asserted by this component evidence. The ceiling at worksheet line d is explicit, including exact multiples: excess=B uses1 step; excess=B+1 uses2. There is no interpolation within a band and no tolerance in the expected credits.

The standalone source calculator uses integer division `(max(0,A-T)+B-1)//B`; it imports no repository module. The CSV records each operand, step count and separate group result so its sums can be audited without any backend.

## Worked component cases

| Case | Inputs and worksheet arithmetic | Line32 credit |
| --- | --- | ---: |
| 2024 Single, threshold | A244857,F1,D0; excess0,step0;149 |149|
| 2024 Single, first excess dollar | A244858;ceil(1/2500)=1;149−6 |143|
| 2024 Single, exact first interval | A247357;ceil(2500/2500)=1;149−6 |143|
| 2024 Single, next excess dollar | A247358;ceil(2501/2500)=2;149−12 |137|
| 2024 MFS, exact first interval | A246107;ceil(1250/1250)=1;149−6 |143|
| 2024 MFS, next excess dollar | A246108;ceil(1251/1250)=2;149−12 |137|
| 2024 MFJ, first excess dollar | A489720,F2,D0;ceil(1/2500)=1;298−6×2 |286|
| 2024 HOH, first excess dollar | A367292,F1,D0;149−6 |143|
| 2024 QSS, first excess dollar | A489720,F2,D0;298−12 |286|
| 2025 Single, first excess dollar | A252204,F1,D0;153−6 |147|
| 2025 MFS, first excess dollar | A252204,F1,D0;153−6 |147|
| 2025 MFJ, official threshold | A504411,F2,D0;step0;2×153 |306|
| 2025 MFJ, first excess dollar | A504412,F2,D0;306−12 |294|
| 2025 HOH, official threshold | A378310,F1,D0;step0;153 |153|
| 2025 HOH, first excess dollar | A378311,F1,D0;153−6 |147|
| 2025 QSS, first excess dollar | A504412,F2,D0;306−12 |294|

Dependent/separate-floor witnesses:

- 2024 Single,F1,D1,step24: personal149−144=5; dependent461−144=317; total322.
- Same,step25: personal max(0,149−150)=0; dependent461−150=311; total311. A combined floor of610−300 would incorrectly give310.
- Same,step26: personal0; dependent461−156=305; total305.
- Same,step76: dependent461−456=5;step77: max(0,461−462)=0.
- 2025 Single,F1,D1,step25: personal153−150=3; dependent475−150=325; total328. Step26:0+(475−156)=319. Step79:0+(475−474)=1;step80:0.
- 2024 MFJ,F2,D3,step1: personal298−12=286; dependents1383−18=1365; total1651. Each of the five exemptions loses6, rather than a single6 or12 reduction on the combined amount.
- 2025 MFJ,F2,D3,step1:306−12=294;1425−18=1407; total1701.
- 2024 Single with one eligible blind and one senior count,F3,D3,step25: max(0,447−450)+1383−450=933.
- 2025 same F3/D3/step25:459−450+1425−450=984.

Income-key controls, both years: a Single line13 amount one dollar below its threshold remains unreduced even if a CA addition makes line17 larger by100,000. A line13 amount one dollar above its threshold still loses6 per exemption even if a CA subtraction makes line17 lower by100,000. These assert only the worksheet’s income key, not gross/net return tax.

## Locked boundary matrix and implementation limits

The companion `ca-2024-2025-exemption-credit-cases.csv` contains 7326 source-derived cases: every status/year; no age/blind additions and0/1/3 dependents; each interval boundary n×B for n0–80, at boundary−1/boundary/boundary+1; plus eligible blind/senior group cases. This covers the first drop, exact-multiple behavior, personal exhaustion and the last dependent dollar. Expected values were fixed before implementation inspection.

Cases use externally eligible counts and whole-dollar line13 premises. Later code work must use existing input meanings; this archive does not authorize new natural fields or silently supply eligibility. If the existing surface cannot express a case, record that limitation rather than inventing an input. Mandatory CA tax-table work is separate from this credit computation; no whole-return exactness is claimed here.
