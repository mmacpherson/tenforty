# Evidence Summary

This page lists, for each kind of return the default `backend="auto"` computes,
what the tests check against independent published values and what they only
check structurally. The suite's headline test count does not certify a typical
return. Many tests repeat one regime at many points, and many check agreement
or shape rather than dollar values.

## The four kinds of check

- **Independent value evidence.** Expected amounts come from official
  publications: a published worked example, a transcribed table row, or a
  derivation from the official instructions made without access to tenforty or
  its output, with its arithmetic archived. Only this kind establishes that a
  dollar amount is right. The rules are in
  [Independent state fixture evidence](validation/state-fixtures/README.md) and
  the repository's contributor guide.
- **Parity.** OTS and graph agree (`tests/parity/`,
  `tests/graph_default_compat_test.py`). This catches a regression in either
  engine, but two engines can share a defect.
- **Snapshot.** Legacy expected values whose independence is not established.
  They detect change; they are not tax-law evidence.
- **Property.** Invariants such as monotonicity, finite-difference agreement and
  identities between outputs, swept by hypothesis. These find real defects but
  do not establish dollar values.

[Tax-Calculator](taxcalc-differential-audit.md) is a separate, independently
written federal model. Agreement with it is strong evidence but not authority:
disagreements are adjudicated against the IRS forms, and it has been wrong in
both directions.

## Federal, 2024–2025 (graph)

| Regime | Independent value evidence | Beyond it, structural only |
|---|---|---|
| Wages, interest, dividends, capital gains; Form 1040 line 16 | Every published IRS Tax Table row, and hand-worked Tax Computation and Qualified Dividends and Capital Gain worksheets (`tests/federal_tax_table_test.py`, [federal brackets](validation/derivations/federal-brackets-2024-2025.md)); Tax-Calculator differential | Property sweeps over combinations |
| Deduction election (`standard_or_itemized`) | Blind derivation of whole returns under each election ([deduction choice](validation/derivations/deduction-choice-2024-2025.md), `tests/deduction_choice_values_test.py`) | |
| 2025 OBBBA: SALT phase-down, Schedule 1-A tips, overtime, vehicle interest, senior deduction | Blind source-only inventory with an IRS-only spot check ([OBBBA](validation/derivations/obbba-2025.md), [review](validation/derivations/obbba-2025-review.md), `tests/obbba_2025_test.py`) | The randomized Tax-Calculator strategy does not yet sample Schedule 1-A, age or SALT dimensions |
| Child tax credit and credit for other dependents | Blind derivations ([CTC](validation/derivations/ctc-2024-2025.md), [ODC](validation/derivations/odc-2024-2025.md)) | Refundable ACTC, EITC and dependent care are not computed |
| Self-employment tax, NIIT, Additional Medicare Tax, QBI | Tax-Calculator differential, with each disagreement adjudicated against the IRS form ([audit](taxcalc-differential-audit.md)) | Wiring tests in `tests/subordinate_forms_test.py` and `tests/qbi_form_8995a_test.py` |
| AMT, including ISO gains | Tax-Calculator differential; Form 6251 derivations for specific lines ([line 8z](validation/derivations/form-6251-line-8z-2024-2025.md)) | The legacy `itemized_deductions` aggregate has no defined AMT treatment (F12, `tenforty-nuc6`); no value evidence covers it |

Not modeled at all: separate spouses' earnings, refundable credits, and
retirement and benefit income. See the [API Reference](../README.md#api-reference).

## States in graph scope, 2024–2025

| State | Independent value evidence | Limits |
|---|---|---|
| OR | All 1,004 published tax-table rows, both columns and statuses; subtraction and exemption-credit boundaries; independent derivations with a fresh review ([OR record](validation/state-fixtures/OR-2024-2025.md)) | The state records stipulate federal liability; they do not prove it. Unmodeled Oregon facts are not covered |
| IL | 24 blind-derived returns; exemption limits and dependent counts; an age-binding source check ([IL record](validation/state-fixtures/IL-2024-2025.md), `tests/il_exemption_allowance_test.py`) | The source cases are under 65, not blind, and not claimable; claimable/blind variants and state modifications are not covered |
| MS | A blind source archive ([MS record](validation/state-fixtures/MS-2024-2025.md)) | **Open defects, being fixed (`tenforty-du9.3.1`):** supplied dependents and age do not reach the additional exemption, joint returns use one spouse column, and whole-dollar rounding is missing. Current tests use unrounded products and avoid meaningful spouse splits |
| AK, FL, NV, SD, TN, TX, WA, WY | None needed for state tax: structural checks that state tax is $0 across statuses, years and incomes | The federal return is the federal engine above; these rows add no separate federal evidence |

## Returns on OTS under the default

The default sends 2018–2023, and 2024–2025 in AZ, CA, MA, MI, NC, NJ, NY, OH,
PA and VA, to OTS. Its evidence is older and mostly federal:

- IRS Direct File scenarios for 2022–2023 ([IRS Validation](irs-validation.md),
  a historical record) and formula-derived bracket checks.
- Independent state derivations exist for some of these states (CA 2024, NC,
  NJ/VA, OH, MA); see the
  [state fixture index](validation/state-fixtures/README.md). Several were
  derived to test the graph backend.
- OTS defects found against the official forms are reported upstream and
  recorded as strict xfails ([upstream reports](upstream-ots-reports.md)). OTS
  also has known mapping bugs in our layer, such as OH status and exemptions, NY
  exemptions and the NC child deduction. OTS under the default is a fallback,
  not a certification.

## Not evidence of correct values

- The compatibility corpus pins every OTS/auto difference to a cited mechanism.
  It is not a tax-law check: both engines can share a defect, and a ledger
  width can hide a smaller error inside it.
- Deep and soak property sweeps found real defects, but monotonicity, parity
  and internal consistency do not establish a dollar value.
- The [browser demo](browser-calculator-contract.md) runs the graph for every
  state. That shows it executes and matches the Python runtime, not that every
  state is validated.
