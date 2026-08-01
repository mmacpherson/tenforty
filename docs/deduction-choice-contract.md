# Federal deduction-choice contract

Status: accepted 2026-08-01. Graph implementation is tracked by
`tenforty-435`; the input-model-v2 replacement is tracked by `tenforty-avr`.

## Decision

The existing `standard_or_itemized` field keeps its historical, asymmetric
meaning:

| Legacy value | Contract |
| --- | --- |
| `"Standard"` | Choose the greater of the federal standard deduction and the supplied itemized deductions. This is the automatic/default mode despite the legacy name. |
| `"Itemized"` | Use the supplied federal itemized deductions even when they are zero or less than the standard deduction. |

`itemized_deductions` supplies the itemized candidate in either mode. The
choice controls the federal Form 1040 deduction. It does not independently
force a state's deduction method; state forms consume the resulting federal
figures or apply their own state-specific rules.

## Why

Changing `"Standard"` to mean "force the standard deduction" would silently
change the default behavior. Both backends have historically selected a larger
itemized amount when the caller left the default unchanged, and callers can
currently supply `itemized_deductions` without also changing the choice field.

Removing forced itemization would make valid returns inexpressible. The IRS
says a married-filing-separately filer cannot take the standard deduction when
their spouse itemizes, so that filer must be able to use itemized deductions
even when the amount is smaller. See [IRS Topic 501](https://www.irs.gov/taxtopics/tc501)
and the [2025 Form 1040 instructions](https://www.irs.gov/instructions/i1040gi).
OpenTaxSolver's `A18` input already implements this election; the graph backend
currently drops the choice and is the nonconforming implementation.

This preserves every default-mode result, retains the required election, and
turns the existing OTS behavior into the explicit compatibility contract.

## Graph implementation contract

`tenforty-435` should make the choice part of the graph rather than selecting a
different result after evaluation:

1. Lower the legacy value to a graph input that distinguishes automatic choice
   from forced itemization in scalar, batch, gradient, and solver paths.
2. In Form 1040, use Schedule A when forced; otherwise use the greater of
   Schedule A and the standard deduction.
3. Export or import the actual itemization decision into Form 6251. Inferring
   itemization from `deduction_taken > standard_deduction` is no longer valid
   once a smaller itemized amount can be forced, and would apply the wrong AMT
   line 2a add-back.
4. Preserve the current default-mode results and make the existing F7 strict
   xfail pass. Update the TaxCalc differential signature so forced-itemization
   differences remain attributable for both tenforty backends; TaxCalc does not
   expose an equivalent force switch.
5. Test a forced amount below the standard deduction, forced zero itemization,
   automatic selection on both sides of the threshold, AMT, scalar/batch
   agreement, and a state return that consumes federal deduction results.

## Input model v2

The legacy name cannot honestly represent all three concepts. Input model v2
should introduce a choice with `Auto`, `Standard`, and `Itemized`, defaulting to
`Auto`. During the compatibility window:

- legacy `"Standard"` lowers to `Auto`;
- legacy `"Itemized"` lowers to `Itemized`;
- the new `Standard` value is the first unambiguous way to force the standard
  deduction.

The legacy field can then follow the alias and deprecation schedule owned by
`tenforty-avr` without changing old calculations during that window.
