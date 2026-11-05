# Federal deduction-choice contract

Status: accepted 2026-10-02 (Mike, recorded on `tenforty-q3r`) and implemented
by `tenforty-q3r`. It replaces the 2026-08-01 legacy contract
(`tenforty-435`), summarized under [History](#history).

## Decision

`standard_or_itemized` takes three values:

| Value | Contract |
| --- | --- |
| `"Auto"` (default) | Take the larger of the federal standard deduction and Schedule A. |
| `"Standard"` | Take the standard deduction, even when Schedule A is larger. |
| `"Itemized"` | Take Schedule A, even when it is smaller or zero. |

`itemized_deductions` and `state_and_local_taxes` supply Schedule A. They
reach Form 1040 line 12 under `"Auto"` (when larger) and `"Itemized"`, and
never under `"Standard"`.

The election sets Form 1040 line 12 and, through it, Form 6251 line 2a: with
Schedule A the line is the Schedule A taxes, otherwise the standard deduction.
It also sets taxable income before the QBI deduction, so the Form 8995 20%
taxable-income cap follows it.

A tax-minimizing choice is a separate decision (`tenforty-f3u`). `"Auto"`
means larger-of, which is not always the cheaper return: for example, a
deduction that lands on gains taxed at 0% saves nothing. TaxCalc chooses on
total tax instead (F19 and F28 in the differential).

The election does not choose a state's deduction method. State forms either
import the federal result (for example Iowa reads Form 1040 taxable income,
Utah and New Mexico read line 12) or apply their own rules to the same
inputs.

## Why

The legacy field gave `"Standard"` the meaning "the larger of the two", so
there was no way to elect the standard deduction when Schedule A was larger.
The election is the taxpayer's (IRC 63(e)): the Form 1040 instructions only
say the larger is usually better. The taxcalc differential found three
returns where this mattered (`tenforty-q3r`, `tenforty-ifa0`), all with
`"Standard"` and a larger Schedule A. Explicit is better than implicit, so
the automatic choice got its own name and `"Standard"` now means what it
says.

Forced itemization stays necessary. A married-filing-separately filer cannot
take the standard deduction when their spouse itemizes ([IRS Topic
501](https://www.irs.gov/taxtopics/tc501)). A filer may also prefer a
smaller federal Schedule A for a state benefit ([Publication
501](https://www.irs.gov/publications/p501)).

## Migration

The default changes from `"Standard"` to `"Auto"`. Before this change
`"Standard"` meant larger-of, and `"Auto"` keeps that behavior, so callers who
use the default see no change in any number.

Callers who pass `"Standard"` explicitly now get the standard deduction even
when Schedule A is larger. To keep the old result, pass `"Auto"`. `"Itemized"`
is unchanged.

## Implementation

**Graph.** Form 1040 reads one input, `us_1040_DeductionChoice`: `-1`
forces the standard deduction, `1` forces Schedule A, and `0` takes the
larger. `UsesItemized` records the outcome, and Form 6251 line 2a imports it.
The Python model lowers each value through `DEDUCTION_CHOICE_ENCODING`, and so
does the browser contract.

**OTS.** OTS has a force-itemize switch (`A18`) but no way to force the
standard deduction; without `A18` it takes the larger. A forced
`"Standard"` therefore sends OTS an empty Schedule A
(`schedule_a_other_deductions` and `schedule_a_state_and_local_taxes` are
zero). The OTS programs for Arizona, California and New York build their own
itemized deduction from the federal Schedule A lines in the federal return,
so an empty one would also empty theirs. For those states OTS raises
`UnsupportedInputError` when `"Standard"` is forced with a nonzero Schedule A
input. The graph backend covers 2024-2025 for all three.

**TaxCalc differential.** TaxCalc has no election switch; it keeps the
itemized total when that strictly lowers tax. A filer who elects the
standard deduction files no Schedule A, so under `"Standard"` the adapter
sends TaxCalc none (`taxcalc_schedule_a_fields`). TaxCalc may still elect
to itemize zero to cut AMT, so F28 excuses `"Auto"` and `"Standard"`; F19
excuses `"Auto"` and `"Itemized"`; F7 still excuses forced itemization below
the standard deduction. These allowances cover TaxCalc's choice, not
tenforty's: the differential separately checks that the deduction tenforty
took (AGI less the QBI deduction less taxable income) is the one the
election requires (`election_violations`), so a backend that ignored the
election would fail even inside an allowance.

## History

The 2026-08-01 contract (`tenforty-435`) kept the legacy asymmetric
meaning: `"Standard"` was the larger-of default and `"Itemized"` forced
Schedule A. It anticipated this three-way choice, with legacy `"Standard"`
lowering to `"Auto"` during a compatibility window. Mike chose instead to
make `"Auto"` the default and give `"Standard"` its plain meaning at once,
since default callers see no change.
