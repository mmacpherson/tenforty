# Itemized deductions

Pass federal itemized deductions as one `tenforty.FederalItemized`:

```python
import tenforty

tenforty.evaluate_return(
    year=2025,
    filing_status="Single",
    w2_income=150_000,
    state_and_local_taxes=8_000,
    federal_itemized=tenforty.FederalItemized(regular_tax=25_000, amt=10_000),
)
```

## What the two amounts mean

Both are amounts **you prepare**, already net of every federal floor, cap and
limitation (the 7.5%-of-AGI medical floor, charitable percentage limits and so
on). Both **exclude every tax** that `state_and_local_taxes` carries, the
Schedule A line 5d total. Those taxes are entered, capped and added back for
the AMT from that input alone, so don't count them twice.

| Field | Meaning |
|---|---|
| `regular_tax` | The itemized deductions other than taxes allowed for the regular tax. |
| `amt` | The same deductions as allowed for the alternative minimum tax: what stays deducted in AMTI. Where Form 6251 refigures one of them, use the refigured amount. Investment interest (line 2c) is one example; `amt` may exceed `regular_tax`. |

Both are required. There is no default AMT treatment. If none of your
deductions is refigured for the AMT, `amt` equals `regular_tax`, but that is
your determination to make, not a default.

`regular_tax - amt` enters AMTI only when the Form 1040 itemizes. A return that
takes the standard deduction never deducted these amounts.

`FederalItemized` is a **federal** amount. It is not a state's itemized
deduction, which each state defines by its own rules.

- **North Carolina, Maryland and Maine:** this release keeps them outside
  `backend="auto"`. Each will take its own amount through `state_facts`.
- **Other graph states with an itemized line:** with `backend="graph"`, the
  `regular_tax` amount still reaches the state's line the way the old aggregate
  did. Those states are outside `backend="auto"` scope for this reason
  (tenforty-nuc6), and this is not an exact state computation.
- **Mississippi:** a nonzero `regular_tax` raises `UnsupportedInputError`, as
  the deprecated `itemized_deductions` does; MS itemizing is not supported
  through either input ([State facts](state-facts.md)).

## Batches, derivatives and the solver

- In `evaluate_returns`, `federal_itemized` takes one model or a list of them.
  Each model is one axis value, crossed or zipped like any list input.
- The frame echoes it as `federal_itemized_regular_tax` and
  `federal_itemized_amt` at its signature position. Both are null when not
  supplied, and an explicit `0.0` is echoed as `0.0`.
- Both amounts are held fixed in `marginal_rate(s)` and `solve_for_income`.
  Naming `federal_itemized`, or one of its fields, as `wrt` or `for_input` is
  unsupported and raises `ValueError`.
- A floor inside a prepared amount does not move with income. The medical
  floor is one example.

## What the OTS backend can take

OTS enters `amt` on Schedule A line 16 and `regular_tax - amt` on line 6.

- Line 6 sits outside the line 5e cap. Form 6251 line 2a adds it back.
- Line 16 is not added back.
- So the regular tax deducts `regular_tax`, and the AMT allows `amt`. This
  holds in every OTS federal year, 2018–2025.

It refuses, before anything is computed:

- **`amt` above `regular_tax`.** That would need a negative line 6. The graph
  backend computes it for 2024 and 2025.
- **Any nonzero `FederalItemized` for Arizona, California or New York.** The
  OTS programs for those states read the federal Schedule A line by line, so
  the line 6 / line 16 split would change their state deduction in ways not
  yet established. California, for instance, leaves line 16 but not line 6 out
  of its high-income limitation. The deprecated `itemized_deductions` keeps
  working there for now.

## The deprecated `itemized_deductions`

The old scalar does not say how much of the amount the AMT allows, so each
backend applied its own shipped convention:

| Backend | Treatment | Effect on AMTI | Recorded `value` |
|---|---|---|---|
| OTS | Schedule A line 6, "other taxes" | Added back in full | `added_back` |
| Graph | Line 16, "other itemized deductions" | Allowed in full | `allowed` |

That is ledger entry F12 in [Migrating between backends](migrating-to-graph-default.md).
Mike approved keeping each backend's shipped numbers for a stated transition.
It is a compatibility exception for shipped behaviour, not an AMT default.

- A nonzero value raises `LegacyItemizedDeductionsWarning`, a
  `DeprecationWarning`, once per call.
- Every return whose Form 1040 itemized records the convention in
  `assumptions`. The record has `id` `federal.itemized_deductions.legacy_amt`,
  `state` `null` and `input_path` `federal_itemized.amt`.
- Passing it together with `federal_itemized` raises `ValueError`.
- It will be removed in the first release after these land:
  - the North Carolina, Maryland and Maine itemized slices;
  - the OTS Arizona/California/New York Schedule A follow-ups.
- Owner: tenforty-nuc6.

### Reproducing an old number

These recipes reproduce what each backend computed from
`itemized_deductions=x`. They are **reproductions, not tax advice**: the right
`amt` is whatever the AMT allows for your deductions.

| To reproduce | Pass |
|---|---|
| the OTS number | `FederalItemized(regular_tax=x, amt=0)` |
| the graph number | `FederalItemized(regular_tax=x, amt=x)` |

On OTS, the Arizona, California and New York refusal above still applies.

## Evidence

The AMT treatment was checked against blind derivations from IRS sources:

- [2024 and 2025](validation/derivations/federal-itemized-amt-2024-2025.md),
  on both backends;
- [2018 to 2023](validation/derivations/federal-itemized-amt-2018-2023.md), on
  OTS.
