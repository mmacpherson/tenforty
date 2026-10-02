# Independent state fixture evidence

The attached records preserve the blind derivations from session `1b251ce6`,
2026-09-30, previously kept only in the author's scratch directory. They were
derived from official publications independently of tenforty. This archive
does not recalculate or change any expected tax value.

| Tests | Evidence kind | Derivation / source locations |
| --- | --- | --- |
| New CA whole returns | Independent derivation, including table lookups | [CA 2024](CA-2024.md) |
| New MS whole returns | Independent derivation | [MS 2024–2025](MS-2024-2025.md) |
| New VT whole returns | Independent derivation, including table lookups | [VT 2024–2025](VT-2024-2025.md) |
| VT parameter assertions | Published table rows / instruction amounts | Same VT record; parameters are not whole-return examples |
| New WI whole returns | Independent derivation, including table lookups | [WI 2024–2025](WI-2024-2025.md) |
| WI Smiths, taxable income $28,653 | Published worked example | Same WI record; distinguish the derived $28,650 midpoint check |
| WI MFS edges | Published schedule amounts; adjacent deltas are structural checks | Same WI record |
| MO chart examples, taxable incomes $3,090 and $12,000 | Published worked examples | [MO 2024–2025](MO-2024-2025.md) |
| MO whole returns | Independent derivation | Same MO record |
| LA 2024 line 10, in-table W-2 cases (x08) | Published table rows; **not blind** | [LA 2024](LA-2024.md) |
| LA 2024 line 10, above-table cases (x08) | Unverified legacy: continuation rule plus rounding, derived non-blind | Same LA record |

A whole-return answer obtained by subtracting deductions/exemptions and then
looking up a table row is **not** an official worked example. The source supplies
the rules, but the deriver supplies the scenario and arithmetic. Likewise, a
printed tax amount does not certify our orchestration of the rest of the return.

The per-case comments retain intermediate values and precision assumptions.
Known xfails are evidence of remaining defects, not passing legal validation.
An unverified legacy label means independence has not been established; it does
not assert that the original author used tenforty. Existing regression snapshots
must not be promoted to tax-law evidence. The wider fixture audit remains open.

Never generate or repair expected tax amounts by running tenforty, either
backend, generated graphs, or implementation tax tables. Check the independent
answer against the implementation only after the answer has been established.
Backend agreement and finite differences are separately useful structural tests,
not independent value oracles. The repository policy is in `AGENTS.md`.
