# Mapping Layer and Testing Strategy Assessment

Status: assessment of `main` at `1calf716`

Scope: natural inputs, OTS and graph adapters, scalar and batch execution,
selected subordinate forms, public outputs, and the tests intended to cover
those boundaries

## Executive assessment

[Issue #278](https://github.com/mmacpherson/tenforty/issues/278) is valid. Its
two reported failures are in tenforty's mapping and orchestration layer, not in
the underlying OTS or graph form implementations.

The current mapping layer is useful adapter infrastructure, but it is not yet
an adequate correctness boundary. It can express direct renames and some
one-to-many fan-out, but it does not explicitly model the tax semantics that
make a mapping correct: source box, taxpayer ownership, household versus
person scope, gross versus net amounts, derivation rules, selected-form input
completeness, or support by execution path. Those decisions are distributed
across several files and are often represented by an absent dictionary entry
or by zero.

The existing test suite is substantial and valuable, but its mapping coverage
is weaker than its engine and end-result coverage. In a focused run, 123
mapping, form-resolution, subordinate-form, batch, parity, and solver tests
passed while the failures described below remained reproducible. The suite is
therefore demonstrating that the system is internally coherent under its
current assumptions, not that all required tax concepts cross every adapter
boundary correctly.

The central answer to “how did I miss this?” is:

> The tests generally ask whether a form runs and produces a plausible result.
> They rarely ask whether each public concept reaches every dependent form,
> with the correct semantic transformation, through every public execution
> path.

Treating mapping as first-class tax-domain behavior would have caught #278.
Applying that perspective also exposes several other current failures, which
is evidence that the approach is useful rather than tailored to one bug.

## What the mapping layer currently is

The intended public dataflow is approximately:

```text
caller input
    -> TaxReturnInput validation and normalization
    -> canonical tax concepts and derivations
    -> OTS inputs or graph nodes
    -> engine evaluation
    -> canonical public outputs
```

The explicit canonical-concept step does not currently exist. Instead:

- `TaxReturnInput` defines broad natural fields and a small amount of
  normalization.
- OTS main-form mappings and output mappings live in `models.py`.
- OTS subordinate-form mappings and orchestration are separate again in
  `models.py` and `core.py`.
- Graph primary, subordinate, state, and output mappings live in
  `mappings.py`.
- Graph scalar, batch, gradient, and solver code consumes those declarations
  differently in `backends/graph.py`.
- Form activation is implicit: the resolved per-year graph
  (`us_tax_graph_<year>.json`, built by Haskell `resolveForms`) contains every
  form, and demand-driven eval touches only the forms reachable from the
  requested outputs.
- Public result models default fields to zero, including outputs that an
  adapter did not actually materialize.

This is enough structure to avoid hard-coding all form line names in evaluation
code. It is not enough structure to establish that the values at either side
of a mapping mean the same thing.

### Current strengths

- Both backends share a natural input model and public result model.
- Direct mappings are mostly declarative rather than embedded in engine code.
- Graph fan-out through `NATURAL_TO_NODES` makes multiple consumers visible.
- Graph scalar and batch paths reject many unsupported nonzero inputs.
- There are focused form-resolution, subordinate-form, mapping-mechanics,
  backend-parity, property, batch, gradient, and solver tests.
- Generated form schemas make it possible to validate destination existence.

These are good foundations. The missing piece is a semantic contract that
connects them.

### Structural weaknesses

| Dimension | Current behavior | Consequence |
|---|---|---|
| Meaning | Natural names map directly to line/node strings | Similar but distinct concepts are treated as aliases |
| Ownership | Taxpayer, spouse, and household scope is not declared | Aggregate wages cannot safely feed a per-person form |
| Derivation | Gross values are often copied directly to consumers | Net, limited, or adjusted concepts can receive the wrong amount |
| Completeness | No inventory classifies every selected-form input | Missing edges silently retain engine defaults |
| Missing data | Zero often means both known zero and unavailable | Unsupported or unmapped behavior looks calculated |
| Execution paths | Scalar, batch, gradient, and solver materialize inputs separately | A fix can apply to one path but not another |
| Outputs | Scalar and batch output maps are separate and result fields default to zero | Missing outputs appear as legitimate zero results |
| Versioning | Tax-year mappings are copied dictionaries | Semantic differences and omissions are hard to review systematically |

## Validation of issue #278

### Schedule SE: the same conceptual omission in both backends

Schedule SE needs the self-employed person's Social Security wages to determine
how much of the annual Social Security wage base remains available for
self-employment earnings.

On current `main`, a Single filer with $60,000 of self-employment profit
produces the same $8,477.73 of SE tax regardless of W-2 wages:

| W-2 wages | OTS result | Graph result | Expected result |
|---:|---:|---:|---:|
| $0 | $8,477.73 | $8,477.73 | $8,477.73 |
| $120,000 | $8,477.73 | $8,477.73 | $7,633.29 |
| $168,600 | $8,477.73 | $8,477.73 | $1,606.89 |

The engines have different adapters, but those adapters make the same mistake:

- OTS's 2024 and 2025 Schedule SE configuration maps
  `self_employment_income -> L2` but never supplies `L8a`.
- Graph's `w2_income` fan-out supplies Form 1040 wages and Form 8959 Medicare
  wages, but not `us_schedule_se_L5_w2_ss_wages`.

The form engines already expose the required inputs. The wrappers do not fill
them.

The omission was not entirely accidental. A prior implementation fed the
household-level `w2_income` field to a per-person Schedule SE input. That is
wrong for Married Filing Jointly because the API cannot say which spouse earned
the wages or which spouse owns the business. Removing that mapping protected
the ambiguous MFJ case, but it also removed correct behavior for filing
statuses where `w2_income` unambiguously belongs to one person.

This is the core design warning from the bug: `w2_income` is being asked to
stand for at least three concepts:

| Concept | Typical source | Scope | Consumer |
|---|---|---|---|
| Income-tax wages | W-2 Box 1 | return | Form 1040 line 1a |
| Social Security wages | W-2 Boxes 3 and 7 | person | Schedule SE line 8a |
| Medicare wages | W-2 Box 5 | person | Form 8959 |

They may have the same value in a simple return. They are not interchangeable
concepts.

### QBI: different mistakes in OTS and graph

The QBI half of #278 is not a shared missing edge.

- OTS never orchestrates Form 8995 and never supplies the resulting deduction
  to Form 1040 line 13. Its result is effectively “no QBI deduction.”
- Graph feeds gross `self_employment_income` directly to Form 8995. QBI should
  reflect attributable deductions, including the deductible half of SE tax,
  and the deduction also needs the taxable-income and net-capital-gain
  limitations.

For Married Filing Jointly with $220,000 of W-2 wages and $80,000 of
self-employment profit, both backends report AGI of $294,348.18, but taxable
income differs:

| Backend | Taxable income | Implied behavior |
|---|---:|---|
| OTS | $265,148.18 | no QBI deduction |
| Graph | $249,148.18 | $16,000 deduction, exactly 20% of gross profit |

Backend parity correctly reveals a disagreement here, but neither backend is
authoritative for the other.

### Scope of PR #279

[PR #279](https://github.com/mmacpherson/tenforty/pull/279) addresses only the
Schedule SE half of #278.

- It derives person-level Schedule SE wages for non-MFJ scalar returns.
- Because the derived field feeds shared mappings, it covers OTS scalar and
  graph scalar.
- It deliberately retains current MFJ behavior because the public API lacks a
  spouse allocation.
- It does not fix graph batch evaluation.
- It does not fix QBI in either backend.

It is a reasonable compatibility fix, but it is not a complete mapping-layer
solution.

## Why the existing tests passed

### 1. The tests vary the producer, not every limiting input

The Schedule SE property test varies self-employment income and verifies that
SE tax is monotonic. That proves the Schedule C profit edge reaches Schedule
SE. It says nothing about whether W-2 Social Security wages reach the wage-base
calculation.

For this form, at least two independent sensitivities matter:

```text
more self-employment earnings -> SE tax should generally increase
more Social Security wages    -> remaining wage base and SE tax should decrease
```

Only the first relationship was tested.

### 2. “Positive” assertions allow partial mappings to pass

Several subordinate-form tests assert that a tax is greater than zero. For
example, the graph Form 8959 test supplies both W-2 and self-employment income
and asserts only that Additional Medicare Tax is positive. W-2 wages alone
already make it positive, so the test passes even though graph never supplies
the SE earnings consumer.

The current reproduction is $865.57 in OTS versus $450.00 in graph for a
Single filer with $250,000 of W-2 wages and $50,000 of self-employment profit.
Both values are positive.

### 3. The targeted regression protected only one side of an ambiguity

`test_se_tax_mfj_w2_above_ss_base` correctly prevents household wages from
being attributed wholesale to a self-employed spouse. There is no complementary
test for the unambiguous Single case near the wage-base boundary. The regression
therefore encoded “do not map aggregate MFJ wages” as “do not let wages affect
Schedule SE.”

### 4. Backend parity cannot detect a shared omission

OTS and graph both omit the Schedule SE wage input and therefore agree to the
cent. Parity is valuable for divergence, but agreement between two adapters is
not proof of correctness when both were built from the same natural-field
assumptions.

### 5. Mapping unit tests test a generic helper, not production contracts

`tests/mapping_test.py` constructs small invented dictionaries and verifies
renaming, callable mappings, and output filtering. It deliberately asserts that
unmapped input keys are ignored. These tests establish dictionary mechanics;
they cannot detect that production Schedule SE is missing `L8a` or that a
selected Form 8995 input has the wrong economic meaning.

### 6. Broad property tests assert weak global invariants

The main Hypothesis return test covers large numerical ranges, but it does not
include `self_employment_income` in its generated inputs. Its assertions focus
on non-negativity, decomposition, and relationships such as taxable income not
exceeding AGI. A return can satisfy all of those invariants while omitting an
entire deduction or form input.

Random breadth is not a substitute for selecting cases around form activation
conditions and phase changes. The Schedule SE defect is most visible around
the Social Security wage base, a narrow semantic boundary rather than a random
numerical edge.

### 7. QBI tests prove existence, not amount

The graph QBI test verifies only that the implied deduction is greater than
zero. It does not verify the chain from Schedule C profit, through attributable
deductions, through the taxable-income and capital-gain limitations, to Form
1040 line 13. At lower income, the taxable-income limitation can also mask the
fact that gross profit was supplied as QBI.

There is no equivalent OTS test requiring a nonzero or exact QBI deduction.

### 8. Scalar and batch equivalence tests use easy scenarios

The existing one-row batch comparisons mostly use W-2-only returns. They do not
exercise normalization-derived fields, subordinate-form fan-out, AMT outputs,
or status-dependent derivations. Graph batch accepts raw columns rather than a
`TaxReturnInput`, so scalar-only validators and computed fields can be skipped.

### 9. Known conformance errors are tolerated

The solver tests explicitly allow a $5,000 tolerance because the solver varies
only the primary graph node while ordinary evaluation fans one public input
into several independent nodes. That turns an observed mapping-path mismatch
into accepted test behavior rather than a failing contract.

## What a first-class mapping layer would mean

Mapping should be treated as tax-domain code with its own model, review surface,
and correctness gates. Each canonical concept needs a declaration equivalent
to:

```python
MappingContract(
    concept="taxpayer.w2.social_security_wages",
    public_sources=("taxpayer_social_security_wages",),
    owner="taxpayer",
    scope="person",
    amount_kind="gross",
    years=(2024, 2025),
    activation="self_employment_income != 0",
    destinations={
        "ots": ("US_1040_Sched_SE.L8a",),
        "graph": ("us_schedule_se_L5_w2_ss_wages",),
    },
    paths=("scalar", "batch_zip", "batch_cross", "gradient", "solve"),
    missing_policy="unsupported",
)
```

The exact representation could be a dataclass, Pydantic model, or generated
manifest. The required information is:

- stable canonical concept;
- source field and dependencies;
- unit and gross/net character;
- taxpayer, spouse, household, business, or return ownership;
- derivation and activation rules;
- years and supported backends;
- every engine destination;
- support for scalar, batch, gradient, and solver paths;
- missing, ambiguous, default, and unsupported behavior;
- authoritative evidence or a documented approximation.

The layer must preserve distinctions that currently collapse into zero:

```text
Known(0)
Known(amount)
Unavailable(reason)
Ambiguous(reason)
Unsupported(reason)
```

Normalization should produce one canonical scenario, and every backend and
execution path should materialize from that scenario. This is especially
important for graph fan-out: copying a public value into independent nodes
makes scalar evaluation work, but gradient and solver operations vary only one
copy unless fan-out is modeled as a single shared concept.

## How the testing strategy should change

### 1. Validate production mapping declarations

Static CI checks should verify that:

- every declared destination exists for every applicable tax year;
- every selected-form input is provided, imported, derived, intentionally
  defaulted, or explicitly unsupported;
- every public input has a support disposition for each backend and path;
- output concepts have unique, existing destinations;
- incompatible ownership, units, or gross/net semantics cannot be connected;
- a year-to-year form change requires an explicit contract review.

### 2. Test the engine boundary directly

Given one normalized scenario, assert the exact values immediately before each
engine runs:

```python
assert ots_inputs["US_1040_Sched_SE"]["L8a"] == 168_600
assert graph_inputs["us_schedule_se_L5_w2_ss_wages"] == 168_600
```

These tests localize a mapping failure without reimplementing Schedule SE.

### 3. Test every consumer of a concept

Maintain a declared consumer list and perturb one canonical concept at a time.
For example, self-employment earnings affect Schedule 1, Schedule SE, Form
8959, and Form 8995 through different transformations. A test should fail if
any applicable consumer remains unchanged.

Sensitivity tests are often more revealing than “tax is positive” tests:

```text
change the source
    -> assert each destination changed as declared
    -> assert at least one affected public output changed in the expected direction
```

### 4. Require execution-path conformance

Every golden scenario should run through all supported paths:

| Path | Required comparison |
|---|---|
| OTS scalar | exact mapping and authoritative outputs |
| Graph scalar | exact mapping and authoritative outputs |
| Graph batch zip, one row | identical schema and values to graph scalar |
| Graph batch cross, one point | identical schema and values to graph scalar |
| Gradient | match a finite difference of the public concept |
| Solver | replay the solved public value through normal evaluation |

An `xfail` can document an incomplete path, but it should be counted as an
open contract violation rather than conformance.

### 5. Use authoritative goldens in addition to parity

Mapping-boundary tests prove the intended value reached the intended line.
They do not prove the intended concept was correct. A smaller set of worked
IRS/state cases or independently validated results must cover material
interactions and thresholds.

Use the three tools for different jobs:

- authoritative goldens detect shared mistakes;
- backend parity detects divergence;
- boundary materialization tests identify the failed edge.

### 6. Generate cases from semantic boundaries

Schedule SE needs cases below, within, and above the remaining Social Security
wage base. QBI needs cases where the QBI component and taxable-income
limitation bind separately, with and without net capital gain. Person-level
forms need taxpayer, spouse, and ambiguous household cases.

This should supplement, not replace, broad Hypothesis generation.

### 7. Mutation-test important mapping edges

Periodically delete or redirect a mapping edge and require the suite to fail.
Useful mutations include:

- remove a subordinate consumer;
- substitute gross profit for a net derivation;
- swap person and household scope;
- omit scalar normalization in batch mode;
- map an output to a default-zero node;
- resolve only the primary consumer for gradient or solver operations.

If deleting Schedule SE `L8a` does not fail CI, the mapping test strategy is not
yet adequate regardless of line coverage.

## Would this strategy have caught #278?

Yes, through several independent checks:

| Failure | First-class gate that fails |
|---|---|
| OTS Schedule SE `L8a` absent | selected-form completeness and boundary materialization |
| Graph Schedule SE wage node absent | declared fan-out and boundary materialization |
| Household wages used for a person form | ownership compatibility |
| Box 1 treated as Box 3 | canonical-concept compatibility |
| OTS Form 1040 line 13 left at zero | selected-form dependency completeness |
| Graph gross profit treated as QBI | gross/net contract and exact derivation fixture |
| Graph Form 8995 limitations incomplete | selected-form completeness and regime goldens |
| Scalar-only fix misses graph batch | execution-path conformance |

The exact wage-base and QBI worked cases provide defense if a contract
declaration is itself wrong.

## Other failures found by applying this lens

This was a targeted, federal-first audit plus a mechanical sample of state and
batch outputs. It is not a certification of every state mapping.

| Finding | Reproduction on `1calf716` | Classification |
|---|---|---|
| Graph batch skips qualified-dividend normalization | W-2 $100,000 plus qualified dividends $5,000: scalar AGI/tax $105,000/$14,591; batch $100,000/$13,491 | path divergence |
| Both backends omit short-term gains from NIIT | W-2 $300,000 plus STCG $50,000 gives NIIT $0; equivalent LTCG gives $1,900 | shared missing consumer |
| Graph omits SE earnings from Form 8959 | W-2 $250,000 plus SE profit $50,000: OTS $865.57; graph $450.00 | missing consumer/derivation |
| Graph ignores forced itemization | W-2 $100,000 and forced $1,000 itemized deduction: OTS taxable income $99,000; graph $85,400 | unsupported natural policy silently ignored |
| Graph gradient and solver lose fan-out consumers | W-2 $250,000: autodiff 0.32 versus one-dollar finite difference 0.329 | path divergence |
| Indiana and Louisiana expose incomplete state outputs | At W-2 $100,000, IN taxable income is $0 with $3,050 tax; LA AGI and taxable income are $0 with $3,668.75 tax | missing output mapping masked by zero |
| Graph batch omits AMT output | W-2 $200,000 plus ISO gain $100,000: scalar AMT $14,383.50; batch AMT $0 while total tax agrees | missing output mapping masked by zero |
| CT, NE, NM, and OR batch state AGI is zero | W-2 $100,000: scalar state AGI $100,000; one-row batch $0 | malformed batch output mapping |
| OTS mapping helpers silently discard unmapped natural keys | This behavior is asserted by `test_unmapped_keys_ignored` | failure policy |

The audit also found an adjacent graph form-calculation defect rather than an
adapter defect: with W-2 wages of $300,000 and $50,000 of pure short-term gains,
OTS federal income tax is $87,764.75 while graph reports $77,764.75, exactly the
graph result for long-term gains. This distinction matters: a first-class
mapping layer makes it easier to show that the value arrived correctly and the
remaining bug belongs inside the form calculation.

The breadth of these findings indicates a systemic boundary problem, not nine
unrelated typos.

## Implementation options

### Option A: harden the existing dictionaries

Keep the current architecture, add companion semantic metadata, validate
production dictionaries, and add boundary/conformance tests.

- Lowest migration cost.
- Can catch #278 quickly.
- Metadata and behavior can still drift because they remain separate.

### Option B: introduce a canonical scenario and mapping registry

Normalize the public API once into typed canonical concepts, then generate or
materialize OTS, graph scalar, graph batch, gradient, and solver inputs from one
registry.

- Best balance of incremental adoption and long-term correctness.
- Makes ownership, derivation, support, and fan-out explicit.
- Requires migration of existing direct dictionaries and batch behavior.

### Option C: generate adapters from form schemas and tax specifications

Treat canonical concepts as part of the Haskell/form specification and
generate Python mappings and validation manifests.

- Strongest prevention of destination drift.
- Largest architectural and tooling investment.
- Still needs human-authored semantic concepts and authoritative goldens;
  generation alone cannot know that Box 1 is not Box 3.

### Option D: continue fixing backend mappings case by case

- Fastest for each isolated report.
- Does not create a completeness criterion.
- Leaves scalar/batch and shared-omission risks intact.

Option B is the recommended direction. Option A can be an effective first
stage if its metadata is used to generate tests rather than becoming passive
documentation.

## Recommended sequence

1. Add failing characterization tests for both halves of #278, including graph
   batch and exact QBI amounts.
2. Decide the public semantics for W-2 Box 1, Social Security, and Medicare
   wages, including taxpayer/spouse ownership and an explicit compatibility
   fallback.
3. Add a mapping trace that exposes normalized concepts, derivations,
   destinations, defaults, and public output sources for one return.
4. Introduce the registry alongside existing dictionaries and fail CI on
   unclassified selected-form inputs or silently dropped nonzero values.
5. Move scalar-only validation into shared normalization used by batch paths.
6. Centralize scalar and batch output contracts and stop zero-filling
   unavailable outputs.
7. Add consumer-sensitivity, one-row path-conformance, and finite-difference
   tests generated from the registry.
8. Extend the semantic audit across all state input and output mappings.
9. Add mapping-edge mutations as a release or scheduled quality gate.

The mapping layer becomes first-class when removing a material edge, replacing
a net amount with a gross one, or skipping normalization in one execution path
causes a precise mapping test to fail before a user can observe the tax error.
