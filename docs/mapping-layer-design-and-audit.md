# First-Class Mapping Contracts

Status: proposed design with an initial audit of `main` at `db9489f`
Tracking: beads task “Design and audit first-class mapping contracts”
Implementation follow-up: `tenforty-vko`

## Summary

The natural-input mapping layer is tax-domain logic, not serialization glue. It
decides which taxpayer owns a value, which tax concept the value represents,
whether it is gross or net, which forms consume it, how missing information is
represented, and whether every public execution path applies the same
derivation.

Today those decisions are distributed across `TaxReturnInput`, OTS form
configuration, graph mappings, form resolution, scalar evaluation, batch
evaluation, and output interpretation. Most mappings are untyped string pairs.
Zero commonly means both “known zero” and “not supplied,” and backend parity is
often the strongest available assertion. This permits a mapping to be absent or
semantically wrong while both engines and the tests remain internally
consistent.

This document proposes an executable mapping contract registry and a
conformance strategy around it. An initial application of the strategy would
have caught both failures in GitHub issue
[#278](https://github.com/mmacpherson/tenforty/issues/278). It also found eight
additional reproducible defects in the current federal, batch, derivative, and
state-output paths.

## Problem statement

The public dataflow is conceptually:

```text
caller values
    │
    ▼
TaxReturnInput validation and normalization
    │
    ▼
canonical tax concepts and derived values
    │
    ├──► OTS form inputs ──► OTS forms ──┐
    │                                    │
    └──► graph input nodes ─► graph ─────┤
                                         ▼
                              canonical public outputs
```

The implementation currently skips the explicit canonical-concept stage.
Natural fields fan directly into engine-specific destinations:

- OTS natural mappings and subordinate-form phases live in `models.py` and
  `core.py`.
- Graph primary and subordinate mappings live in `mappings.py`.
- Graph scalar, batch, gradient, and solver paths materialize or resolve those
  mappings independently in `backends/graph.py`.
- Output mappings are separate again.

That structure admits five classes of failure:

1. **Missing edge:** a required form input is left at its default.
2. **Wrong edge:** a value reaches the wrong line or node.
3. **Semantic alias:** a nearby but different concept is reused, such as W-2
   Box 1 wages for Box 3 Social Security wages.
4. **Missing derivation:** a gross or household value is used where a net or
   person-specific value is required.
5. **Path divergence:** scalar, batch, gradient, or solver evaluation applies a
   different mapping or normalization contract.

## Goals

The mapping design should make the following properties executable and
testable:

- Every public input has an exact tax meaning, unit, ownership, and missing-data
  policy.
- Every selected engine input is mapped, derived, intentionally defaulted, or
  explicitly unsupported.
- A natural value reaches every consumer that depends on it.
- A destination receives a semantically equivalent value, not merely a
  numerically convenient proxy.
- OTS and graph adapters consume the same normalized scenario.
- Scalar, batch, gradient, and solver paths honor the same contract.
- Public outputs distinguish known zero from unavailable or unsupported.
- Tax-year changes are reviewable as contract changes.
- Failures identify a mapping edge or derivation rather than only a final tax
  discrepancy.

## Non-goals

This design does not require exposing every line of every supported tax form in
the natural API. It does not replace engine formula tests, and it does not claim
that backend parity proves tax correctness. It also does not require duplicating
the OTS or graph formula implementation in Python tests.

## Proposed model

### Canonical concepts

A public field must identify one canonical concept. Concepts that differ by
source box, owner, scope, or derivation remain separate even if they are often
numerically equal.

For example, the current `w2_income` convenience field spans at least three
concepts:

| Concept | Source | Owner | Consumer |
|---|---|---|---|
| Income-tax wages | W-2 Box 1 approximation | return | Form 1040 line 1a |
| Social Security wages | W-2 Boxes 3 and 7 | person | Schedule SE line 8a |
| Medicare wages | W-2 Box 5 plus relevant adjustments | person/return | Form 8959 |

Those concepts need separate canonical values. A convenience policy may derive
one from another in a restricted case, but the approximation must be named,
conditional, and observable.

Likewise, Schedule C profit and qualified business income are distinct:

```text
schedule_c_profit
    └── minus attributable deductions, including deductible half of SE tax
        └──► qualified_business_income
```

### Contract record

The concrete representation can be a dataclass, Pydantic model, or generated
manifest. It should carry information equivalent to:

```python
MappingContract(
    concept="taxpayer.w2.social_security_wages",
    public_sources=("taxpayer_w2_social_security_wages",),
    value_type="money",
    unit="USD",
    owner="taxpayer",
    scope="person",
    tax_years=(2024, 2025),
    activation="self_employment_income != 0",
    derivation=None,
    missing_policy="unsupported",
    destinations={
        "ots": ("US_1040_Sched_SE.L8a",),
        "graph": ("us_schedule_se_L5_w2_ss_wages",),
    },
    execution_paths=("scalar", "batch_zip", "batch_cross", "gradient", "solve"),
    evidence="2024 Schedule SE line 8a",
)
```

At minimum, each contract needs:

- a stable concept identifier;
- public source field or derivation dependencies;
- value type and unit;
- taxpayer, spouse, household, business, or return ownership;
- gross/net and aggregate/per-person scope;
- applicable years and filing conditions;
- engine destinations;
- missing/default/unsupported behavior;
- supported execution paths;
- authoritative evidence or an explicit approximation note.

### Support and missing values

The following states must not collapse into the number zero:

```text
Known(0)
Known(amount)
Unavailable(reason)
Ambiguous(reason)
Unsupported(reason)
```

The internal representation need not expose a tagged union immediately, but
contract validation must retain the distinction. Compatibility APIs may still
return floats during migration if they also emit structured diagnostics.

Examples:

- A Single taxpayer with no Social Security wages has a known zero.
- MFJ aggregate wages with no spouse allocation are ambiguous for Schedule SE.
- A form the selected backend cannot model is unsupported.
- An output omitted from the adapter is unavailable, not a calculated zero.

### Normalize once, materialize many times

`TaxReturnInput` should normalize into a canonical scenario before form
resolution. OTS scalar, graph scalar, graph batch, gradient, and solver should
all consume that representation.

This prevents validators and computed fields from being scalar-only behavior.
For vectorized evaluation, the same rules may have a vectorized implementation,
but they must share contract fixtures and conformance tests.

Fan-out should occur from one canonical concept. Graph consumers should import
or reference a shared canonical input where possible. Copying the same public
number into independent graph nodes makes ordinary evaluation work but causes
autodiff and solving to vary only one copy.

### Form resolution

Form resolution should depend on activated canonical concepts, not raw truthy
public values. A derived nonzero concept must activate its form. A convenience
input should not activate a form merely because it happens to share a source
field.

For every input node on a selected form, the registry must classify the node as:

- provided by a canonical concept;
- imported from another form;
- derived inside the form;
- intentionally defaulted with a documented reason; or
- unsupported for the selected scenario.

An unclassified selected-form input is a CI error. This avoids treating every
optional form line as required while still detecting silent omissions.

### Output contracts

Output mappings need the same treatment as input mappings. A contract states
which engine node represents each public output and whether it is calculated,
proxied, unavailable, or unsupported.

Adapters must not fill an unrequested or unmapped output with zero merely to
complete a model. Scalar and batch APIs should return the same columns and the
same values for a one-row scenario.

## Testing strategy

### Registry validation

Static tests validate that:

- contract destinations exist for every applicable year;
- selected-form inputs have a classification;
- public fields have support declarations for each backend/year/state;
- no destination receives incompatible units or ownership;
- derivation dependencies exist and do not form an invalid phase cycle;
- output contracts are unique or explicitly allow aliases;
- year-to-year graph or OTS field changes require a contract update.

String existence is necessary but not sufficient. Semantic metadata is what
distinguishes Box 1, Box 3, and Box 5 wages.

### Boundary materialization tests

Tests should inspect the boundary immediately before an engine runs. Given a
canonical scenario, assert exact OTS line values and graph node values without
reimplementing the form formula.

For Schedule SE:

```python
assert ots_inputs["US_1040_Sched_SE"]["L8a"] == 168_600
assert graph_inputs["us_schedule_se_L5_w2_ss_wages"] == 168_600
```

For QBI, test each edge separately:

```text
80,000 Schedule C profit
  → 5,651.82 deductible half of SE tax
  → 74,348.18 QBI
  → Form 8995 limitation
  → 1040 line 13
```

### Consumer fan-out tests

Each canonical concept gets a consumer list. A test changes the concept and
asserts that every applicable destination changes by the declared transform.

Examples include taxable interest flowing to Form 1040 and Form 8960, or
self-employment earnings flowing to Schedule 1, Schedule SE, Form 8959, and
Form 8995. This would have exposed graph Form 8959's missing SE input.

### Execution-path conformance

Every golden scenario runs through all supported public paths:

| Path | Required comparison |
|---|---|
| OTS scalar | canonical expected values |
| Graph scalar | canonical expected values |
| Graph batch zip, one row | exact equality with Graph scalar |
| Graph batch cross, one point | exact equality with Graph scalar |
| Gradient | finite-difference derivative of the public natural input |
| Solver | replay solved public input through ordinary evaluation |

A strict `xfail` records a contract violation; it does not make a path
conformant.

### Authoritative goldens

Mapping tests prove that intended dataflow occurred. They cannot prove that the
intended concept or transform is correct. A smaller set of exact worked cases
must therefore come from IRS/state worksheets or another authoritative source.

Backend parity remains useful but is subordinate to those goldens:

- authoritative goldens detect shared mistakes;
- parity detects backend divergence;
- boundary tests localize the failed mapping or derivation.

### Regime and interaction coverage

Cases should be selected from contract activation conditions and form regime
changes, not only broad random ranges.

Schedule SE needs wages below, inside, and above the point where the remaining
wage base constrains SE earnings. QBI needs cases where the QBI component and
taxable-income limitation bind separately, plus capital-gain and threshold
cases. Person-specific forms need taxpayer, spouse, and ambiguous household
allocation cases.

### Mutation testing

CI should periodically remove or redirect important mapping edges and verify
that the suite fails. Useful mutations include:

- delete a subordinate consumer;
- replace a net derivation with its gross source;
- swap taxpayer and household ownership;
- change an output node to zero/default;
- skip normalization in batch evaluation;
- resolve only the primary node of a fan-out concept.

Mapping-edge mutation detection is a more meaningful metric here than line
coverage alone.

## Would this have caught issue #278?

Yes. Different proposed gates catch each part independently:

| Failure | Gate that fails |
|---|---|
| OTS Schedule SE `L8a` absent | selected-form input classification and boundary materialization |
| Graph Schedule SE wage node absent | consumer fan-out and boundary materialization |
| Household wages used for a per-person form | ownership/scope compatibility |
| Box 1 wages inferred as Box 3 wages | canonical-concept compatibility |
| OTS 1040 line 13 left at zero | selected-form output/input dependency classification |
| Graph gross profit used as QBI | gross/net compatibility and derivation fixture |
| Graph Form 8995 net-capital-gain input left at zero | selected-form input classification and fan-out |
| Proposed PR fixes scalar but not graph batch | execution-path conformance |

The exact-value regime tests would also catch both final results even if a
registry declaration were itself wrong. This defense in depth is the positive
signal sought by this design exercise.

## Initial audit of the current layer

### Method

The initial audit covered the 2024 and 2025 natural input model, OTS federal and
subordinate configurations, graph federal mappings, graph scalar/batch
materialization, graph gradient/solver resolution, and a mechanical state-output
check.

The audit used four probes:

1. Inventory every `TaxReturnInput` field and its OTS and graph consumers.
2. Inspect selected form input nodes for unclassified natural concepts.
3. Compare one-row graph scalar and graph batch outputs.
4. Compare graph public-input gradients with one-dollar finite differences.

It also ran exact interaction scenarios around Schedule SE, Form 8959, Form
8960, Form 8995, deduction selection, AMT, and state output mapping.

This is a federal-first audit, not a complete certification of every state
mapping. The full state semantic audit is tracked by `tenforty-r91`.

### Confirmed findings

| ID | Finding | Evidence | Tracking |
|---|---|---|---|
| F1 | Schedule SE and QBI mapping failures from #278 | Both backends keep SE tax at 8,477.73 when Single wages rise from 0 to 168,600; OTS QBI is zero and graph uses gross profit | `tenforty-6hr` |
| F2 | Graph batch skips qualified-dividend normalization | Single, W-2 100,000 and qualified dividends 5,000: scalar AGI 105,000/tax 14,591; batch AGI 100,000/tax 13,491 | `tenforty-92d` |
| F3 | Both backends omit short-term gains from NIIT | Single, W-2 300,000 and STCG 50,000 reports NIIT 0; the equivalent LTCG case reports 1,900 | `tenforty-bd0` |
| F4 | Graph omits SE earnings from Form 8959 | Single, W-2 250,000 and SE profit 50,000: OTS Additional Medicare Tax 865.57; graph 450.00 | `tenforty-ixq` |
| F5 | Graph ignores forced itemization | Single, W-2 100,000, itemized deduction 1,000, forced Itemized: OTS taxable income 99,000; graph 85,400 | `tenforty-435` |
| F6 | Gradient and solver resolve only the primary fan-out node | At W-2 250,000, W-2 autodiff is 0.32 versus finite difference 0.329; SE-income autodiff is 0.24 versus 0.32465154 | `tenforty-e85` |
| F7 | Indiana and Louisiana public state outputs are silently zero | At W-2 100,000, IN taxable income is 0 with tax 3,050; LA adjusted and taxable income are 0 with tax 3,668.75 | `tenforty-s83` |
| F8 | Graph batch output contracts are incomplete | ISO gain 100,000: scalar AMT 14,383.50, batch AMT 0 with matching total tax; CT/NE/NM/OR batch state AGI is 0 while scalar is 100,000 | `tenforty-54q` |
| F9 | OTS silently ignores unsupported public natural inputs | `map_natural_to_ots_input` drops non-mapped keys, unlike graph's usual explicit unsupported error | “Reject or report unsupported OTS natural inputs” |

The STCG probe also exposed an adjacent graph form-calculation defect rather
than an adapter defect: pure short-term gains receive the same preferential
rate as long-term gains. It is tracked separately as `tenforty-gy7` so that
mapping and engine/form responsibilities remain distinguishable.

### Important semantic risks

These are design risks rather than fully quantified defects from this audit:

| Risk | Current behavior | Required decision |
|---|---|---|
| W-2 wage aliases | `w2_income` is sent to Form 1040 income-tax wages and Form 8959 Medicare wages; PR #279 would also infer Social Security wages | Add Box-specific, person-specific concepts and define any compatibility fallback |
| Known zero versus missing | OTS templates and graph inputs default to zero; output adapters also zero-fill missing values | Introduce support/missing status and structured diagnostics |
| Aggregate itemized deductions | One total is placed in federal Schedule A “other deductions” and reused by state forms | Define this as an explicit approximation or model deduction categories |
| Generic state adjustment | The same public name represents different state concepts, including an Alabama standard-deduction amount | Split canonical concepts or declare state-specific semantics |
| Gross natural fan-out | A public amount is copied directly to consumers that require net or limited amounts | Require a derivation per destination concept |
| Output proxies | Some state outputs map after-deduction or federal values into approximate public concepts | Mark proxies explicitly and test their documented meaning |

### Why existing tests did not expose the new findings

The same patterns recur:

- scalar-only tests do not exercise batch normalization or batch outputs;
- parity tests can preserve shared omissions;
- “positive” tests prove a form fires, not that every income source reaches it;
- random ranges avoid decisive boundaries or constrain related fields so that
  missing consumers do not activate;
- gradient tests assert only that a value lies between zero and one;
- output models fill missing values with zero, making incomplete adapters look
  complete;
- mapping unit tests verify generic dictionary mechanics with invented mappings,
  not the repository's production contracts.

For example, the Form 8959 graph test adds W-2 and SE income and checks only that
tax is positive. W-2 income alone already makes it positive, so the missing SE
edge is not observed. Solver tests keep income below the NIIT threshold, where
ignoring the Form 8960 fan-out does not affect the round trip.

## Proposed implementation sequence

### Stage 1: Observability and contract inventory

Introduce the registry alongside existing dictionaries. Generate a report of
public concept support by backend, year, state, and execution path. Add a debug
trace that shows canonical values, derivations, destinations, defaults, and
outputs for one evaluation.

During this stage, existing behavior can remain compatible, but unclassified
selected-form inputs and silent nonzero drops should produce test failures or
structured warnings.

### Stage 2: Shared normalization and path conformance

Move validator and computed-field behavior into one normalization layer used by
scalar and batch evaluation. Centralize graph output contracts. Add one-row
scalar/batch equivalence tests and finite-difference checks for every fan-out
input.

### Stage 3: Semantic input corrections

Split Box 1, Box 3, and Box 5 wage concepts and introduce taxpayer/spouse
ownership where required. Replace gross fan-out with derived concepts for QBI,
Additional Medicare Tax, and other subordinate forms. Decide compatibility
fallbacks explicitly.

### Stage 4: Coverage and enforcement

Add authoritative interaction goldens, selected-form completeness enforcement,
year-diff review, and mapping mutation tests. Extend the semantic inventory to
all state inputs and outputs.

## Acceptance criteria

The mapping layer is first-class when:

- production mapping declarations, not invented dictionaries, are directly
  tested;
- every public concept has semantic and support metadata;
- every selected form input and public output has a declared disposition;
- no nonzero public input is silently dropped;
- unavailable and ambiguous values cannot masquerade as calculated zero;
- scalar and one-row batch results have identical schemas and values;
- public-input gradients match finite differences through every consumer;
- exact authoritative cases cover the intersections and boundaries named by
  contracts;
- deleting or redirecting a material mapping edge causes CI to fail;
- the checks above fail against the pre-fix implementation of #278.

## Decisions for maintainers

Three policy choices affect the implementation shape:

1. Whether the natural API promises exact tax concepts or documented
   approximations. Either is viable, but approximations must be explicit in
   contracts and outputs.
2. Whether person-specific inputs are added incrementally for Schedule SE and
   Form 8959 or introduced as a structured taxpayer/spouse model.
3. Whether compatibility APIs return diagnostics alongside floats or begin
   rejecting ambiguous and unsupported scenarios immediately.

The testing and registry design applies under any of those choices. What it
removes is the current possibility that the choice is made implicitly by a
missing mapping and represented as zero.
