"""Graph-based tax computation backend with autodiff and solver support."""

from __future__ import annotations

import itertools
import logging
import pathlib
from functools import lru_cache

from ..mappings import (
    DERIVED_NATURAL_SOURCES,
    FILING_STATUS_MAP,
    LINE_TO_NATURAL,
    NATURAL_TO_NODE,
    NATURAL_TO_NODES,
    STATE_FORM_NAMES,
    STATE_GRAPH_CONFIGS,
    derived_chain_factor,
    state_natural_to_node,
    state_output_lines,
)
from ..models import (
    NATURAL_FORM_CONFIG,
    STATE_TO_FORM,
    InterpretedTaxReturn,
    OTSDeductionType,
    OTSState,
    TaxReturnInput,
)

_STATE_PREFIXES = tuple(f"{name}_" for name in STATE_FORM_NAMES.values())
_ALL_KNOWN_PREFIXES = ("us_", *_STATE_PREFIXES)

logger = logging.getLogger(__name__)

_MODEL_CONTEXT_FIELDS = {"year", "state", "filing_status"}


def _natural_values(tax_input: TaxReturnInput) -> dict[str, object]:
    """Lower a validated public input model to graph-compatible natural values."""
    values = tax_input.model_dump(exclude=_MODEL_CONTEXT_FIELDS)
    values["standard_or_itemized"] = float(
        tax_input.standard_or_itemized == OTSDeductionType.ITEMIZED
    )
    return values


_INCOME_TAX_STATES_WITHOUT_GRAPH_CONFIG = {
    s
    for s, form_id in STATE_TO_FORM.items()
    if form_id is not None and s not in STATE_GRAPH_CONFIGS
}
if _INCOME_TAX_STATES_WITHOUT_GRAPH_CONFIG:
    logger.debug(
        "States with income tax forms but no StateGraphConfig (will use OTS backend): %s",
        _INCOME_TAX_STATES_WITHOUT_GRAPH_CONFIG,
    )


FEDERAL_OUTPUT_NODES: dict[str, str] = {
    "us_1040_L11_agi": "federal_adjusted_gross_income",
    "us_1040_L15_taxable_income": "federal_taxable_income",
    "us_form_8995_L16_qbi_deduction": "federal_qbi_deduction",
    "us_1040_L24_total_tax": "federal_total_tax",
    "us_form_6251_L11_amt": "federal_amt",
    "us_schedule_se_L10_se_tax": "federal_se_tax",
    "us_form_8960_L17_niit": "federal_niit",
    "us_form_8959_L18_total_additional_medicare": "federal_additional_medicare_tax",
}
_FEDERAL_FIELD_TO_NODE = {field: node for node, field in FEDERAL_OUTPUT_NODES.items()}

_STATE_ZERO_FIELDS = (
    "state_adjusted_gross_income",
    "state_taxable_income",
    "state_total_tax",
    "state_tax_bracket",
    "state_effective_tax_rate",
)


def _state_output_node(form_name: str, line_name: str) -> str:
    """Resolve a state output line to a graph node name.

    A line that is already a fully-qualified node (e.g. "us_1040_L11_agi", which
    CT/NE/NM/OR reuse for state AGI) is used as-is; a bare line (e.g.
    "L17_ca_agi") is prefixed with the state form. Single- and batch-eval must
    resolve these identically, or the batch silently zero-fills a mis-prefixed
    node.
    """
    if "_" in line_name and not line_name.startswith("L"):
        return line_name
    return f"{form_name}_{line_name}"


def _lowered_input_nodes(
    natural_name: str, state: OTSState | None, year: int
) -> list[str]:
    """Every graph input node one natural input is written to, in one tax year.

    Scalar and batch evaluation both lower through this, so a natural reaches the
    same nodes, and is unsupported in the same cases, on either path.
    """
    nodes = list(NATURAL_TO_NODES.get(natural_name, []))
    state_node = state_natural_to_node(state, year).get(natural_name)
    if state_node:
        nodes.append(state_node)
    return nodes


# Naturals that federal law reads as well as some states. A nonzero value is
# supported only when the federal graph reads it too: a state node alone would
# let the federal return silently ignore it (num_dependents dropped the child
# tax credit for CA and WI, tenforty-aqx.4.1.6).
_FEDERAL_LAW_NATURALS = frozenset({"num_dependents"})


def _reads_nonzero(natural_name: str, node_names: list[str]) -> bool:
    """Whether every return that legally reads this natural is given its value."""
    if natural_name in _FEDERAL_LAW_NATURALS and natural_name not in NATURAL_TO_NODES:
        return False
    return bool(node_names)


@lru_cache(maxsize=4)
def _graph_input_names(year: int) -> frozenset[str]:
    return frozenset(_load_resolved_graph(year).input_names())


def _require_input_nodes(
    natural_name: str, node_names: list[str], state: OTSState | None, year: int
) -> None:
    """Reject a mapping that names a node absent from the year's graph.

    A mapped node is required whatever the value: a zero that has nowhere to go
    is the same mapping defect as a nonzero one, and checking only nonzero
    values is what let scalar evaluation pass a mapping batch evaluation
    rejected.
    """
    missing = [n for n in node_names if n not in _graph_input_names(year)]
    if missing:
        raise RuntimeError(
            "Graph backend mapping error: expected input node not found.\n"
            f"Year: {year}\n"
            f"State: {state.value if state else None}\n"
            f"Natural field: {natural_name}\n"
            f"Expected node: {', '.join(missing)}"
        )


# OTS reads the federal Dependents count in every year but no federal
# computation uses it, so mapping it is not support (tenforty-aqx.4.1.6).
_OTS_IGNORED_NATURALS = frozenset({"num_dependents"})


def _ots_maps_inputs(names: list[str], state: OTSState | None, year: int) -> bool:
    """Whether OTS computes this year's return for the state and reads every input."""
    if any(name in _OTS_IGNORED_NATURALS for name in names):
        return False
    forms = ["US_1040"]
    state_form = STATE_TO_FORM.get(state) if state is not None else None
    if state_form is not None:
        forms.append(state_form)
    configs = [NATURAL_FORM_CONFIG.get((year, form)) for form in forms]
    if any(config is None for config in configs):
        return False
    return all(any(name in config.input_map for config in configs) for name in names)


def _raise_unsupported(
    unsupported: list[tuple[str, object]], state: OTSState | None, year: int
) -> None:
    if not unsupported:
        return
    details = "\n".join(f"- {k}={v!r}" for k, v in unsupported)
    if _ots_maps_inputs([name for name, _ in unsupported], state, year):
        remedy = "Provide these as 0 for now, or use backend='ots'."
    else:
        remedy = (
            "Provide these as 0; the OTS backend does not compute them for this "
            "return either."
        )
    raise NotImplementedError(
        "Graph backend does not yet support some non-zero inputs.\n"
        f"{remedy}\n"
        f"Unsupported inputs:\n{details}"
    )


def _state_output_node_for_field(state: OTSState, field: str, year: int) -> str:
    """Resolve a public state output field to its selected state's graph node."""
    form_name = STATE_FORM_NAMES.get(state)
    if form_name is None:
        raise ValueError(f"Graph backend does not support state: {state.value}")

    line_name = state_output_lines(state, year).get(field)
    if line_name is None:
        raise ValueError(
            f"State {state.value} does not provide output field {field!r} for {year}"
        )
    return _state_output_node(form_name, line_name)


def _federal_effective_tax_rate(total_tax: float, agi: float) -> float:
    """Effective rate as a percent; 0 when AGI is non-positive (avoid div-by-0)."""
    return (total_tax / agi * 100.0) if agi > 0 else 0.0


def _forms_dir() -> pathlib.Path:
    """Get the forms directory path."""
    pkg_forms = pathlib.Path(__file__).parent.parent / "forms"
    if pkg_forms.exists():
        return pkg_forms
    return pathlib.Path(__file__).parent.parent.parent.parent / "forms"


@lru_cache(maxsize=4)
def _load_resolved_graph(year: int):
    """Load the pre-resolved one-graph-per-year (federal + all states).

    The Haskell compiler resolves every cross-form import at build time
    (tenforty-ovz), so there is nothing to link at runtime — just load and
    evaluate. Eval is demand-driven, so requesting one state's outputs only
    touches that state plus federal; the other states stay dormant.
    """
    from ..graphlib import Graph

    return Graph.from_json((_forms_dir() / f"us_tax_graph_{year}.json").read_text())


class GraphBackend:
    """Backend using tax computation graph with autodiff and solver support."""

    name = "graph"
    supported_years = (2024, 2025)

    def __init__(self) -> None:
        """Initialize the graph backend."""
        self._available: bool | None = None

    def is_available(self) -> bool:
        """Check if graph backend is available."""
        if self._available is not None:
            return self._available
        try:
            from ..graphlib import Graph  # noqa: F401

            self._available = True
        except ImportError:
            self._available = False
        return self._available

    def _create_evaluator(self, tax_input: TaxReturnInput):
        """Create an evaluator for the given input."""
        from ..graphlib import FilingStatus, Runtime

        # Determine required forms based on inputs and state
        inputs_dict = _natural_values(tax_input)
        graph = _load_resolved_graph(tax_input.year.value)
        filing_status = FilingStatus.from_str(
            FILING_STATUS_MAP.get(tax_input.filing_status, "single")
        )
        evaluator = Runtime(graph, filing_status)

        # Unset inputs default to 0 in eval; only the provided values are set
        # below. Zeroing all ~800 country-wide inputs here cost ~1.9 ms/return.
        natural_values = inputs_dict

        year = tax_input.year.value
        unsupported: list[tuple[str, object]] = []
        for natural_name, value in natural_values.items():
            node_names = _lowered_input_nodes(natural_name, tax_input.state, year)
            _require_input_nodes(natural_name, node_names, tax_input.state, year)
            if value == 0 or value is None:
                continue
            if not _reads_nonzero(natural_name, node_names):
                unsupported.append((natural_name, value))
                continue
            for node_name in node_names:
                evaluator.set(node_name, float(value))

        _raise_unsupported(unsupported, tax_input.state, year)

        return evaluator, graph

    def evaluate(self, tax_input: TaxReturnInput) -> InterpretedTaxReturn:
        """Evaluate tax return using graph evaluator."""
        if not self.is_available():
            raise RuntimeError("Graph backend is not available")

        if tax_input.state and tax_input.state != OTSState.NONE:
            if tax_input.state not in STATE_FORM_NAMES:
                raise ValueError(
                    f"Graph backend does not support state returns for {tax_input.state.value}"
                )

        evaluator, _graph = self._create_evaluator(tax_input)

        # The resolved per-year graph always carries the full federal return, and
        # unset inputs read as 0, so every node in FEDERAL_OUTPUT_NODES is present
        # and evaluates — a missing one is a real graph defect and should surface,
        # not be swallowed to 0.
        result: dict[str, float] = {
            field: evaluator.eval(node) for node, field in FEDERAL_OUTPUT_NODES.items()
        }

        total_tax = result["federal_total_tax"]
        result["total_tax"] = total_tax
        result["federal_effective_tax_rate"] = _federal_effective_tax_rate(
            total_tax, result["federal_adjusted_gross_income"]
        )
        result["federal_tax_bracket"] = 0.0

        for field in _STATE_ZERO_FIELDS:
            result[field] = 0.0

        if tax_input.state and tax_input.state != OTSState.NONE:
            state_result = self._evaluate_state(
                evaluator, tax_input.state, tax_input.year.value
            )
            result.update(state_result)
            result["total_tax"] = result["federal_total_tax"] + result.get(
                "state_total_tax", 0.0
            )

        return InterpretedTaxReturn(**result)

    def evaluate_batch(
        self,
        year: int,
        state: OTSState | None,
        inputs: dict[str, list[object]],
        statuses: list[str],
        mode: str = "cross",
    ) -> dict[str, list[float]]:
        """Evaluate multiple scenarios efficiently using graph batch API.

        With mode="cross" (default), computes Cartesian product of inputs x statuses.
        With mode="zip", evaluates pre-formed scenarios by zipping input columns.
        """
        if not self.is_available():
            raise RuntimeError("Graph backend is not available")

        if state and state != OTSState.NONE:
            if state not in STATE_FORM_NAMES:
                raise ValueError(
                    f"Graph backend does not support state returns for {state.value}"
                )

        unsupported: list[tuple[str, object]] = []
        for natural_name, values in inputs.items():
            if _reads_nonzero(
                natural_name, _lowered_input_nodes(natural_name, state, year)
            ):
                continue
            sample = next((v for v in values if v not in (0, 0.0, None)), None)
            if sample is not None:
                unsupported.append((natural_name, sample))
        _raise_unsupported(unsupported, state, year)

        if mode == "cross":
            # A natural input fans out to several graph nodes, and the Rust
            # cross API treats every node column as an independent axis — so
            # the product must be taken here, at natural-name granularity,
            # and evaluated as tied rows. Axis order matches the Rust cross:
            # inputs outermost in dict order, statuses innermost.
            axis_names = list(inputs.keys())
            axis_values = [
                list(values) if values else [0.0] for values in inputs.values()
            ]
            expanded: dict[str, list[object]] = {name: [] for name in axis_names}
            expanded_statuses: list[str] = []
            for combo in itertools.product(*axis_values):
                for status in statuses:
                    for name, value in zip(axis_names, combo, strict=True):
                        expanded[name].append(value)
                    expanded_statuses.append(status)
            inputs = expanded
            statuses = expanded_statuses
            mode = "zip"

        # Normalize each materialized row through TaxReturnInput so the batch
        # path applies the same validators (the qualified>ordinary dividend
        # lift) and computed fields (schedule_se_ss_wages, Schedule SE line 8a)
        # that evaluate_return() applies via the single-scenario model. Rows are
        # concrete here — cross mode expanded to zip above — so each entry in
        # `statuses` pairs with one value per column, and the status-dependent
        # line-8a derivation can be done per row on the Python side.
        model_fields = set(TaxReturnInput.model_fields)
        scenario_fields = [name for name in inputs if name in model_fields]
        normalized: dict[str, list[float]] = {}
        for i, status in enumerate(statuses):
            tax_input = TaxReturnInput(
                year=year,
                state=state or OTSState.NONE,
                filing_status=status,
                **{name: inputs[name][i] for name in scenario_fields},
            )
            dumped = _natural_values(tax_input)
            for name, value in dumped.items():
                normalized.setdefault(name, []).append(float(value))
        inputs = normalized

        graph = _load_resolved_graph(year)

        graph_inputs = {}
        for natural_name, values in inputs.items():
            node_names = _lowered_input_nodes(natural_name, state, year)
            _require_input_nodes(natural_name, node_names, state, year)
            for node_name in node_names:
                graph_inputs[node_name] = values

        # Output contract as (node, field) pairs — the same federal node->field
        # map the single path uses (so federal_amt is actually requested, not
        # zero-filled), plus this state's output lines resolved through the
        # shared helper. A pair list, not a dict: one node can feed two fields
        # (CT/NE/NM/OR reuse us_1040_L11_agi for both federal and state AGI),
        # which the single path handles by evaluating that node into each.
        output_pairs: list[tuple[str, str]] = list(FEDERAL_OUTPUT_NODES.items())
        if state and state != OTSState.NONE:
            state_form = STATE_FORM_NAMES.get(state)
            if state_form:
                for key, line in state_output_lines(state, year).items():
                    output_pairs.append((_state_output_node(state_form, line), key))

        requested_nodes = list(dict.fromkeys(node for node, _ in output_pairs))

        # Reject a requested output node that isn't in the graph rather than let
        # eval_scenarios silently return a 0.0 column for it (the failure mode
        # that returned state_adjusted_gross_income=0 for CT/NE/NM/OR).
        graph_nodes = set(graph.node_names())
        missing_outputs = sorted(n for n in requested_nodes if n not in graph_nodes)
        if missing_outputs:
            raise RuntimeError(
                "Graph backend output contract error: requested output nodes "
                f"absent from the resolved graph: {missing_outputs}"
            )

        # Call the batch API
        graph_statuses = [FILING_STATUS_MAP.get(s, s) for s in statuses]
        eval_fn = graph.eval_scenarios_zip if mode == "zip" else graph.eval_scenarios
        status_col, input_cols, output_cols = eval_fn(
            graph_inputs, graph_statuses, requested_nodes
        )

        # Build final dictionary
        rev_fs_map = {v: k.value for k, v in FILING_STATUS_MAP.items()}
        final_results = {"filing_status": [rev_fs_map.get(s, s) for s in status_col]}

        # Translate graph input names back to natural names if possible
        rev_federal = {}
        for nat, nodes in NATURAL_TO_NODES.items():
            for node in nodes:
                rev_federal.setdefault(node, nat)
        rev_state = {v: k for k, v in state_natural_to_node(state, year).items()}

        for node_name, values in input_cols.items():
            natural_name = (
                rev_federal.get(node_name) or rev_state.get(node_name) or node_name
            )
            final_results[natural_name] = values

        # Map each (node, field) pair back — a node feeding two fields sets both.
        for node_name, field in output_pairs:
            final_results[field] = output_cols[node_name]

        count = len(status_col)

        # Federal derived fields — mirror the single path exactly.
        final_results["federal_tax_bracket"] = [0.0] * count
        final_results["federal_effective_tax_rate"] = [
            _federal_effective_tax_rate(ft, agi)
            for ft, agi in zip(
                final_results["federal_total_tax"],
                final_results["federal_adjusted_gross_income"],
                strict=True,
            )
        ]

        # State fields default to 0 when no state was requested (matching the
        # single path); tax_bracket / effective_rate stay 0 even with a state.
        for field in _STATE_ZERO_FIELDS:
            final_results.setdefault(field, [0.0] * count)

        final_results["total_tax"] = [
            f + s
            for f, s in zip(
                final_results["federal_total_tax"],
                final_results["state_total_tax"],
                strict=True,
            )
        ]

        # federal_income_tax = total federal tax minus the subordinate taxes,
        # the decomposition the InterpretedTaxReturn validator applies.
        final_results["federal_income_tax"] = [
            ft - se - niit - admed
            for ft, se, niit, admed in zip(
                final_results["federal_total_tax"],
                final_results["federal_se_tax"],
                final_results["federal_niit"],
                final_results["federal_additional_medicare_tax"],
                strict=True,
            )
        ]

        return final_results

    def _evaluate_state(
        self, evaluator, state: OTSState, year: int
    ) -> dict[str, float] | None:
        """Evaluate state outputs from linked graph."""
        if state not in STATE_FORM_NAMES:
            raise ValueError(f"Graph backend does not support state: {state.value}")

        form_name = STATE_FORM_NAMES[state]
        return {
            result_key: evaluator.eval(_state_output_node(form_name, line_name))
            for result_key, line_name in state_output_lines(state, year).items()
        }

    def _resolve_input_node(
        self, tax_input: TaxReturnInput, var: str, output_node: str | None = None
    ) -> str:
        """Resolve the graph input node for a natural variable.

        Prefers federal vs state mappings based on the output node namespace.
        """
        if isinstance(var, str) and var.startswith(_ALL_KNOWN_PREFIXES):
            return var

        state_mapping = state_natural_to_node(tax_input.state, tax_input.year.value)
        federal_node = NATURAL_TO_NODE.get(var)
        state_node = state_mapping.get(var)

        if output_node and output_node.startswith("us_"):
            input_node = federal_node or state_node
        elif output_node and output_node.startswith(_STATE_PREFIXES):
            input_node = state_node or federal_node
        else:
            input_node = state_node or federal_node

        if input_node:
            return input_node

        input_node = var
        if not isinstance(input_node, str):
            input_node = str(input_node)
        if not input_node.startswith(_ALL_KNOWN_PREFIXES):
            input_node = f"us_1040_{input_node}"
        return input_node

    def _input_nodes(
        self, tax_input: TaxReturnInput, var: str, output_node: str | None = None
    ) -> list[str]:
        """Every graph node a natural input is written to.

        `_create_evaluator` fans one natural input out to several nodes —
        `w2_income` reaches both the 1040 wage line and Form 8959's Medicare
        wages. A derivative or solve with respect to that natural input has to
        account for all of them, so this returns the same set evaluation
        writes; resolving a single "primary" node instead is what let the
        derivative silently omit every subordinate form.

        Federal and state nodes are both included. A node that cannot
        influence the requested output simply contributes a zero partial, so
        there is no need to guess which namespace the caller meant.

        DERIVED naturals count as well. `schedule_se_ss_wages` is computed from
        `w2_income`, so evaluation writes the filer's wages to Schedule SE line 5a
        and the shared social security wage base couples W2 to SE tax — but the
        node is filed under the derived natural's own key, so a gradient that
        consulted only `NATURAL_TO_NODES[var]` dropped that coupling entirely
        (tenforty-hrp). Only derivations with a unit slope contribute: an adjoint
        sum is unweighted, so a factor other than 0 or 1 could not be represented
        here — `derived_chain_factor` raises on one rather than let it be counted
        wrong or dropped in silence.

        This reaches `solve` as well, which names the same nodes and assigns the
        candidate to every one of them. Omitting the derived node left the solver
        searching a function the library does not compute, and converging on a
        point that is not a root.

        The result is deduplicated. Evaluation is idempotent to a repeated
        node — assigning it twice leaves the same value — but `gradient_sum`
        adds one adjoint per name, so a node named twice would have its
        contribution counted twice. The mapping tables are maintained by hand,
        so the two must not be allowed to disagree.
        """
        if isinstance(var, str) and var.startswith(_ALL_KNOWN_PREFIXES):
            return [var]

        nodes = _lowered_input_nodes(var, tax_input.state, tax_input.year.value)
        if var in _FEDERAL_LAW_NATURALS and not _reads_nonzero(var, nodes):
            raise NotImplementedError(
                f"Graph backend cannot vary {var}: the federal return does not "
                "read it yet, so a derivative or solution would omit its federal "
                "effect."
            )

        for derived, source in DERIVED_NATURAL_SOURCES.items():
            if (
                source == var
                and derived_chain_factor(tax_input, derived, source) == 1.0
            ):
                nodes.extend(NATURAL_TO_NODES.get(derived, []))

        if nodes:
            return list(dict.fromkeys(nodes))

        return [self._resolve_input_node(tax_input, var, output_node)]

    def _output_nodes(self, tax_input: TaxReturnInput, output: str) -> list[str]:
        """Resolve a public output to the graph nodes whose sum it denotes."""
        if output.startswith(_ALL_KNOWN_PREFIXES):
            return [output]

        has_state = bool(tax_input.state and tax_input.state != OTSState.NONE)
        if output == "total_tax":
            nodes = [_FEDERAL_FIELD_TO_NODE["federal_total_tax"]]
            if has_state:
                nodes.append(
                    _state_output_node_for_field(
                        tax_input.state, "state_total_tax", tax_input.year.value
                    )
                )
            return nodes

        federal_node = _FEDERAL_FIELD_TO_NODE.get(output)
        if federal_node:
            return [federal_node]

        if output.startswith("state_"):
            if not has_state:
                raise ValueError(f"Output {output!r} requires a state return")
            return [
                _state_output_node_for_field(
                    tax_input.state, output, tax_input.year.value
                )
            ]

        for line, natural in LINE_TO_NATURAL.items():
            if natural == output:
                return [f"us_1040_{line}"]

        return [f"us_1040_{output}"]

    def gradient(
        self, tax_input: TaxReturnInput, output: str, wrt: str
    ) -> float | None:
        """Compute sensitivity, using planning tangents for tax-table quantization."""
        if not self.is_available():
            return None

        evaluator, _ = self._create_evaluator(tax_input)
        output_nodes = self._output_nodes(tax_input, output)
        input_nodes = self._input_nodes(tax_input, wrt, output_nodes[0])

        return evaluator.gradient_multi_output(output_nodes, input_nodes)

    def gradients(
        self, tax_input: TaxReturnInput, output: str
    ) -> dict[str, float] | None:
        """Compute gradients for every continuous public input.

        Smooth inputs share one reverse traversal per resolved output. At an
        active piecewise boundary, each affected input retains the scalar API's
        composed right-hand derivative convention. Table-dependent outputs
        instead compose scoped planning tangents and directional tie rules.
        """
        if not self.is_available():
            return None

        evaluator, _ = self._create_evaluator(tax_input)
        output_nodes = self._output_nodes(tax_input, output)
        state_mapping = state_natural_to_node(tax_input.state, tax_input.year.value)
        natural_names = [
            name
            for name, field in TaxReturnInput.model_fields.items()
            if field.annotation is float
            and (name in NATURAL_TO_NODES or name in state_mapping)
        ]
        input_groups = [
            self._input_nodes(tax_input, name, output_nodes[0])
            for name in natural_names
        ]
        values = evaluator.gradients_multi_output(output_nodes, input_groups)

        return dict(zip(natural_names, values, strict=True))

    def solve(
        self, tax_input: TaxReturnInput, output: str, target: float, var: str
    ) -> float | None:
        """Solve for input value that produces target output.

        Uses Newton's method with autodiff gradients. The solver:
        - Constrains solutions to be non-negative (income inputs can't be negative)
        - Uses a minimum initial guess of $50k to avoid zero-gradient regions
        - Returns None if the solver doesn't converge

        Limitations:
        - For underdetermined problems (multiple inputs affect the output),
          returns *a* solution, not necessarily *the* solution
        - May not converge for targets outside the achievable range
        """
        if not self.is_available():
            return None

        evaluator, _ = self._create_evaluator(tax_input)
        output_nodes = self._output_nodes(tax_input, output)
        input_nodes = self._input_nodes(tax_input, var, output_nodes[0])

        natural_values = _natural_values(tax_input)
        current_val = natural_values.get(var, 0)

        if current_val == 0:
            natural_var = var
            # Reverse lookup attempt
            for nat, nodes in NATURAL_TO_NODES.items():
                if var in nodes:
                    natural_var = nat
                    break
            current_val = natural_values.get(natural_var, 0)

        min_guess = 50000
        tax_estimate = max(target * 5, min_guess) if target > 0 else min_guess
        if current_val > 0:
            initial_guess = max(current_val, tax_estimate)
        else:
            initial_guess = tax_estimate

        try:
            return evaluator.solve_multi_output(
                output_nodes, target, input_nodes, initial_guess
            )
        except Exception as exc:
            msg = str(exc)
            if "Failed to converge" in msg or "Zero gradient" in msg:
                return None
            raise
