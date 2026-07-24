"""Graph-based tax computation backend with autodiff and solver support."""

from __future__ import annotations

import itertools
import logging
import pathlib
from functools import lru_cache

from ..mappings import (
    FILING_STATUS_MAP,
    LINE_TO_NATURAL,
    NATURAL_TO_NODE,
    NATURAL_TO_NODES,
    STATE_FORM_NAMES,
    STATE_GRAPH_CONFIGS,
    STATE_NATURAL_TO_NODE,
    STATE_OUTPUT_LINES,
)
from ..models import STATE_TO_FORM, InterpretedTaxReturn, OTSState, TaxReturnInput

_STATE_PREFIXES = tuple(f"{name}_" for name in STATE_FORM_NAMES.values())
_ALL_KNOWN_PREFIXES = ("us_", *_STATE_PREFIXES)

logger = logging.getLogger(__name__)

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
    "us_1040_L24_total_tax": "federal_total_tax",
    "us_form_6251_L11_amt": "federal_amt",
    "us_schedule_se_L10_se_tax": "federal_se_tax",
    "us_form_8960_L17_niit": "federal_niit",
    "us_form_8959_L18_total_additional_medicare": "federal_additional_medicare_tax",
}

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
        inputs_dict = tax_input.model_dump(
            exclude={"year", "state", "filing_status", "standard_or_itemized"}
        )
        graph = _load_resolved_graph(tax_input.year.value)
        filing_status = FilingStatus.from_str(
            FILING_STATUS_MAP.get(tax_input.filing_status, "single")
        )
        evaluator = Runtime(graph, filing_status)

        # Unset inputs default to 0 in eval; only the provided values are set
        # below. Zeroing all ~800 country-wide inputs here cost ~1.9 ms/return.
        natural_values = inputs_dict

        unsupported: list[tuple[str, object]] = []
        state_mapping = STATE_NATURAL_TO_NODE.get(tax_input.state, {})

        for natural_name, value in natural_values.items():
            if value == 0 or value is None:
                continue

            handled = False

            # 1. Check Federal mapping (primary + subordinate nodes)
            if natural_name in NATURAL_TO_NODES:
                node_names = NATURAL_TO_NODES[natural_name]
                for i, node_name in enumerate(node_names):
                    try:
                        evaluator.set(node_name, float(value))
                        handled = True
                    except Exception as exc:
                        if i == 0:
                            raise RuntimeError(
                                "Graph backend mapping error: expected input node not found.\n"
                                f"Natural field: {natural_name}\n"
                                f"Expected node: {node_name}"
                            ) from exc

            # 2. Check State mapping
            if natural_name in state_mapping:
                node_name = state_mapping[natural_name]
                try:
                    evaluator.set(node_name, float(value))
                    handled = True
                except Exception as exc:
                    raise RuntimeError(
                        "Graph backend mapping error: expected state input node not found.\n"
                        f"State: {tax_input.state.value if tax_input.state else None}\n"
                        f"Natural field: {natural_name}\n"
                        f"Expected node: {node_name}"
                    ) from exc

            if not handled:
                unsupported.append((natural_name, value))

        if unsupported:
            details = "\n".join(f"- {k}={v!r}" for k, v in unsupported)
            raise NotImplementedError(
                "Graph backend does not yet support some non-zero inputs.\n"
                "Provide these as 0 for now, or use backend='ots'.\n"
                f"Unsupported inputs:\n{details}"
            )

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
            state_result = self._evaluate_state(evaluator, tax_input.state)
            result.update(state_result)
            result["total_tax"] = result["federal_total_tax"] + result.get(
                "state_total_tax", 0.0
            )

        return InterpretedTaxReturn(**result)

    def evaluate_batch(
        self,
        year: int,
        state: OTSState | None,
        inputs: dict[str, list[float]],
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

        # Enforce the same "unsupported non-zero inputs" rule as the single-scenario path.
        state_mapping = STATE_NATURAL_TO_NODE.get(state, {})
        unsupported: list[tuple[str, object]] = []
        for natural_name, values in inputs.items():
            if natural_name in NATURAL_TO_NODES or natural_name in state_mapping:
                continue
            if any(v not in (0, 0.0, None) for v in values):
                sample = next((v for v in values if v not in (0, 0.0, None)), None)
                unsupported.append((natural_name, sample))

        if unsupported:
            details = "\n".join(f"- {k}={v!r}" for k, v in unsupported)
            raise NotImplementedError(
                "Graph backend does not yet support some non-zero inputs.\n"
                "Provide these as 0 for now, or use backend='ots'.\n"
                f"Unsupported inputs:\n{details}"
            )

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
            expanded: dict[str, list[float]] = {name: [] for name in axis_names}
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
            dumped = TaxReturnInput(
                year=year,
                state=state or OTSState.NONE,
                filing_status=status,
                **{name: inputs[name][i] for name in scenario_fields},
            ).model_dump(
                exclude={"year", "state", "filing_status", "standard_or_itemized"}
            )
            for name, value in dumped.items():
                normalized.setdefault(name, []).append(float(value))
        inputs = normalized

        graph = _load_resolved_graph(year)

        # Map natural input names to graph node names
        graph_inputs = {}
        input_names = set(graph.input_names())

        for natural_name, values in inputs.items():
            node_names = []
            if natural_name in NATURAL_TO_NODES:
                node_names.extend(NATURAL_TO_NODES[natural_name])
            if natural_name in state_mapping:
                node_names.append(state_mapping[natural_name])

            for i, node_name in enumerate(node_names):
                if node_name not in input_names:
                    if i == 0:
                        raise RuntimeError(
                            "Graph backend mapping error: expected input node not found.\n"
                            f"State: {state.value if state else None}\n"
                            f"Natural field: {natural_name}\n"
                            f"Expected node: {node_name}"
                        )
                    continue
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
                for line, key in STATE_OUTPUT_LINES.get(state, {}).items():
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
        rev_state = {v: k for k, v in state_mapping.items()}

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

    def _evaluate_state(self, evaluator, state: OTSState) -> dict[str, float] | None:
        """Evaluate state outputs from linked graph."""
        if state not in STATE_FORM_NAMES:
            raise ValueError(f"Graph backend does not support state: {state.value}")

        form_name = STATE_FORM_NAMES[state]
        result = {}
        output_map = STATE_OUTPUT_LINES.get(state, {})

        for line_name, result_key in output_map.items():
            result[result_key] = evaluator.eval(
                _state_output_node(form_name, line_name)
            )

        return result

    def _resolve_input_node(
        self, tax_input: TaxReturnInput, var: str, output_node: str | None = None
    ) -> str:
        """Resolve the graph input node for a natural variable.

        Prefers federal vs state mappings based on the output node namespace.
        """
        if isinstance(var, str) and var.startswith(_ALL_KNOWN_PREFIXES):
            return var

        state_mapping = STATE_NATURAL_TO_NODE.get(tax_input.state, {})
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

        The result is deduplicated. Evaluation is idempotent to a repeated
        node — assigning it twice leaves the same value — but `gradient_sum`
        adds one adjoint per name, so a node named twice would have its
        contribution counted twice. The mapping tables are maintained by hand,
        so the two must not be allowed to disagree.
        """
        if isinstance(var, str) and var.startswith(_ALL_KNOWN_PREFIXES):
            return [var]

        nodes = list(NATURAL_TO_NODES.get(var, []))
        state_node = STATE_NATURAL_TO_NODE.get(tax_input.state, {}).get(var)
        if state_node:
            nodes.append(state_node)

        if nodes:
            return list(dict.fromkeys(nodes))

        return [self._resolve_input_node(tax_input, var, output_node)]

    def gradient(
        self, tax_input: TaxReturnInput, output: str, wrt: str
    ) -> float | None:
        """Compute gradient using autodiff."""
        if not self.is_available():
            return None

        evaluator, _ = self._create_evaluator(tax_input)

        if output.startswith(_ALL_KNOWN_PREFIXES):
            output_node = output
        else:
            output_node = None
            for line, natural in LINE_TO_NATURAL.items():
                if natural == output:
                    output_node = line
                    break
            if output_node is None:
                output_node = f"us_1040_{output}"
            else:
                output_node = f"us_1040_{output_node}"

        input_nodes = self._input_nodes(tax_input, wrt, output_node)

        return evaluator.gradient_multi(output_node, input_nodes)

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

        if output.startswith(_ALL_KNOWN_PREFIXES):
            output_node = output
        else:
            output_node = None
            for line, natural in LINE_TO_NATURAL.items():
                if natural == output:
                    output_node = line
                    break
            if output_node is None:
                output_node = f"us_1040_{output}"
            else:
                output_node = f"us_1040_{output_node}"

        input_nodes = self._input_nodes(tax_input, var, output_node)

        natural_values = tax_input.model_dump(
            exclude={"year", "state", "filing_status", "standard_or_itemized"}
        )
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
            return evaluator.solve_multi(
                output_node, target, input_nodes, initial_guess
            )
        except Exception as exc:
            msg = str(exc)
            if "Failed to converge" in msg or "Zero gradient" in msg:
                return None
            raise
