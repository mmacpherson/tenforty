"""Graph-based tax computation backend with autodiff and solver support."""

from __future__ import annotations

import logging
import pathlib
from functools import lru_cache

from ..form_resolution import resolve_forms
from ..mappings import (
    FILING_STATUS_MAP,
    LINE_TO_NATURAL,
    NATURAL_TO_NODE,
    STATE_FORM_NAMES,
    STATE_NATURAL_TO_NODE,
    STATE_OUTPUT_LINES,
)
from ..models import InterpretedTaxReturn, OTSState, TaxReturnInput

logger = logging.getLogger(__name__)


def _forms_dir() -> pathlib.Path:
    """Get the forms directory path."""
    pkg_forms = pathlib.Path(__file__).parent.parent / "forms"
    if pkg_forms.exists():
        return pkg_forms
    return pathlib.Path(__file__).parent.parent.parent.parent / "forms"


@lru_cache(maxsize=8)
def _load_graph(form_id: str, year: int):
    """Load a graph for a given form and year."""
    try:
        from ..graphlib import Graph
    except ImportError:
        raise ImportError("Graph backend requires tenforty.graphlib module") from None

    form_path = _forms_dir() / f"{form_id}_{year}.json"
    if not form_path.exists():
        return None

    return Graph.from_json(form_path.read_text())


@lru_cache(maxsize=16)
def _link_graphs(year: int, form_ids: tuple[str, ...]):
    """Link a specific set of graphs for a given year."""
    try:
        from ..graphlib import GraphSet
    except ImportError:
        raise ImportError("Graph backend requires tenforty.graphlib module") from None

    gs = GraphSet()

    for form_id in form_ids:
        graph = _load_graph(form_id, year)
        if graph is None:
            raise ValueError(f"Required graph not found: {form_id}_{year}")
        gs.add(form_id, graph)

    # Verify no unresolved imports remain (should be handled by resolve_forms,
    # but strictly enforced here).
    unresolved = gs.unresolved_imports()
    if unresolved:
        mismatched_years = sorted({u.year for u in unresolved if u.year != year})
        if mismatched_years:
            mismatches = sorted(
                {(u.form, u.line, u.year) for u in unresolved if u.year != year}
            )
            mismatch_lines = "\n".join(
                f"- {form}:{line} ({imp_year})" for form, line, imp_year in mismatches
            )
            raise RuntimeError(
                "Graph backend does not support linking mixed-year graphs.\n"
                f"Requested year: {year}\n"
                "Mismatched imports:\n"
                f"{mismatch_lines}"
            )

        missing = sorted({(u.form, u.line, u.year) for u in unresolved})
        missing_lines = "\n".join(
            f"- {form}:{line} ({imp_year})" for form, line, imp_year in missing
        )
        loaded = ", ".join(gs.forms())
        raise RuntimeError(
            "Graph backend cannot link required forms: unresolved imports remain.\n"
            f"Loaded forms: {loaded}\n"
            "Unresolved imports:\n"
            f"{missing_lines}"
        )

    return gs.link()


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
        form_ids = resolve_forms(
            tax_input.year.value,
            tax_input.state.value if tax_input.state else None,
            inputs_dict,
            _forms_dir(),
        )

        graph = _link_graphs(tax_input.year.value, tuple(form_ids))
        filing_status = FilingStatus.from_str(
            FILING_STATUS_MAP.get(tax_input.filing_status, "single")
        )
        evaluator = Runtime(graph, filing_status)

        for input_name in graph.input_names():
            evaluator.set(input_name, 0.0)

        natural_values = inputs_dict

        unsupported: list[tuple[str, object]] = []
        state_mapping = STATE_NATURAL_TO_NODE.get(tax_input.state, {})

        for natural_name, value in natural_values.items():
            if value == 0 or value is None:
                continue

            handled = False

            # 1. Check Federal mapping
            if natural_name in NATURAL_TO_NODE:
                node_name = NATURAL_TO_NODE[natural_name]
                try:
                    evaluator.set(node_name, float(value))
                    handled = True
                except Exception as exc:
                    # It is possible the node is not in the graph if we didn't link that form
                    # (though usually federal forms are always linked).
                    # Re-raise for debugging transparency as per roadmap.
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

        result = {}

        agi = evaluator.eval("us_1040_L11_agi")
        result["federal_adjusted_gross_income"] = agi

        taxable = evaluator.eval("us_1040_L15_taxable_income")
        result["federal_taxable_income"] = taxable

        total_tax = evaluator.eval("us_1040_L24_total_tax")
        result["federal_total_tax"] = total_tax
        result["total_tax"] = total_tax

        # Avoid divide-by-zero behavior inside the graph when AGI is 0.
        result["federal_effective_tax_rate"] = (
            (total_tax / agi * 100.0) if agi > 0 else 0.0
        )

        result["federal_tax_bracket"] = 0.0
        try:
            result["federal_amt"] = evaluator.eval("us_form_6251_L11_amt")
        except Exception as exc:
            logger.debug("AMT evaluation failed (Form 6251 may not be linked): %s", exc)
            result["federal_amt"] = 0.0
        result["state_adjusted_gross_income"] = 0.0
        result["state_taxable_income"] = 0.0
        result["state_total_tax"] = 0.0
        result["state_tax_bracket"] = 0.0
        result["state_effective_tax_rate"] = 0.0

        if tax_input.state and tax_input.state != OTSState.NONE:
            state_result = self._evaluate_state(evaluator, tax_input.state)
            result.update(state_result)
            result["total_tax"] = result["federal_total_tax"] + result.get(
                "state_total_tax", 0.0
            )

        return InterpretedTaxReturn(**result)

    def _evaluate_state(self, evaluator, state: OTSState) -> dict[str, float] | None:
        """Evaluate state outputs from linked graph."""
        if state not in STATE_FORM_NAMES:
            raise ValueError(f"Graph backend does not support state: {state.value}")

        form_name = STATE_FORM_NAMES[state]
        result = {}
        output_map = STATE_OUTPUT_LINES.get(state, {})

        for line_name, result_key in output_map.items():
            result[result_key] = evaluator.eval(f"{form_name}_{line_name}")

        return result

    def _resolve_input_node(
        self, tax_input: TaxReturnInput, var: str, output_node: str | None = None
    ) -> str:
        """Resolve the graph input node for a natural variable.

        Prefers federal vs state mappings based on the output node namespace.
        """
        if isinstance(var, str) and var.startswith(("us_", "ca_")):
            return var

        state_mapping = STATE_NATURAL_TO_NODE.get(tax_input.state, {})
        federal_node = NATURAL_TO_NODE.get(var)
        state_node = state_mapping.get(var)

        if output_node and output_node.startswith("us_"):
            input_node = federal_node or state_node
        elif output_node and output_node.startswith("ca_"):
            input_node = state_node or federal_node
        else:
            input_node = state_node or federal_node

        if input_node:
            return input_node

        input_node = var
        if not isinstance(input_node, str):
            input_node = str(input_node)
        if not input_node.startswith(("us_", "ca_")):
            input_node = f"us_1040_{input_node}"
        return input_node

    def gradient(
        self, tax_input: TaxReturnInput, output: str, wrt: str
    ) -> float | None:
        """Compute gradient using autodiff."""
        if not self.is_available():
            return None

        evaluator, _ = self._create_evaluator(tax_input)

        if output.startswith(("us_", "ca_")):
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

        input_node = self._resolve_input_node(tax_input, wrt, output_node)

        return evaluator.gradient(output_node, input_node)

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

        if output.startswith(("us_", "ca_")):
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

        input_node = self._resolve_input_node(tax_input, var, output_node)

        natural_values = tax_input.model_dump(
            exclude={"year", "state", "filing_status", "standard_or_itemized"}
        )
        current_val = natural_values.get(var, 0)

        if current_val == 0:
            natural_var = var
            # Reverse lookup attempt
            for nat, node in NATURAL_TO_NODE.items():
                if node == var:
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
            return evaluator.solve(output_node, target, input_node, initial_guess)
        except Exception as exc:
            msg = str(exc)
            if "Failed to converge" in msg or "Zero gradient" in msg:
                return None
            raise
