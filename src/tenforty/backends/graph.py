"""Graph-based tax computation backend with autodiff and solver support."""

from __future__ import annotations

import pathlib
from functools import lru_cache

from ..mappings import (
    CAPITAL_GAINS_FIELDS,
    FILING_STATUS_MAP,
    LINE_TO_NATURAL,
    NATURAL_TO_LINE,
    STATE_FORM_NAMES,
    STATE_OUTPUT_LINES,
)
from ..models import InterpretedTaxReturn, OTSState, TaxReturnInput


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


@lru_cache(maxsize=8)
def _load_linked_graph(year: int, state: OTSState | None):
    """Load and link federal and state graphs for a given year."""
    try:
        from ..graphlib import GraphSet
    except ImportError:
        raise ImportError("Graph backend requires tenforty.graphlib module") from None

    gs = GraphSet()

    federal = _load_graph("us_1040", year)
    if federal is None:
        raise ValueError(f"No federal graph available for year {year}")
    gs.add("us_1040", federal)

    if state and state != OTSState.NONE and state in STATE_FORM_NAMES:
        form_name = STATE_FORM_NAMES[state]
        state_graph = _load_graph(form_name, year)
        if state_graph is None:
            raise ValueError(f"No {form_name} graph available for year {year}")
        gs.add(form_name, state_graph)

    unresolved = gs.unresolved_imports()
    if unresolved:
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

        graph = _load_linked_graph(tax_input.year.value, tax_input.state)
        filing_status = FilingStatus.from_str(
            FILING_STATUS_MAP.get(tax_input.filing_status, "single")
        )
        evaluator = Runtime(graph, filing_status)

        for input_name in graph.input_names():
            evaluator.set(input_name, 0.0)

        natural_values = tax_input.model_dump(
            exclude={"year", "state", "filing_status"}
        )

        cap_gains = natural_values.get(
            "short_term_capital_gains", 0
        ) + natural_values.get("long_term_capital_gains", 0)
        if cap_gains != 0:
            evaluator.set("us_1040_L7a_capital_gain", float(cap_gains))

        for natural_name, value in natural_values.items():
            if natural_name in CAPITAL_GAINS_FIELDS:
                continue
            if natural_name in NATURAL_TO_LINE and value != 0:
                line_name = NATURAL_TO_LINE[natural_name]
                try:
                    evaluator.set(f"us_1040_{line_name}", float(value))
                except Exception as exc:
                    raise RuntimeError(
                        "Graph backend mapping error: expected input node not found.\n"
                        f"Natural field: {natural_name}\n"
                        f"Expected node: us_1040_{line_name}"
                    ) from exc

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

        agi = evaluator.eval("us_1040_L11a_agi")
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

    def gradient(
        self, tax_input: TaxReturnInput, output: str, wrt: str
    ) -> float | None:
        """Compute gradient using autodiff."""
        if not self.is_available():
            return None

        evaluator, _ = self._create_evaluator(tax_input)

        output_node = LINE_TO_NATURAL.get(output, output)
        for line, natural in LINE_TO_NATURAL.items():
            if natural == output:
                output_node = line
                break
        else:
            output_node = output

        input_node = NATURAL_TO_LINE.get(wrt, wrt)

        return evaluator.gradient(f"us_1040_{output_node}", f"us_1040_{input_node}")

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

        output_node = output
        for line, natural in LINE_TO_NATURAL.items():
            if natural == output:
                output_node = line
                break

        input_node = NATURAL_TO_LINE.get(var, var)

        natural_values = tax_input.model_dump(
            exclude={"year", "state", "filing_status"}
        )
        current_val = natural_values.get(var, 0)
        if current_val == 0:
            natural_var = var
            for nat, line in NATURAL_TO_LINE.items():
                if line == var:
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
            return evaluator.solve(
                f"us_1040_{output_node}", target, f"us_1040_{input_node}", initial_guess
            )
        except Exception as exc:
            msg = str(exc)
            if "Failed to converge" in msg or "Zero gradient" in msg:
                return None
            raise
