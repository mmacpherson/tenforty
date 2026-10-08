"""Tests for backend abstraction layer."""

import pytest

from tenforty.backends import OTSBackend, available_backends, get_backend
from tenforty.backends.protocol import TaxBackend
from tenforty.mappings import state_natural_to_node
from tenforty.models import OTSState, TaxReturnInput


class TestOTSBackend:
    """Tests for OTS backend."""

    def test_ots_backend_is_available(self):
        """OTS backend should be available when otslib is installed."""
        backend = OTSBackend()
        assert backend.is_available()

    def test_ots_backend_implements_protocol(self):
        """OTS backend should implement TaxBackend protocol."""
        backend = OTSBackend()
        assert isinstance(backend, TaxBackend)

    def test_ots_backend_evaluate(self):
        """OTS backend should evaluate tax returns."""
        backend = OTSBackend()
        tax_input = TaxReturnInput(year=2024, w2_income=100_000)
        result = backend.evaluate(tax_input)
        assert result.federal_total_tax > 5000

    def test_ots_backend_supported_years(self):
        """OTS backend should support years 2018-2024."""
        backend = OTSBackend()
        assert 2018 in backend.supported_years
        assert 2024 in backend.supported_years

    def test_ots_backend_gradient_returns_none(self):
        """OTS backend does not support autodiff."""
        backend = OTSBackend()
        tax_input = TaxReturnInput(year=2024, w2_income=100_000)
        result = backend.gradient(tax_input, "total_tax", "w2_income")
        assert result is None

    def test_ots_backend_solve_returns_none(self):
        """OTS backend does not support solver."""
        backend = OTSBackend()
        tax_input = TaxReturnInput(year=2024, w2_income=100_000)
        result = backend.solve(tax_input, "total_tax", 10000, "w2_income")
        assert result is None


class TestBackendSelection:
    """Tests for backend selection logic."""

    def test_available_backends_includes_ots(self):
        """OTS should always be in available backends."""
        backends = available_backends()
        assert "ots" in backends

    def test_get_backend_ots(self):
        """Should be able to get OTS backend explicitly."""
        backend = get_backend("ots")
        assert backend.name == "ots"

    def test_get_backend_default_is_ots(self):
        """Default backend should be OTS."""
        backend = get_backend()
        assert backend.name == "ots"


@pytest.mark.requires_graph
class TestGraphBackend:
    """Tests for graph backend (requires graph module)."""

    def test_graph_backend_available(self):
        """Graph backend availability test."""
        from tenforty.backends import GraphBackend

        backend = GraphBackend()
        assert backend.is_available()

    def test_graph_backend_evaluate(self):
        """Graph backend should evaluate tax returns."""
        from tenforty.backends import GraphBackend

        backend = GraphBackend()
        tax_input = TaxReturnInput(year=2024, w2_income=100_000)
        result = backend.evaluate(tax_input)
        assert result.federal_total_tax > 0

    def test_graph_backend_ca_requires_all_imports(self):
        """Graph backend should load full CA import closure."""
        from tenforty import evaluate_return

        result = evaluate_return(
            year=2024, state="CA", w2_income=100_000, backend="graph"
        )
        assert result.state_total_tax > 0

    def test_graph_backend_gradient(self):
        """Graph backend should compute gradients."""
        from tenforty.backends import GraphBackend

        backend = GraphBackend()
        tax_input = TaxReturnInput(year=2024, w2_income=100_000)
        result = backend.gradient(tax_input, "L24_total_tax", "w2_income")
        assert result is not None
        assert 0 < result < 1

    def test_graph_backend_solve(self):
        """Graph backend should solve for inputs."""
        from tenforty.backends import GraphBackend

        backend = GraphBackend()
        tax_input = TaxReturnInput(year=2024, w2_income=0)
        # 2024 Tax Table row 67,950-68,000, Single: a reportable tax amount.
        result = backend.solve(tax_input, "L24_total_tax", 10_008, "w2_income")
        assert result is not None
        assert result > 0

    def test_graph_backend_integration_resolves_schedule_d(self):
        """Integration test: capital gains flow through Schedule D in the resolved graph."""
        from tenforty.backends import GraphBackend

        backend = GraphBackend()
        # Providing capital gains routes through us_schedule_d
        tax_input = TaxReturnInput(
            year=2024, w2_income=100_000, short_term_capital_gains=5000
        )

        # The resolved per-year graph already wires Schedule D into us_1040, so
        # the gains must reach total tax.
        result = backend.evaluate(tax_input)

        # Verify result is sane (tax should include tax on gains)
        assert result.federal_total_tax > 0

    @pytest.mark.parametrize(
        ("year", "state"),
        [
            (year, state)
            for year in (2024, 2025)
            for state in OTSState
            if "num_dependents" in state_natural_to_node(state, year)
        ],
    )
    def test_dependents_a_state_reads_are_refused_while_federal_ignores_them(
        self, year, state
    ):
        """A state mapping must not let the federal return drop num_dependents.

        The federal graph does not read num_dependents, so accepting it because a
        state does would silently omit the child tax credit (tenforty-aqx.4.1.6).
        """
        from tenforty import evaluate_return, evaluate_returns

        with pytest.raises(NotImplementedError, match="num_dependents"):
            evaluate_return(
                year=year,
                state=state.value,
                filing_status="Married/Joint",
                w2_income=60_000,
                num_dependents=2,
                backend="graph",
            )
        with pytest.raises(NotImplementedError, match="num_dependents"):
            evaluate_returns(
                year=[year],
                state=[state.value],
                filing_status=["Married/Joint"],
                w2_income=[60_000],
                num_dependents=[2],
                backend="graph",
            )

    @pytest.mark.parametrize(
        ("year", "state", "output"),
        [
            (year, state, output)
            for year in (2024, 2025)
            for state in ("WI", "CA", None)
            for output in ("federal_total_tax", "state_total_tax", "total_tax")
            if state or output != "state_total_tax"
        ],
    )
    def test_dependents_cannot_be_varied_while_federal_ignores_them(
        self, year, state, output
    ):
        """Gradient and solve must not differentiate num_dependents state-only.

        Evaluation at zero dependents is allowed, so the refusal has to come from
        choosing num_dependents as the variable (tenforty-aqx.4.1.6).
        """
        from tenforty.backends import GraphBackend

        backend = GraphBackend()
        tax_input = TaxReturnInput(
            year=year,
            state=state,
            filing_status="Married/Joint",
            w2_income=60_000,
        )
        backend.evaluate(tax_input)
        with pytest.raises(NotImplementedError, match="num_dependents"):
            backend.gradient(tax_input, output, "num_dependents")
        with pytest.raises(NotImplementedError, match="num_dependents"):
            backend.solve(tax_input, output, 3_000.0, "num_dependents")

    def test_graph_refusal_points_to_ots_only_where_ots_reads_the_input(self):
        """The OTS hint appears only when OTS computes the return and the input."""
        from tenforty import evaluate_return

        with pytest.raises(NotImplementedError) as refusal:
            evaluate_return(
                year=2024, w2_income=50_000, dependent_exemptions=1.0, backend="graph"
            )
        assert "backend='ots'" not in str(refusal.value)

    @pytest.mark.parametrize("state", [None, "WI"])
    def test_dependents_refusal_does_not_point_to_ots(self, state):
        """OTS reads Dependents federally but never computes with it."""
        from tenforty import evaluate_return

        with pytest.raises(NotImplementedError, match="num_dependents") as refusal:
            evaluate_return(
                year=2024,
                state=state,
                w2_income=50_000,
                num_dependents=1,
                backend="graph",
            )
        assert "backend='ots'" not in str(refusal.value)

    def test_graph_backend_batch_rejects_unsupported_nonzero_inputs(self):
        """Batch evaluation should be as strict as single-scenario evaluation."""
        from tenforty import evaluate_returns

        with pytest.raises(NotImplementedError, match="Unsupported inputs"):
            evaluate_returns(
                year=2024,
                w2_income=[100_000],
                dependent_exemptions=[100.0],
                backend="graph",
            )
