"""Tests for backend abstraction layer."""

import pytest

from tenforty.backends import OTSBackend, available_backends, get_backend
from tenforty.backends.protocol import TaxBackend
from tenforty.models import TaxReturnInput


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
        result = backend.solve(tax_input, "L24_total_tax", 10000, "w2_income")
        assert result is not None
        assert result > 0

    def test_graph_backend_missing_dependency_fails(self, monkeypatch):
        """Graph backend should fail loudly when a required form graph is missing."""
        from tenforty.backends import graph as graph_module

        # Clear cache to ensure clean state
        graph_module._load_graph.cache_clear()
        graph_module._link_graphs.cache_clear()

        # Call _link_graphs with a list that is missing a dependency
        # ca_540 imports ca_schedule_ca (and others), so omitting them should trigger the check
        with pytest.raises(RuntimeError, match="unresolved imports"):
            graph_module._link_graphs(2024, ("us_1040", "ca_540"))

    def test_graph_backend_integration_resolves_schedule_d(self):
        """Integration test: inputting capital gains should automatically load Schedule D."""
        from tenforty.backends import GraphBackend

        backend = GraphBackend()
        # Providing capital gains requires us_schedule_d
        tax_input = TaxReturnInput(
            year=2024, w2_income=100_000, short_term_capital_gains=5000
        )

        # This will fail if resolve_forms doesn't find Schedule D,
        # because us_1040 has an import for it that would otherwise be unresolved.
        result = backend.evaluate(tax_input)

        # Verify result is sane (tax should include tax on gains)
        assert result.federal_total_tax > 0

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
