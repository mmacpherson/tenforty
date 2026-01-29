"""Tests for unified API."""

import pytest

from tenforty import evaluate_return, evaluate_returns, marginal_rate, solve_for_income


class TestEvaluateReturn:
    """Tests for evaluate_return function."""

    def test_evaluate_return_default_backend(self):
        """evaluate_return should work with default backend selection."""
        result = evaluate_return(year=2024, w2_income=100_000)
        assert result.federal_total_tax > 5000

    def test_evaluate_return_explicit_ots(self):
        """evaluate_return should work with explicit OTS backend."""
        result = evaluate_return(year=2024, w2_income=100_000, backend="ots")
        assert result.federal_total_tax > 5000

    def test_evaluate_return_all_parameters(self):
        """evaluate_return should accept all tax parameters."""
        result = evaluate_return(
            year=2024,
            state="CA",
            filing_status="Single",
            num_dependents=0,
            standard_or_itemized="Standard",
            w2_income=100_000,
            taxable_interest=1000,
            qualified_dividends=500,
            ordinary_dividends=1000,
            short_term_capital_gains=2000,
            long_term_capital_gains=5000,
            schedule_1_income=0,
            itemized_deductions=0,
            state_adjustment=0,
            incentive_stock_option_gains=0,
            backend="ots",
        )
        assert result.federal_total_tax > 0
        assert result.state_total_tax > 0

    def test_evaluate_return_filing_statuses(self):
        """evaluate_return should work with different filing statuses."""
        statuses = ["Single", "Married/Joint", "Head_of_House"]
        for status in statuses:
            result = evaluate_return(
                year=2024, filing_status=status, w2_income=100_000, backend="ots"
            )
            assert result.federal_total_tax > 0


class TestEvaluateReturns:
    """Tests for evaluate_returns function."""

    def test_evaluate_returns_single_value(self):
        """evaluate_returns should work with single values."""
        df = evaluate_returns(year=2024, w2_income=100_000, backend="ots")
        assert len(df) == 1
        assert df["federal_total_tax"][0] > 5000

    def test_evaluate_returns_multiple_incomes(self):
        """evaluate_returns should generate grid for multiple incomes."""
        df = evaluate_returns(
            year=2024, w2_income=[50_000, 100_000, 150_000], backend="ots"
        )
        assert len(df) == 3
        federal_total_tax = df["federal_total_tax"].to_list()
        assert federal_total_tax == sorted(federal_total_tax)

    def test_evaluate_returns_multiple_years(self):
        """evaluate_returns should work with multiple years."""
        df = evaluate_returns(year=[2023, 2024], w2_income=100_000, backend="ots")
        assert len(df) == 2


class TestMarginalRate:
    """Tests for marginal_rate function (requires graph backend)."""

    @pytest.fixture
    def graph_available(self):
        """Check if graph backend is available."""
        try:
            from tenforty.backends import GraphBackend

            backend = GraphBackend()
            return backend.is_available()
        except ImportError:
            return False

    def test_marginal_rate_raises_without_graph(self, graph_available):
        """marginal_rate should raise if graph backend unavailable."""
        if graph_available:
            pytest.skip("Graph backend is available")

        with pytest.raises(RuntimeError):
            marginal_rate(year=2024, w2_income=100_000)

    def test_marginal_rate_returns_rate(self, graph_available):
        """marginal_rate should return tax rate."""
        if not graph_available:
            pytest.skip("Graph backend not available")

        rate = marginal_rate(year=2024, w2_income=100_000)
        assert 0 < rate < 1


class TestSolveForIncome:
    """Tests for solve_for_income function (requires graph backend)."""

    @pytest.fixture
    def graph_available(self):
        """Check if graph backend is available."""
        try:
            from tenforty.backends import GraphBackend

            backend = GraphBackend()
            return backend.is_available()
        except ImportError:
            return False

    def test_solve_for_income_raises_without_graph(self, graph_available):
        """solve_for_income should raise if graph backend unavailable."""
        if graph_available:
            pytest.skip("Graph backend is available")

        with pytest.raises(RuntimeError):
            solve_for_income(target_tax=10000, year=2024)

    def test_solve_for_income_returns_income(self, graph_available):
        """solve_for_income should return income value."""
        if not graph_available:
            pytest.skip("Graph backend not available")

        income = solve_for_income(target_tax=10000, year=2024)
        assert income > 0


class TestBackwardsCompatibility:
    """Tests for backwards compatibility with existing API."""

    def test_import_evaluate_return(self):
        """evaluate_return should be importable from tenforty."""
        from tenforty import evaluate_return as er

        assert callable(er)

    def test_import_evaluate_returns(self):
        """evaluate_returns should be importable from tenforty."""
        from tenforty import evaluate_returns as er

        assert callable(er)

    def test_existing_api_unchanged(self):
        """Existing API calls should work without changes."""
        result = evaluate_return(
            year=2024,
            state=None,
            filing_status="Single",
            w2_income=100_000,
        )
        assert result.federal_total_tax > 5000
        assert hasattr(result, "federal_adjusted_gross_income")
        assert hasattr(result, "federal_taxable_income")
        assert hasattr(result, "total_tax")
