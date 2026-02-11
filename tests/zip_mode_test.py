"""Tests for evaluate_returns(mode="zip") zip-semantics batch API."""

import polars as pl
import pytest

from tenforty import evaluate_return, evaluate_returns


class TestZipSemantics:
    """Zip semantics: N list inputs -> exactly N rows (not N^k)."""

    def test_two_incomes_produce_two_rows(self):
        """Two W2 incomes should produce exactly two rows."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status="Single",
            w2_income=[50000.0, 100000.0],
            mode="zip",
        )

        assert isinstance(result, pl.DataFrame)
        assert len(result) == 2
        assert result["w2_income"].to_list() == [50000.0, 100000.0]

    def test_multiple_inputs_zipped_not_crossed(self):
        """Multiple list inputs should be zipped, not crossed."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status="Single",
            w2_income=[50000.0, 100000.0, 150000.0],
            taxable_interest=[1000.0, 2000.0, 3000.0],
            mode="zip",
        )

        assert len(result) == 3

    def test_all_scalar_returns_one_row(self):
        """All scalar inputs should return a single row."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status="Single",
            w2_income=75000.0,
            mode="zip",
        )

        assert len(result) == 1
        assert result["federal_total_tax"].item() > 0


class TestBroadcasting:
    """Scalar inputs broadcast to match list length."""

    def test_scalar_status_broadcast(self):
        """Scalar filing status should broadcast to match list inputs."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status="Single",
            w2_income=[50000.0, 100000.0, 150000.0],
            mode="zip",
        )

        assert len(result) == 3
        assert all(s == "Single" for s in result["filing_status"].to_list())

    def test_mixed_scalar_and_list(self):
        """Mix of scalar and list inputs should broadcast scalars."""
        result = evaluate_returns(
            year=2024,
            state=None,
            filing_status="Single",
            w2_income=[50000.0, 100000.0],
            taxable_interest=500.0,
            mode="zip",
        )

        assert len(result) == 2


class TestLengthValidation:
    """List length mismatches raise ValueError."""

    def test_mismatched_lengths_raises(self):
        """Mismatched list lengths should raise ValueError."""
        with pytest.raises(ValueError, match="same length"):
            evaluate_returns(
                year=2024,
                w2_income=[50000.0, 100000.0],
                taxable_interest=[1000.0, 2000.0, 3000.0],
                mode="zip",
            )

    def test_status_length_mismatch_raises(self):
        """Filing status list length mismatch should raise ValueError."""
        with pytest.raises(ValueError, match="same length"):
            evaluate_returns(
                year=2024,
                filing_status=["Single", "Married/Joint", "Single"],
                w2_income=[50000.0, 100000.0],
                mode="zip",
            )


class TestConsistencyWithSingleEval:
    """Each zip row matches evaluate_return() for same parameters."""

    @pytest.mark.parametrize(
        "params",
        [
            {
                "year": 2024,
                "state": "NY",
                "filing_status": "Single",
                "w2_income": 75000.0,
            },
            {
                "year": 2024,
                "state": None,
                "filing_status": "Single",
                "w2_income": 50000.0,
            },
            {
                "year": 2024,
                "state": "MA",
                "filing_status": "Married/Joint",
                "w2_income": 120000.0,
            },
        ],
        ids=["NY-75k", "Federal-only-50k", "MA-120k-MFJ"],
    )
    def test_zip_matches_single(self, params):
        """Zip result should match single evaluate_return() call."""
        single_result = evaluate_return(**params)
        zip_result = evaluate_returns(**params, mode="zip")

        assert len(zip_result) == 1
        row = zip_result.row(0, named=True)

        assert row["federal_total_tax"] == single_result.federal_total_tax
        assert row["federal_taxable_income"] == single_result.federal_taxable_income
        assert (
            row["federal_adjusted_gross_income"]
            == single_result.federal_adjusted_gross_income
        )
        assert row["total_tax"] == single_result.total_tax

    def test_multi_row_consistency(self):
        """Each row in multi-row zip should match individual calls."""
        incomes = [50000.0, 100000.0, 150000.0]

        zip_result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status="Single",
            w2_income=incomes,
            mode="zip",
        )

        for i, income in enumerate(incomes):
            single = evaluate_return(
                year=2024, state="NY", filing_status="Single", w2_income=income
            )
            row = zip_result.row(i, named=True)
            assert row["federal_total_tax"] == single.federal_total_tax
            assert row["total_tax"] == single.total_tax


class TestMultipleFilingStatuses:
    """Per-row filing statuses in zip mode."""

    def test_different_statuses_per_row(self):
        """Different filing statuses per row should be evaluated correctly."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status=["Single", "Married/Joint", "Single"],
            w2_income=[75000.0, 150000.0, 100000.0],
            mode="zip",
        )

        assert len(result) == 3
        statuses = result["filing_status"].to_list()
        assert statuses[0] == "Single"
        assert statuses[1] == "Married/Joint"
        assert statuses[2] == "Single"

        for i in range(3):
            row = result.row(i, named=True)
            assert row["federal_total_tax"] > 0


class TestEdgeCases:
    """Edge cases."""

    def test_zero_income(self):
        """Zero income should produce zero federal tax."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status="Single",
            w2_income=0.0,
            mode="zip",
        )

        assert len(result) == 1
        assert result["federal_total_tax"].item() == 0

    def test_federal_only(self):
        """Federal-only (state=None) should have zero state tax."""
        result = evaluate_returns(
            year=2024,
            state=None,
            filing_status="Single",
            w2_income=[50000.0, 100000.0],
            mode="zip",
        )

        assert len(result) == 2
        assert all(t > 0 for t in result["federal_total_tax"].to_list())
        assert all(t == 0 for t in result["state_total_tax"].to_list())

    def test_output_columns_present(self):
        """All expected output columns should be present."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            w2_income=100000.0,
            mode="zip",
        )

        expected_cols = [
            "year",
            "state",
            "filing_status",
            "federal_total_tax",
            "federal_taxable_income",
            "federal_adjusted_gross_income",
            "state_total_tax",
            "total_tax",
        ]
        for col in expected_cols:
            assert col in result.columns, f"Missing column: {col}"


class TestVectorYearAndState:
    """mode='zip' supports vector year and state via grouping."""

    def test_vector_year(self):
        """mode='zip' should support vector year."""
        result = evaluate_returns(
            year=[2023, 2024],
            state="NY",
            filing_status="Single",
            w2_income=[50000.0, 50000.0],
            mode="zip",
        )

        assert len(result) == 2
        assert result["year"].to_list() == [2023, 2024]

    def test_vector_state(self):
        """mode='zip' should support vector state."""
        result = evaluate_returns(
            year=2024,
            state=["NY", "MA"],
            filing_status="Single",
            w2_income=[100000.0, 100000.0],
            mode="zip",
        )

        assert len(result) == 2
        assert result["state"].to_list() == ["NY", "MA"]
        assert all(t > 0 for t in result["state_total_tax"].to_list())
