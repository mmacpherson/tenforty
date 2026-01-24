"""Tests for the evaluate_returns() batch API."""

import polars as pl
import pytest

from tenforty import evaluate_return, evaluate_returns


class TestBatchBasics:
    """Basic batch processing tests."""

    def test_single_item_batch(self):
        """Single-item input should work and return one-row DataFrame."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status="Single",
            w2_income=75000.0,
        )

        assert isinstance(result, pl.DataFrame)
        assert len(result) == 1
        assert result["w2_income"][0] == 75000.0
        assert result["federal_total_tax"][0] > 0

    def test_multiple_w2_incomes(self):
        """Multiple W2 incomes should produce correct number of rows."""
        incomes = [50000.0, 75000.0, 100000.0, 150000.0]
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status="Single",
            w2_income=incomes,
        )

        assert len(result) == len(incomes)
        assert result["w2_income"].to_list() == incomes

    def test_multiple_states(self):
        """Multiple states in single batch call."""
        states = ["NY", "MA", "CA"]
        result = evaluate_returns(
            year=2024,
            state=states,
            filing_status="Single",
            w2_income=100000.0,
        )

        assert len(result) == len(states)
        assert result["state"].to_list() == states
        for row in result.iter_rows(named=True):
            assert row["state_total_tax"] > 0

    def test_multiple_years(self):
        """Multiple years in single batch call."""
        years = [2022, 2023, 2024]
        result = evaluate_returns(
            year=years,
            state="NY",
            filing_status="Single",
            w2_income=100000.0,
        )

        assert len(result) == len(years)
        assert result["year"].to_list() == years

    def test_cartesian_product(self):
        """Multiple parameters create cartesian product."""
        years = [2023, 2024]
        states = ["NY", "MA"]
        incomes = [50000.0, 100000.0]

        result = evaluate_returns(
            year=years,
            state=states,
            w2_income=incomes,
        )

        expected_rows = len(years) * len(states) * len(incomes)
        assert len(result) == expected_rows


class TestDataFrameStructure:
    """Tests for DataFrame output structure and types."""

    def test_output_columns_present(self):
        """All expected columns should be present in output."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            w2_income=100000.0,
        )

        expected_input_cols = [
            "year",
            "state",
            "filing_status",
            "w2_income",
            "num_dependents",
        ]
        expected_output_cols = [
            "federal_total_tax",
            "federal_taxable_income",
            "federal_adjusted_gross_income",
            "state_total_tax",
            "total_tax",
        ]

        for col in expected_input_cols + expected_output_cols:
            assert col in result.columns, f"Missing column: {col}"

    def test_numeric_columns_are_numeric(self):
        """Tax-related columns should have numeric types."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            w2_income=100000.0,
        )

        numeric_cols = [
            "w2_income",
            "federal_total_tax",
            "state_total_tax",
            "total_tax",
            "federal_taxable_income",
        ]

        for col in numeric_cols:
            dtype = result[col].dtype
            assert dtype.is_numeric(), f"Column {col} should be numeric, got {dtype}"

    def test_state_column_is_string(self):
        """State column should be string type."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            w2_income=100000.0,
        )

        assert result["state"].dtype == pl.Utf8

    def test_no_null_values_in_tax_columns(self):
        """Core tax columns should not have null values."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            w2_income=[50000.0, 100000.0],
        )

        assert result["federal_total_tax"].null_count() == 0
        assert result["total_tax"].null_count() == 0


class TestConsistency:
    """Tests that batch API matches single-return API."""

    @pytest.mark.parametrize(
        "params",
        [
            {
                "year": 2024,
                "state": "NY",
                "filing_status": "Single",
                "w2_income": 75000,
            },
            {
                "year": 2024,
                "state": "MA",
                "filing_status": "Single",
                "w2_income": 100000,
            },
            {
                "year": 2023,
                "state": "CA",
                "filing_status": "Married/Joint",
                "w2_income": 150000,
            },
            {
                "year": 2024,
                "state": None,
                "filing_status": "Single",
                "w2_income": 50000,
            },
        ],
        ids=["NY-75k", "MA-100k", "CA-150k", "Federal-only-50k"],
    )
    def test_batch_matches_single(self, params):
        """Batch result should match single evaluate_return() call."""
        single_result = evaluate_return(**params)
        batch_result = evaluate_returns(**params)

        assert len(batch_result) == 1
        row = batch_result.row(0, named=True)

        assert row["federal_total_tax"] == single_result.federal_total_tax
        assert row["federal_taxable_income"] == single_result.federal_taxable_income
        assert (
            row["federal_adjusted_gross_income"]
            == single_result.federal_adjusted_gross_income
        )
        assert row["total_tax"] == single_result.total_tax

        if params["state"] is not None:
            assert row["state_total_tax"] == single_result.state_total_tax

    def test_batch_order_preserved(self):
        """Results should be in same order as input combinations."""
        incomes = [100000.0, 50000.0, 75000.0]
        result = evaluate_returns(
            year=2024,
            state="NY",
            w2_income=incomes,
        )

        assert result["w2_income"].to_list() == incomes


class TestEdgeCases:
    """Edge case handling tests."""

    def test_zero_income(self):
        """Zero income should produce zero or near-zero tax."""
        result = evaluate_returns(
            year=2024,
            state="NY",
            w2_income=0.0,
        )

        assert len(result) == 1
        assert result["federal_total_tax"][0] == 0

    def test_federal_only_no_state(self):
        """Federal-only calculation (state=None) should work."""
        result = evaluate_returns(
            year=2024,
            state=None,
            w2_income=100000.0,
        )

        assert len(result) == 1
        assert result["federal_total_tax"][0] > 0
        assert result["state_total_tax"][0] == 0

    def test_mixed_state_and_none(self):
        """Mix of state and None should work."""
        result = evaluate_returns(
            year=2024,
            state=["NY", None, "MA"],
            w2_income=100000.0,
        )

        assert len(result) == 3
        assert result.filter(pl.col("state") == "NY")["state_total_tax"][0] > 0
        assert result.filter(pl.col("state").is_null())["state_total_tax"][0] == 0
        assert result.filter(pl.col("state") == "MA")["state_total_tax"][0] > 0

    def test_all_filing_statuses(self):
        """All filing statuses should work in batch mode."""
        statuses = ["Single", "Married/Joint", "Head_of_House", "Married/Sep"]
        result = evaluate_returns(
            year=2024,
            state="NY",
            filing_status=statuses,
            w2_income=100000.0,
        )

        assert len(result) == len(statuses)
        for row in result.iter_rows(named=True):
            assert row["federal_total_tax"] > 0
