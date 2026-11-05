"""The three-way federal deduction choice: Auto (the default), Standard, Itemized.

Auto takes the larger of the standard deduction and Schedule A. Standard and
Itemized force their choice whichever is larger (IRC 63(e); Form 1040
instructions, line 12). See docs/deduction-choice-contract.md.

Standard deduction amounts are published parameters: 2024 Single $14,600 and
MFJ $29,200 (Rev. Proc. 2023-34 section 3.15); 2025 Single $15,750 (2025 Form
1040 instructions, line 12, as amended by OBBBA).
"""

import inspect

import pytest

import tenforty
from tenforty import (
    UnsupportedInputError,
    evaluate_return,
    evaluate_returns,
    marginal_rate,
    solve_for_income,
)
from tenforty.backends.graph import GraphBackend
from tenforty.models import OTSDeductionType, TaxReturnInput

STANDARD_DEDUCTION_SINGLE = {2024: 14_600.0, 2025: 15_750.0}
BACKENDS = ["ots", "graph"]


def test_the_choice_has_three_values():
    """Auto, Standard and Itemized, spelled as the public API accepts them."""
    assert [choice.value for choice in OTSDeductionType] == [
        "Auto",
        "Standard",
        "Itemized",
    ]


@pytest.mark.parametrize(
    "function",
    [
        tenforty.evaluate_return,
        tenforty.evaluate_returns,
        tenforty.marginal_rate,
        tenforty.marginal_rates,
        tenforty.solve_for_income,
    ],
)
def test_every_entry_point_defaults_to_auto(function):
    """Default callers keep the larger-of behavior under its honest name."""
    default = inspect.signature(function).parameters["standard_or_itemized"].default
    assert default == "Auto"
    assert TaxReturnInput().standard_or_itemized == OTSDeductionType.AUTO


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("year", [2024, 2025])
def test_each_choice_selects_its_deduction(backend, year):
    """Standard forces the standard deduction even when Schedule A is larger."""
    standard = STANDARD_DEDUCTION_SINGLE[year]
    common = dict(year=year, filing_status="Single", w2_income=100_000, backend=backend)
    larger = standard + 5_000
    smaller = 10_000

    def taxable_income(choice, itemized):
        return evaluate_return(
            standard_or_itemized=choice, itemized_deductions=itemized, **common
        ).federal_taxable_income

    assert taxable_income("Auto", larger) == 100_000 - larger
    assert taxable_income("Auto", smaller) == 100_000 - standard
    assert taxable_income("Standard", larger) == 100_000 - standard
    assert taxable_income("Standard", smaller) == 100_000 - standard
    assert taxable_income("Itemized", larger) == 100_000 - larger
    assert taxable_income("Itemized", smaller) == 100_000 - smaller


@pytest.mark.parametrize("backend", BACKENDS)
def test_forced_standard_ignores_state_and_local_taxes(backend):
    """The capped SALT deduction only counts when Schedule A is used."""
    common = dict(
        year=2024,
        filing_status="Single",
        w2_income=100_000,
        itemized_deductions=10_000,
        state_and_local_taxes=9_000,
        backend=backend,
    )

    auto = evaluate_return(standard_or_itemized="Auto", **common)
    forced = evaluate_return(standard_or_itemized="Standard", **common)

    assert auto.federal_taxable_income == 100_000 - 19_000
    assert forced.federal_taxable_income == 100_000 - 14_600


def test_forced_standard_drives_form_6251_line_2a():
    """Taking the standard deduction adds it back on Form 6251 line 2a."""
    backend = GraphBackend()
    common = dict(
        year=2024,
        filing_status="Single",
        w2_income=150_000,
        itemized_deductions=30_000,
        incentive_stock_option_gains=200_000,
    )

    auto, _ = backend._create_evaluator(TaxReturnInput(**common))
    forced, _ = backend._create_evaluator(
        TaxReturnInput(standard_or_itemized="Standard", **common)
    )

    assert auto.eval("us_1040_UsesItemized") == 1.0
    assert auto.eval("us_form_6251_L2a_taxes_addback") == 0.0
    assert forced.eval("us_1040_UsesItemized") == 0.0
    assert forced.eval("us_1040_L12Final_deduction_amount") == 14_600.0
    assert forced.eval("us_form_6251_L2a_taxes_addback") == 14_600.0


@pytest.mark.parametrize("backend", BACKENDS)
def test_each_choice_survives_zip_batches(backend):
    """Zip rows may make different elections in one batch."""
    results = evaluate_returns(
        year=2024,
        filing_status="Single",
        standard_or_itemized=["Auto", "Standard", "Itemized"],
        w2_income=100_000,
        itemized_deductions=20_000,
        backend=backend,
        mode="zip",
    )

    assert results["standard_or_itemized"].to_list() == ["Auto", "Standard", "Itemized"]
    assert results["federal_taxable_income"].to_list() == [80_000.0, 85_400.0, 80_000.0]


@pytest.mark.parametrize("backend", BACKENDS)
def test_each_choice_is_a_cross_axis(backend):
    """Cross mode keeps all three choices as rows."""
    results = evaluate_returns(
        year=2024,
        filing_status="Single",
        standard_or_itemized=["Auto", "Standard", "Itemized"],
        w2_income=100_000,
        itemized_deductions=[10_000, 20_000],
        backend=backend,
    )

    rows = list(
        zip(
            results["standard_or_itemized"].to_list(),
            results["itemized_deductions"].to_list(),
            results["federal_taxable_income"].to_list(),
            strict=True,
        )
    )
    assert sorted(rows) == sorted(
        [
            ("Auto", 10_000.0, 85_400.0),
            ("Auto", 20_000.0, 80_000.0),
            ("Standard", 10_000.0, 85_400.0),
            ("Standard", 20_000.0, 85_400.0),
            ("Itemized", 10_000.0, 90_000.0),
            ("Itemized", 20_000.0, 80_000.0),
        ]
    )


def test_forced_standard_reaches_gradients_and_solver():
    """Autodiff and inverse solving evaluate the forced standard branch."""
    common = dict(year=2024, filing_status="Single", itemized_deductions=30_000)

    # At $60,000 of wages Auto itemizes $30,000, leaving taxable income of
    # 30,000 in the 12% bracket; forced Standard ignores the itemized amount.
    auto_slope = marginal_rate(w2_income=60_000, wrt="itemized_deductions", **common)
    forced_slope = marginal_rate(
        w2_income=60_000,
        wrt="itemized_deductions",
        standard_or_itemized="Standard",
        **common,
    )
    # $13,847 is the 2024 Tax Table row 85,400-85,450, Single; forced Standard
    # reaches it at wages of 100,000 plus up to $50.
    forced_income = solve_for_income(13_847, standard_or_itemized="Standard", **common)

    assert auto_slope == pytest.approx(-0.12)
    assert forced_slope == pytest.approx(0.0)
    assert 100_000.0 <= forced_income < 100_050.0


def test_forced_standard_reaches_a_state_importing_federal_taxable_income():
    """Iowa consumes Form 1040 taxable income rather than recomputing deductions."""
    common = dict(
        year=2024,
        state="IA",
        filing_status="Single",
        w2_income=100_000,
        itemized_deductions=20_000,
        backend="graph",
    )

    auto = evaluate_return(**common)
    forced = evaluate_return(standard_or_itemized="Standard", **common)

    assert auto.state_taxable_income == 80_000.0
    assert forced.state_taxable_income == 85_400.0


OTS_STATES_READING_FEDERAL_SCHEDULE_A = [
    (2024, "AZ"),
    (2018, "CA"),
    (2023, "CA"),
    (2021, "NY"),
    (2023, "NY"),
]


@pytest.mark.parametrize(("year", "state"), OTS_STATES_READING_FEDERAL_SCHEDULE_A)
@pytest.mark.parametrize(
    "schedule_a", [{"itemized_deductions": 20_000}, {"state_and_local_taxes": 5_000}]
)
def test_ots_refuses_forced_standard_where_the_state_reads_schedule_a(
    year, state, schedule_a
):
    """These OTS state programs rebuild their deduction from federal Schedule A.

    OTS has no way to force the standard deduction except to send it no
    Schedule A, which would also empty the state's own itemized deduction.
    """
    with pytest.raises(UnsupportedInputError, match="standard_or_itemized"):
        evaluate_return(
            year=year,
            state=state,
            filing_status="Single",
            w2_income=100_000,
            standard_or_itemized="Standard",
            backend="ots",
            **schedule_a,
        )


@pytest.mark.parametrize(("year", "state"), OTS_STATES_READING_FEDERAL_SCHEDULE_A)
def test_ots_accepts_those_states_without_schedule_a_or_with_auto(year, state):
    """The refusal is limited to the case the state program cannot represent."""
    common = dict(
        year=year, state=state, filing_status="Single", w2_income=100_000, backend="ots"
    )

    evaluate_return(standard_or_itemized="Standard", **common)
    evaluate_return(standard_or_itemized="Auto", itemized_deductions=20_000, **common)


def test_ots_batch_refuses_forced_standard_before_computing():
    """A batch is refused whole, like the graph backend's unsupported inputs."""
    with pytest.raises(UnsupportedInputError, match="standard_or_itemized"):
        evaluate_returns(
            year=2023,
            state="CA",
            filing_status="Single",
            w2_income=100_000,
            itemized_deductions=[0, 20_000],
            standard_or_itemized="Standard",
            backend="ots",
            on_error="ignore",
        )


@pytest.mark.parametrize("backend", BACKENDS)
def test_forced_standard_in_a_state_without_schedule_a_import(backend):
    """Other states take the federal result; Oregon is computed on both backends."""
    result = evaluate_return(
        year=2024,
        state="OR",
        filing_status="Single",
        w2_income=100_000,
        itemized_deductions=20_000,
        standard_or_itemized="Standard",
        backend=backend,
    )

    assert result.federal_taxable_income == 85_400.0
