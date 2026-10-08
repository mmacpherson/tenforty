"""State value checks with explicit independent or unverified legacy provenance."""

from .evidence import (
    CA_EVIDENCE,
    HI_EVIDENCE,
    IN_EVIDENCE,
    LA_DERIVED_EVIDENCE,
    LA_TABLE_EVIDENCE,
    MS_EVIDENCE,
    NJ_VA_EVIDENCE,
    VT_EVIDENCE,
    WI_EVIDENCE,
)
from .tax_scenario import KnownDefect, TaxScenario


def _wi_band_tolerance(
    marginal_rate: float, sd_phase_rate: float = 0.0, *, tax_table: bool
) -> float:
    """Bound on |official WI Form 1 table value - exact statutory formula|.

    The Standard Deduction Table prices the Wis. Stat. 71.05(22) formula at the
    midpoint of a $500 band of line 7 income, so line 8 can differ from the exact
    formula by up to $250 times the phase-down rate. Below $100k of TI the Tax
    Table prices tax at the midpoint of a $100 band, up to $50 of TI. Both reach
    tax at the marginal rate, and line 12 is rounded to whole dollars.

    This bounds the WI scenarios below, each of which sits inside one bracket and
    one phase-out segment across both bands. It is not a general bound: a band
    straddling a rate or phase-out kink can exceed it.
    """
    return 0.50 + marginal_rate * (250.0 * sd_phase_rate + (50.0 if tax_table else 0.0))


CA_WHOLE_DOLLAR_TOLERANCE = 1.0


# Upper bound and rate of each bracket, "over the previous bound, not over this".
CA_SINGLE_2024_BRACKETS = (
    (10_756.0, 0.01),
    (25_499.0, 0.02),
    (40_245.0, 0.04),
    (55_866.0, 0.06),
    (70_606.0, 0.08),
    (360_659.0, 0.093),
    (432_787.0, 0.103),
    (721_314.0, 0.113),
    (float("inf"), 0.123),
)


def _table_band_tolerance(
    brackets: tuple[tuple[float, float], ...], low: float, high: float
) -> float:
    """Bound on |published tax-table row - exact bracket formula| for one row.

    A tax table prices the band from ``low`` to ``high`` at its midpoint and
    rounds to the dollar; the graph computes the exact formula (whether it
    should is the open precision decision, tenforty-xew). If one rate holds
    across the band, the gap is at most that rate times half the band, plus
    $0.50 of rounding. A bracket boundary inside the band breaks that bound, so
    it is rejected rather than estimated.
    """
    boundaries = [upper for upper, _ in brackets[:-1]]
    straddled = [b for b in boundaries if low < b < high]
    assert not straddled, f"table band {low}-{high} straddles {straddled}"
    rate = next(rate for upper, rate in brackets if high <= upper)
    return 0.50 + rate * (high - low) / 2.0


def _ca_table_tolerance(low: float, high: float) -> float:
    """Band bound for an FTB 2024 Form 540 Tax Table row, Single (tenforty-xew)."""
    return _table_band_tolerance(CA_SINGLE_2024_BRACKETS, low, high)


LA_2024_TABLE_URL = "https://dam.ldr.la.gov/taxforms/IT540(2024)D13%20TT.pdf"


def nj_table_gap(rate: float, taxable_income: float, row_low: float) -> KnownDefect:
    """Signed schedule-minus-table bound for these non-kink $50 NJ rows.

    NJ requires its printed table below $100,000. The blind record establishes
    each row and its single marginal rate. The schedule prices actual income
    rather than the midpoint, with at most $0.50 of printed-row rounding.
    """
    assert row_low <= taxable_income < row_low + 50
    center = rate * (taxable_income - (row_low + 25))
    return KnownDefect(
        quantity="state_total_tax",
        minimum=center - 0.50,
        maximum=center + 0.50,
        reason="NJ requires the tax-table row; graph uses the schedule (tenforty-xew)",
    )


VA_SCHEDULE_ROUNDING_GAP = KnownDefect(
    quantity="state_total_tax",
    minimum=-0.50,
    maximum=0.50,
    reason="VA Form 760 requires whole-dollar schedule tax; graph is unrounded "
    "(tenforty-xew, not a table-band difference)",
)


IN_LINE8_ROUNDING_GAP = KnownDefect(
    quantity="state_total_tax",
    minimum=-0.50,
    maximum=0.50,
    reason="IN IT-40 line 8 rounds to whole dollars (booklet 'Rounding "
    "Required'); graph is unrounded (tenforty-b72.42)",
)


def hi_table_gap(
    rate: float, taxable_income: float, row_low: float, base_offset: float
) -> KnownDefect:
    """Signed graph-minus-table bound for a non-kink $50 HI Tax Table row.

    N-11 line 27 requires the table below $100,000. DOTAX built every cell from
    the printed schedule at the row midpoint, rounded to the dollar. The graph
    prices exact cumulative brackets at actual income, unrounded. ``base_offset``
    is exact cumulative base minus printed base for the row's bracket; the
    arithmetic is in docs/validation/state-fixtures/HI-2024-2025.md, Part C.
    """
    assert row_low <= taxable_income < row_low + 50
    center = rate * (taxable_income - (row_low + 25)) + base_offset
    return KnownDefect(
        quantity="state_total_tax",
        minimum=center - 0.50,
        maximum=center + 0.50,
        reason="HI requires the tax-table row below $100,000; graph uses the "
        "schedule (tenforty-tj2.20)",
    )


def hi_schedule_gap(base_offset: float, rounding: float) -> KnownDefect:
    """Graph minus the statutory HI schedule at $100,000 or more, whole dollars.

    HRS 235-51 prints whole-dollar bases; the graph uses exact cumulative bases
    and does not round. Both terms come from published figures only.
    """
    center = base_offset + rounding
    return KnownDefect(
        quantity="state_total_tax",
        minimum=center - 0.01,
        maximum=center + 0.01,
        reason="HI schedule uses printed statutory bases rounded to the dollar; "
        "graph uses exact cumulative bases unrounded (tenforty-b72.41)",
    )


def la_2024_table_gap(
    rate: float,
    row_low: float,
    row_high: float,
    tax_table_income: float,
    rounding_adjustment: float = 0.0,
) -> KnownDefect:
    """Graph-minus-table signature for a 2024 Louisiana tax-table value (tenforty-xew).

    IT-540 line 10 is the tax-table value (2024 IT-540 instructions, PDF page 3).
    Each $250 row prices its midpoint and rounds to the dollar: every parsed cell
    of the table equals the lowest-bracket-first formula at the row midpoint
    within $0.50. Above the last row ($51,000; $101,000 MFJ/QSS) the table adds
    4.25% of the excess to the last row's value, and that continuation is then
    rounded to the nearest dollar like every entry ("About This Form", item 4,
    same page). The graph computes the exact formula at the return's own income,
    so graph minus the raw table value is ``rate`` times the distance from the
    row midpoint to that income (capped at the last row's upper edge), plus or
    minus $0.50 for the printed row's rounding. ``rate`` is the single bracket
    rate in force across the row; the exemption term is constant within a row
    and cancels.

    ``rounding_adjustment`` is the independently computed rounded continuation
    less the raw continuation, for above-table cases; it shifts the center
    (graph minus rounded value = graph minus raw value minus the adjustment) and
    does not widen the $0.50 allowance. Returns with more than eight exemptions
    use a different table rule (reduce income, read column eight) and are out of
    scope here.
    """
    midpoint = (row_low + row_high) / 2.0
    center = rate * (min(tax_table_income, row_high) - midpoint) - rounding_adjustment
    return KnownDefect(
        quantity="state_total_tax",
        minimum=center - 0.50,
        maximum=center + 0.50,
        reason=(
            "LA 2024 line 10 is the tax-table value; the graph prices the exact "
            "bracket formula rather than the table row (tenforty-xew)"
        ),
    )


# SILVER_STANDARD_STATE_SCENARIOS: Formula-derived from published state tax brackets.
# Federal tax below $100,000 of taxable income is the IRS Tax Table row the
# Form 1040 line 16 instructions require (inline "Tax Table row" comments,
# tests/fixtures/federal_tax_table). Each was restated from the rate-formula
# value in the scenario's comment after confirming that value was exactly the
# formula on the stated taxable income; QDCG cases price lines 22 and 24 so.
SILVER_STANDARD_STATE_SCENARIOS = [
    # ========== ALABAMA SCENARIOS ==========
    # AL 2024 & 2025: 3-bracket system (2%, 4%, 5%)
    # Single: 2% up to $500, 4% $500-$3,000, 5% over $3,000
    # MFJ: 2% up to $1,000, 4% $1,000-$6,000, 5% over $6,000
    # Standard deduction phases out based on AL AGI; we provide the amount as input.
    #
    # AL Single, $25,000 W2 (2024)
    # Total income: $25,000, Adjustments: $0, AL AGI: $25,000
    # Standard deduction from chart (AL AGI $25,000): $3,000 (max for Single)
    # AL taxable: $25,000 - $3,000 = $22,000
    # AL tax: $500 * 0.02 + $2,500 * 0.04 + $19,000 * 0.05
    #       = $10 + $100 + $950 = $1,060.00
    # Federal: AGI $25,000, std ded $14,600, taxable $10,400
    # Federal tax: $10,400 * 0.10 = $1,040.00
    TaxScenario(
        source="AL 2024 Tax Brackets (computed)",
        description="AL Single, $25,000 income (2024)",
        year=2024,
        state="AL",
        filing_status="Single",
        w2_income=25000.0,
        state_adjustment=3000.0,  # Standard deduction from AL chart
        expected_federal_tax=1043.0,  # Tax Table row 10,400-10,450
        expected_state_tax=1060.0,
        expected_federal_agi=25000.0,
        backend="graph",
    ),
    # AL MFJ, $30,000 W2 (2024)
    # Total income: $30,000, Adjustments: $0, AL AGI: $30,000
    # Standard deduction from chart (AL AGI $30,000): $6,925 (MFJ, phasing out)
    # AL taxable: $30,000 - $6,925 = $23,075
    # AL tax: $1,000 * 0.02 + $5,000 * 0.04 + $17,075 * 0.05
    #       = $20 + $200 + $853.75 = $1,073.75
    # Federal: AGI $30,000, std ded $29,200, taxable $800
    # Federal tax: $800 * 0.10 = $80.00
    TaxScenario(
        source="AL 2024 Tax Brackets (computed)",
        description="AL MFJ, $30,000 income (2024)",
        year=2024,
        state="AL",
        filing_status="Married/Joint",
        w2_income=30000.0,
        state_adjustment=6925.0,  # Standard deduction from AL chart
        expected_federal_tax=81.0,  # Tax Table row 800-825
        expected_state_tax=1073.75,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # AL Single, $50,000 W2 (2024)
    # Total income: $50,000, Adjustments: $0, AL AGI: $50,000
    # Standard deduction from chart: AL AGI $50,000 is way above phase-out end,
    # but for simplicity we'll use minimum of $2,500 (or could use $0)
    # AL taxable: $50,000 - $2,500 = $47,500
    # AL tax: $500 * 0.02 + $2,500 * 0.04 + $44,500 * 0.05
    #       = $10 + $100 + $2,225 = $2,335.00
    # Federal: AGI $50,000, std ded $14,600, taxable $35,400
    # Federal tax: $11,925 * 0.10 + $23,475 * 0.12 = $1,192.50 + $2,817 = $4,009.50 -> $4,016
    TaxScenario(
        source="AL 2024 Tax Brackets (computed)",
        description="AL Single, $50,000 income (2024)",
        year=2024,
        state="AL",
        filing_status="Single",
        w2_income=50000.0,
        state_adjustment=2500.0,  # Standard deduction minimum (high AGI)
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=2335.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # AL MFJ, $100,000 W2 (2024)
    # Total income: $100,000, Adjustments: $0, AL AGI: $100,000
    # Standard deduction: AL AGI $100,000 >> phase-out end, use minimum $5,000
    # AL taxable: $100,000 - $5,000 = $95,000
    # AL tax: $1,000 * 0.02 + $5,000 * 0.04 + $89,000 * 0.05
    #       = $20 + $200 + $4,450 = $4,670.00
    # Federal: AGI $100,000, std ded $29,200, taxable $70,800
    # Federal tax: $23,850 * 0.10 + $46,950 * 0.12 = $2,385 + $5,634 = $8,019 -> $8,032
    TaxScenario(
        source="AL 2024 Tax Brackets (computed)",
        description="AL MFJ, $100,000 income (2024)",
        year=2024,
        state="AL",
        filing_status="Married/Joint",
        w2_income=100000.0,
        state_adjustment=5000.0,  # Standard deduction minimum (high AGI)
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=4670.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # AL Single, $50,000 W2 (2025)
    # Rates and brackets unchanged from 2024
    # Total income: $50,000, Adjustments: $0, AL AGI: $50,000
    # Standard deduction: $2,500 (minimum, same as 2024)
    # AL taxable: $50,000 - $2,500 = $47,500
    # AL tax: $500 * 0.02 + $2,500 * 0.04 + $44,500 * 0.05
    #       = $10 + $100 + $2,225 = $2,335.00
    # Federal 2025: Std ded $15,000, taxable $35,000
    # Federal tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="AL 2025 Tax Brackets (computed)",
        description="AL Single, $50,000 income (2025)",
        year=2025,
        state="AL",
        filing_status="Single",
        w2_income=50000.0,
        state_adjustment=2500.0,  # Standard deduction minimum (high AGI)
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=2335.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # AL MFJ, $100,000 W2 (2025)
    # Total income: $100,000, Adjustments: $0, AL AGI: $100,000
    # Standard deduction: $5,000 (minimum, same as 2024)
    # AL taxable: $100,000 - $5,000 = $95,000
    # AL tax: $1,000 * 0.02 + $5,000 * 0.04 + $89,000 * 0.05
    #       = $20 + $200 + $4,450 = $4,670.00
    # Federal 2025: Std ded $30,000, taxable $70,000
    # Federal tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="AL 2025 Tax Brackets (computed)",
        description="AL MFJ, $100,000 income (2025)",
        year=2025,
        state="AL",
        filing_status="Married/Joint",
        w2_income=100000.0,
        state_adjustment=5000.0,  # Standard deduction minimum (high AGI)
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=4670.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== ARIZONA SCENARIOS ==========
    # AZ 2024: Flat 2.5% rate
    # Standard Deduction: Single $14,600, MFJ $29,200, HoH $21,900
    #
    # AZ Single, $50,000 W2
    # Fed AGI: $50,000
    # AZ AGI: $50,000 (assuming no exemptions/additions/subtractions)
    # AZ Taxable: $50,000 - $14,600 = $35,400
    # AZ Tax: $35,400 * 0.025 = $885.00
    # Federal taxable: $50,000 - $14,600 = $35,400
    # Federal tax: $11,925 x 0.10 + $23,475 x 0.12 = $1,192.50 + $2,817 = $4,009.50 -> $4016
    TaxScenario(
        source="AZ 2024 Tax Brackets (computed)",
        description="AZ Single, $50,000 income",
        year=2024,
        state="AZ",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=885.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # AZ MFJ, $100,000 W2
    # Fed AGI: $100,000
    # AZ AGI: $100,000
    # AZ Taxable: $100,000 - $29,200 = $70,800
    # AZ Tax: $70,800 * 0.025 = $1,770.00
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax: $23,850 x 0.10 + $46,950 x 0.12 = $2,385 + $5,634 = $8,019 -> $8032
    TaxScenario(
        source="AZ 2024 Tax Brackets (computed)",
        description="AZ MFJ, $100,000 income",
        year=2024,
        state="AZ",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=1770.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # AZ HoH, $75,000 W2
    # Fed AGI: $75,000
    # AZ AGI: $75,000
    # AZ Taxable: $75,000 - $21,900 = $53,100
    # AZ Tax: $53,100 * 0.025 = $1,327.50
    # Federal taxable: $75,000 - $21,900 = $53,100
    # Federal tax: $6,041 (OTS calculation)
    TaxScenario(
        source="AZ 2024 Tax Brackets (computed)",
        description="AZ HoH, $75,000 income",
        year=2024,
        state="AZ",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=1327.50,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # AZ 2025: Flat 2.5% rate (unchanged from 2024)
    # Standard Deduction: Single $15,750, MFJ $31,500, HoH $23,625
    #
    # AZ Single, $50,000 W2
    # Fed AGI: $50,000
    # AZ AGI: $50,000
    # AZ Taxable: $50,000 - $15,750 = $34,250
    # AZ Tax: $34,250 * 0.025 = $856.25
    # Federal 2025: Std ded $15,000, taxable $35,000
    # Federal tax: $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="AZ 2025 Tax Brackets (computed)",
        description="AZ Single, $50,000 income (2025, increased std ded)",
        year=2025,
        state="AZ",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=856.25,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== CALIFORNIA SCENARIOS ==========
    # CA 2024: Standard deduction $5,540, Personal exemption credit $149
    # Brackets: 1% ($0-$10,756), 2% ($10,756-$25,499), 4% ($25,499-$40,245),
    #           6% ($40,245-$55,866), 8% ($55,866-$70,606), 9.3% ($70,606+)
    #
    # Provenance audit (tenforty-b72.16); labels stay unverified-legacy.
    # Commit 18f4954 moved these six from exact-formula values to OTS output
    # ("align with OTS rounding"), federal and state alike. Re-checked against
    # official tables, retrieved 2026-10-01:
    # - Federal: each expected value equals the Single column of the IRS 2024
    #   Tax Table, https://www.irs.gov/pub/irs-prior/i1040tt--2024.pdf, and the
    #   graph prices line 16 from that table, so the check is exact (re-checked
    #   2026-10-02 against tests/fixtures/federal_tax_table/irs_tax_table_2024.csv).
    # - State: the first five equal the FTB 2024 Form 540 Tax Table row less the
    #   $149 credit, https://www.ftb.ca.gov/forms/2024/2024-540-booklet.pdf
    #   (pp. 69-72). The $105,540 case was $5,693, the rounded rate-schedule
    #   formula OTS computes; Form 540 requires the table at TI <= $100,000, and
    #   row 99,951-100,000 (p. 73) gives $5,840 - $149 = $5,691, now expected.
    # Each state tolerance is the band bound of the case's actual table row
    # (_table_band_tolerance; table vs formula is tenforty-xew). The $16,296
    # case keeps the default: its CA row 10,751-10,850 straddles the $10,756
    # bracket bound, and every row through it is under the $149 credit, so the
    # expected and graph values are both exactly $0.
    #
    # CA Single at top of 1% bracket
    # CA taxable: $10,756, CA tax: $10,756 x 0.01 = $107.56, less $149 credit = $0
    # Federal taxable: $1,696, Federal tax: $169.60 (formula) -> $169 (IRS Tax Table)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single at top of 1% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=16296.0,  # CA taxable $10,756 + $5,540 std ded
        expected_federal_tax=169.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # CA Single in 2% bracket
    # CA taxable: $20,000, CA tax: $107.56 + $184.88 = $292.44, less $149 credit = $143.44
    # Federal taxable: $10,940, Federal tax: $1,094.00 (formula) -> $1093 (IRS Tax Table)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 2% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=25540.0,  # CA taxable $20,000 + $5,540 std ded
        expected_federal_tax=1093.0,
        expected_state_tax=143.0,  # OTS rounds to nearest dollar ($143.44 -> $143)
        state_tax_tolerance=_ca_table_tolerance(19_950, 20_050),
        backend="graph",
    ),
    # CA Single in 4% bracket
    # CA taxable: $35,000, CA tax: $402.42 + $380.04 = $782.46, less $149 credit = $633.46
    # Federal taxable: $25,940, Federal tax: $2,880.80 (formula) -> $2879 (IRS Tax Table)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 4% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=40540.0,  # CA taxable $35,000 + $5,540 std ded
        expected_federal_tax=2879.0,
        expected_state_tax=633.0,  # OTS rounds to nearest dollar ($633.46 -> $633)
        state_tax_tolerance=_ca_table_tolerance(34_950, 35_050),
        backend="graph",
    ),
    # CA Single in 6% bracket
    # CA taxable: $50,000, CA tax: $992.26 + $585.30 = $1,577.56, less $149 credit = $1,428.56
    # Federal taxable: $40,940, Federal tax: $4,680.80 (formula) -> $4679 (IRS Tax Table)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 6% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=55540.0,  # CA taxable $50,000 + $5,540 std ded
        expected_federal_tax=4679.0,
        expected_state_tax=1429.0,  # OTS rounds to nearest dollar ($1428.56 -> $1429)
        state_tax_tolerance=_ca_table_tolerance(49_950, 50_050),
        backend="graph",
    ),
    # CA Single in 8% bracket
    # CA taxable: $65,000, CA tax: $1,929.52 + $730.72 = $2,660.24, less $149 credit = $2,511.24
    # Federal taxable: $55,940, Federal tax: $7,359.80 (formula) -> $7357 (IRS Tax Table)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 8% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=70540.0,  # CA taxable $65,000 + $5,540 std ded
        expected_federal_tax=7357.0,
        expected_state_tax=2511.0,  # OTS rounds to nearest dollar ($2511.24 -> $2511)
        state_tax_tolerance=_ca_table_tolerance(64_950, 65_050),
        backend="graph",
    ),
    # CA Single in 9.3% bracket
    # CA taxable: $100,000, CA tax: $3,108.72 + $2,733.64 = $5,842.36, less $149 credit = $5,693.36
    # Federal taxable: $90,940, Federal tax: $15,059.80 (formula) -> $15057 (IRS Tax Table)
    TaxScenario(
        source="CA 2024 Tax Brackets (computed)",
        description="CA Single in 9.3% bracket",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=105540.0,  # CA taxable $100,000 + $5,540 std ded
        expected_federal_tax=15057.0,
        # FTB 2024 Form 540 Tax Table (TI <= $100,000), row 99,951-100,000 =
        # $5,840, less $149 = $5,691; booklet p. 73,
        # https://www.ftb.ca.gov/forms/2024/2024-540-booklet.pdf, retrieved
        # 2026-10-01. Was $5,693, the rounded rate-schedule formula OTS computes.
        expected_state_tax=5691.0,
        state_tax_tolerance=_ca_table_tolerance(99_950, 100_000),
        backend="graph",
    ),
    # ---------- CA 2024 official values (tenforty-b72.3) ----------
    # Source: FTB 2024 Personal Income Tax Booklet, Form 540,
    # https://www.ftb.ca.gov/forms/2024/2024-540-booklet.pdf, retrieved 2026-09-30.
    # Std deduction p. 13 (Single $5,540; MFJ/HoH $11,080); personal exemption
    # credit p. 12 ($149 per box); AGI Limitation Worksheet p. 14; Tax Table
    # pp. 69-74 (TI <= $100,000); Tax Rate Schedules X/Y/Z p. 75.
    # Derived blind, session 1b251ce6.
    # Form 540 line 31 is whole dollars (Tax Table rows priced at the $100 midpoint;
    # rate-schedule result rounded to the dollar, p. 75 Step 4). The graph computes
    # the exact bracket formula, so these fixtures allow $1: the rate-schedule gap
    # is at most $0.50, and the tax-table gap in these rows is at most $0.36.
    # CA Single $30,000 W2 (2024). Fixture kind: tax-table + derived-arithmetic.
    # TI $24,460; Tax Table p. 70 row 24,451-24,550 col "1 or 3" = $382; less $149 exemption credit (p. 12) = $233
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA Single, $30,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=30000.0,
        expected_state_tax=233.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA Single $120,000 W2 (2024). Fixture kind: derived-arithmetic.
    # TI $114,460; Schedule X (p. 75): $3,108.72 + 9.3% x $43,854 = $7,187.14 -> $7,187; less $149 = $7,038
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA Single, $120,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=120000.0,
        expected_state_tax=7038.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA Single $600,000 W2 (2024). Fixture kind: derived-arithmetic.
    # TI $594,460; Schedule X (p. 75): $37,512.83 + 11.3% x $161,673 = $55,781.88 -> $55,782; exemption credit fully phased out (p. 14 worksheet: ceil(355,143 / 2,500) x $6 = $858 > $149)
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA Single, $600,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=600000.0,
        expected_state_tax=55782.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA MFJ $30,000 W2 (2024). Fixture kind: tax-table + derived-arithmetic.
    # TI $18,920; Tax Table p. 69 row 18,851-18,950 col "2 or 5" = $189; less $298 exemption credit, floored at $0
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA MFJ, $30,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Married/Joint",
        w2_income=30000.0,
        expected_state_tax=0.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA MFJ $120,000 W2 (2024). Fixture kind: derived-arithmetic.
    # TI $108,920; Schedule Y (p. 75): $1,984.52 + 6% x $28,430 = $3,690.32 -> $3,690; less $298 = $3,392
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA MFJ, $120,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_state_tax=3392.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA MFJ $600,000 W2 (2024). Fixture kind: derived-arithmetic.
    # TI $588,920; Schedule Y (p. 75): $6,217.44 + 9.3% x $447,708 = $47,854.28 -> $47,854; exemption credit fully phased out (p. 14: ceil(110,281 / 2,500) x $6 x 2 = $540 > $298)
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA MFJ, $600,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Married/Joint",
        w2_income=600000.0,
        expected_state_tax=47854.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA HoH $30,000 W2 (2024). Fixture kind: tax-table + derived-arithmetic.
    # TI $18,920; Tax Table p. 69 row 18,851-18,950 col "4" = $189; less $149 = $40
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA HoH, $30,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Head_of_House",
        w2_income=30000.0,
        expected_state_tax=40.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA HoH $120,000 W2 (2024). Fixture kind: derived-arithmetic.
    # TI $108,920; Schedule Z (p. 75): $3,511.13 + 9.3% x $12,813 = $4,702.74 -> $4,703; less $149 = $4,554
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA HoH, $120,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Head_of_House",
        w2_income=120000.0,
        expected_state_tax=4554.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA HoH $600,000 W2 (2024). Fixture kind: derived-arithmetic.
    # TI $588,920; Schedule Z (p. 75): $50,293.33 + 11.3% x $327 = $50,330.28 -> $50,330; exemption credit fully phased out (p. 14: ceil(232,709 / 2,500) x $6 = $564 > $149)
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA HoH, $600,000 W2, official Form 540 (2024)",
        year=2024,
        state="CA",
        filing_status="Head_of_House",
        w2_income=600000.0,
        expected_state_tax=50330.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
    ),
    # CA Single $250,000 W2 (2024). Fixture kind: derived-arithmetic.
    # Implementer-derived from the same booklet's rules, not by the blind deriver.
    # TI $244,460; Schedule X (p. 75): $3,108.72 + 9.3% x $173,854 = $19,277.14
    # -> $19,277. AGI Limitation Worksheet (p. 14): excess $5,143 / $2,500 = 2.06,
    # rounded UP to 3; 3 x $6 = $18; exemption credit $149 - $18 = $131.
    # Net $19,277 - $131 = $19,146.
    TaxScenario(
        source="FTB 2024 Form 540 Booklet (official)",
        state_evidence=CA_EVIDENCE,
        description="CA Single, $250,000 W2, exemption credit in phase-out (2024)",
        year=2024,
        state="CA",
        filing_status="Single",
        w2_income=250000.0,
        expected_state_tax=19146.0,
        state_tax_tolerance=CA_WHOLE_DOLLAR_TOLERANCE,
        backend="graph",
        known_failure=(
            "CA 540 line 32 phase-out is continuous in the graph ($0.0024 per "
            "excess AGI dollar); FTB AGI Limitation Worksheet rounds excess/$2,500 "
            "UP to a whole step, $6 per step. Graph credit $136.66 vs official "
            "$131; graph net $19,140.48 vs $19,146 (tenforty-b72.17)."
        ),
    ),
    # ========== MASSACHUSETTS SCENARIOS ==========
    # MA 2024: Flat 5% rate, Personal exemption $4,400 (Single), $8,800 (MFJ), $6,800 (HoH)
    # 4% surtax on income over $1,053,750 (2024) / $1,083,150 (2025)
    # Also: 8.5% on short-term capital gains, 12% on long-term collectibles
    # Source: MA DOR Form 1 instructions 2024/2025
    #
    # MA Single, low income (2024)
    # Federal AGI: $20,000
    # MA total income: $20,000 (imports federal AGI)
    # MA income after deductions: $20,000 (no deductions)
    # Personal exemption: $4,400
    # MA taxable: $20,000 - $4,400 = $15,600
    # MA tax: $15,600 x 0.05 = $780
    # Federal taxable: $5,400, Federal tax: $540 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $20,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=20000.0,
        expected_federal_tax=543.0,  # Tax Table row 5,400-5,450
        expected_state_tax=780.0,
        backend="graph",
    ),
    # MA Single, middle income (2024)
    # Federal AGI: $50,000
    # MA total income: $50,000
    # Personal exemption: $4,400
    # MA taxable: $50,000 - $4,400 = $45,600
    # MA tax: $45,600 x 0.05 = $2,280
    # Federal taxable: $35,400, Federal tax: $4,016 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $50,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=2280.0,
        backend="graph",
    ),
    # MA Single, higher income (2024)
    # Federal AGI: $100,000
    # MA total income: $100,000
    # Personal exemption: $4,400
    # MA taxable: $100,000 - $4,400 = $95,600
    # MA tax: $95,600 x 0.05 = $4,780
    # Federal taxable: $85,400, Federal tax: $13,841 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA Single, $100,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13847.0,  # Tax Table row 85,400-85,450
        expected_state_tax=4780.0,
        backend="graph",
    ),
    # MA MFJ, middle income (2024)
    # Federal AGI: $100,000
    # MA total income: $100,000
    # Personal exemption: $8,800
    # MA taxable: $100,000 - $8,800 = $91,200
    # MA tax: $91,200 x 0.05 = $4,560
    # Federal taxable: $70,800, Federal tax: $8,032 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA MFJ, $100,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=4560.0,
        backend="graph",
    ),
    # MA Head_of_House, middle income (2024)
    # Federal AGI: $75,000
    # MA total income: $75,000
    # Personal exemption: $6,800
    # MA taxable: $75,000 - $6,800 = $68,200
    # MA tax: $68,200 x 0.05 = $3,410
    # Federal taxable: $53,100, Federal tax: $6,041 (formula, graph backend)
    TaxScenario(
        source="MA 2024 Tax Brackets (computed)",
        description="MA HoH, $75,000 income (2024)",
        year=2024,
        state="MA",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=3410.0,
        backend="graph",
    ),
    # MA Single, surtax income (2025)
    # Federal AGI: $1,200,000
    # MA total income: $1,200,000
    # Personal exemption: $4,400
    # MA taxable: $1,200,000 - $4,400 = $1,195,600
    # MA base tax: $1,195,600 x 0.05 = $59,780
    # Surtaxable income: $1,195,600 - $1,083,150 = $112,450
    # MA surtax: $112,450 x 0.04 = $4,498
    # MA total tax: $59,780 + $4,498 = $64,278
    # Federal taxable: $1,184,400 (2025 std ded $15,600)
    # Federal tax (2025): $395,192.75 (income tax)
    #   + Additional Medicare Tax: ($1,200,000 - $200,000) * 0.009 = $9,000
    #   = $404,192.75
    TaxScenario(
        source="MA 2025 Tax Brackets (computed)",
        description="MA Single, $1.2M income with surtax (2025)",
        year=2025,
        state="MA",
        filing_status="Single",
        w2_income=1200000.0,
        expected_federal_tax=404192.75,
        expected_state_tax=64278.0,
        backend="graph",
    ),
    # ========== CONNECTICUT SCENARIOS ==========
    # CT 2024 Single, low income (no exemption phaseout)
    # Federal AGI: $20,000
    # CT personal exemption: $15,000 (no phaseout, AGI < $30,000 threshold)
    # CT taxable: $20,000 - $15,000 = $5,000
    # CT tax: $5,000 * 0.02 = $100
    # Federal standard deduction (2024): $14,600
    # Federal taxable: $20,000 - $14,600 = $5,400
    # Federal tax: $5,400 * 0.10 = $540
    TaxScenario(
        source="CT 2024 Tax Brackets (computed)",
        description="CT Single, $20,000 income (2024)",
        year=2024,
        state="CT",
        filing_status="Single",
        w2_income=20000.0,
        expected_federal_tax=543.0,  # Tax Table row 5,400-5,450
        expected_state_tax=100.0,
        expected_federal_agi=20000.0,
        backend="graph",
    ),
    # CT 2024 Single, middle income (partial exemption phaseout)
    # Federal AGI: $40,000
    # CT personal exemption: Base $15,000, phaseout starts at $30,000
    #   Excess AGI = $40,000 - $30,000 = $10,000
    #   Exemption = $15,000 - $10,000 = $5,000
    # CT taxable: $40,000 - $5,000 = $35,000
    # CT tax: $10,000 * 0.02 + $25,000 * 0.045 = $200 + $1,125 = $1,325
    # Federal taxable: $40,000 - $14,600 = $25,400
    # Federal tax: $11,600 * 0.10 + $13,800 * 0.12 = $1,160 + $1,656 = $2,816
    TaxScenario(
        source="CT 2024 Tax Brackets (computed)",
        description="CT Single, $40,000 income, partial exemption (2024)",
        year=2024,
        state="CT",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=1325.0,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # CT 2024 MFJ, middle income (complete exemption phaseout)
    # Federal AGI: $100,000
    # CT personal exemption: Base $24,000, phaseout starts at $48,000
    #   Excess AGI = $100,000 - $48,000 = $52,000
    #   Exemption = max(0, $24,000 - $52,000) = $0
    # CT taxable: $100,000 - $0 = $100,000
    # CT tax: $20,000 * 0.02 + $80,000 * 0.045 = $400 + $3,600 = $4,000
    # Federal standard deduction (MFJ 2024): $29,200
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    TaxScenario(
        source="CT 2024 Tax Brackets (computed)",
        description="CT MFJ, $100,000 income, no exemption (2024)",
        year=2024,
        state="CT",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=4000.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # CT 2024 HoH, middle income (complete exemption phaseout)
    # Federal AGI: $75,000
    # CT personal exemption: Base $19,000, phaseout starts at $38,000
    #   Excess AGI = $75,000 - $38,000 = $37,000
    #   Exemption = max(0, $19,000 - $37,000) = $0
    # CT taxable: $75,000 - $0 = $75,000
    # CT tax: $16,000 * 0.02 + $59,000 * 0.045 = $320 + $2,655 = $2,975
    # Federal standard deduction (HoH 2024): $21,900
    # Federal taxable: $75,000 - $21,900 = $53,100
    # Federal tax: $16,550 * 0.10 + $36,550 * 0.12 = $1,655 + $4,386 = $6,041
    TaxScenario(
        source="CT 2024 Tax Brackets (computed)",
        description="CT HoH, $75,000 income, no exemption (2024)",
        year=2024,
        state="CT",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=2975.0,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # CT 2025 Single, higher income (no exemption, 3rd bracket)
    # Federal AGI: $90,000
    # CT personal exemption: Base $15,000, phaseout starts at $30,000
    #   Excess AGI = $90,000 - $30,000 = $60,000
    #   Exemption = max(0, $15,000 - $60,000) = $0
    # CT taxable: $90,000 - $0 = $90,000
    # CT tax: $10,000 * 0.02 + $40,000 * 0.045 + $40,000 * 0.055
    #       = $200 + $1,800 + $2,200 = $4,200
    # Federal standard deduction (2025): $15,000
    # Federal taxable: $90,000 - $15,000 = $75,000
    # Federal tax: $11,925 * 0.10 + $36,550 * 0.12 + $26,525 * 0.22
    #       = $1,192.50 + $4,386 + $5,835.50 = $11,414
    TaxScenario(
        source="CT 2025 Tax Brackets (computed)",
        description="CT Single, $90,000 income, 3rd bracket (2025)",
        year=2025,
        state="CT",
        filing_status="Single",
        w2_income=90000.0,
        expected_federal_tax=11255.0,  # Tax Table row 74,250-74,300
        expected_state_tax=4200.0,
        expected_federal_agi=90000.0,
        backend="graph",
    ),
    # ========== DISTRICT OF COLUMBIA (DC) SCENARIOS ==========
    # DC 2024 & 2025: 7-bracket progressive system (4%, 6%, 6.5%, 8.5%, 9.25%, 9.75%, 10.75%)
    # Brackets are uniform across all filing statuses
    # Brackets: $0-$10k (4%), $10k-$40k (6%), $40k-$60k (6.5%), $60k-$250k (8.5%),
    #           $250k-$500k (9.25%), $500k-$1M (9.75%), $1M+ (10.75%)
    # Standard deduction 2024: Single/MFS $14,600, MFJ/QW $29,200, HoH $21,900
    # Standard deduction 2025: Single/MFS $15,000, MFJ/QW $30,000, HoH $22,500
    # DC imports federal AGI (US 1040 L11)
    #
    # DC Single, $45,000 W2 (2024) - spans 4% and 6% brackets
    # Federal: AGI=$45k, Std Ded=$14,600, Taxable=$30,400
    # DC: AGI=$45k, Std Ded=$14,600, Taxable=$30,400
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$30,400: $20,400*0.06=$1,224
    #         Total: $400 + $1,224 = $1,624
    TaxScenario(
        source="DC 2024 Tax Rate Schedules (computed)",
        description="DC Single, $45,000 W2",
        year=2024,
        state="DC",
        filing_status="Single",
        w2_income=45000.0,
        expected_federal_tax=3419.0,  # Tax Table row 30,400-30,450
        expected_state_tax=1624.0,
        expected_federal_agi=45000.0,
        backend="graph",
    ),
    # DC MFJ, $90,000 W2 (2024) - spans three brackets
    # Federal: AGI=$90k, Std Ded=$29,200, Taxable=$60,800
    # DC: AGI=$90k, Std Ded=$29,200, Taxable=$60,800
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$40k: $30k*0.06=$1,800,
    #         $40k-$60k: $20k*0.065=$1,300, $60k-$60,800: $800*0.085=$68
    #         Total: $400 + $1,800 + $1,300 + $68 = $3,568
    TaxScenario(
        source="DC 2024 Tax Rate Schedules (computed)",
        description="DC MFJ, $90,000 W2",
        year=2024,
        state="DC",
        filing_status="Married/Joint",
        w2_income=90000.0,
        expected_federal_tax=6835.0,  # Tax Table row 60,800-60,850
        expected_state_tax=3568.0,
        expected_federal_agi=90000.0,
        backend="graph",
    ),
    # DC HoH, $70,000 W2 (2024) - spans multiple brackets
    # Federal: AGI=$70k, Std Ded=$21,900, Taxable=$48,100
    # DC: AGI=$70k, Std Ded=$21,900, Taxable=$48,100
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$40k: $30k*0.06=$1,800,
    #         $40k-$48,100: $8,100*0.065=$526.50
    #         Total: $400 + $1,800 + $526.50 = $2,726.50
    TaxScenario(
        source="DC 2024 Tax Rate Schedules (computed)",
        description="DC HoH, $70,000 W2",
        year=2024,
        state="DC",
        filing_status="Head_of_House",
        w2_income=70000.0,
        expected_federal_tax=5444.0,  # Tax Table row 48,100-48,150
        expected_state_tax=2726.5,
        expected_federal_agi=70000.0,
        backend="graph",
    ),
    # DC Single, $70,000 W2 (2024) - 3rd bracket (6.5%)
    # Federal: AGI=$70k, Std Ded=$14,600, Taxable=$55,400
    # DC: AGI=$70k, Std Ded=$14,600, Taxable=$55,400
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$40k: $30k*0.06=$1,800,
    #         $40k-$60k: $20k*0.065=$1,300
    #         (Note: $55,400 < $60k, so stays in 3rd bracket)
    #         Total would be: $400 + $1,800 + $1,300 * ($55,400-$40k)/$20k
    #         Actually: $40k-$55,400: $15,400*0.065=$1,001
    #         Total: $400 + $1,800 + $1,001 = $3,201
    TaxScenario(
        source="DC 2024 Tax Rate Schedules (computed)",
        description="DC Single, $70,000 W2 (3rd bracket)",
        year=2024,
        state="DC",
        filing_status="Single",
        w2_income=70000.0,
        expected_federal_tax=7247.0,  # Tax Table row 55,400-55,450
        expected_state_tax=3201.0,
        expected_federal_agi=70000.0,
        backend="graph",
    ),
    # DC Single, $50,000 W2 (2025) - test 2025 standard deduction
    # Federal: AGI=$50k, Std Ded=$15,750 (2025 OBBBA), Taxable=$34,250
    # DC: AGI=$50k, Std Ded=$15,750, Taxable=$34,250
    # DC tax: $0-$10k: $10k*0.04=$400, $10k-$34,250: $24,250*0.06=$1,455
    #         Total: $400 + $1,455 = $1,855
    TaxScenario(
        source="DC 2025 Tax Rate Schedules (computed)",
        description="DC Single, $50,000 W2 (2025)",
        year=2025,
        state="DC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1855.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== VERMONT SCENARIOS ==========
    # VT IN-111 starts from federal AGI (Line 1), subtracts VT's own standard
    # deduction (Line 4) and personal exemptions (Line 5e: $5,300 each for 2025,
    # $5,100 for 2024), then taxes Line 7 from the $100-band tax tables below
    # $75,000 or the rate schedules above it (IN-111 instructions pp.6-7, p.13).
    # W-2 only, under 65, not blind, no dependents: AGI = wages, IN-112 mods = 0.
    #
    # Every expected value below was derived blind, session 1b251ce6, from the
    # VT Department of Taxes PDFs retrieved 2026-09-30:
    #   2025: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf
    #   2024: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf
    # They are strict known failures: the graph omits the Line 5e personal
    # exemption, and it prices Line 7 with the exact bracket formula where VT
    # prescribes the tax table (midpoint of a $100 band) or a rounded published
    # base tax, a residual of a few dollars ($3.70 observed; tenforty-xew precision contract).
    #
    # VT Single, $30,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.15, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 30,000 - 7,650 std - 5,300 exemptions = 17,050
    # VT tax: row 17,000-17,100 -> $571
    TaxScenario(
        source="VT 2025 IN-111 instructions p.15 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT Single, $30,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Single",
        w2_income=30000.0,
        expected_state_tax=571.0,
        expected_federal_agi=30000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,300): "
            "VT TI 22,350 vs official 17,050; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT Single, $90,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.13, retrieved 2026-09-30
    # Fixture kind: derived-arithmetic; derived blind, session 1b251ce6
    # VT TI: 90,000 - 7,650 std - 5,300 exemptions = 77,050
    # VT tax: Schedule X: 3,345 + 6.6% x (77,050 - 75,000) = 3,480.30 -> $3,480
    TaxScenario(
        source="VT 2025 IN-111 instructions p.13 (derived-arithmetic)",
        state_evidence=VT_EVIDENCE,
        description="VT Single, $90,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Single",
        w2_income=90000.0,
        expected_state_tax=3480.0,
        expected_federal_agi=90000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,300): "
            "VT TI 82,350 vs official 77,050; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT Single, $300,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.13, p.7, retrieved 2026-09-30
    # Fixture kind: derived-arithmetic; derived blind, session 1b251ce6
    # VT TI: 300,000 - 7,650 std - 5,300 exemptions = 287,050
    # VT tax: Schedule X: 16,175 + 8.75% x (287,050 - 249,700) = 19,443.13; 3% x AGI = 9,000 does not bind -> $19,443
    TaxScenario(
        source="VT 2025 IN-111 instructions p.13, p.7 (derived-arithmetic)",
        state_evidence=VT_EVIDENCE,
        description="VT Single, $300,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Single",
        w2_income=300000.0,
        expected_state_tax=19443.0,
        expected_federal_agi=300000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,300): "
            "VT TI 292,350 vs official 287,050; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT MFJ, $30,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.14, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 30,000 - 15,300 std - 10,600 exemptions = 4,100
    # VT tax: row 4,100-4,200 -> $139
    TaxScenario(
        source="VT 2025 IN-111 instructions p.14 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT MFJ, $30,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Married/Joint",
        w2_income=30000.0,
        expected_state_tax=139.0,
        expected_federal_agi=30000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $10,600): "
            "VT TI 14,700 vs official 4,100; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT MFJ, $90,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.18, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 90,000 - 15,300 std - 10,600 exemptions = 64,100
    # VT tax: row 64,100-64,200 -> $2,149
    TaxScenario(
        source="VT 2025 IN-111 instructions p.18 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT MFJ, $90,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Married/Joint",
        w2_income=90000.0,
        expected_state_tax=2149.0,
        expected_federal_agi=90000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $10,600): "
            "VT TI 74,700 vs official 64,100; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT MFJ, $300,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.13, p.7, retrieved 2026-09-30
    # Fixture kind: derived-arithmetic; derived blind, session 1b251ce6
    # VT TI: 300,000 - 15,300 std - 10,600 exemptions = 274,100
    # VT tax: Schedule Y-1: 10,482 + 7.6% x (274,100 - 199,450) = 16,155.40; 3% x AGI = 9,000 does not bind -> $16,155
    TaxScenario(
        source="VT 2025 IN-111 instructions p.13, p.7 (derived-arithmetic)",
        state_evidence=VT_EVIDENCE,
        description="VT MFJ, $300,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Married/Joint",
        w2_income=300000.0,
        expected_state_tax=16155.0,
        expected_federal_agi=300000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $10,600): "
            "VT TI 284,700 vs official 274,100; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT HoH, $30,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.14, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 30,000 - 11,450 std - 5,300 exemptions = 13,250
    # VT tax: row 13,200-13,300 -> $444
    TaxScenario(
        source="VT 2025 IN-111 instructions p.14 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT HoH, $30,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Head_of_House",
        w2_income=30000.0,
        expected_state_tax=444.0,
        expected_federal_agi=30000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,300): "
            "VT TI 18,550 vs official 13,250; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT HoH, $90,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.18, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 90,000 - 11,450 std - 5,300 exemptions = 73,250
    # VT tax: row 73,200-73,300 -> $2,683
    TaxScenario(
        source="VT 2025 IN-111 instructions p.18 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT HoH, $90,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Head_of_House",
        w2_income=90000.0,
        expected_state_tax=2683.0,
        expected_federal_agi=90000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,300): "
            "VT TI 78,550 vs official 73,250; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT HoH, $300,000 W2 (2025)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2025.pdf p.13, p.7, retrieved 2026-09-30
    # Fixture kind: derived-arithmetic; derived blind, session 1b251ce6
    # VT TI: 300,000 - 11,450 std - 5,300 exemptions = 283,250
    # VT tax: Schedule Z: 17,179 + 8.75% x (283,250 - 276,850) = 17,739.00; 3% x AGI = 9,000 does not bind -> $17,739
    TaxScenario(
        source="VT 2025 IN-111 instructions p.13, p.7 (derived-arithmetic)",
        state_evidence=VT_EVIDENCE,
        description="VT HoH, $300,000 W2 (2025)",
        year=2025,
        state="VT",
        filing_status="Head_of_House",
        w2_income=300000.0,
        expected_state_tax=17739.0,
        expected_federal_agi=300000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,300): "
            "VT TI 288,550 vs official 283,250; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT Single, $30,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.15, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 30,000 - 7,400 std - 5,100 exemptions = 17,500
    # VT tax: row 17,500-17,600 -> $588
    TaxScenario(
        source="VT 2024 IN-111 instructions p.15 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT Single, $30,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Single",
        w2_income=30000.0,
        expected_state_tax=588.0,
        expected_federal_agi=30000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,100): "
            "VT TI 22,600 vs official 17,500; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT Single, $90,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.13, retrieved 2026-09-30
    # Fixture kind: derived-arithmetic; derived blind, session 1b251ce6
    # VT TI: 90,000 - 7,400 std - 5,100 exemptions = 77,500
    # VT tax: Schedule X: 3,393 + 6.6% x (77,500 - 75,000) = 3,558.00 -> $3,558
    TaxScenario(
        source="VT 2024 IN-111 instructions p.13 (derived-arithmetic)",
        state_evidence=VT_EVIDENCE,
        description="VT Single, $90,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Single",
        w2_income=90000.0,
        expected_state_tax=3558.0,
        expected_federal_agi=90000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,100): "
            "VT TI 82,600 vs official 77,500; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT Single, $300,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.13, p.7, retrieved 2026-09-30
    # Fixture kind: derived-arithmetic; derived blind, session 1b251ce6
    # VT TI: 300,000 - 7,400 std - 5,100 exemptions = 287,500
    # VT tax: Schedule X: 15,675 + 8.75% x (287,500 - 242,000) = 19,656.25; 3% x AGI = 9,000 does not bind -> $19,656
    TaxScenario(
        source="VT 2024 IN-111 instructions p.13, p.7 (derived-arithmetic)",
        state_evidence=VT_EVIDENCE,
        description="VT Single, $300,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Single",
        w2_income=300000.0,
        expected_state_tax=19656.0,
        expected_federal_agi=300000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,100): "
            "VT TI 292,600 vs official 287,500; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT MFJ, $30,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.14, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 30,000 - 14,850 std - 10,200 exemptions = 4,950
    # VT tax: row 4,900-5,000 -> $166
    TaxScenario(
        source="VT 2024 IN-111 instructions p.14 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT MFJ, $30,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Married/Joint",
        w2_income=30000.0,
        expected_state_tax=166.0,
        expected_federal_agi=30000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $10,200): "
            "VT TI 15,150 vs official 4,950; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT MFJ, $90,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.18, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 90,000 - 14,850 std - 10,200 exemptions = 64,950
    # VT tax: row 64,900-65,000 -> $2,176
    TaxScenario(
        source="VT 2024 IN-111 instructions p.18 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT MFJ, $90,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Married/Joint",
        w2_income=90000.0,
        expected_state_tax=2176.0,
        expected_federal_agi=90000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $10,200): "
            "VT TI 75,150 vs official 64,950; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT MFJ, $300,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.13, p.7, retrieved 2026-09-30
    # Fixture kind: derived-arithmetic; derived blind, session 1b251ce6
    # VT TI: 300,000 - 14,850 std - 10,200 exemptions = 274,950
    # VT tax: Schedule Y-1: 10,159 + 7.6% x (274,950 - 193,300) = 16,364.40; 3% x AGI = 9,000 does not bind -> $16,364
    TaxScenario(
        source="VT 2024 IN-111 instructions p.13, p.7 (derived-arithmetic)",
        state_evidence=VT_EVIDENCE,
        description="VT MFJ, $300,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Married/Joint",
        w2_income=300000.0,
        expected_state_tax=16364.0,
        expected_federal_agi=300000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $10,200): "
            "VT TI 285,150 vs official 274,950; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT HoH, $30,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.14, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 30,000 - 11,100 std - 5,100 exemptions = 13,800
    # VT tax: row 13,800-13,900 -> $464
    TaxScenario(
        source="VT 2024 IN-111 instructions p.14 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT HoH, $30,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Head_of_House",
        w2_income=30000.0,
        expected_state_tax=464.0,
        expected_federal_agi=30000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,100): "
            "VT TI 18,900 vs official 13,800; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT HoH, $90,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.18, retrieved 2026-09-30
    # Fixture kind: tax-table; derived blind, session 1b251ce6
    # VT TI: 90,000 - 11,100 std - 5,100 exemptions = 73,800
    # VT tax: row 73,800-73,900 -> $2,788
    TaxScenario(
        source="VT 2024 IN-111 instructions p.18 (tax-table)",
        state_evidence=VT_EVIDENCE,
        description="VT HoH, $90,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Head_of_House",
        w2_income=90000.0,
        expected_state_tax=2788.0,
        expected_federal_agi=90000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,100): "
            "VT TI 78,900 vs official 73,800; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # VT HoH, $300,000 W2 (2024)
    # Source: https://tax.vermont.gov/sites/tax/files/documents/IN-111-Instr-2024.pdf p.13, p.7, retrieved 2026-09-30
    # Fixture kind: derived-arithmetic; derived blind, session 1b251ce6
    # VT TI: 300,000 - 11,100 std - 5,100 exemptions = 283,800
    # VT tax: Schedule Z: 16,647 + 8.75% x (283,800 - 268,300) = 18,003.25; 3% x AGI = 9,000 does not bind -> $18,003
    TaxScenario(
        source="VT 2024 IN-111 instructions p.13, p.7 (derived-arithmetic)",
        state_evidence=VT_EVIDENCE,
        description="VT HoH, $300,000 W2 (2024)",
        year=2024,
        state="VT",
        filing_status="Head_of_House",
        w2_income=300000.0,
        expected_state_tax=18003.0,
        expected_federal_agi=300000.0,
        known_failure=(
            "Graph omits VT personal exemption (IN-111 Line 5e, $5,100): "
            "VT TI 288,900 vs official 283,800; exemption missing (tenforty-b72.19), "
            "then table-band precision (tenforty-xew)"
        ),
        backend="graph",
    ),
    # ========== DELAWARE SCENARIOS ==========
    # DE 2024 & 2025: 7-bracket progressive system (0%, 2.2%, 3.9%, 4.8%, 5.2%, 5.55%, 6.6%)
    # Brackets are the same for all filing statuses.
    # Standard deduction: Single/MFS/HoH $3,250, MFJ $6,500
    # Additional std deduction: $2,500 if 65+ or blind
    # Personal exemption credit: $110 per exemption (applied after tax computation)
    #
    # DE Single, $30,000 W2 (2024) - tests multiple brackets
    # Federal AGI: $30,000
    # DE AGI: $30,000 (no additions/subtractions)
    # DE Standard deduction: $3,250
    # DE Taxable: $30,000 - $3,250 = $26,750
    # DE Tax: $0-$2k: $0, $2k-$5k: $3k*0.022=$66, $5k-$10k: $5k*0.039=$195,
    #         $10k-$20k: $10k*0.048=$480, $20k-$25k: $5k*0.052=$260,
    #         $25k-$26,750: $1,750*0.0555=$97.13
    #         Total: $1,098.13
    # Federal: Std ded $14,600, taxable $15,400
    # Federal tax: $11,925 * 0.10 + $3,475 * 0.12 = $1,609.50
    TaxScenario(
        source="DE 2024 Tax Brackets (computed)",
        description="DE Single, $30,000 income (2024)",
        year=2024,
        state="DE",
        filing_status="Single",
        w2_income=30000.0,
        expected_federal_tax=1619.0,  # Tax Table row 15,400-15,450
        expected_state_tax=1098.13,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # DE Single, $50,000 W2 (2024) - tests 6th bracket
    # Federal AGI: $50,000
    # DE AGI: $50,000
    # DE Standard deduction: $3,250
    # DE Taxable: $50,000 - $3,250 = $46,750
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$60k: $21,750*0.0555=$1,207.13
    #         Total: $0 + $66 + $195 + $480 + $260 + $1,207.13 = $2,208.13
    # Federal: Std ded $14,600, taxable $35,400
    # Federal tax: $11,925 * 0.10 + $23,475 * 0.12 = $4,009.50
    TaxScenario(
        source="DE 2024 Tax Brackets (computed)",
        description="DE Single, $50,000 income, 6th bracket (2024)",
        year=2024,
        state="DE",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=2208.13,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # DE MFJ, $100,000 W2 (2024) - tests 7th bracket (top rate)
    # Federal AGI: $100,000
    # DE AGI: $100,000
    # DE Standard deduction: $6,500
    # DE Taxable: $100,000 - $6,500 = $93,500
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$60k: $35k*0.0555=$1,942.50,
    #         $60k-$93,500: $33,500*0.066=$2,211
    #         Total: $0 + $66 + $195 + $480 + $260 + $1,942.50 + $2,211 = $5,154.50
    # Federal: Std ded $29,200, taxable $70,800
    # Federal tax: $23,850 * 0.10 + $46,950 * 0.12 = $2,385 + $5,634 = $8,019
    TaxScenario(
        source="DE 2024 Tax Brackets (computed)",
        description="DE Married/Joint, $100,000 income, 7th bracket (2024)",
        year=2024,
        state="DE",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=5154.50,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # DE Head of Household, $60,000 W2 (2024) - tests HoH filing status and 7th bracket
    # Federal AGI: $60,000
    # DE AGI: $60,000
    # DE Standard deduction: $3,250 (HoH uses same as Single)
    # DE Taxable: $60,000 - $3,250 = $56,750
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$60k: $35k*0.0555=$1,942.50,
    #         $60k-$56,750: -$3,250 (doesn't reach 7th bracket)
    # Wait, $56,750 < $60,000, so we're still in 6th bracket
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$56,750: $31,750*0.0555=$1,762.13
    #         Total: $0 + $66 + $195 + $480 + $260 + $1,762.13 = $2,763.13
    # Federal: Std ded $21,900, taxable $38,100
    # Federal tax: $17,850 * 0.10 + $20,250 * 0.12 = $1,785 + $2,430 = $4,215
    TaxScenario(
        source="DE 2024 Tax Brackets (computed)",
        description="DE Head_of_House, $60,000 income (2024)",
        year=2024,
        state="DE",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_federal_tax=4244.0,  # Tax Table row 38,100-38,150
        expected_state_tax=2763.13,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # DE Single, $80,000 W2 (2025) - tests 2025 values and 7th bracket
    # Federal AGI: $80,000
    # DE AGI: $80,000
    # DE Standard deduction: $3,250 (unchanged in 2025)
    # DE Taxable: $80,000 - $3,250 = $76,750
    # DE Tax: $0-$2k: $0, $2k-$5k: $66, $5k-$10k: $195, $10k-$20k: $480,
    #         $20k-$25k: $260, $25k-$60k: $35k*0.0555=$1,942.50,
    #         $60k-$76,750: $16,750*0.066=$1,105.50
    #         Total: $0 + $66 + $195 + $480 + $260 + $1,942.50 + $1,105.50 = $4,049.50
    # Federal: Std ded $15,000, taxable $65,000
    # Federal tax: $11,925 * 0.10 + $36,550 * 0.12 + $16,525 * 0.22
    #       = $1,192.50 + $4,386 + $3,635.50 = $9,214
    TaxScenario(
        source="DE 2025 Tax Brackets (computed)",
        description="DE Single, $80,000 income, 7th bracket (2025)",
        year=2025,
        state="DE",
        filing_status="Single",
        w2_income=80000.0,
        expected_federal_tax=9055.0,  # Tax Table row 64,250-64,300
        expected_state_tax=4049.0,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # ========== KANSAS SCENARIOS ==========
    # KS 2024 & 2025: 2-bracket progressive system (5.2%, 5.58%)
    # Single/MFS/HoH: 5.2% up to $23,000, then 5.58% above
    # MFJ: 5.2% up to $46,000, then 5.58% above
    # Standard deductions: Single $3,605, MFJ $8,240, MFS $4,120, HoH $6,180
    # Personal exemptions: MFJ $18,320, others $9,160
    # Dependent exemption: $2,320 per dependent
    #
    # KS Single, $30,000 W2 (2024)
    # Federal AGI: $30,000
    # KS AGI: $30,000 (no modifications)
    # KS Standard deduction: $3,605
    # KS Personal exemption: $9,160
    # KS Taxable: $30,000 - $3,605 - $9,160 = $17,235
    # KS Tax: $17,235 * 0.052 = $896.22
    # Federal: Std ded $14,600, taxable $15,400
    # Federal tax: $11,925 * 0.10 + $3,475 * 0.12 = $1,192.50 + $417 = $1,609.50
    TaxScenario(
        source="KS 2024 Tax Brackets (computed)",
        description="KS Single, $30,000 income (2024)",
        year=2024,
        state="KS",
        filing_status="Single",
        w2_income=30000.0,
        dependent_exemptions=9160.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=1619.0,  # Tax Table row 15,400-15,450
        expected_state_tax=896.22,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # KS Single, $50,000 W2 (2024) - tests 2nd bracket
    # Federal AGI: $50,000
    # KS AGI: $50,000
    # KS Standard deduction: $3,605
    # KS Personal exemption: $9,160
    # KS Taxable: $50,000 - $3,605 - $9,160 = $37,235
    # KS Tax: $23,000 * 0.052 + $14,235 * 0.0558 = $1,196 + $794.31 = $1,990.31
    # Federal: Std ded $14,600, taxable $35,400
    # Federal tax: $11,925 * 0.10 + $23,475 * 0.12 = $1,192.50 + $2,817 = $4,009.50
    TaxScenario(
        source="KS 2024 Tax Brackets (computed)",
        description="KS Single, $50,000 income, 2nd bracket (2024)",
        year=2024,
        state="KS",
        filing_status="Single",
        w2_income=50000.0,
        dependent_exemptions=9160.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1990.31,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # KS MFJ, $100,000 W2 (2024) - tests MFJ brackets
    # Federal AGI: $100,000
    # KS AGI: $100,000
    # KS Standard deduction: $8,240
    # KS Personal exemption: $18,320
    # KS Taxable: $100,000 - $8,240 - $18,320 = $73,440
    # KS Tax: $46,000 * 0.052 + $27,440 * 0.0558 = $2,392 + $1,531.15 = $3,923.15
    # Federal: Std ded $29,200, taxable $70,800
    # Federal tax: $23,850 * 0.10 + $46,950 * 0.12 = $2,385 + $5,634 = $8,019
    TaxScenario(
        source="KS 2024 Tax Brackets (computed)",
        description="KS MFJ, $100,000 income (2024)",
        year=2024,
        state="KS",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=18320.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3923.15,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # KS HoH, $60,000 W2 (2024) - tests HoH filing status
    # Federal AGI: $60,000
    # KS AGI: $60,000
    # KS Standard deduction: $6,180
    # KS Personal exemption: $9,160
    # KS Taxable: $60,000 - $6,180 - $9,160 = $44,660
    # KS Tax: $23,000 * 0.052 + $21,660 * 0.0558 = $1,196 + $1,208.63 = $2,404.63
    # Federal: Std ded $21,900, taxable $38,100
    # Federal tax: $16,550 * 0.10 + $21,550 * 0.12 = $1,655 + $2,586 = $4,241
    TaxScenario(
        source="KS 2024 Tax Brackets (computed)",
        description="KS HoH, $60,000 income (2024)",
        year=2024,
        state="KS",
        filing_status="Head_of_House",
        w2_income=60000.0,
        dependent_exemptions=9160.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=4244.0,  # Tax Table row 38,100-38,150
        expected_state_tax=2404.63,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # KS Single, $80,000 W2 (2025) - tests 2025 with same rates as 2024
    # Federal AGI: $80,000
    # KS AGI: $80,000
    # KS Standard deduction: $3,605 (unchanged from 2024)
    # KS Personal exemption: $9,160 (unchanged from 2024)
    # KS Taxable: $80,000 - $3,605 - $9,160 = $67,235
    # KS Tax: $23,000 * 0.052 + $44,235 * 0.0558 = $1,196 + $2,468.31 = $3,664.31
    # Federal 2025: Std ded $15,000, taxable $65,000
    # Federal tax: $11,925 * 0.10 + $36,550 * 0.12 + $16,525 * 0.22
    #           = $1,192.50 + $4,386 + $3,635.50 = $9,214
    TaxScenario(
        source="KS 2025 Tax Brackets (computed)",
        description="KS Single, $80,000 income (2025)",
        year=2025,
        state="KS",
        filing_status="Single",
        w2_income=80000.0,
        dependent_exemptions=9160.0,  # Personal exemption only (std deduction auto-computed)
        expected_federal_tax=9055.0,  # Tax Table row 64,250-64,300
        expected_state_tax=3664.31,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # ========== OREGON SCENARIOS ==========
    # OR 2024 Single, low income (2024)
    # Federal AGI: $30,000
    # OR additions: $0, OR subtractions: $0
    # OR income before deductions: $30,000
    # OR standard deduction: $2,745
    # OR taxable: $30,000 - $2,745 = $27,255
    # OR tax: $4,400 * 0.0475 + ($11,050 - $4,400) * 0.0675 + ($27,255 - $11,050) * 0.0875
    #       = $209 + $448.875 + $1,417.9375 = $2,075.8125
    # Federal standard deduction (2024): $14,600
    # Federal taxable: $30,000 - $14,600 = $15,400
    # Federal tax: $11,600 * 0.10 + $3,800 * 0.12 = $1,160 + $456 = $1,616
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR Single, $30,000 income (2024)",
        year=2024,
        state="OR",
        filing_status="Single",
        w2_income=30000.0,
        expected_federal_tax=1619.0,  # Tax Table row 15,400-15,450
        expected_state_tax=2075.81,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # OR Single, middle income (2024)
    # Federal AGI: $60,000
    # OR income before deductions: $60,000
    # OR standard deduction: $2,745
    # OR taxable: $60,000 - $2,745 = $57,255
    # OR tax: $4,400 * 0.0475 + ($11,050 - $4,400) * 0.0675 + ($57,255 - $11,050) * 0.0875
    #       = $209 + $448.875 + $4,042.9375 = $4,700.8125
    # Federal taxable: $60,000 - $14,600 = $45,400
    # Federal tax: $11,600 * 0.10 + $33,800 * 0.12 = $1,160 + $4,056 = $5,216
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR Single, $60,000 income (2024)",
        year=2024,
        state="OR",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5219.0,  # Tax Table row 45,400-45,450
        expected_state_tax=4700.81,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # OR Single, high income - top bracket (2024)
    # Federal AGI: $150,000
    # OR income before deductions: $150,000
    # OR standard deduction: $2,745
    # OR taxable: $150,000 - $2,745 = $147,255
    # OR tax: $4,400 * 0.0475 + ($11,050 - $4,400) * 0.0675 + ($125,000 - $11,050) * 0.0875 + ($147,255 - $125,000) * 0.099
    #       = $209 + $448.875 + $9,970.625 + $2,203.245 = $12,831.745
    # Federal taxable: $150,000 - $14,600 = $135,400
    # Federal tax (formula, graph backend): $25,538.50
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR Single, $150,000 income, top bracket (2024)",
        year=2024,
        state="OR",
        filing_status="Single",
        w2_income=150000.0,
        expected_federal_tax=25538.50,
        expected_state_tax=12831.75,
        expected_federal_agi=150000.0,
        backend="graph",
    ),
    # OR MFJ, middle income (2024)
    # Federal AGI: $100,000
    # OR income before deductions: $100,000
    # OR standard deduction (MFJ): $5,495
    # OR taxable: $100,000 - $5,495 = $94,505
    # OR tax: $8,800 * 0.0475 + ($22,100 - $8,800) * 0.0675 + ($94,505 - $22,100) * 0.0875
    #       = $418 + $897.75 + $6,335.4375 = $7,651.1875
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR MFJ, $100,000 income (2024)",
        year=2024,
        state="OR",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=7651.19,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # OR HoH, middle income (2024)
    # Federal AGI: $75,000
    # OR income before deductions: $75,000
    # OR standard deduction (HoH): $4,420
    # OR taxable: $75,000 - $4,420 = $70,580
    # OR tax: $8,600 * 0.0475 + ($21,500 - $8,600) * 0.0675 + ($70,580 - $21,500) * 0.0875
    #       = $408.50 + $870.75 + $4,294.50 = $5,573.75
    # Federal taxable: $75,000 - $21,900 = $53,100
    # Federal tax: $16,550 * 0.10 + $36,550 * 0.12 = $1,655 + $4,386 = $6,041
    TaxScenario(
        source="OR 2024 Tax Brackets (computed)",
        description="OR HoH, $75,000 income (2024)",
        year=2024,
        state="OR",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=5573.75,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # OR Single, 2025 with increased standard deduction
    # Federal AGI: $80,000
    # OR income before deductions: $80,000
    # OR standard deduction (2025): $2,835
    # OR taxable: $80,000 - $2,835 = $77,165
    # OR tax: $4,400 * 0.0475 + ($11,050 - $4,400) * 0.0675 + ($77,165 - $11,050) * 0.0875
    #       = $209 + $448.875 + $5,785.0625 = $6,442.9375
    # Federal taxable (2025): $80,000 - $15,000 = $65,000
    # Federal tax (formula, graph backend): $9,214.00
    TaxScenario(
        source="OR 2025 Tax Brackets (computed)",
        description="OR Single, $80,000 income (2025)",
        year=2025,
        state="OR",
        filing_status="Single",
        w2_income=80000.0,
        expected_federal_tax=9055.0,  # Tax Table row 64,250-64,300
        expected_state_tax=6442.94,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # ========== NEW YORK SCENARIOS ==========
    # NY 2024 Single: Standard deduction $8,000
    # Brackets: 4% ($0-$8,500), 4.5% ($8,500-$11,700), 5.25% ($11,700-$13,900),
    #           5.5% ($13,900-$80,650), 6% ($80,650-$215,400), 6.85% ($215,400+)
    # NY 2024 MFJ: Standard deduction $16,050
    # Brackets: 4% ($0-$17,150), 4.5% ($17,150-$23,600), 5.25% ($23,600-$27,900),
    #           5.5% ($27,900-$161,550), 6% ($161,550-$323,200), 6.85% ($323,200+)
    #
    # Provenance audit (tenforty-b72.16); labels stay unverified-legacy.
    # Commit 09c8442 replaced the exact-formula values of the first three cases
    # ($340, $407.50, $2,585) with OTS output ($296, $364, $2,587); the comments
    # below still carry the superseded arithmetic. Re-checked against official
    # sources, retrieved 2026-10-01, those OTS values equal the return values:
    # NYS 2024 Tax Table, https://www.tax.ny.gov/pit/file/tax-tables/it201i-2024.htm,
    # rows 8,500-8,550 = $341, 10,000-10,050 = $409, 50,000-50,050 = $2,587,
    # less household credit table 1 (IT-201-I 2024 p. 12,
    # https://www.tax.ny.gov/pdf/2024/inc/it201i_2024.pdf) of $45, $45, $0.
    # Federal values equal the IRS 2024 Tax Table Single column (i1040tt--2024).
    #
    # The graph's IT-201 (tenforty-spec/forms/NYIT201_2024.hs) computes line 39
    # as the exact bracket formula, and line 40 household credit is a keyInput
    # defaulting to 0. IT-201-I p. 12 requires the Tax Table when NYAGI <=
    # $107,650 and TI < $65,000, and the rate schedule (p. 33) publishes
    # whole-dollar bases ($600 over $13,900) where exact bracket arithmetic gives
    # $599.50. The known_defects below record the resulting residuals.
    #
    # The three cases with NYAGI > $107,650 stay OTS-only. Their expectations are
    # the rate-schedule formula without the IT-201-I tax computation worksheets
    # (pp. 34, 36) the return requires. The graph omits those worksheets too, so
    # on the graph they would pass for the wrong reason.
    #
    # NY Single at top of 4% bracket
    # NY taxable: $8,500, NY tax: $8,500 x 0.04 = $340
    # Household Credit (FAGI $16,500): $45. Net Tax: $295.
    # Federal taxable: $1,900, Federal tax: $191 (Table row 1,900-1,925)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single at top of 4% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=16500.0,  # NY taxable $8,500 + $8,000 std ded
        expected_federal_tax=191.0,
        expected_state_tax=296.0,  # OTS $296 (+$1 rounding)
        backend="graph",
        known_defects=(
            KnownDefect(
                "state_total_tax",
                43.5,
                44.5,
                "Graph omits NY household credit (IT-201 line 40 is an input, "
                "default 0; table 1 gives $45 at FAGI $16,500): +$45. Graph line 39 is "
                "the exact formula, not Tax Table row 8,500-8,550 ($341): -$1. Net +$44.",
            ),
        ),
    ),
    # NY Single in 4.5% bracket
    # NY taxable: $10,000, NY tax: $340 + $67.50 = $407.50
    # Household Credit (FAGI $18,000): $45. Net Tax: $362.50.
    # Federal taxable: $3,400, Federal tax: $343 (Table row 3,400-3,450)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single in 4.5% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=18000.0,  # NY taxable $10,000 + $8,000 std ded
        expected_federal_tax=343.0,
        expected_state_tax=364.0,  # OTS $364 (+$1.50 rounding)
        backend="graph",
        known_defects=(
            KnownDefect(
                "state_total_tax",
                43.0,
                44.0,
                "Graph omits NY household credit (IT-201 line 40 is an input, "
                "default 0; table 1 gives $45 at FAGI $18,000): +$45. Graph line 39 is "
                "the exact formula $407.50, not Tax Table row 10,000-10,050 ($409): "
                "-$1.50. Net +$43.50.",
            ),
        ),
    ),
    # NY Single in 5.5% bracket
    # NY taxable: $50,000, NY tax: $599.50 + $1,985.50 = $2,585
    # Household Credit (FAGI $58,000): $0.
    # Federal taxable: $43,400, Federal tax: $4,979 (Table row 43,400-43,450)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single in 5.5% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=58000.0,  # NY taxable $50,000 + $8,000 std ded
        expected_federal_tax=4979.0,
        expected_state_tax=2587.0,  # OTS $2587 (+$2 rounding)
        backend="graph",
        known_defects=(
            KnownDefect(
                "state_total_tax",
                -2.5,
                -1.5,
                "Graph NY line 39 is the exact bracket formula: base $599.50 at "
                "$13,900 vs the published rate-schedule base $600 (-$0.50), and no Tax "
                "Table pricing for TI < $65,000 (row 50,000-50,050 = $2,587; -$1.50). "
                "Net -$2. No household credit at FAGI $58,000.",
            ),
        ),
    ),
    # NY Single in 6% bracket
    # NY taxable: $100,000, NY tax: $4,270.75 + $1,161 = $5,431.75
    # Supplemental Tax applies for NY AGI > $107,650. FAGI=$108,000.
    # Federal taxable: $93,400, Federal tax: $15,601 (Formula) -> $15,607 (Table)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single in 6% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=108000.0,  # NY taxable $100,000 + $8,000 std ded
        expected_federal_tax=15607.0,
        expected_state_tax=5431.75,
        known_failure="Silver Standard formula lacks NY Supplemental Tax implementation (OTS computes $5,435.98).",
    ),
    # NY Single in 6.85% bracket
    # NY taxable: $250,000, NY tax: $12,355.75 + $2,370.10 = $14,725.85
    # Federal taxable: $243,400, Federal tax: $55,574.50
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY Single in 6.85% bracket",
        year=2024,
        state="NY",
        filing_status="Single",
        w2_income=258000.0,  # NY taxable $250,000 + $8,000 std ded
        expected_federal_tax=55574.5,
        expected_state_tax=14725.85,
        known_failure="Silver Standard formula lacks NY Supplemental Tax implementation (OTS computes $16,854.11).",
    ),
    # NY MFJ in 5.5% bracket
    # NY taxable: $100,000, NY tax: $1,202 + $3,965.50 = $5,167.50
    # Federal taxable: $86,850, Federal tax: $9,958 (Formula) -> $9,961 (Table)
    TaxScenario(
        source="NY 2024 Tax Brackets (computed)",
        description="NY MFJ in 5.5% bracket",
        year=2024,
        state="NY",
        filing_status="Married/Joint",
        w2_income=116050.0,  # NY taxable $100,000 + $16,050 std ded
        expected_federal_tax=9961.0,
        expected_state_tax=5167.5,
        known_failure=(
            "OTS computes state=$5,223.36 (+$55.86): NYAGI $116,050 > $107,650, so"
            " OTS applies IT-201-I (2024) p. 34 tax computation worksheet 1"
            " recapture, 0.168 x ($5,500 - $5,167.50) = $55.86; the expected value"
            " is the rate-schedule formula without that worksheet."
        ),
    ),
    # ========== PENNSYLVANIA SCENARIOS ==========
    # PA 2024: Flat 3.07% rate, no standard deduction, no personal exemption
    # PA tax = sum(max(0, income_class_i)) * 0.0307
    # These use graph backend (OTS PA_40 crashes).
    # Federal tax values are exact formula (not Tax Table) since graph backend
    # computes federal tax via formula rather than OTS's table lookup.
    #
    # PA Single, $50,000 W2 only
    # PA taxable: $50,000, PA tax: $50,000 x 0.0307 = $1,535
    # Federal taxable: $35,400, Federal tax: $1,160 + ($35,400 - $11,600) * 0.12 = $4,016
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $50,000 W2 only",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1535.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # PA Single, $100,000 W2 only
    # PA taxable: $100,000, PA tax: $100,000 x 0.0307 = $3,070
    # Federal taxable: $85,400, Federal tax: $5,426 + ($85,400 - $47,150) * 0.22 = $13,841
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $100,000 W2 only",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13847.0,  # Tax Table row 85,400-85,450
        expected_state_tax=3070.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # PA Single, $75,000 W2 + $5,000 interest
    # PA taxable: $80,000, PA tax: $80,000 x 0.0307 = $2,456
    # Federal taxable: $65,400, Federal tax: $5,426 + ($65,400 - $47,150) * 0.22 = $9,441
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $75,000 W2 + $5,000 interest",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=75000.0,
        taxable_interest=5000.0,
        expected_federal_tax=9447.0,  # Tax Table row 65,400-65,450
        expected_state_tax=2456.0,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # PA Single, $60,000 W2 + $3,000 dividends
    # PA taxable: $63,000, PA tax: $63,000 x 0.0307 = $1,934.10
    # Federal: uses preferential rates for qualified dividends
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $60,000 W2 + $3,000 dividends",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=60000.0,
        qualified_dividends=3000.0,
        ordinary_dividends=3000.0,
        expected_federal_tax=5425.25,  # QDCG worksheet, line 22 Tax Table row 45,400-45,450
        expected_state_tax=1934.1,
        expected_federal_agi=63000.0,
        backend="graph",
    ),
    # PA Single, $80,000 W2 + $2,000 interest + $1,000 dividends
    # PA taxable: $83,000, PA tax: $83,000 x 0.0307 = $2,548.10
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA Single, $80,000 W2 + $2,000 interest + $1,000 dividends",
        year=2024,
        state="PA",
        filing_status="Single",
        w2_income=80000.0,
        taxable_interest=2000.0,
        qualified_dividends=1000.0,
        ordinary_dividends=1000.0,
        expected_federal_tax=10037.0,  # QDCG worksheet, line 22 Tax Table row 67,400-67,450
        expected_state_tax=2548.1,
        expected_federal_agi=83000.0,
        backend="graph",
    ),
    # PA MFJ, $120,000 W2 only
    # PA taxable: $120,000, PA tax: $120,000 x 0.0307 = $3,684
    # Federal MFJ: taxable $90,800 ($120K - $29.2K std ded), in 12% bracket
    # Federal tax: $2,320 + ($90,800 - $23,200) * 0.12 = $10,432
    TaxScenario(
        source="PA 2024 Tax Brackets (computed)",
        description="PA MFJ, $120,000 W2 only",
        year=2024,
        state="PA",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10435.0,  # Tax Table row 90,800-90,850
        expected_state_tax=3684.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== WISCONSIN SCENARIOS ==========
    # WI Form 1, wages only, under 65, no dependents, no credits; line 7 = wages.
    # Derived blind, session 1b251ce6, retrieved 2026-09-30, from
    #   I24 https://www.revenue.wi.gov/TaxForms2024/2024-Form1-Inst.pdf
    #   I25 https://www.revenue.wi.gov/TaxForms2025/2025-Form1-Inst.pdf
    # SD is the official Standard Deduction Table row (pp. 35-37, $500 bands);
    # exemptions are $700 per filer (line 10a). Tax is the official Tax Table row
    # (pp. 38-43, $100 bands) below $100k of TI, else the p. 44 worksheet rounded
    # to whole dollars. The graph computes the exact statutory formulas, so each
    # tolerance is the band-effect bound from _wi_band_tolerance.
    # WI 2024 Single $12,000: SD 13,230 (0-13,000) + exemptions 700 -> TI 0, tax 0
    # I24 p.35, p.38; derived-arithmetic (TI is 0; tax-table row 0-20 is $0)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $12k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=12000.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # WI 2024 Single $20,000: SD 13,088 (20,000-20,500) + exemptions 700 -> TI 6,212, tax 219
    # I24 p.35, p.38; tax-table (row 6,200-6,300)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $20k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=20000.0,
        expected_state_tax=219.0,
        state_tax_tolerance=_wi_band_tolerance(0.035, 0.12, tax_table=True),
        backend="graph",
    ),
    # WI 2024 Single $60,000: SD 8,288 (60,000-60,500) + exemptions 700 -> TI 51,012, tax 2,319
    # I24 p.35, p.41; tax-table (row 51,000-51,100)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $60k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=60000.0,
        expected_state_tax=2319.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, 0.12, tax_table=True),
        backend="graph",
    ),
    # WI 2024 Single $150,000: SD 0 (129,500 and over) + exemptions 700 -> TI 149,300, tax 7,526
    # I24 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $150k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=150000.0,
        expected_state_tax=7526.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, tax_table=False),
        backend="graph",
    ),
    # WI 2024 Single $400,000: SD 0 (Single column is 0 from the 129,500 row;
    # formula zero ~129,320) + exemptions 700 -> TI 399,300, tax 22,750
    # I24 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $400k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Single",
        w2_income=400000.0,
        expected_state_tax=22750.0,
        state_tax_tolerance=_wi_band_tolerance(0.0765, tax_table=False),
        backend="graph",
    ),
    # WI 2024 MFJ $12,000: SD 24,490 + exemptions 1,400 -> TI 0, tax 0
    # I24 p.35; derived-arithmetic (TI is 0; tax-table row 0-20 is $0)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $12k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Married/Joint",
        w2_income=12000.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # WI 2024 MFJ $20,000: SD 24,490 + exemptions 1,400 -> TI 0, tax 0
    # I24 p.35; derived-arithmetic (TI is 0; tax-table row 0-20 is $0)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $20k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Married/Joint",
        w2_income=20000.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # WI 2024 MFJ $60,000: SD 18,017 (60,000-60,500) + exemptions 1,400 -> TI 40,583, tax 1,634
    # I24 p.35, p.40; tax-table (row 40,500-40,600)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $60k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Married/Joint",
        w2_income=60000.0,
        expected_state_tax=1634.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, 0.19778, tax_table=True),
        backend="graph",
    ),
    # WI 2024 MFJ $150,000: SD 216 (150,000-150,500) + exemptions 1,400 -> TI 148,384, tax 7,349
    # I24 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $150k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Married/Joint",
        w2_income=150000.0,
        expected_state_tax=7349.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, 0.19778, tax_table=False),
        backend="graph",
    ),
    # WI 2024 MFJ $400,000: SD 0 (151,344 or over) + exemptions 1,400 -> TI 398,600, tax 20,610
    # I24 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $400k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Married/Joint",
        w2_income=400000.0,
        expected_state_tax=20610.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, tax_table=False),
        backend="graph",
    ),
    # WI 2024 HoH $12,000: SD 17,090 + exemptions 700 -> TI 0, tax 0
    # I24 p.35; derived-arithmetic (TI is 0; tax-table row 0-20 is $0)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $12k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Head_of_House",
        w2_income=12000.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # WI 2024 HoH $20,000: SD 16,824 (20,000-20,500) + exemptions 700 -> TI 2,476, tax 86
    # I24 p.35, p.38; tax-table (row 2,400-2,500)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $20k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Head_of_House",
        w2_income=20000.0,
        expected_state_tax=86.0,
        state_tax_tolerance=_wi_band_tolerance(0.035, 0.22515, tax_table=True),
        backend="graph",
    ),
    # WI 2024 HoH $60,000: SD 8,288 (60,000-60,500) + exemptions 700 -> TI 51,012, tax 2,319
    # I24 p.35, p.41; tax-table (row 51,000-51,100)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $60k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_state_tax=2319.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, 0.12, tax_table=True),
        backend="graph",
    ),
    # WI 2024 HoH $150,000: SD 0 + exemptions 700 -> TI 149,300, tax 7,526
    # I24 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $150k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Head_of_House",
        w2_income=150000.0,
        expected_state_tax=7526.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, tax_table=False),
        backend="graph",
    ),
    # WI 2024 HoH $400,000: SD 0 + exemptions 700 -> TI 399,300, tax 22,750
    # I24 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2024 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $400k wages (2024)",
        year=2024,
        state="WI",
        filing_status="Head_of_House",
        w2_income=400000.0,
        expected_state_tax=22750.0,
        state_tax_tolerance=_wi_band_tolerance(0.0765, tax_table=False),
        backend="graph",
    ),
    # WI 2025 Single $12,000: SD 13,560 (0-13,390) + exemptions 700 -> TI 0, tax 0
    # I25 p.35, p.38; derived-arithmetic (TI is 0; tax-table row 0-20 is $0)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $12k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Single",
        w2_income=12000.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # WI 2025 Single $20,000: SD 13,476 (20,000-20,500) + exemptions 700 -> TI 5,824, tax 205
    # I25 p.35, p.38; tax-table (row 5,800-5,900)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $20k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Single",
        w2_income=20000.0,
        expected_state_tax=205.0,
        state_tax_tolerance=_wi_band_tolerance(0.035, 0.12, tax_table=True),
        backend="graph",
    ),
    # WI 2025 Single $60,000: SD 8,676 (60,000-60,500) + exemptions 700 -> TI 50,624, tax 2,098
    # I25 p.35, p.41; tax-table (row 50,600-50,700)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $60k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Single",
        w2_income=60000.0,
        expected_state_tax=2098.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, 0.12, tax_table=True),
        backend="graph",
    ),
    # WI 2025 Single $150,000: SD 0 (132,500 and over) + exemptions 700 -> TI 149,300, tax 7,326
    # I25 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $150k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Single",
        w2_income=150000.0,
        expected_state_tax=7326.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, tax_table=False),
        backend="graph",
    ),
    # WI 2025 Single $400,000: SD 0 (Single column is 0 from the 132,500 row;
    # formula zero ~132,550) + exemptions 700 -> TI 399,300, tax 22,363
    # I25 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI Single, $400k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Single",
        w2_income=400000.0,
        expected_state_tax=22363.0,
        state_tax_tolerance=_wi_band_tolerance(0.0765, tax_table=False),
        backend="graph",
    ),
    # WI 2025 MFJ $12,000: SD 25,110 + exemptions 1,400 -> TI 0, tax 0
    # I25 p.35; derived-arithmetic (TI is 0; tax-table row 0-20 is $0)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $12k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Married/Joint",
        w2_income=12000.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # WI 2025 MFJ $20,000: SD 25,110 + exemptions 1,400 -> TI 0, tax 0
    # I25 p.35; derived-arithmetic (TI is 0; tax-table row 0-20 is $0)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $20k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Married/Joint",
        w2_income=20000.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # WI 2025 MFJ $60,000: SD 18,773 (60,000-60,500) + exemptions 1,400 -> TI 39,827, tax 1,577
    # I25 p.35, p.40; tax-table (row 39,800-39,900)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $60k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Married/Joint",
        w2_income=60000.0,
        expected_state_tax=1577.0,
        state_tax_tolerance=_wi_band_tolerance(0.044, 0.19778, tax_table=True),
        backend="graph",
    ),
    # WI 2025 MFJ $150,000: SD 973 (150,000-150,500) + exemptions 1,400 -> TI 147,627, tax 7,042
    # I25 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $150k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Married/Joint",
        w2_income=150000.0,
        expected_state_tax=7042.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, 0.19778, tax_table=False),
        backend="graph",
    ),
    # WI 2025 MFJ $400,000: SD 0 (155,169 or more) + exemptions 1,400 -> TI 398,600, tax 20,344
    # I25 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI MFJ, $400k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Married/Joint",
        w2_income=400000.0,
        expected_state_tax=20344.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, tax_table=False),
        backend="graph",
    ),
    # WI 2025 HoH $12,000: SD 17,520 + exemptions 700 -> TI 0, tax 0
    # I25 p.35; derived-arithmetic (TI is 0; tax-table row 0-20 is $0)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $12k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Head_of_House",
        w2_income=12000.0,
        expected_state_tax=0.0,
        backend="graph",
    ),
    # WI 2025 HoH $20,000: SD 17,362 (20,000-20,500) + exemptions 700 -> TI 1,938, tax 68
    # I25 p.35, p.38; tax-table (row 1,900-2,000)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $20k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Head_of_House",
        w2_income=20000.0,
        expected_state_tax=68.0,
        state_tax_tolerance=_wi_band_tolerance(0.035, 0.22515, tax_table=True),
        backend="graph",
    ),
    # WI 2025 HoH $60,000: SD 8,676 (60,000-60,500) + exemptions 700 -> TI 50,624, tax 2,098
    # I25 p.35, p.41; tax-table (row 50,600-50,700)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $60k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_state_tax=2098.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, 0.12, tax_table=True),
        backend="graph",
    ),
    # WI 2025 HoH $150,000: SD 0 + exemptions 700 -> TI 149,300, tax 7,326
    # I25 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $150k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Head_of_House",
        w2_income=150000.0,
        expected_state_tax=7326.0,
        state_tax_tolerance=_wi_band_tolerance(0.053, tax_table=False),
        backend="graph",
    ),
    # WI 2025 HoH $400,000: SD 0 + exemptions 700 -> TI 399,300, tax 22,363
    # I25 p.37, p.44; derived-arithmetic (p. 44 Tax Computation Worksheet)
    TaxScenario(
        source="WI 2025 Form 1 Instructions (official tables)",
        state_evidence=WI_EVIDENCE,
        description="WI HoH, $400k wages (2025)",
        year=2025,
        state="WI",
        filing_status="Head_of_House",
        w2_income=400000.0,
        expected_state_tax=22363.0,
        state_tax_tolerance=_wi_band_tolerance(0.0765, tax_table=False),
        backend="graph",
    ),
    # ========== NJ/VA BLIND-DERIVED STATE SCENARIOS ==========
    # Record: docs/validation/state-fixtures/NJ-VA-2024-2025.md.
    # State answers only were re-derived; federal expectations are IRS Tax
    # Table rows (tj2.19), re-checked against tests/fixtures/federal_tax_table.
    # Blind case 1: 30,000 - 1,000 = 29,000; mandatory NJ row 29,000-29,050.
    TaxScenario(
        source="NJ 2024 official tax table (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="NJ Single, $30k income (1.75% bracket)",
        year=2024,
        state="NJ",
        filing_status="Single",
        w2_income=30000.0,
        expected_federal_tax=1619.0,  # Tax Table row 15,400-15,450
        expected_state_tax=438.0,
        expected_federal_agi=30000.0,
        known_defects=(nj_table_gap(0.0175, 29_000, 29_000),),
        backend="graph",
    ),
    # Blind case 2: 60,000 - 1,000 = 59,000; mandatory NJ row 59,000-59,050.
    TaxScenario(
        source="NJ 2024 official tax table (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="NJ Single, $60k income (5.525% bracket)",
        year=2024,
        state="NJ",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5219.0,  # Tax Table row 45,400-45,450
        expected_state_tax=1769.0,
        expected_federal_agi=60000.0,
        known_defects=(nj_table_gap(0.05525, 59_000, 59_000),),
        backend="graph",
    ),
    # Blind case 3: 65,000 - 2,000 = 63,000; NJ joint column, row 63,000-63,050.
    TaxScenario(
        source="NJ 2024 official tax table (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="NJ MFJ, $65k income (2.45% bracket)",
        year=2024,
        state="NJ",
        filing_status="Married/Joint",
        w2_income=65000.0,
        expected_federal_tax=3835.0,  # Tax Table row 35,800-35,850
        expected_state_tax=1124.0,
        expected_federal_agi=65000.0,
        known_defects=(nj_table_gap(0.0245, 63_000, 63_000),),
        backend="graph",
    ),
    # Blind case 4: 100,000 - 1,000 = 99,000; NJ HoH column, below table cutoff.
    TaxScenario(
        source="NJ 2024 official tax table (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="NJ HoH, $100k income (5.525% bracket)",
        year=2024,
        state="NJ",
        filing_status="Head_of_House",
        w2_income=100000.0,
        expected_federal_tax=10547.0,  # Tax Table row 78,100-78,150
        expected_state_tax=2696.0,
        expected_federal_agi=100000.0,
        known_defects=(nj_table_gap(0.05525, 99_000, 99_000),),
        backend="graph",
    ),
    # Blind case 5: 50,000 - 1,000 = 49,000; independently checked 2025 table.
    TaxScenario(
        source="NJ 2025 official tax table (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="NJ Single, $50k income (2025, rates unchanged)",
        year=2025,
        state="NJ",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1216.0,
        expected_federal_agi=50000.0,
        known_defects=(nj_table_gap(0.05525, 49_000, 49_000),),
        backend="graph",
    ),
    # Blind case 6: TI 40,570; schedule 2,075.275 -> 2,075 (table agrees).
    TaxScenario(
        source="VA 2024 official rounded schedule (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="VA Single, $50,000 W2 only",
        year=2024,
        state="VA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=2075.0,
        expected_federal_agi=50000.0,
        known_defects=(VA_SCHEDULE_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 7: TI 81,140; schedule 4,408.05 -> 4,408 (optional table: 4,409).
    # All wages belong to the primary filer: spouse adjustment is zero.
    TaxScenario(
        source="VA 2024 official rounded schedule (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="VA MFJ, $100,000 W2 only",
        year=2024,
        state="VA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=4408.0,
        expected_federal_agi=100000.0,
        known_defects=(VA_SCHEDULE_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 8: TI 15,320; schedule 636 (optional table: 637).
    TaxScenario(
        source="VA 2025 official rounded schedule (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="VA Single, $25,000 W2 only (2025 std deduction)",
        year=2025,
        state="VA",
        filing_status="Single",
        w2_income=25000.0,
        expected_federal_tax=928.0,  # Tax Table row 9,250-9,300
        expected_state_tax=636.0,
        expected_federal_agi=25000.0,
        backend="graph",
    ),
    # Blind case 9: TI 100,640; schedule 5,529.30 -> 5,529, above table range.
    # All wages belong to the primary filer: spouse adjustment is zero.
    TaxScenario(
        source="VA 2025 official rounded schedule (blind derivation)",
        state_evidence=NJ_VA_EVIDENCE,
        description="VA MFJ, $120,000 W2 only (2025 std deduction)",
        year=2025,
        state="VA",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10146.0,  # Tax Table row 88,500-88,550
        expected_state_tax=5529.0,
        expected_federal_agi=120000.0,
        known_defects=(VA_SCHEDULE_ROUNDING_GAP,),
        backend="graph",
    ),
    # ========== MISSOURI SCENARIOS ==========
    # Missouri fixtures live in tests/mo_1040_test.py: official MO values are
    # whole dollars; that module documents the $1 chart-rounding tolerance.
    # ========== MINNESOTA SCENARIOS ==========
    # MN 2024: 4 brackets (5.35%, 6.80%, 7.85%, 9.85%)
    # Standard deduction: $14,575 (Single/MFS), $29,150 (MFJ), $21,862.50 (HoH)
    # Brackets (Single): $31,690, $104,090, $193,240
    # Brackets (MFJ): $46,330, $184,040, $321,450
    # Brackets (HoH): $39,010, $156,760, $256,880
    #
    # MN 2025: Same 4 brackets, adjusted by 2.886% inflation
    # Standard deduction: $14,950 (Single/MFS), $29,900 (MFJ), $22,500 (HoH)
    # Brackets (Single): $32,570, $106,990, $198,630
    # Brackets (MFJ): $47,620, $189,180, $330,410
    # Brackets (HoH): $40,100, $161,130, $264,050
    #
    # MN Single, $50,000 W2 (2024)
    # Fed AGI: $50,000
    # MN Taxable: $50,000 - $14,575 = $35,425
    # MN Tax: $31,690 x 5.35% + $3,735 x 6.80% = $1,695.42 + $253.98 = $1,949.40
    # Federal taxable: $50,000 - $14,600 = $35,400
    # Federal tax (OTS tables): $4,016.00
    TaxScenario(
        source="MN 2024 Tax Brackets (computed)",
        description="MN Single, $50,000 income (2024)",
        year=2024,
        state="MN",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1949.40,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MN MFJ, $100,000 W2 (2024)
    # Fed AGI: $100,000
    # MN Taxable: $100,000 - $29,150 = $70,850
    # MN Tax: $46,330 x 5.35% + $24,520 x 6.80% = $2,478.66 + $1,667.36 = $4,146.02
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax (OTS tables): $8,032.00
    TaxScenario(
        source="MN 2024 Tax Brackets (computed)",
        description="MN MFJ, $100,000 income (2024)",
        year=2024,
        state="MN",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=4146.02,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MN Single, $50,000 W2 (2025)
    # Fed AGI: $50,000
    # MN Taxable: $50,000 - $14,950 = $35,050
    # MN Tax: $32,570 x 5.35% + $2,480 x 6.80% = $1,742.50 + $168.64 = $1,911.14
    # Federal 2025 taxable: $50,000 - $15,000 = $35,000
    # Federal tax: $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="MN 2025 Tax Brackets (computed)",
        description="MN Single, $50,000 income (2025)",
        year=2025,
        state="MN",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1911.14,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MN MFJ, $100,000 W2 (2025)
    # Fed AGI: $100,000
    # MN Taxable: $100,000 - $29,900 = $70,100
    # MN Tax: $47,620 x 5.35% + $22,480 x 6.80% = $2,547.67 + $1,528.64 = $4,076.31
    # Federal 2025 taxable: $100,000 - $30,000 = $70,000
    # Federal tax: $23,850 x 0.10 + $46,150 x 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="MN 2025 Tax Brackets (computed)",
        description="MN MFJ, $100,000 income (2025)",
        year=2025,
        state="MN",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=4076.31,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MN HoH, $75,000 W2 (2025)
    # Fed AGI: $75,000
    # MN Taxable: $75,000 - $22,500 = $52,500
    # MN Tax: $40,100 x 5.35% + $12,400 x 6.80% = $2,145.35 + $843.20 = $2,988.55
    # Federal 2025 taxable: $75,000 - $22,500 = $52,500
    # Federal tax (graph-computed): $5,960.00
    TaxScenario(
        source="MN 2025 Tax Brackets (computed)",
        description="MN HoH, $75,000 income (2025)",
        year=2025,
        state="MN",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=5825.0,
        expected_state_tax=2988.55,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== SOUTH CAROLINA SCENARIOS ==========
    # SC 2024: 3 brackets (0%, 3%, 6.2%), same for all filing statuses
    # Brackets: $0-$3,560 (0%), $3,560-$17,830 (3%), over $17,830 (6.2%)
    # Dependent exemption: $4,790 per dependent
    # SC starts with federal taxable income (not AGI)
    # No standard deduction (only dependent exemptions)
    #
    # SC 2025: 3 brackets (0%, 3%, 6%), same for all filing statuses
    # Brackets: $0-$3,560 (0%), $3,560-$17,830 (3%), over $17,830 (6%)
    # Dependent exemption: $4,930 per dependent
    #
    # SC Single, $50,000 W2 (2024, no dependents)
    # Fed AGI: $50,000, Fed std deduction: $14,600
    # Fed taxable: $50,000 - $14,600 = $35,400
    # SC taxable: $35,400 (no exemptions)
    # SC tax: $0 + ($17,830 - $3,560) * 0.03 + ($35,400 - $17,830) * 0.062
    #       = $0 + $14,270 * 0.03 + $17,570 * 0.062
    #       = $0 + $428.10 + $1,089.34 = $1,517.44
    # Federal tax (OTS tables): $4,016.00
    TaxScenario(
        source="SC 2024 Tax Brackets (computed)",
        description="SC Single, $50,000 income (2024)",
        year=2024,
        state="SC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1517.44,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # SC MFJ, $100,000 W2 (2024, no dependents)
    # Fed AGI: $100,000, Fed std deduction: $29,200
    # Fed taxable: $100,000 - $29,200 = $70,800
    # SC taxable: $70,800 (no exemptions)
    # SC tax: $0 + $14,270 * 0.03 + ($70,800 - $17,830) * 0.062
    #       = $0 + $428.10 + $52,970 * 0.062
    #       = $0 + $428.10 + $3,284.14 = $3,712.24
    # Federal tax (OTS tables): $8,032.00
    TaxScenario(
        source="SC 2024 Tax Brackets (computed)",
        description="SC MFJ, $100,000 income (2024)",
        year=2024,
        state="SC",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3712.24,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # SC Single, $50,000 W2 (2025, no dependents)
    # Fed std deduction: $15,750
    # Fed taxable: $50,000 - $15,750 = $34,250
    # SC taxable: $34,250 (no exemptions)
    # SC tax (6% top rate): $0 + $14,270 * 0.03 + ($34,250 - $17,830) * 0.06
    #       = $0 + $428.10 + $16,420 * 0.06
    #       = $0 + $428.10 + $985.20 = $1,413.30
    # Federal 2025 tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="SC 2025 Tax Brackets (computed)",
        description="SC Single, $50,000 income (2025)",
        year=2025,
        state="SC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1413.30,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # SC MFJ, $100,000 W2 (2025, no dependents)
    # Fed std deduction: $31,500
    # Fed taxable: $100,000 - $31,500 = $68,500
    # SC taxable: $68,500 (no exemptions)
    # SC tax: $0 + $14,270 * 0.03 + ($68,500 - $17,830) * 0.06
    #       = $0 + $428.10 + $50,670 * 0.06
    #       = $0 + $428.10 + $3,040.20 = $3,468.30
    # Federal 2025 tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="SC 2025 Tax Brackets (computed)",
        description="SC MFJ, $100,000 income (2025)",
        year=2025,
        state="SC",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=3468.30,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # SC HoH, $75,000 W2 (2025, no dependents)
    # Fed std deduction: $23,625
    # Fed taxable: $75,000 - $23,625 = $51,375
    # SC taxable: $51,375 (no exemptions)
    # SC tax: $0 + $14,270 * 0.03 + ($51,375 - $17,830) * 0.06
    #       = $0 + $428.10 + $33,545 * 0.06
    #       = $0 + $428.10 + $2,012.70 = $2,440.80
    # Federal 2025 tax (graph-computed): $5,960.00
    TaxScenario(
        source="SC 2025 Tax Brackets (computed)",
        description="SC HoH, $75,000 income (2025)",
        year=2025,
        state="SC",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=5825.0,
        expected_state_tax=2440.80,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== UTAH SCENARIOS ==========
    # UT 2024: Flat 4.55% rate, Personal exemption $2,046 per dependent
    # UT 2025: Flat 4.5% rate, Personal exemption $2,111 per dependent
    # Utah tax = (Fed AGI - additions + subtractions) * rate
    # Then subtract 6% credit on (personal exemptions + federal deductions - state tax deduction)
    # These scenarios assume no additions/subtractions, no state tax deduction.
    #
    # UT 2024 Single, $50,000 W2, no dependents
    # Fed AGI: $50,000, Fed std ded: $14,600, Fed taxable: $35,400
    # Fed tax (formula): $11,600 * 0.10 + $23,800 * 0.12 = $1,160 + $2,856 = $4,016
    # UT taxable: $50,000 (no adjustments)
    # UT tax initial: $50,000 * 0.0455 = $2,275.00
    # Personal exemptions: $0, Federal deductions: $14,600
    # Credit: ($0 + $14,600 - $0) * 0.06 = $876.00
    # UT tax: $2,275.00 - $876.00 = $1,399.00
    TaxScenario(
        source="UT 2024 Tax Brackets (computed)",
        description="UT Single, $50,000 W2, no dependents",
        year=2024,
        state="UT",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1399.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # UT 2024 MFJ, $100,000 W2, no dependents
    # Fed AGI: $100,000, Fed std ded: $29,200, Fed taxable: $70,800
    # Fed tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    # UT taxable: $100,000
    # UT tax initial: $100,000 * 0.0455 = $4,550.00
    # Credit: ($0 + $29,200 - $0) * 0.06 = $1,752.00
    # UT tax: $4,550.00 - $1,752.00 = $2,798.00
    TaxScenario(
        source="UT 2024 Tax Brackets (computed)",
        description="UT MFJ, $100,000 W2, no dependents",
        year=2024,
        state="UT",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=2798.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # UT 2025 Single, $50,000 W2, no dependents
    # Fed std ded 2025: $15,750, Fed taxable: $34,250
    # Fed tax: $11,925 * 0.10 + $22,325 * 0.12 = $1,192.50 + $2,679 = $3,871.50
    # UT taxable: $50,000
    # UT tax initial: $50,000 * 0.045 = $2,250.00
    # Credit: ($0 + $15,750 - $0) * 0.06 = $945.00
    # UT tax: $2,250.00 - $945.00 = $1,305.00
    TaxScenario(
        source="UT 2025 Tax Brackets (computed)",
        description="UT Single, $50,000 W2, no dependents (2025)",
        year=2025,
        state="UT",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1305.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # UT 2025 MFJ, $75,000 W2, no dependents
    # Fed std ded 2025: $31,500, Fed taxable: $43,500
    # Fed tax: $23,850 * 0.10 + $19,650 * 0.12 = $2,385 + $2,358 = $4,743
    # UT taxable: $75,000
    # UT tax initial: $75,000 * 0.045 = $3,375.00
    # Credit: ($0 + $31,500 - $0) * 0.06 = $1,890.00
    # UT tax: $3,375.00 - $1,890.00 = $1,485.00
    TaxScenario(
        source="UT 2025 Tax Brackets (computed)",
        description="UT MFJ, $75,000 W2, no dependents (2025)",
        year=2025,
        state="UT",
        filing_status="Married/Joint",
        w2_income=75000.0,
        expected_federal_tax=4746.0,  # Tax Table row 43,500-43,550
        expected_state_tax=1485.00,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== IOWA SCENARIOS ==========
    # IA 2024: Progressive brackets (4.4%, 4.82%, 5.7%)
    # Single: 0-6,210: 4.4%, 6,210-31,050: 4.82%, 31,050+: 5.7%
    # MFJ: 0-12,420: 4.4%, 12,420-62,100: 4.82%, 62,100+: 5.7%
    # IA 2025: Flat 3.8% rate
    # IA taxable income = Federal taxable income + Iowa modifications
    #
    # IA 2024 Single, $40,000 W2, no dependents
    # Fed AGI: $40,000, Fed std ded: $14,600, Fed taxable: $25,400
    # Fed tax: $11,600 * 0.10 + $13,800 * 0.12 = $1,160 + $1,656 = $2,816
    # IA taxable: $25,400 (assuming no Iowa modifications)
    # IA tax: $6,210 * 0.044 + ($25,400 - $6,210) * 0.0482
    #       = $273.24 + $19,190 * 0.0482
    #       = $273.24 + $924.96 = $1,198.20
    TaxScenario(
        source="IA 2024 Tax Brackets (computed)",
        description="IA Single, $40,000 W2, no dependents",
        year=2024,
        state="IA",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=1198.20,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # IA 2024 MFJ, $80,000 W2, no dependents
    # Fed AGI: $80,000, Fed std ded: $29,200, Fed taxable: $50,800
    # Fed tax: $23,200 * 0.10 + $27,600 * 0.12 = $2,320 + $3,312 = $5,632
    # IA taxable: $50,800 (assuming no Iowa modifications)
    # IA tax: $12,420 * 0.044 + ($50,800 - $12,420) * 0.0482
    #       = $546.48 + $38,380 * 0.0482
    #       = $546.48 + $1,849.92 = $2,396.40
    TaxScenario(
        source="IA 2024 Tax Brackets (computed)",
        description="IA MFJ, $80,000 W2, no dependents",
        year=2024,
        state="IA",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5635.0,  # Tax Table row 50,800-50,850
        expected_state_tax=2396.40,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # IA 2024 Single, $35,000 W2, no dependents (higher bracket)
    # Fed AGI: $35,000, Fed std ded: $14,600, Fed taxable: $20,400
    # Fed tax: $11,600 * 0.10 + $8,800 * 0.12 = $1,160 + $1,056 = $2,216
    # IA taxable: $20,400 (assuming no Iowa modifications)
    # IA tax: $6,210 * 0.044 + ($20,400 - $6,210) * 0.0482
    #       = $273.24 + $14,190 * 0.0482
    #       = $273.24 + $683.96 = $957.20
    TaxScenario(
        source="IA 2024 Tax Brackets (computed)",
        description="IA Single, $35,000 W2, no dependents",
        year=2024,
        state="IA",
        filing_status="Single",
        w2_income=35000.0,
        expected_federal_tax=2219.0,  # Tax Table row 20,400-20,450
        expected_state_tax=957.20,
        expected_federal_agi=35000.0,
        backend="graph",
    ),
    # IA 2025 Single, $50,000 W2, no dependents (flat tax)
    # Fed AGI: $50,000, Fed std ded 2025: $15,750, Fed taxable: $34,250
    # Fed tax: $11,925 * 0.10 + $22,325 * 0.12 = $1,192.50 + $2,679 = $3,871.50
    # IA taxable: $34,250 (assuming no Iowa modifications)
    # IA tax: $34,250 * 0.038 = $1,301.50
    TaxScenario(
        source="IA 2025 Tax Brackets (computed)",
        description="IA Single, $50,000 W2, no dependents (2025 flat tax)",
        year=2025,
        state="IA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1301.50,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # IA 2025 MFJ, $100,000 W2, no dependents (flat tax)
    # Fed AGI: $100,000, Fed std ded 2025: $31,500, Fed taxable: $68,500
    # Fed tax: $23,850 * 0.10 + $44,650 * 0.12 = $2,385 + $5,358 = $7,743
    # IA taxable: $68,500 (assuming no Iowa modifications)
    # IA tax: $68,500 * 0.038 = $2,603.00
    TaxScenario(
        source="IA 2025 Tax Brackets (computed)",
        description="IA MFJ, $100,000 W2, no dependents (2025 flat tax)",
        year=2025,
        state="IA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=2603.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== IDAHO SCENARIOS ==========
    # ID 2024: 5.695% on income above $4,673 (single) or $9,346 (MFJ/HoH/QW)
    # ID 2025: 5.3% on income above $4,811 (single) or $9,622 (MFJ/HoH/QW)
    # Standard deduction: Single $14,600 (2024), $15,000 (2025)
    #                    MFJ $29,200 (2024), $30,000 (2025)
    #                    HoH $21,900 (2024), $22,500 (2025)
    # ID taxable income = ID adjusted income - std ded - QBI deduction
    # ID tax = max(0, (ID taxable - threshold) * rate)
    #
    # ID 2024 Single, $50,000 W2, no dependents
    # Fed AGI: $50,000, Fed std ded: $14,600, Fed taxable: $35,400
    # Fed tax: $11,600 * 0.10 + $23,800 * 0.12 = $1,160 + $2,856 = $4,016
    # ID adjusted income: $50,000 (imports from federal)
    # ID std ded: $14,600, ID taxable: $50,000 - $14,600 = $35,400
    # ID tax: ($35,400 - $4,673) * 0.05695 = $30,727 * 0.05695 = $1,749.90
    TaxScenario(
        source="ID 2024 Tax Rate Schedule (computed)",
        description="ID Single, $50,000 W2, no dependents",
        year=2024,
        state="ID",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1749.90,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ID 2024 MFJ, $100,000 W2, no dependents
    # Fed AGI: $100,000, Fed std ded: $29,200, Fed taxable: $70,800
    # Fed tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    # ID adjusted income: $100,000
    # ID std ded: $29,200, ID taxable: $100,000 - $29,200 = $70,800
    # ID tax: ($70,800 - $9,346) * 0.05695 = $61,454 * 0.05695 = $3,499.81
    TaxScenario(
        source="ID 2024 Tax Rate Schedule (computed)",
        description="ID MFJ, $100,000 W2, no dependents",
        year=2024,
        state="ID",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3499.81,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ID 2024 HoH, $75,000 W2, no dependents
    # Fed AGI: $75,000, Fed std ded: $21,900, Fed taxable: $53,100
    # Fed tax: $16,550 * 0.10 + $36,550 * 0.12 = $1,655 + $4,386 = $6,041
    # ID adjusted income: $75,000
    # ID std ded: $21,900, ID taxable: $75,000 - $21,900 = $53,100
    # ID tax: ($53,100 - $9,346) * 0.05695 = $43,754 * 0.05695 = $2,491.80
    TaxScenario(
        source="ID 2024 Tax Rate Schedule (computed)",
        description="ID HoH, $75,000 W2, no dependents",
        year=2024,
        state="ID",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=2491.80,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ID 2025 Single, $60,000 W2, no dependents (reduced rate)
    # Fed AGI: $60,000, Fed std ded 2025: $15,750 (OBBBA), Fed taxable: $44,250
    # Fed tax: $11,925 * 0.10 + $32,325 * 0.12 = $1,192.50 + $3,879 = $5,071.50
    # ID adjusted income: $60,000
    # ID std ded: $15,750, ID taxable: $60,000 - $15,750 = $44,250
    # ID tax: ($44,250 - $4,811) * 0.053 = $39,439 * 0.053 = $2,090.27
    TaxScenario(
        source="ID 2025 Tax Rate Schedule (computed)",
        description="ID Single, $60,000 W2, no dependents (2025 reduced rate)",
        year=2025,
        state="ID",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5075.0,  # Tax Table row 44,250-44,300
        expected_state_tax=2090.27,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # ID 2025 MFJ, $120,000 W2, no dependents (reduced rate)
    # Fed AGI: $120,000, Fed std ded 2025: $31,500 (OBBBA), Fed taxable: $88,500
    # Fed tax: $23,850 * 0.10 + $64,650 * 0.12 = $2,385 + $7,758 = $10,143
    # ID adjusted income: $120,000
    # ID std ded: $31,500, ID taxable: $120,000 - $31,500 = $88,500
    # ID tax: ($88,500 - $9,622) * 0.053 = $78,878 * 0.053 = $4,180.53
    TaxScenario(
        source="ID 2025 Tax Rate Schedule (computed)",
        description="ID MFJ, $120,000 W2, no dependents (2025 reduced rate)",
        year=2025,
        state="ID",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10146.0,  # Tax Table row 88,500-88,550
        expected_state_tax=4180.53,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== MISSISSIPPI SCENARIOS ==========
    # MS Form 80-105, both years: 0% on the first $10,000 of taxable income, then
    # 4.7% (2024) / 4.4% (2025). Filing-status exemption (Line 11): Single $6,000,
    # MFJ $12,000, HoH $8,000. Standard deduction: Single $2,300, MFJ $4,600,
    # HoH $3,400. MS taxable income = MS AGI - exemptions - deduction.
    #
    # Precision: Line 17 is rounded half-up to whole dollars on the return
    # (I24 p.21 / I25 p.22). These fixtures carry the deriver's unrounded product,
    # matching the silver convention of formula-exact cents (see tenforty-xew).
    #
    # HoH rows are the literal "Head of Family box, zero dependents" reading
    # (Line 11 = $8,000, Line 10 = $0). A legal HoH return needs a dependent
    # ($8,000 + $1,500 = $9,500; I24 p.5-6 / I25 p.6, Line 4), but the graph
    # backend rejects nonzero num_dependents and MS maps no dependent-exemption
    # input (tenforty-avr.1), so that variant is not expressible here.
    #
    # MFJ: the Schedule of Tax Computation applies the $10,000 zero band per
    # spouse column; the API carries no spouse split, and the graph applies one
    # band to joint taxable income. Only MFJ rows where no split can matter
    # (taxable income under $10,000) come from the deriver.
    #
    # MS 2024 Single, $20,000 W2, no dependents
    # Source: MS DOR Form 80-100-24-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf
    #   p.5 (exemptions, std deduction), p.21 (rate), p.26 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 20,000 - 6,000 - 2,300 = 11,700; tax: 1,700 x 0.047 = 79.90
    TaxScenario(
        source="MS 2024 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS Single, $20,000 W2, no dependents (2024)",
        year=2024,
        state="MS",
        filing_status="Single",
        w2_income=20000.0,
        expected_state_tax=79.90,
        backend="graph",
    ),
    # MS 2024 Single, $50,000 W2, no dependents
    # Source: MS DOR Form 80-100-24-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf
    #   p.5 (exemptions, std deduction), p.21 (rate), p.26 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 50,000 - 6,000 - 2,300 = 41,700; tax: 31,700 x 0.047 = 1,489.90
    TaxScenario(
        source="MS 2024 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS Single, $50,000 W2, no dependents (2024)",
        year=2024,
        state="MS",
        filing_status="Single",
        w2_income=50000.0,
        expected_state_tax=1489.90,
        backend="graph",
    ),
    # MS 2024 Single, $150,000 W2, no dependents
    # Source: MS DOR Form 80-100-24-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf
    #   p.5 (exemptions, std deduction), p.21 (rate), p.26 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 150,000 - 6,000 - 2,300 = 141,700; tax: 131,700 x 0.047 = 6,189.90
    TaxScenario(
        source="MS 2024 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS Single, $150,000 W2, no dependents (2024)",
        year=2024,
        state="MS",
        filing_status="Single",
        w2_income=150000.0,
        expected_state_tax=6189.90,
        backend="graph",
    ),
    # MS 2024 MFJ, $20,000 W2, no dependents
    # Source: MS DOR Form 80-100-24-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf
    #   p.5 (exemptions, std deduction), p.21 (rate), p.26 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 20,000 - 12,000 - 4,600 = 3,400; tax: 0 (under the $10,000 band) = 0.00
    TaxScenario(
        source="MS 2024 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS MFJ, $20,000 W2, no dependents (2024)",
        year=2024,
        state="MS",
        filing_status="Married/Joint",
        w2_income=20000.0,
        expected_state_tax=0.00,
        backend="graph",
    ),
    # MS 2024 HoH, $20,000 W2, no dependents
    # Source: MS DOR Form 80-100-24-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf
    #   p.5 (exemptions, std deduction), p.21 (rate), p.26 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 20,000 - 8,000 - 3,400 = 8,600; tax: 0 (under the $10,000 band) = 0.00
    TaxScenario(
        source="MS 2024 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS HoH, $20,000 W2, no dependents (2024)",
        year=2024,
        state="MS",
        filing_status="Head_of_House",
        w2_income=20000.0,
        expected_state_tax=0.00,
        backend="graph",
    ),
    # MS 2024 HoH, $50,000 W2, no dependents
    # Source: MS DOR Form 80-100-24-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf
    #   p.5 (exemptions, std deduction), p.21 (rate), p.26 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 50,000 - 8,000 - 3,400 = 38,600; tax: 28,600 x 0.047 = 1,344.20
    TaxScenario(
        source="MS 2024 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS HoH, $50,000 W2, no dependents (2024)",
        year=2024,
        state="MS",
        filing_status="Head_of_House",
        w2_income=50000.0,
        expected_state_tax=1344.20,
        backend="graph",
    ),
    # MS 2024 HoH, $150,000 W2, no dependents
    # Source: MS DOR Form 80-100-24-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/Forms/Individual/80100241.pdf
    #   p.5 (exemptions, std deduction), p.21 (rate), p.26 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 150,000 - 8,000 - 3,400 = 138,600; tax: 128,600 x 0.047 = 6,044.20
    TaxScenario(
        source="MS 2024 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS HoH, $150,000 W2, no dependents (2024)",
        year=2024,
        state="MS",
        filing_status="Head_of_House",
        w2_income=150000.0,
        expected_state_tax=6044.20,
        backend="graph",
    ),
    # MS 2025 Single, $20,000 W2, no dependents
    # Source: MS DOR Form 80-100-25-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
    #   p.5 (exemptions, std deduction), p.22 (rate), p.27 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 20,000 - 6,000 - 2,300 = 11,700; tax: 1,700 x 0.044 = 74.80
    TaxScenario(
        source="MS 2025 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS Single, $20,000 W2, no dependents (2025)",
        year=2025,
        state="MS",
        filing_status="Single",
        w2_income=20000.0,
        expected_state_tax=74.80,
        backend="graph",
    ),
    # MS 2025 Single, $50,000 W2, no dependents
    # Source: MS DOR Form 80-100-25-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
    #   p.5 (exemptions, std deduction), p.22 (rate), p.27 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 50,000 - 6,000 - 2,300 = 41,700; tax: 31,700 x 0.044 = 1,394.80
    TaxScenario(
        source="MS 2025 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS Single, $50,000 W2, no dependents (2025)",
        year=2025,
        state="MS",
        filing_status="Single",
        w2_income=50000.0,
        expected_state_tax=1394.80,
        backend="graph",
    ),
    # MS 2025 Single, $150,000 W2, no dependents
    # Source: MS DOR Form 80-100-25-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
    #   p.5 (exemptions, std deduction), p.22 (rate), p.27 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 150,000 - 6,000 - 2,300 = 141,700; tax: 131,700 x 0.044 = 5,794.80
    TaxScenario(
        source="MS 2025 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS Single, $150,000 W2, no dependents (2025)",
        year=2025,
        state="MS",
        filing_status="Single",
        w2_income=150000.0,
        expected_state_tax=5794.80,
        backend="graph",
    ),
    # MS 2025 MFJ, $20,000 W2, no dependents
    # Source: MS DOR Form 80-100-25-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
    #   p.5 (exemptions, std deduction), p.22 (rate), p.27 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 20,000 - 12,000 - 4,600 = 3,400; tax: 0 (under the $10,000 band) = 0.00
    TaxScenario(
        source="MS 2025 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS MFJ, $20,000 W2, no dependents (2025)",
        year=2025,
        state="MS",
        filing_status="Married/Joint",
        w2_income=20000.0,
        expected_state_tax=0.00,
        backend="graph",
    ),
    # MS 2025 HoH, $20,000 W2, no dependents
    # Source: MS DOR Form 80-100-25-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
    #   p.5 (exemptions, std deduction), p.22 (rate), p.27 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 20,000 - 8,000 - 3,400 = 8,600; tax: 0 (under the $10,000 band) = 0.00
    TaxScenario(
        source="MS 2025 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS HoH, $20,000 W2, no dependents (2025)",
        year=2025,
        state="MS",
        filing_status="Head_of_House",
        w2_income=20000.0,
        expected_state_tax=0.00,
        backend="graph",
    ),
    # MS 2025 HoH, $50,000 W2, no dependents
    # Source: MS DOR Form 80-100-25-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
    #   p.5 (exemptions, std deduction), p.22 (rate), p.27 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 50,000 - 8,000 - 3,400 = 38,600; tax: 28,600 x 0.044 = 1,258.40
    TaxScenario(
        source="MS 2025 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS HoH, $50,000 W2, no dependents (2025)",
        year=2025,
        state="MS",
        filing_status="Head_of_House",
        w2_income=50000.0,
        expected_state_tax=1258.40,
        backend="graph",
    ),
    # MS 2025 HoH, $150,000 W2, no dependents
    # Source: MS DOR Form 80-100-25-1-1-000 instructions,
    #   https://www.dor.ms.gov/sites/default/files/tax-forms/individual/80100251%202.pdf
    #   p.5 (exemptions, std deduction), p.22 (rate), p.27 (schedule); retrieved 2026-09-30.
    # Kind: derived-arithmetic; derived blind, session 1b251ce6.
    # TI: 150,000 - 8,000 - 3,400 = 138,600; tax: 128,600 x 0.044 = 5,658.40
    TaxScenario(
        source="MS 2025 Form 80-100 instructions (derived blind)",
        state_evidence=MS_EVIDENCE,
        description="MS HoH, $150,000 W2, no dependents (2025)",
        year=2025,
        state="MS",
        filing_status="Head_of_House",
        w2_income=150000.0,
        expected_state_tax=5658.40,
        backend="graph",
    ),
    # Scenarios below are formula-derived in-repo (not from the blind deriver).
    # The MFJ ones assume all wages belong to one spouse (one $10,000 zero band).
    # MS 2024 MFJ, $100,000 W2, no dependents
    # Fed AGI: $100,000, Fed std ded: $29,200, Fed taxable: $70,800
    # Fed tax: $23,200 * 0.10 + $47,600 * 0.12 = $2,320 + $5,712 = $8,032
    # MS AGI: $100,000
    # MS exemption: $12,000, MS std ded: $4,600
    # MS taxable: $100,000 - $12,000 - $4,600 = $83,400
    # MS tax: ($83,400 - $10,000) * 0.047 = $73,400 * 0.047 = $3,449.80
    TaxScenario(
        source="MS 2024 Tax Rate Schedule (computed)",
        description="MS MFJ, $100,000 W2, no dependents",
        year=2024,
        state="MS",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3449.80,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MS 2025 Single, $60,000 W2, no dependents (reduced rate)
    # Fed AGI: $60,000, Fed std ded 2025: $15,000, Fed taxable: $45,000
    # Fed tax: $11,925 * 0.10 + $33,075 * 0.12 = $1,192.50 + $3,969 = $5,161.50
    # MS AGI: $60,000
    # MS exemption: $6,000, MS std ded: $2,300
    # MS taxable: $60,000 - $6,000 - $2,300 = $51,700
    # MS tax: ($51,700 - $10,000) * 0.044 = $41,700 * 0.044 = $1,834.80
    TaxScenario(
        source="MS 2025 Tax Rate Schedule (computed)",
        description="MS Single, $60,000 W2, no dependents (2025 reduced rate)",
        year=2025,
        state="MS",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5075.0,  # Tax Table row 44,250-44,300
        expected_state_tax=1834.80,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # MS 2025 MFJ, $120,000 W2, no dependents (reduced rate)
    # Fed AGI: $120,000, Fed std ded 2025: $30,000, Fed taxable: $90,000
    # Fed tax: $23,850 * 0.10 + $66,150 * 0.12 = $2,385 + $7,938 = $10,323
    # MS AGI: $120,000
    # MS exemption: $12,000, MS std ded: $4,600
    # MS taxable: $120,000 - $12,000 - $4,600 = $103,400
    # MS tax: ($103,400 - $10,000) * 0.044 = $93,400 * 0.044 = $4,109.60
    TaxScenario(
        source="MS 2025 Tax Rate Schedule (computed)",
        description="MS MFJ, $120,000 W2, no dependents (2025 reduced rate)",
        year=2025,
        state="MS",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10146.0,  # Tax Table row 88,500-88,550
        expected_state_tax=4109.60,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== MONTANA SCENARIOS ==========
    # MT 2024: Two-bracket system for both ordinary income and capital gains
    # Ordinary income: 4.7% up to threshold, 5.9% above
    # Capital gains: 3.0% up to threshold, 4.1% above
    # Thresholds 2024: Single/MFS $20,500, MFJ/QW $41,000, HoH $30,750
    #
    # MT 2025: Same rates, inflation-adjusted thresholds
    # Thresholds 2025: Single/MFS $21,100, MFJ/QW $42,200, HoH $31,700
    #
    # MT imports federal taxable income (US 1040 L15), applies Schedule I adjustments,
    # then splits into ordinary income and capital gains, taxed separately.
    # Source: Montana Form 2 instructions, Montana Code Annotated 15-30-2103
    # MT Single, $30,000 W2 (2024) - entirely in lower bracket
    # Federal: AGI=$30k, Std Ded=$14,600, Taxable=$15,400, Tax=$1,616
    # MT: Imports fed taxable=$15,400, all ordinary income
    # MT tax: $15,400 * 0.047 = $723.80 (all in lower bracket)
    TaxScenario(
        source="MT 2024 Tax Brackets (computed)",
        description="MT Single, $30,000 W2",
        year=2024,
        state="MT",
        filing_status="Single",
        w2_income=30000.0,
        expected_federal_tax=1619.0,  # Tax Table row 15,400-15,450
        expected_state_tax=723.80,
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # MT MFJ, $60,000 W2 (2024) - stays in lower bracket
    # Federal: AGI=$60k, Std Ded=$29,200, Taxable=$30,800, Tax=$3,232
    # MT: Imports fed taxable=$30,800, all ordinary income
    # MT tax: $30,800 * 0.047 = $1,447.60 (all in lower bracket, under $41k threshold)
    TaxScenario(
        source="MT 2024 Tax Brackets (computed)",
        description="MT MFJ, $60,000 W2",
        year=2024,
        state="MT",
        filing_status="Married/Joint",
        w2_income=60000.0,
        expected_federal_tax=3235.0,  # Tax Table row 30,800-30,850
        expected_state_tax=1447.60,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # MT HoH, $50,000 W2 (2024) - stays in lower bracket
    # Federal: AGI=$50k, Std Ded=$21,900, Taxable=$28,100, Tax=$3,041
    # MT: Imports fed taxable=$28,100, all ordinary income
    # MT tax: $28,100 * 0.047 = $1,320.70 (all in lower bracket, under $30,750 threshold)
    TaxScenario(
        source="MT 2024 Tax Brackets (computed)",
        description="MT HoH, $50,000 W2",
        year=2024,
        state="MT",
        filing_status="Head_of_House",
        w2_income=50000.0,
        expected_federal_tax=3044.0,  # Tax Table row 28,100-28,150
        expected_state_tax=1320.70,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MT Single, $40,000 W2 + $5,000 LT capital gains (2024)
    # Federal: AGI=$45k, Std Ded=$14,600, Taxable=$30,400, Tax=$3,416 -> $2,816 (QDCGT)
    # MT: Imports fed taxable=$30,400, treats all as ordinary income
    # (Note: Capital gains split not yet implemented in graph backend)
    # MT tax: $20,500 * 0.047 + ($30,400 - $20,500) * 0.059 = $963.50 + $584.10 = $1,547.60
    TaxScenario(
        source="MT 2024 Tax Brackets (computed)",
        description="MT Single, $40,000 W2 + $5,000 LT cap gains",
        year=2024,
        state="MT",
        filing_status="Single",
        w2_income=40000.0,
        long_term_capital_gains=5000.0,
        expected_federal_tax=2819.0,  # QDCG worksheet, line 22 Tax Table row 25,400-25,450
        expected_state_tax=1547.60,
        expected_federal_agi=45000.0,
        backend="graph",
    ),
    # MT Single, $35,000 W2 (2025) - test 2025 with adjusted thresholds
    # Federal: AGI=$35k, Std Ded=$15,750, Taxable=$19,250, Tax=$2,071.50
    # MT: Imports fed taxable=$19,250, all ordinary income
    # MT tax: $19,250 * 0.047 = $904.75 (all in lower bracket, under $21,100 threshold)
    TaxScenario(
        source="MT 2025 Tax Brackets (computed)",
        description="MT Single, $35,000 W2 (2025)",
        year=2025,
        state="MT",
        filing_status="Single",
        w2_income=35000.0,
        expected_federal_tax=2075.0,  # Tax Table row 19,250-19,300
        expected_state_tax=904.75,
        expected_federal_agi=35000.0,
        backend="graph",
    ),
    # ========== RHODE ISLAND SCENARIOS ==========
    # RI 2024: Three-bracket system (3.75%, 4.75%, 5.99%)
    # Brackets 2024: $0-$77,450, $77,450-$176,050, $176,050+
    # Standard deduction 2024: Single $10,550, MFJ $21,150, HoH $15,850
    # Personal exemption 2024: $4,950 per person
    #
    # RI 2025: Same rates, inflation-adjusted thresholds
    # Brackets 2025: $0-$79,900, $79,900-$181,650, $181,650+
    # Standard deduction 2025: Single $10,900, MFJ $21,800, HoH $16,350
    # Personal exemption 2025: $5,100 per person
    #
    # RI imports federal AGI (US 1040 L11), applies RI Schedule M modifications,
    # then subtracts standard deduction and personal exemptions.
    # Source: RI Division of Taxation Advisory 2024-01 and 2024-26
    # RI Single, $40,000 W2 (2024) - entirely in first bracket
    # Federal: AGI=$40k, Std Ded=$14,600, Taxable=$25,400
    # RI: AGI=$40k, Std Ded=$10,550, Exemption=$0 (not provided), Taxable=$29,450
    # RI tax: $29,450 * 0.0375 = $1,104.375
    # Note: Personal exemptions not included in graph backend (requires num_exemptions input)
    TaxScenario(
        source="RI 2024 Tax Brackets (computed)",
        description="RI Single, $40,000 W2",
        year=2024,
        state="RI",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=1104.38,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # RI MFJ, $80,000 W2 (2024) - entirely in first bracket
    # Federal: AGI=$80k, Std Ded=$29,200, Taxable=$50,800
    # RI: AGI=$80k, Std Ded=$21,150, Exemption=$0 (not provided), Taxable=$58,850
    # RI tax: $58,850 * 0.0375 = $2,206.875
    TaxScenario(
        source="RI 2024 Tax Brackets (computed)",
        description="RI MFJ, $80,000 W2",
        year=2024,
        state="RI",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5635.0,  # Tax Table row 50,800-50,850
        expected_state_tax=2206.88,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # RI HoH, $55,000 W2 (2024) - entirely in first bracket
    # Federal: AGI=$55k, Std Ded=$21,900, Taxable=$33,100
    # RI: AGI=$55k, Std Ded=$15,850, Exemption=$0 (not provided), Taxable=$39,150
    # RI tax: $39,150 * 0.0375 = $1,468.125
    TaxScenario(
        source="RI 2024 Tax Brackets (computed)",
        description="RI HoH, $55,000 W2",
        year=2024,
        state="RI",
        filing_status="Head_of_House",
        w2_income=55000.0,
        expected_federal_tax=3644.0,  # Tax Table row 33,100-33,150
        expected_state_tax=1468.12,
        expected_federal_agi=55000.0,
        backend="graph",
    ),
    # RI Single, $100,000 W2 (2024) - crosses into second bracket
    # Federal: AGI=$100k, Std Ded=$14,600, Taxable=$85,400
    # RI: AGI=$100k, Std Ded=$10,550, Exemption=$0 (not provided), Taxable=$89,450
    # RI tax: $77,450 * 0.0375 + ($89,450 - $77,450) * 0.0475
    #       = $2,904.375 + $570.00 = $3,474.375
    TaxScenario(
        source="RI 2024 Tax Brackets (computed)",
        description="RI Single, $100,000 W2",
        year=2024,
        state="RI",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13847.0,  # Tax Table row 85,400-85,450
        expected_state_tax=3474.38,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # RI Single, $45,000 W2 (2025) - test 2025 with adjusted thresholds
    # Federal: AGI=$45k, Std Ded=$15,000, Taxable=$30,000
    # RI: AGI=$45k, Std Ded=$10,900, Exemption=$0 (not provided), Taxable=$34,100
    # RI tax: $34,100 * 0.0375 = $1,278.75 (all in first bracket)
    TaxScenario(
        source="RI 2025 Tax Brackets (computed)",
        description="RI Single, $45,000 W2 (2025)",
        year=2025,
        state="RI",
        filing_status="Single",
        w2_income=45000.0,
        expected_federal_tax=3275.0,  # Tax Table row 29,250-29,300
        expected_state_tax=1278.75,
        expected_federal_agi=45000.0,
        backend="graph",
    ),
    # ========== NORTH DAKOTA SCENARIOS ==========
    # ND 2024: 3-bracket system (0%, 1.95%, 2.50%)
    # Brackets for Single: $0-$47,150 (0%), $47,150-$238,200 (1.95%), $238,200+ (2.50%)
    # Brackets for MFJ: $0-$78,775 (0%), $78,775-$289,975 (1.95%), $289,975+ (2.50%)
    # Std deduction 2024: Single $14,600, MFJ $29,200
    # Std deduction 2025: Single $15,000, MFJ $30,000
    # ND imports federal AGI (US 1040 L11)
    #
    # ND Single, $40,000 W2 (2024) - entirely in 0% bracket
    # Federal: AGI=$40k, Std Ded=$14,600, Taxable=$25,400
    # ND: AGI=$40k, Std Ded=$14,600, Taxable=$25,400
    # ND tax: $25,400 < $47,150, so 0% bracket = $0
    TaxScenario(
        source="ND 2024 Tax Rate Schedules (computed)",
        description="ND Single, $40,000 W2",
        year=2024,
        state="ND",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=0.0,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # ND MFJ, $80,000 W2 (2024) - entirely in 0% bracket
    # Federal: AGI=$80k, Std Ded=$29,200, Taxable=$50,800
    # ND: AGI=$80k, Std Ded=$29,200, Taxable=$50,800
    # ND tax: $50,800 < $78,775, so 0% bracket = $0
    TaxScenario(
        source="ND 2024 Tax Rate Schedules (computed)",
        description="ND MFJ, $80,000 W2",
        year=2024,
        state="ND",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5635.0,  # Tax Table row 50,800-50,850
        expected_state_tax=0.0,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # ND Single, $70,000 W2 (2024) - crosses into 1.95% bracket
    # Federal: AGI=$70k, Std Ded=$14,600, Taxable=$55,400
    # ND: AGI=$70k, Std Ded=$14,600, Taxable=$55,400
    # ND tax: $47,150 * 0% + ($55,400 - $47,150) * 1.95% = $8,250 * 0.0195 = $160.875
    TaxScenario(
        source="ND 2024 Tax Rate Schedules (computed)",
        description="ND Single, $70,000 W2",
        year=2024,
        state="ND",
        filing_status="Single",
        w2_income=70000.0,
        expected_federal_tax=7247.0,  # Tax Table row 55,400-55,450
        expected_state_tax=160.88,
        expected_federal_agi=70000.0,
        backend="graph",
    ),
    # ND MFJ, $120,000 W2 (2024) - crosses into 1.95% bracket
    # Federal: AGI=$120k, Std Ded=$29,200, Taxable=$90,800
    # ND: AGI=$120k, Std Ded=$29,200, Taxable=$90,800
    # ND tax: $78,775 * 0% + ($90,800 - $78,775) * 1.95% = $12,025 * 0.0195 = $234.4875
    TaxScenario(
        source="ND 2024 Tax Rate Schedules (computed)",
        description="ND MFJ, $120,000 W2",
        year=2024,
        state="ND",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10435.0,  # Tax Table row 90,800-90,850
        expected_state_tax=234.49,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ND Single, $75,000 W2 (2025) - test 2025 thresholds
    # Federal: AGI=$75k, Std Ded=$15,750 (2025 OBBBA), Taxable=$59,250
    # ND: AGI=$75k, Std Ded=$15,750, Taxable=$59,250
    # ND tax: $48,475 * 0% + ($59,250 - $48,475) * 1.95% = $10,775 * 0.0195 = $210.1125
    TaxScenario(
        source="ND 2025 Tax Rate Schedules (computed)",
        description="ND Single, $75,000 W2 (2025)",
        year=2025,
        state="ND",
        filing_status="Single",
        w2_income=75000.0,
        expected_federal_tax=7955.0,  # Tax Table row 59,250-59,300
        expected_state_tax=210.11,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== MICHIGAN SCENARIOS ==========
    # MI 2024: Flat 4.25% rate, Personal exemption $5,600
    # MI 2025: Flat 4.25% rate, Personal exemption $5,800
    # No standard deduction for most taxpayers (only age-based for 67+)
    # MI tax = (Federal AGI - exemptions) * 0.0425
    # These use graph backend with exemptions set to 0 (not auto-computed).
    #
    # MI 2024 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000, MI AGI: $50,000, MI taxable: $50,000
    # MI tax: $50,000 x 0.0425 = $2,125.00
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="MI 2024 Tax Brackets (computed)",
        description="MI Single, $50,000 W2 only",
        year=2024,
        state="MI",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=2125.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MI 2024 Single, $100,000 W2 only, no exemptions
    # Federal AGI: $100,000, MI AGI: $100,000, MI taxable: $100,000
    # MI tax: $100,000 x 0.0425 = $4,250.00
    # Federal taxable: $85,400, Federal tax: $13,841
    TaxScenario(
        source="MI 2024 Tax Brackets (computed)",
        description="MI Single, $100,000 W2 only",
        year=2024,
        state="MI",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13847.0,  # Tax Table row 85,400-85,450
        expected_state_tax=4250.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MI 2024 Single, $75,000 W2 + $5,000 interest, no exemptions
    # Federal AGI: $80,000, MI AGI: $80,000, MI taxable: $80,000
    # MI tax: $80,000 x 0.0425 = $3,400.00
    # Federal taxable: $65,400, Federal tax: $9,441
    TaxScenario(
        source="MI 2024 Tax Brackets (computed)",
        description="MI Single, $75,000 W2 + $5,000 interest",
        year=2024,
        state="MI",
        filing_status="Single",
        w2_income=75000.0,
        taxable_interest=5000.0,
        expected_federal_tax=9447.0,  # Tax Table row 65,400-65,450
        expected_state_tax=3400.0,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # MI 2024 MFJ, $120,000 W2 only, no exemptions
    # Federal AGI: $120,000, MI AGI: $120,000, MI taxable: $120,000
    # MI tax: $120,000 x 0.0425 = $5,100.00
    # Federal taxable: $90,800, Federal tax: $10,432
    TaxScenario(
        source="MI 2024 Tax Brackets (computed)",
        description="MI MFJ, $120,000 W2 only",
        year=2024,
        state="MI",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10435.0,  # Tax Table row 90,800-90,850
        expected_state_tax=5100.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # MI 2025 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000, MI AGI: $50,000, MI taxable: $50,000
    # MI tax: $50,000 x 0.0425 = $2,125.00
    # Federal taxable: $35,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $1,192.50 (10% on $11,925) + $2,769.00 (12% on $23,075) = $3,961.50
    TaxScenario(
        source="MI 2025 Tax Brackets (computed)",
        description="MI Single, $50,000 W2 only",
        year=2025,
        state="MI",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=2125.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MI 2025 MFJ, $120,000 W2 only, no exemptions
    # Federal AGI: $120,000, MI AGI: $120,000, MI taxable: $120,000
    # MI tax: $120,000 x 0.0425 = $5,100.00
    # Federal taxable: $90,000 (AGI - $30,000 std ded)
    # Federal tax (2025 MFJ): $23,850 x 0.10 + $66,150 x 0.12 = $2,385 + $7,938 = $10,323
    TaxScenario(
        source="MI 2025 Tax Brackets (computed)",
        description="MI MFJ, $120,000 W2 only",
        year=2025,
        state="MI",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10146.0,  # Tax Table row 88,500-88,550
        expected_state_tax=5100.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # MI 2025 Single, $100,000 W2 only, no exemptions
    # Federal AGI: $100,000, MI AGI: $100,000, MI taxable: $100,000
    # MI tax: $100,000 x 0.0425 = $4,250.00
    # Federal taxable: $85,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 x 0.10 + $36,550 x 0.12 + $36,525 x 0.22
    #   = $1,192.50 + $4,386 + $8,035.50 = $13,614
    TaxScenario(
        source="MI 2025 Tax Brackets (computed)",
        description="MI Single, $100,000 W2 only",
        year=2025,
        state="MI",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13455.0,  # Tax Table row 84,250-84,300
        expected_state_tax=4250.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== ILLINOIS SCENARIOS ==========
    # IL 2024: Flat 4.95% rate, Personal exemption $2,775
    # IL 2025: Flat 4.95% rate (unchanged), Personal exemption $2,850
    # No standard deduction (uses personal exemptions instead)
    # Exemption phase-out: $250k (Single/MFS/HoH/QW), $500k (MFJ)
    # IL tax = (Federal AGI - exemptions) * 0.0495
    # These use graph backend with exemptions set to 0 (not auto-computed).
    #
    # IL 2024 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000
    # IL Base Income: $50,000 (no additions/subtractions)
    # IL Net Income: $50,000 (exemption = $0)
    # IL Tax: $50,000 * 0.0495 = $2,475.00
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="IL 2024 Tax Rate (computed)",
        description="IL Single, $50,000 W2 only",
        year=2024,
        state="IL",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=2475.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # IL 2024 Single, $100,000 W2 only, no exemptions
    # Federal AGI: $100,000
    # IL Base Income: $100,000
    # IL Net Income: $100,000 (exemption = $0)
    # IL Tax: $100,000 * 0.0495 = $4,950.00
    # Federal taxable: $85,400, Federal tax: $13,841
    TaxScenario(
        source="IL 2024 Tax Rate (computed)",
        description="IL Single, $100,000 W2 only",
        year=2024,
        state="IL",
        filing_status="Single",
        w2_income=100000.0,
        expected_federal_tax=13847.0,  # Tax Table row 85,400-85,450
        expected_state_tax=4950.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # IL 2024 MFJ, $120,000 W2 only, no exemptions
    # Federal AGI: $120,000
    # IL Base Income: $120,000
    # IL Net Income: $120,000 (exemption = $0)
    # IL Tax: $120,000 * 0.0495 = $5,940.00
    # Federal taxable: $90,800, Federal tax: $10,432
    TaxScenario(
        source="IL 2024 Tax Rate (computed)",
        description="IL MFJ, $120,000 W2 only",
        year=2024,
        state="IL",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10435.0,  # Tax Table row 90,800-90,850
        expected_state_tax=5940.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # IL 2024 Head_of_House, $75,000 W2 only, no exemptions
    # Federal AGI: $75,000
    # IL Base Income: $75,000
    # IL Net Income: $75,000 (exemption = $0)
    # IL Tax: $75,000 * 0.0495 = $3,712.50
    # Federal taxable: $53,100 ($75K - $21.9K std ded)
    # Federal tax (HoH 2024): $16,550 * 0.10 + $36,550 * 0.12 = $1,655 + $4,386 = $6,041
    TaxScenario(
        source="IL 2024 Tax Rate (computed)",
        description="IL HoH, $75,000 W2 only",
        year=2024,
        state="IL",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=3712.5,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # IL 2025 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000
    # IL Base Income: $50,000
    # IL Net Income: $50,000 (exemption = $0)
    # IL Tax: $50,000 * 0.0495 = $2,475.00
    # Federal taxable: $35,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="IL 2025 Tax Rate (computed)",
        description="IL Single, $50,000 W2 only (2025)",
        year=2025,
        state="IL",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=2475.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # IL 2025 MFJ, $120,000 W2 only, no exemptions
    # Federal AGI: $120,000
    # IL Base Income: $120,000
    # IL Net Income: $120,000 (exemption = $0)
    # IL Tax: $120,000 * 0.0495 = $5,940.00
    # Federal taxable: $90,000 (AGI - $30,000 std ded)
    # Federal tax (2025 MFJ): $23,850 * 0.10 + $66,150 * 0.12 = $2,385 + $7,938 = $10,323
    TaxScenario(
        source="IL 2025 Tax Rate (computed)",
        description="IL MFJ, $120,000 W2 only (2025)",
        year=2025,
        state="IL",
        filing_status="Married/Joint",
        w2_income=120000.0,
        expected_federal_tax=10146.0,  # Tax Table row 88,500-88,550
        expected_state_tax=5940.0,
        expected_federal_agi=120000.0,
        backend="graph",
    ),
    # ========== INDIANA BLIND-DERIVED SCENARIOS ==========
    # Record: docs/validation/state-fixtures/IN-2024-2025.md (cases 1-14, 4F, 11F).
    # IT-40 line 6 = Schedule 3 line 7: $2,000 MFJ / $1,000 otherwise, plus
    # $1,000 per dependent and $1,500 per qualifying child ($3,000 in the
    # child's first year). The API has no Indiana dependent or child concept
    # (tenforty-avr.1), so children enter as the dependent_exemptions TOTAL
    # (Schedule 3 line 7 dollars); childless cases rely on the derived base.
    # No federal expectation: the record derives only the Indiana return.
    # Line 8 rounds half-up to whole dollars (booklet "Rounding Required").
    # Blind case 1: 100,000 - 1,000 = 99,000; 99,000 x .0305 = 3,019.50 -> 3,020.
    TaxScenario(
        source="IN 2024 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Single, $100k W2 (2024, case 1)",
        year=2024,
        state="IN",
        filing_status="Single",
        w2_income=100000.0,
        expected_state_tax=3020.0,
        known_defects=(IN_LINE8_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 2: 50,000 - 1,000 = 49,000; 49,000 x .0305 = 1,494.50 -> 1,495.
    TaxScenario(
        source="IN 2024 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Single, $50k W2 (2024, case 2)",
        year=2024,
        state="IN",
        filing_status="Single",
        w2_income=50000.0,
        expected_state_tax=1495.0,
        known_defects=(IN_LINE8_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 3: 90,000 - 7,000 = 83,000; 83,000 x .0305 = 2,531.50 -> 2,532.
    TaxScenario(
        source="IN 2024 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Married/Joint, $90k W2, two children (2024, case 3)",
        year=2024,
        state="IN",
        filing_status="Married/Joint",
        w2_income=90000.0,
        dependent_exemptions=7000.0,
        expected_state_tax=2532.0,
        known_defects=(IN_LINE8_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 4: 45,000 - 3,500 = 41,500; 41,500 x .0305 = 1,265.75 -> 1,266.
    TaxScenario(
        source="IN 2024 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Head_of_House, $45k W2, one child (2024, case 4)",
        year=2024,
        state="IN",
        filing_status="Head_of_House",
        w2_income=45000.0,
        dependent_exemptions=3500.0,
        expected_state_tax=1266.0,
        known_defects=(IN_LINE8_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 5: 40,000 - 1,000 = 39,000; 39,000 x .0305 = 1,189.50 -> 1,190.
    TaxScenario(
        source="IN 2024 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Married/Sep, $40k W2 (2024, case 5)",
        year=2024,
        state="IN",
        filing_status="Married/Sep",
        w2_income=40000.0,
        expected_state_tax=1190.0,
        known_defects=(IN_LINE8_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 6: 70,000 - 3,500 = 66,500; 66,500 x .0305 = 2,028.25 -> 2,028.
    TaxScenario(
        source="IN 2024 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Widow(er), $70k W2, one child (2024, case 6)",
        year=2024,
        state="IN",
        filing_status="Widow(er)",
        w2_income=70000.0,
        dependent_exemptions=3500.0,
        expected_state_tax=2028.0,
        known_defects=(IN_LINE8_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 7: 20,000 - 1,000 = 19,000; 19,000 x .0305 = 579.50 -> 580.
    TaxScenario(
        source="IN 2024 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Single, $20k W2 (2024, case 7)",
        year=2024,
        state="IN",
        filing_status="Single",
        w2_income=20000.0,
        expected_state_tax=580.0,
        known_defects=(IN_LINE8_ROUNDING_GAP,),
        backend="graph",
    ),
    # Blind case 4F: 45,000 - 5,000 = 40,000; 40,000 x .0305 = 1,220.00 -> 1,220.
    TaxScenario(
        source="IN 2024 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Head_of_House, $45k W2, one first-year child (2024, case 4F)",
        year=2024,
        state="IN",
        filing_status="Head_of_House",
        w2_income=45000.0,
        dependent_exemptions=5000.0,
        expected_state_tax=1220.0,
        backend="graph",
    ),
    # Blind case 8: 100,000 - 1,000 = 99,000; 99,000 x .03 = 2,970.00 -> 2,970.
    TaxScenario(
        source="IN 2025 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Single, $100k W2 (2025, case 8)",
        year=2025,
        state="IN",
        filing_status="Single",
        w2_income=100000.0,
        expected_state_tax=2970.0,
        backend="graph",
    ),
    # Blind case 9: 50,000 - 1,000 = 49,000; 49,000 x .03 = 1,470.00 -> 1,470.
    TaxScenario(
        source="IN 2025 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Single, $50k W2 (2025, case 9)",
        year=2025,
        state="IN",
        filing_status="Single",
        w2_income=50000.0,
        expected_state_tax=1470.0,
        backend="graph",
    ),
    # Blind case 10: 90,000 - 7,000 = 83,000; 83,000 x .03 = 2,490.00 -> 2,490.
    TaxScenario(
        source="IN 2025 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Married/Joint, $90k W2, two children (2025, case 10)",
        year=2025,
        state="IN",
        filing_status="Married/Joint",
        w2_income=90000.0,
        dependent_exemptions=7000.0,
        expected_state_tax=2490.0,
        backend="graph",
    ),
    # Blind case 11: 45,000 - 3,500 = 41,500; 41,500 x .03 = 1,245.00 -> 1,245.
    TaxScenario(
        source="IN 2025 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Head_of_House, $45k W2, one child (2025, case 11)",
        year=2025,
        state="IN",
        filing_status="Head_of_House",
        w2_income=45000.0,
        dependent_exemptions=3500.0,
        expected_state_tax=1245.0,
        backend="graph",
    ),
    # Blind case 12: 40,000 - 1,000 = 39,000; 39,000 x .03 = 1,170.00 -> 1,170.
    TaxScenario(
        source="IN 2025 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Married/Sep, $40k W2 (2025, case 12)",
        year=2025,
        state="IN",
        filing_status="Married/Sep",
        w2_income=40000.0,
        expected_state_tax=1170.0,
        backend="graph",
    ),
    # Blind case 13: 70,000 - 3,500 = 66,500; 66,500 x .03 = 1,995.00 -> 1,995.
    TaxScenario(
        source="IN 2025 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Widow(er), $70k W2, one child (2025, case 13)",
        year=2025,
        state="IN",
        filing_status="Widow(er)",
        w2_income=70000.0,
        dependent_exemptions=3500.0,
        expected_state_tax=1995.0,
        backend="graph",
    ),
    # Blind case 14: 20,000 - 1,000 = 19,000; 19,000 x .03 = 570.00 -> 570.
    TaxScenario(
        source="IN 2025 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Single, $20k W2 (2025, case 14)",
        year=2025,
        state="IN",
        filing_status="Single",
        w2_income=20000.0,
        expected_state_tax=570.0,
        backend="graph",
    ),
    # Blind case 11F: 45,000 - 5,000 = 40,000; 40,000 x .03 = 1,200.00 -> 1,200.
    TaxScenario(
        source="IN 2025 Form IT-40 / Schedule 3 (blind derivation)",
        state_evidence=IN_EVIDENCE,
        description="IN Head_of_House, $45k W2, one first-year child (2025, case 11F)",
        year=2025,
        state="IN",
        filing_status="Head_of_House",
        w2_income=45000.0,
        dependent_exemptions=5000.0,
        expected_state_tax=1200.0,
        backend="graph",
    ),
    # Legacy IN rows, kept for their federal tax/AGI checks only (unverified
    # legacy). Their state expectations omitted the mandatory Schedule 3
    # line 1 exemption and were retired in r91.1; see IN-2024-2025.md.
    # IN 2024 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="IN 2024 Tax Rate (computed)",
        description="IN Single, $50,000 W2 only",
        year=2024,
        state="IN",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # IN 2024 MFJ, $100,000 W2 only, no exemptions
    # Federal AGI: $100,000
    # Federal taxable: $70,800, Federal tax: $8,032
    TaxScenario(
        source="IN 2024 Tax Rate (computed)",
        description="IN MFJ, $100,000 W2 only",
        year=2024,
        state="IN",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # IN 2024 Head_of_House, $75,000 W2 only, no exemptions
    # Federal AGI: $75,000
    # Federal taxable: $53,100, Federal tax: $6,041
    TaxScenario(
        source="IN 2024 Tax Rate (computed)",
        description="IN HoH, $75,000 W2 only",
        year=2024,
        state="IN",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # IN 2025 Single, $50,000 W2 only, no exemptions
    # Federal AGI: $50,000
    # Federal taxable: $35,000, Federal tax: $3,961.50
    TaxScenario(
        source="IN 2025 Tax Rate (computed)",
        description="IN Single, $50,000 W2 only (2025)",
        year=2025,
        state="IN",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== COLORADO SCENARIOS ==========
    # CO 2024: Flat 4.25% rate, starts from federal taxable income
    # CO 2025: Flat 4.4% rate, starts from federal taxable income
    # Colorado uses federal taxable income (US 1040 L15) as starting point,
    # not federal AGI.
    #
    # CO 2024 Single, $50,000 W2
    # Federal AGI: $50,000
    # Federal standard deduction (2024 Single): $14,600
    # Federal taxable income: $50,000 - $14,600 = $35,400
    # CO starting point (L1): $35,400
    # CO taxable income (L11, assuming no additions/subtractions): $35,400
    # CO tax (L12): $35,400 * 0.0425 = $1,504.50
    # Federal tax: $4,016
    TaxScenario(
        source="CO 2024 Tax Rate (computed)",
        description="CO Single, $50,000 W2 only",
        year=2024,
        state="CO",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1504.50,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # CO 2024 MFJ, $100,000 W2
    # Federal AGI: $100,000
    # Federal standard deduction (2024 MFJ): $29,200
    # Federal taxable income: $100,000 - $29,200 = $70,800
    # CO starting point (L1): $70,800
    # CO taxable income (L11): $70,800
    # CO tax (L12): $70,800 * 0.0425 = $3,009.00
    # Federal tax: $8,032
    TaxScenario(
        source="CO 2024 Tax Rate (computed)",
        description="CO MFJ, $100,000 W2 only",
        year=2024,
        state="CO",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3009.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # CO 2024 HoH, $75,000 W2
    # Federal AGI: $75,000
    # Federal standard deduction (2024 HoH): $21,900
    # Federal taxable income: $75,000 - $21,900 = $53,100
    # CO starting point (L1): $53,100
    # CO taxable income (L11): $53,100
    # CO tax (L12): $53,100 * 0.0425 = $2,256.75
    # Federal tax: $6,041
    TaxScenario(
        source="CO 2024 Tax Rate (computed)",
        description="CO HoH, $75,000 W2 only",
        year=2024,
        state="CO",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=2256.75,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # CO 2025 Single, $50,000 W2
    # Federal AGI: $50,000
    # Federal standard deduction (2025 Single): $15,750
    # Federal taxable income: $50,000 - $15,750 = $34,250
    # CO starting point (L1): $34,250
    # CO taxable income (L11): $34,250
    # CO tax (L12): $34,250 * 0.044 = $1,507.00
    # Federal tax: $3,871.50
    TaxScenario(
        source="CO 2025 Tax Rate (computed)",
        description="CO Single, $50,000 W2 only (2025)",
        year=2025,
        state="CO",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1507.0,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== KENTUCKY SCENARIOS ==========
    # KY uses a flat 4% tax rate on taxable income.
    # Standard deduction: $3,160 for 2024, $3,270 for 2025
    # Kentucky starts from federal AGI and applies state-specific additions/subtractions
    # to arrive at Kentucky AGI, then subtracts deductions to get taxable income.
    #
    # KY 2024 Single, $50,000 W2 only, no add-backs/subtractions
    # Federal AGI: $50,000
    # KY AGI (L9): $50,000 (no additions or subtractions)
    # KY standard deduction: $3,160
    # KY taxable income (L11): $50,000 - $3,160 = $46,840
    # KY tax (L12): $46,840 * 0.04 = $1,873.60
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="KY 2024 Tax Rate (computed)",
        description="KY Single, $50,000 W2 only",
        year=2024,
        state="KY",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1873.6,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # KY 2024 MFJ, $100,000 W2 only, no add-backs/subtractions
    # Federal AGI: $100,000
    # KY AGI (L9): $100,000
    # KY standard deduction: $3,160 (single deduction for joint filers)
    # KY taxable income (L11): $100,000 - $3,160 = $96,840
    # KY tax (L12): $96,840 * 0.04 = $3,873.60
    # Federal taxable: $70,800, Federal tax: $8,032
    TaxScenario(
        source="KY 2024 Tax Rate (computed)",
        description="KY MFJ, $100,000 W2 only",
        year=2024,
        state="KY",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3873.6,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # KY 2024 Head_of_House, $75,000 W2 only, no add-backs/subtractions
    # Federal AGI: $75,000
    # KY AGI (L9): $75,000
    # KY standard deduction: $3,160
    # KY taxable income (L11): $75,000 - $3,160 = $71,840
    # KY tax (L12): $71,840 * 0.04 = $2,873.60
    # Federal taxable: $53,100, Federal tax: $6,041
    TaxScenario(
        source="KY 2024 Tax Rate (computed)",
        description="KY HoH, $75,000 W2 only",
        year=2024,
        state="KY",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=2873.6,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # KY 2025 Single, $50,000 W2 only, no add-backs/subtractions
    # Federal AGI: $50,000
    # KY AGI (L9): $50,000
    # KY standard deduction (2025): $3,270
    # KY taxable income (L11): $50,000 - $3,270 = $46,730
    # KY tax (L12): $46,730 * 0.04 = $1,869.20
    # Federal taxable: $35,000, Federal tax: $3,961.50
    TaxScenario(
        source="KY 2025 Tax Rate (computed)",
        description="KY Single, $50,000 W2 only (2025)",
        year=2025,
        state="KY",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1869.2,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # ========== NORTH CAROLINA SCENARIOS ==========
    # NC 2024: Flat 4.5% rate
    # Standard Deduction: Single $12,750, MFJ $25,500
    #
    # NC Single, $50,000 W2
    # Fed AGI: $50,000
    # NC Taxable: $50,000 - $12,750 = $37,250
    # NC Tax: $37,250 * 0.045 = $1,676.25
    TaxScenario(
        source="NC 2024 Tax Brackets (computed)",
        description="NC Single, $50,000 income",
        year=2024,
        state="NC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1676.25,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # NC MFJ, $100,000 W2
    # Fed AGI: $100,000
    # NC Taxable: $100,000 - $25,500 = $74,500
    # NC Tax: $74,500 * 0.045 = $3,352.50
    TaxScenario(
        source="NC 2024 Tax Brackets (computed)",
        description="NC MFJ, $100,000 income",
        year=2024,
        state="NC",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3352.50,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # NC 2025: Flat 4.25% rate (reduced from 4.5%)
    # Standard Deduction: Single $12,750 (unchanged from 2024)
    #
    # NC Single, $50,000 W2
    # Fed AGI: $50,000
    # NC Taxable: $50,000 - $12,750 = $37,250
    # NC Tax: $37,250 * 0.0425 = $1,583.125
    # Federal: $15,000 std ded, taxable $35,000
    # Federal tax: $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="NC 2025 Tax Brackets (computed)",
        description="NC Single, $50,000 income (2025, 4.25% rate)",
        year=2025,
        state="NC",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1583.125,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    TaxScenario(
        source="OH 2025 Tax Brackets (computed)",
        description="OH MFJ, $75,000 income (2025, middle bracket)",
        year=2025,
        state="OH",
        filing_status="Married/Joint",
        w2_income=75000.0,
        expected_federal_tax=4746.0,  # Tax Table row 43,500-43,550
        expected_state_tax=1688.125,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # ========== OKLAHOMA SCENARIOS ==========
    # OK 2024 & 2025: 6-bracket progressive system (0.25%, 0.75%, 1.75%, 2.75%, 3.75%, 4.75%)
    # Single/MFS: 0-1,000: 0.25%, 1,001-2,500: 0.75%, 2,501-3,750: 1.75%,
    #             3,751-4,900: 2.75%, 4,901-7,200: 3.75%, 7,201+: 4.75%
    # MFJ/HoH/QW: 0-2,000: 0.25%, 2,001-5,000: 0.75%, 5,001-7,500: 1.75%,
    #             7,501-9,800: 2.75%, 9,801-12,200: 3.75%, 12,201+: 4.75%
    # Standard deduction: Single $6,350, MFJ $12,700, HoH $9,350
    # OK taxable income = OK AGI - standard deduction
    # Rates and deductions unchanged between 2024 and 2025
    #
    # OK 2024 Single, $40,000 W2, no dependents
    # Fed AGI: $40,000, Fed std ded: $14,600, Fed taxable: $25,400
    # Fed tax: $11,600 * 0.10 + $13,800 * 0.12 = $1,160 + $1,656 = $2,816
    # OK AGI: $40,000 (imports from federal, no additions/subtractions)
    # OK std ded: $6,350, OK taxable: $40,000 - $6,350 = $33,650
    # OK tax: $1,000 * 0.0025 + ($2,500 - $1,000) * 0.0075 + ($3,750 - $2,500) * 0.0175
    #       + ($4,900 - $3,750) * 0.0275 + ($7,200 - $4,900) * 0.0375
    #       + ($33,650 - $7,200) * 0.0475
    #       = $2.50 + $11.25 + $21.875 + $31.625 + $86.25 + $1,256.375
    #       = $1,409.875
    TaxScenario(
        source="OK 2024 Tax Brackets (computed)",
        description="OK Single, $40,000 W2, no dependents",
        year=2024,
        state="OK",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=1409.875,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # OK 2024 MFJ, $80,000 W2, no dependents
    # Fed AGI: $80,000, Fed std ded: $29,200, Fed taxable: $50,800
    # Fed tax: $23,200 * 0.10 + $27,600 * 0.12 = $2,320 + $3,312 = $5,632
    # OK AGI: $80,000
    # OK std ded: $12,700, OK taxable: $80,000 - $12,700 = $67,300
    # OK tax: $2,000 * 0.0025 + ($5,000 - $2,000) * 0.0075 + ($7,500 - $5,000) * 0.0175
    #       + ($9,800 - $7,500) * 0.0275 + ($12,200 - $9,800) * 0.0375
    #       + ($67,300 - $12,200) * 0.0475
    #       = $5.00 + $22.50 + $43.75 + $63.25 + $90.00 + $2,617.25
    #       = $2,841.75
    TaxScenario(
        source="OK 2024 Tax Brackets (computed)",
        description="OK MFJ, $80,000 W2, no dependents",
        year=2024,
        state="OK",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5635.0,  # Tax Table row 50,800-50,850
        expected_state_tax=2841.75,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # OK 2024 HoH, $60,000 W2, no dependents
    # Fed AGI: $60,000, Fed std ded: $21,900, Fed taxable: $38,100
    # Fed tax: $16,550 * 0.10 + $21,550 * 0.12 = $1,655 + $2,586 = $4,241
    # OK AGI: $60,000
    # OK std ded: $9,350, OK taxable: $60,000 - $9,350 = $50,650
    # OK tax (HoH follows MFJ brackets):
    # $2,000 * 0.0025 + ($5,000 - $2,000) * 0.0075 + ($7,500 - $5,000) * 0.0175
    #       + ($9,800 - $7,500) * 0.0275 + ($12,200 - $9,800) * 0.0375
    #       + ($50,650 - $12,200) * 0.0475
    #       = $5.00 + $22.50 + $43.75 + $63.25 + $90.00 + $1,826.38
    #       = $2,050.88
    TaxScenario(
        source="OK 2024 Tax Brackets (computed)",
        description="OK HoH, $60,000 W2, no dependents",
        year=2024,
        state="OK",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_federal_tax=4244.0,  # Tax Table row 38,100-38,150
        expected_state_tax=2050.88,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # OK 2025 Single, $50,000 W2, no dependents (same rates as 2024)
    # Fed AGI: $50,000, Fed std ded 2025: $15,000, Fed taxable: $35,000
    # Fed tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    # OK AGI: $50,000
    # OK std ded: $6,350, OK taxable: $50,000 - $6,350 = $43,650
    # OK tax: $1,000 * 0.0025 + ($2,500 - $1,000) * 0.0075 + ($3,750 - $2,500) * 0.0175
    #       + ($4,900 - $3,750) * 0.0275 + ($7,200 - $4,900) * 0.0375
    #       + ($43,650 - $7,200) * 0.0475
    #       = $2.50 + $11.25 + $21.875 + $31.625 + $86.25 + $1,731.375
    #       = $1,884.875
    TaxScenario(
        source="OK 2025 Tax Brackets (computed)",
        description="OK Single, $50,000 W2, no dependents (2025, same rates as 2024)",
        year=2025,
        state="OK",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1884.875,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # OK 2025 MFJ, $100,000 W2, no dependents (same rates as 2024)
    # Fed AGI: $100,000, Fed std ded 2025: $30,000, Fed taxable: $70,000
    # Fed tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    # OK AGI: $100,000
    # OK std ded: $12,700, OK taxable: $100,000 - $12,700 = $87,300
    # OK tax: $2,000 * 0.0025 + ($5,000 - $2,000) * 0.0075 + ($7,500 - $5,000) * 0.0175
    #       + ($9,800 - $7,500) * 0.0275 + ($12,200 - $9,800) * 0.0375
    #       + ($87,300 - $12,200) * 0.0475
    #       = $5.00 + $22.50 + $43.75 + $63.25 + $90.00 + $3,567.25
    #       = $3,791.75
    TaxScenario(
        source="OK 2025 Tax Brackets (computed)",
        description="OK MFJ, $100,000 W2, no dependents (2025, same rates as 2024)",
        year=2025,
        state="OK",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=3791.75,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== ARKANSAS SCENARIOS ==========
    # AR uses 5-bracket progressive tax:
    # All filing statuses: 0-5,499: 0%, 5,500-10,899: 2%, 10,900-15,599: 3%,
    #                      15,600-25,699: 3.4%, 25,700+: 3.9%
    # Standard Deduction: Single/MFS/HoH $2,410, MFJ/QW $4,820
    #
    # AR 2024 Single, $40,000 W2, no dependents
    # Fed AGI: $40,000, Fed std ded: $14,600, Fed taxable: $25,400
    # Fed tax: $11,600 * 0.10 + $13,800 * 0.12 = $1,160 + $1,656 = $2,816
    # AR AGI: $40,000 (no additions/subtractions)
    # AR std ded: $2,410, AR taxable: $40,000 - $2,410 = $37,590
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($37,590 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $463.749
    #       = $1,056.149
    TaxScenario(
        source="AR 2024 Tax Brackets (computed)",
        description="AR Single, $40,000 W2, no dependents",
        year=2024,
        state="AR",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=1056.149,
        expected_federal_agi=40000.0,
        backend="graph",
    ),
    # AR 2024 MFJ, $80,000 W2, no dependents
    # Fed AGI: $80,000, Fed std ded: $29,200, Fed taxable: $50,800
    # Fed tax: $23,200 * 0.10 + $27,600 * 0.12 = $2,320 + $3,312 = $5,632
    # AR AGI: $80,000
    # AR std ded: $4,820, AR taxable: $80,000 - $4,820 = $75,180
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($75,180 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $1,929.759
    #       = $2,522.159
    TaxScenario(
        source="AR 2024 Tax Brackets (computed)",
        description="AR MFJ, $80,000 W2, no dependents",
        year=2024,
        state="AR",
        filing_status="Married/Joint",
        w2_income=80000.0,
        expected_federal_tax=5635.0,  # Tax Table row 50,800-50,850
        expected_state_tax=2522.159,
        expected_federal_agi=80000.0,
        backend="graph",
    ),
    # AR 2024 HoH, $60,000 W2, no dependents
    # Fed AGI: $60,000, Fed std ded: $21,900, Fed taxable: $38,100
    # Fed tax: $16,550 * 0.10 + $21,550 * 0.12 = $1,655 + $2,586 = $4,241
    # AR AGI: $60,000
    # AR std ded: $2,410, AR taxable: $60,000 - $2,410 = $57,590
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($57,590 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $1,243.749
    #       = $1,836.149
    TaxScenario(
        source="AR 2024 Tax Brackets (computed)",
        description="AR HoH, $60,000 W2, no dependents",
        year=2024,
        state="AR",
        filing_status="Head_of_House",
        w2_income=60000.0,
        expected_federal_tax=4244.0,  # Tax Table row 38,100-38,150
        expected_state_tax=1836.149,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # AR 2025 Single, $50,000 W2, no dependents (same rates/brackets as 2024)
    # Fed AGI: $50,000, Fed std ded 2025: $15,000, Fed taxable: $35,000
    # Fed tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    # AR AGI: $50,000
    # AR std ded: $2,410, AR taxable: $50,000 - $2,410 = $47,590
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($47,590 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $853.749
    #       = $1,446.149
    TaxScenario(
        source="AR 2025 Tax Brackets (computed)",
        description="AR Single, $50,000 W2, no dependents (2025, same rates as 2024)",
        year=2025,
        state="AR",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1446.149,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # AR 2025 MFJ, $100,000 W2, no dependents (same rates/brackets as 2024)
    # Fed AGI: $100,000, Fed std ded 2025: $30,000, Fed taxable: $70,000
    # Fed tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923
    # AR AGI: $100,000
    # AR std ded: $4,820, AR taxable: $100,000 - $4,820 = $95,180
    # AR tax: $5,499 * 0.00 + ($10,899 - $5,499) * 0.02 + ($15,599 - $10,899) * 0.03
    #       + ($25,699 - $15,599) * 0.034 + ($95,180 - $25,699) * 0.039
    #       = $0 + $108.00 + $141.00 + $343.40 + $2,709.759
    #       = $3,302.159
    TaxScenario(
        source="AR 2025 Tax Brackets (computed)",
        description="AR MFJ, $100,000 W2, no dependents (2025, same rates as 2024)",
        year=2025,
        state="AR",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=3302.159,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== GEORGIA SCENARIOS ==========
    # GA 2024: Flat 5.39% rate
    # Standard Deduction: Single $12,000, MFJ $24,000
    # Dependent Exemption: $4,000 per dependent
    #
    # GA Single, $50,000 W2, no dependents
    # Fed AGI: $50,000
    # GA AGI: $50,000 (no additions/subtractions)
    # GA Taxable: $50,000 - $12,000 = $38,000
    # GA Tax: $38,000 * 0.0539 = $2,048.20
    # Federal taxable: $35,400, Federal tax: $4,016
    TaxScenario(
        source="GA 2024 Tax Brackets (computed)",
        description="GA Single, $50,000 income, no dependents",
        year=2024,
        state="GA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=2048.20,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # GA Single, $100,000 W2, 2 dependents ($8,000 exemptions)
    # Fed AGI: $100,000
    # GA AGI: $100,000
    # GA Taxable: $100,000 - $12,000 - $8,000 = $80,000
    # GA Tax: $80,000 * 0.0539 = $4,312.00
    # Federal taxable: $100,000 - $14,600 = $85,400, Federal tax: $13,841
    TaxScenario(
        source="GA 2024 Tax Brackets (computed)",
        description="GA Single, $100,000 income, $8,000 exemptions",
        year=2024,
        state="GA",
        filing_status="Single",
        w2_income=100000.0,
        dependent_exemptions=8000.0,
        expected_federal_tax=13847.0,  # Tax Table row 85,400-85,450
        expected_state_tax=4312.0,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # GA MFJ, $100,000 W2, no dependents
    # Fed AGI: $100,000
    # GA AGI: $100,000
    # GA Taxable: $100,000 - $24,000 = $76,000
    # GA Tax: $76,000 * 0.0539 = $4,096.40
    # Federal taxable: $70,800, Federal tax: $8,032
    TaxScenario(
        source="GA 2024 Tax Brackets (computed)",
        description="GA MFJ, $100,000 income, no dependents",
        year=2024,
        state="GA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=4096.40,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # GA Head of Household, $75,000 W2, $4,000 exemptions
    # Fed AGI: $75,000
    # GA AGI: $75,000
    # GA Taxable: $75,000 - $12,000 - $4,000 = $59,000
    # GA Tax: $59,000 * 0.0539 = $3,180.10
    # Federal taxable: $75,000 - $21,900 = $53,100
    # Federal tax: $16,550 x 0.10 + $36,550 x 0.12 = $1,655 + $4,386 = $6,041
    TaxScenario(
        source="GA 2024 Tax Brackets (computed)",
        description="GA HoH, $75,000 income, $4,000 exemptions",
        year=2024,
        state="GA",
        filing_status="Head_of_House",
        w2_income=75000.0,
        dependent_exemptions=4000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=3180.10,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    # GA 2025: Flat 5.19% rate (reduced from 5.39%)
    # Standard Deduction: Single $12,000, MFJ $24,000 (unchanged from 2024)
    # Dependent Exemption: $4,000 per dependent (unchanged from 2024)
    #
    # GA Single, $50,000 W2, no dependents
    # Fed AGI: $50,000
    # GA AGI: $50,000
    # GA Taxable: $50,000 - $12,000 = $38,000
    # GA Tax: $38,000 * 0.0519 = $1,972.20
    # Federal taxable: $35,000 (AGI - $15,000 std ded)
    # Federal tax (2025): $11,925 x 0.10 + $23,075 x 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="GA 2025 Tax Brackets (computed)",
        description="GA Single, $50,000 income (2025, 5.19% rate)",
        year=2025,
        state="GA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1972.20,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # GA MFJ, $100,000 W2, $8,000 exemptions (2025)
    # Fed AGI: $100,000
    # GA AGI: $100,000
    # GA Taxable: $100,000 - $24,000 - $8,000 = $68,000
    # GA Tax: $68,000 * 0.0519 = $3,529.20
    # Federal taxable: $70,000 (AGI - $30,000 std ded)
    # Federal tax (2025 MFJ): $23,850 x 0.10 + $46,150 x 0.12 = $2,385 + $5,538 = $7,923
    TaxScenario(
        source="GA 2025 Tax Brackets (computed)",
        description="GA MFJ, $100,000 income, $8,000 exemptions (2025, 5.19% rate)",
        year=2025,
        state="GA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=8000.0,
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=3529.20,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== MARYLAND SCENARIOS ==========
    # MD 2024: Standard deduction 15% of MD AGI (min $1,800, max $2,700)
    # Personal exemption: $3,200 (accepted as input, not computed)
    # Schedule I (Single/MFS/Dep): 8 brackets (2%, 3%, 4%, 4.75%, 5%, 5.25%, 5.5%, 5.75%)
    # Schedule II (MFJ/HoH/QSS): Same rates, different thresholds
    #
    # MD Single, $50,000 W2 (2024)
    # Fed AGI: $50,000
    # MD AGI: $50,000
    # MD Std Ded: min($2,700, max($1,800, $50,000 x 0.15)) = min($2,700, $7,500) = $2,700
    # MD Taxable: $50,000 - $2,700 - $3,200 = $44,100
    # MD Tax (Schedule I):
    #   $1,000 x 0.02 = $20.00
    #   $1,000 x 0.03 = $30.00
    #   $1,000 x 0.04 = $40.00
    #   $41,100 x 0.0475 = $1,952.25
    #   Total: $2,042.25
    # Federal taxable: $50,000 - $14,600 = $35,400
    # Federal tax (OTS tables): $4,016.00
    TaxScenario(
        source="MD 2024 Tax Brackets (computed)",
        description="MD Single, $50,000 income (2024)",
        year=2024,
        state="MD",
        filing_status="Single",
        w2_income=50000.0,
        dependent_exemptions=3200.0,  # Personal exemption
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=2042.25,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MD MFJ, $100,000 W2 (2024)
    # Fed AGI: $100,000
    # MD AGI: $100,000
    # MD Std Ded: min($2,700, max($1,800, $100,000 x 0.15)) = min($2,700, $15,000) = $2,700
    # MD Taxable: $100,000 - $2,700 - $6,400 = $90,900 (assuming 2 exemptions)
    # MD Tax (Schedule II):
    #   $1,000 x 0.02 = $20.00
    #   $1,000 x 0.03 = $30.00
    #   $1,000 x 0.04 = $40.00
    #   $87,900 x 0.0475 = $4,175.25
    #   Total: $4,265.25
    # Federal taxable: $100,000 - $29,200 = $70,800
    # Federal tax (OTS tables): $8,032.00
    TaxScenario(
        source="MD 2024 Tax Brackets (computed)",
        description="MD MFJ, $100,000 income (2024)",
        year=2024,
        state="MD",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=6400.0,  # 2 personal exemptions
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=4265.25,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MD 2025: Standard deduction flat $3,350 (Single/MFS) or $6,700 (MFJ/HoH/QSS)
    # Personal exemption: $3,200 (unchanged)
    # Schedule I (Single/MFS/Dep): 10 brackets (adds 6.25% at $500k, 6.50% at $1M)
    # Schedule II (MFJ/HoH/QSS): 10 brackets (adds 6.25% at $600k, 6.50% at $1.2M)
    #
    # MD Single, $50,000 W2 (2025)
    # Fed AGI: $50,000
    # MD AGI: $50,000
    # MD Std Ded: $3,350 (flat amount)
    # MD Taxable: $50,000 - $3,350 - $3,200 = $43,450
    # MD Tax (Schedule I):
    #   $1,000 x 0.02 = $20.00
    #   $1,000 x 0.03 = $30.00
    #   $1,000 x 0.04 = $40.00
    #   $40,450 x 0.0475 = $1,921.375
    #   Total: $2,011.375
    # Federal taxable: $50,000 - $15,000 = $35,000
    # Federal tax (OTS tables 2025): $3,961.50
    TaxScenario(
        source="MD 2025 Tax Brackets (computed)",
        description="MD Single, $50,000 income (2025, flat std ded)",
        year=2025,
        state="MD",
        filing_status="Single",
        w2_income=50000.0,
        dependent_exemptions=3200.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=2011.375,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # MD MFJ, $100,000 W2 (2025)
    # Fed AGI: $100,000
    # MD AGI: $100,000
    # MD Std Ded: $6,700 (flat amount for MFJ)
    # MD Taxable: $100,000 - $6,700 - $6,400 = $86,900
    # MD Tax (Schedule II):
    #   $1,000 x 0.02 = $20.00
    #   $1,000 x 0.03 = $30.00
    #   $1,000 x 0.04 = $40.00
    #   $83,900 x 0.0475 = $3,985.25
    #   Total: $4,075.25
    # Federal taxable: $100,000 - $30,000 = $70,000
    # Federal tax (OTS tables 2025 MFJ): $7,923.00
    TaxScenario(
        source="MD 2025 Tax Brackets (computed)",
        description="MD MFJ, $100,000 income (2025, flat std ded)",
        year=2025,
        state="MD",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=6400.0,
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=4075.25,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # MD Single, high earner testing new 2025 brackets
    # MD Single, $600,000 W2 (2025)
    # Fed AGI: $600,000
    # MD AGI: $600,000
    # MD Std Ded: $3,350
    # MD Taxable: $600,000 - $3,350 - $3,200 = $593,450
    # MD Tax (computed): $32,975.625
    # Federal tax (OTS tables 2025 Single): $174,034.75
    #   + Additional Medicare Tax: ($600,000 - $200,000) * 0.009 = $3,600
    #   = $177,634.75
    TaxScenario(
        source="MD 2025 Tax Brackets (computed)",
        description="MD Single, $600,000 income (2025, tests 6.25% bracket)",
        year=2025,
        state="MD",
        filing_status="Single",
        w2_income=600000.0,
        dependent_exemptions=3200.0,
        expected_federal_tax=177634.75,
        expected_state_tax=32975.625,
        expected_federal_agi=600000.0,
        backend="graph",
    ),
    # ========== LOUISIANA SCENARIOS ==========
    # LA 2024: 3-bracket system (1.85%, 3.5%, 4.25%)
    # Single/MFS/HoH: 1.85% up to $12,500, 3.5% $12,500-$50,000, 4.25% over $50,000
    # MFJ/QW: 1.85% up to $25,000, 3.5% $25,000-$100,000, 4.25% over $100,000
    # Combined personal exemption-standard deduction: Single $4,500, MFJ/HoH $9,000 (+ $1,000 per additional exemption)
    # The exemptions are deducted from the lowest bracket first (La. R.S. 47:32(A)(1),
    # 294, 295(B); RIB 25-012 PDF page 1 note 1), so the tax is the bracket tax on
    # tax table income less the bracket tax on the exemption amount. IT-540 line 10
    # is the 2024 tax table value (LA_2024_TABLE_URL), which prices each $250 row
    # at its midpoint; the expected values below are table values, and the
    # formula gap is a known defect (la_2024_table_gap, tenforty-xew).
    #
    # LA 2025: Flat 3% tax
    # Standard deduction: Single $12,500, MFJ/HoH $25,000
    # No dependent exemptions
    #
    # LA Single, $25,000 W2 (2024)
    # Federal AGI: $25,000
    # LA tax table income (L9): $25,000
    # Exemptions: $4,500 (base, 1 exemption)
    # LA taxable: $25,000 - $4,500 = $20,500
    # Tax on $25,000: $12,500 * 0.0185 + $12,500 * 0.035 = $231.25 + $437.50 = $668.75
    # Tax on $4,500 of exemptions: $4,500 * 0.0185 = $83.25
    # Formula: $668.75 - $83.25 = $585.50.
    # Legal value (line 10 is the table): 2024 Tax Table, PDF page 1, Single,
    # row $25,000-$25,250, column 1 exemption: $590.
    # Federal taxable: $25,000 - $14,600 = $10,400
    # Federal tax: $10,400 * 0.10 = $1,040.00
    TaxScenario(
        source="LA 2024 Tax Table (tax-table)",
        description="LA Single, $25,000 income (2024)",
        year=2024,
        state="LA",
        filing_status="Single",
        w2_income=25000.0,
        dependent_exemptions=4500.0,
        expected_federal_tax=1043.0,  # Tax Table row 10,400-10,450
        expected_state_tax=590.00,
        state_evidence=LA_TABLE_EVIDENCE,
        known_defects=(la_2024_table_gap(0.035, 25_000, 25_250, 25_000),),
        expected_federal_agi=25000.0,
        backend="graph",
    ),
    # LA Single, $60,000 W2 (2024)
    # Federal AGI: $60,000
    # LA tax table income: $60,000
    # Exemptions: $4,500
    # LA taxable: $60,000 - $4,500 = $55,500
    # Tax on $60,000: $12,500 * 0.0185 + $37,500 * 0.035 + $10,000 * 0.0425
    #       = $231.25 + $1,312.50 + $425.00 = $1,968.75
    # Tax on $4,500 of exemptions: $4,500 * 0.0185 = $83.25
    # Formula: $1,968.75 - $83.25 = $1,885.50.
    # Legal value: 2024 Tax Table, PDF page 2, Single, last row $50,750-$51,000,
    # column 1: $1,498, "Plus 4.25% of Tax Table Income in Excess of $51,000":
    # $1,498 + $9,000 x 0.0425 = $1,498 + $382.50 = $1,880.50, rounded to the
    # nearest dollar (2024 IT-540 instructions, PDF page 3, "About This Form"
    # item 4), half up: $1,881.
    # Federal taxable: $60,000 - $14,600 = $45,400
    # Federal tax: $11,600 * 0.10 + $33,800 * 0.12 = $1,160 + $4,056 = $5,216.00
    TaxScenario(
        source="LA 2024 Tax Table (tax-table)",
        description="LA Single, $60,000 income, all 3 brackets (2024)",
        year=2024,
        state="LA",
        filing_status="Single",
        w2_income=60000.0,
        dependent_exemptions=4500.0,
        expected_federal_tax=5219.0,  # Tax Table row 45,400-45,450
        expected_state_tax=1881.00,
        state_evidence=LA_DERIVED_EVIDENCE,
        known_defects=(
            la_2024_table_gap(
                0.0425, 50_750, 51_000, 60_000, rounding_adjustment=1881.00 - 1880.50
            ),
        ),
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # LA MFJ, $30,000 W2 (2024)
    # Federal AGI: $30,000
    # LA tax table income: $30,000
    # Exemptions: $9,000 (base, 2 exemptions)
    # LA taxable: $30,000 - $9,000 = $21,000
    # Tax on $30,000: $25,000 * 0.0185 + $5,000 * 0.035 = $462.50 + $175.00 = $637.50
    # Tax on $9,000 of exemptions: $9,000 * 0.0185 = $166.50
    # Formula: $637.50 - $166.50 = $471.00.
    # Legal value: 2024 Tax Table, PDF page 3, MFJ, row $30,000-$30,250,
    # column 2 exemptions: $475.
    # Federal taxable: $30,000 - $29,200 = $800
    # Federal tax: $800 * 0.10 = $80.00
    TaxScenario(
        source="LA 2024 Tax Table (tax-table)",
        description="LA MFJ, $30,000 income (2024)",
        year=2024,
        state="LA",
        filing_status="Married/Joint",
        w2_income=30000.0,
        dependent_exemptions=9000.0,
        expected_federal_tax=81.0,  # Tax Table row 800-825
        expected_state_tax=475.00,
        state_evidence=LA_TABLE_EVIDENCE,
        known_defects=(la_2024_table_gap(0.035, 30_000, 30_250, 30_000),),
        expected_federal_agi=30000.0,
        backend="graph",
    ),
    # LA MFJ, $150,000 W2 (2024)
    # Federal AGI: $150,000
    # LA tax table income: $150,000
    # Exemptions: $9,000
    # LA taxable: $150,000 - $9,000 = $141,000
    # Tax on $150,000: $25,000 * 0.0185 + $75,000 * 0.035 + $50,000 * 0.0425
    #       = $462.50 + $2,625.00 + $2,125.00 = $5,212.50
    # Tax on $9,000 of exemptions: $9,000 * 0.0185 = $166.50
    # Formula: $5,212.50 - $166.50 = $5,046.00.
    # Legal value: 2024 Tax Table, PDF page 6, MFJ, last row $100,750-$101,000,
    # column 2: $2,958, "Plus 4.25% of Tax Table Income in Excess of $101,000":
    # $2,958 + $49,000 x 0.0425 = $2,958 + $2,082.50 = $5,040.50, rounded to the
    # nearest dollar (PDF page 3, item 4), half up: $5,041.
    # Federal taxable: $150,000 - $29,200 = $120,800
    # Federal tax: $23,200 * 0.10 + $71,100 * 0.12 + $26,500 * 0.22 = $2,320 + $8,532 + $5,830 = $16,682.00
    TaxScenario(
        source="LA 2024 Tax Table (tax-table)",
        description="LA MFJ, $150,000 income, all 3 brackets (2024)",
        year=2024,
        state="LA",
        filing_status="Married/Joint",
        w2_income=150000.0,
        dependent_exemptions=9000.0,
        expected_federal_tax=16682.00,
        expected_state_tax=5041.00,
        state_evidence=LA_DERIVED_EVIDENCE,
        known_defects=(
            la_2024_table_gap(
                0.0425, 100_750, 101_000, 150_000, rounding_adjustment=5041.00 - 5040.50
            ),
        ),
        expected_federal_agi=150000.0,
        backend="graph",
    ),
    # LA Single, $50,000 W2 (2025)
    # Federal AGI: $50,000
    # LA taxable: $50,000 - $12,500 (standard deduction) = $37,500
    # LA tax: $37,500 * 0.03 = $1,125.00
    # Federal taxable: $50,000 - $15,000 = $35,000
    # Federal tax: $11,925 * 0.10 + $23,075 * 0.12 = $1,192.50 + $2,769 = $3,961.50
    TaxScenario(
        source="LA 2025 Tax Reform (computed)",
        description="LA Single, $50,000 income, flat 3% tax (2025)",
        year=2025,
        state="LA",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1125.00,
        expected_federal_agi=50000.0,
        backend="graph",
    ),
    # LA MFJ, $100,000 W2 (2025)
    # Federal AGI: $100,000
    # LA taxable: $100,000 - $25,000 (standard deduction) = $75,000
    # LA tax: $75,000 * 0.03 = $2,250.00
    # Federal taxable: $100,000 - $30,000 = $70,000
    # Federal tax: $23,850 * 0.10 + $46,150 * 0.12 = $2,385 + $5,538 = $7,923.00
    TaxScenario(
        source="LA 2025 Tax Reform (computed)",
        description="LA MFJ, $100,000 income, flat 3% tax (2025)",
        year=2025,
        state="LA",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=7746.0,  # Tax Table row 68,500-68,550
        expected_state_tax=2250.00,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    # ========== NEW MEXICO SCENARIOS ==========
    # NM 2024 Single, middle income
    # Federal AGI: $40,000
    # Federal std deduction (2024): $14,600
    # NM taxable: $40,000 - $14,600 = $25,400
    # NM tax:
    #   First $5,500 @ 1.7%: $93.50
    #   $5,501-$11,000 ($5,500) @ 3.2%: $176.00
    #   $11,001-$16,000 ($5,000) @ 4.7%: $235.00
    #   $16,001-$25,400 ($9,400) @ 4.9%: $460.60
    #   Total: $965.10
    # Federal taxable: $25,400, Federal tax: $2,816 (formula)
    TaxScenario(
        source="NM 2024 Tax Brackets (computed)",
        description="NM Single, $40,000 income (2024)",
        year=2024,
        state="NM",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=965.10,
        backend="graph",
    ),
    # NM 2024 Married/Joint, higher income
    # Federal AGI: $100,000
    # Federal std deduction (2024): $29,200
    # NM taxable: $100,000 - $29,200 = $70,800
    # NM tax:
    #   First $8,000 @ 1.7%: $136.00
    #   $8,001-$16,000 ($8,000) @ 3.2%: $256.00
    #   $16,001-$24,000 ($8,000) @ 4.7%: $376.00
    #   $24,001-$70,800 ($46,800) @ 4.9%: $2,293.20
    #   Total: $3,061.20
    # Federal taxable: $70,800, Federal tax: $8,032 (formula, graph backend)
    TaxScenario(
        source="NM 2024 Tax Brackets (computed)",
        description="NM MFJ, $100,000 income (2024)",
        year=2024,
        state="NM",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3061.20,
        backend="graph",
    ),
    # NM 2024 Head_of_House, middle income
    # Federal AGI: $75,000
    # Federal std deduction (2024): $21,900
    # NM taxable: $75,000 - $21,900 = $53,100
    # NM tax:
    #   First $8,000 @ 1.7%: $136.00
    #   $8,001-$16,000 ($8,000) @ 3.2%: $256.00
    #   $16,001-$24,000 ($8,000) @ 4.7%: $376.00
    #   $24,001-$53,100 ($29,100) @ 4.9%: $1,425.90
    #   Total: $2,193.90
    # Federal taxable: $53,100, Federal tax: $6,041 (formula, graph backend)
    TaxScenario(
        source="NM 2024 Tax Brackets (computed)",
        description="NM HoH, $75,000 income (2024)",
        year=2024,
        state="NM",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=2193.90,
        backend="graph",
    ),
    # NM 2024 Single, high income
    # Federal AGI: $250,000
    # Federal std deduction (2024): $14,600
    # NM taxable: $250,000 - $14,600 = $235,400
    # NM tax:
    #   First $5,500 @ 1.7%: $93.50
    #   $5,501-$11,000 ($5,500) @ 3.2%: $176.00
    #   $11,001-$16,000 ($5,000) @ 4.7%: $235.00
    #   $16,001-$210,000 ($194,000) @ 4.9%: $9,506.00
    #   $210,001-$235,400 ($25,400) @ 5.9%: $1,498.60
    #   Total: $11,509.10
    # Federal taxable: $235,400, Federal tax: $53,014.50 (income tax)
    #   + Additional Medicare Tax: ($250,000 - $200,000) * 0.009 = $450
    #   = $53,464.50
    TaxScenario(
        source="NM 2024 Tax Brackets (computed)",
        description="NM Single, $250,000 income (2024)",
        year=2024,
        state="NM",
        filing_status="Single",
        w2_income=250000.0,
        expected_federal_tax=53464.5,
        expected_state_tax=11509.10,
        backend="graph",
    ),
    # NM 2025 Single, middle income (new 6-bracket structure)
    # Federal AGI: $50,000
    # Federal std deduction (2025): $15,750
    # NM taxable: $50,000 - $15,750 = $34,250
    # NM tax:
    #   First $5,500 @ 1.5%: $82.50
    #   $5,501-$16,500 ($11,000) @ 3.2%: $352.00
    #   $16,501-$33,500 ($17,000) @ 4.3%: $731.00
    #   $33,501-$34,250 ($750) @ 4.7%: $35.25
    #   Total: $1,200.75
    # Federal taxable: $34,250, Federal tax: $3,871.50
    TaxScenario(
        source="NM 2025 Tax Brackets (computed)",
        description="NM Single, $50,000 income (2025)",
        year=2025,
        state="NM",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1200.75,
        backend="graph",
    ),
    # NE 2024 Single, moderate income
    # Federal AGI: $40,000
    # NE std deduction (2024): $8,350
    # NE taxable: $40,000 - $8,350 = $31,650
    # NE tax:
    #   First $3,900 @ 2.46%: $95.94
    #   $3,901-$23,370 ($19,470) @ 3.51%: $683.40
    #   $23,371-$31,650 ($8,280) @ 5.01%: $414.83
    #   Total: $1,194.17
    TaxScenario(
        source="NE 2024 Tax Brackets (computed)",
        description="NE Single, $40,000 income (2024)",
        year=2024,
        state="NE",
        filing_status="Single",
        w2_income=40000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=1194.17,
        backend="graph",
    ),
    # NE 2024 Married/Joint, higher income
    # Federal AGI: $100,000
    # NE std deduction (2024): $16,700
    # NE taxable: $100,000 - $16,700 = $83,300
    # NE tax:
    #   First $7,790 @ 2.46%: $191.63
    #   $7,791-$46,760 ($38,970) @ 3.51%: $1,367.85
    #   $46,761-$75,340 ($28,580) @ 5.01%: $1,431.86
    #   $75,341-$83,300 ($7,960) @ 5.84%: $464.86
    #   Total: $3,456.20
    TaxScenario(
        source="NE 2024 Tax Brackets (computed)",
        description="NE MFJ, $100,000 income (2024)",
        year=2024,
        state="NE",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=3456.20,
        backend="graph",
    ),
    # NE 2024 Head_of_House, middle income
    # Federal AGI: $75,000
    # NE std deduction (2024): $12,250
    # NE taxable: $75,000 - $12,250 = $62,750
    # NE tax:
    #   First $7,270 @ 2.46%: $178.84
    #   $7,271-$37,400 ($30,130) @ 3.51%: $1,057.56
    #   $37,401-$55,850 ($18,450) @ 5.01%: $924.35
    #   $55,851-$62,750 ($6,900) @ 5.84%: $402.96
    #   Total: $2,563.71
    TaxScenario(
        source="NE 2024 Tax Brackets (computed)",
        description="NE HoH, $75,000 income (2024)",
        year=2024,
        state="NE",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=2563.71,
        backend="graph",
    ),
    # NE 2024 Married/Separate, moderate income
    # Federal AGI: $50,000
    # NE std deduction (2024): $8,350
    # NE taxable: $50,000 - $8,350 = $41,650
    # NE tax:
    #   First $3,900 @ 2.46%: $95.94
    #   $3,901-$23,370 ($19,470) @ 3.51%: $683.40
    #   $23,371-$37,670 ($14,300) @ 5.01%: $716.43
    #   $37,671-$41,650 ($3,980) @ 5.84%: $232.43
    #   Total: $1,728.20
    TaxScenario(
        source="NE 2024 Tax Brackets (computed)",
        description="NE MFS, $50,000 income (2024)",
        year=2024,
        state="NE",
        filing_status="Married/Sep",
        w2_income=50000.0,
        expected_federal_tax=4019.0,  # Tax Table row 35,400-35,450
        expected_state_tax=1728.20,
        backend="graph",
    ),
    # NE 2025 Single, moderate income (test 2025 rate reduction)
    # Federal AGI: $50,000
    # NE std deduction (2025): $8,600
    # NE taxable: $50,000 - $8,600 = $41,400
    # NE tax:
    #   First $4,030 @ 2.46%: $99.14
    #   $4,031-$24,120 ($20,090) @ 3.51%: $705.16
    #   $24,121-$38,870 ($14,750) @ 5.01%: $738.98
    #   $38,871-$41,400 ($2,530) @ 5.20%: $131.56
    #   Total: $1,674.84
    TaxScenario(
        source="NE 2025 Tax Brackets (computed)",
        description="NE Single, $50,000 income (2025)",
        year=2025,
        state="NE",
        filing_status="Single",
        w2_income=50000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1674.84,
        backend="graph",
    ),
    # ========== NEW HAMPSHIRE SCENARIOS ==========
    # NH DP-10 taxes interest and dividends only (not W2 income)
    # 2024: 3% flat rate on I&D income above exemption
    # 2025: Tax repealed (0% rate)
    # Exemption: Single $2,400, MFJ $4,800
    #
    # NH 2024 Single, $50,000 W2, $10,000 interest, no dividends
    # NH taxable I&D: $10,000 - $2,400 = $7,600
    # NH tax: $7,600 * 0.03 = $228.00
    # Federal: AGI $60,000, std ded $14,600, taxable $45,400
    # Federal tax: $11,925 * 0.10 + $33,475 * 0.12 = $1,192.50 + $4,017 = $5,209.50 -> $5,216
    TaxScenario(
        source="NH 2024 DP-10 (computed)",
        description="NH Single, $50,000 W2 + $10,000 interest (2024)",
        year=2024,
        state="NH",
        filing_status="Single",
        w2_income=50000.0,
        taxable_interest=10000.0,
        expected_federal_tax=5219.0,  # Tax Table row 45,400-45,450
        expected_state_tax=228.0,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # NH 2024 MFJ, $80,000 W2, $20,000 interest, $5,000 dividends
    # NH taxable I&D: ($20,000 + $5,000) - $4,800 = $20,200
    # NH tax: $20,200 * 0.03 = $606.00
    # Federal: AGI $105,000, std ded $29,200, taxable $75,800
    # Federal tax: $23,850 * 0.10 + $51,950 * 0.12 = $2,385 + $6,234 = $8,619 -> $8,632
    TaxScenario(
        source="NH 2024 DP-10 (computed)",
        description="NH MFJ, $80,000 W2 + $25,000 I&D (2024)",
        year=2024,
        state="NH",
        filing_status="Married/Joint",
        w2_income=80000.0,
        taxable_interest=20000.0,
        ordinary_dividends=5000.0,
        expected_federal_tax=8635.0,  # Tax Table row 75,800-75,850
        expected_state_tax=606.0,
        expected_federal_agi=105000.0,
        backend="graph",
    ),
    # NH 2025 Single, $50,000 W2, $10,000 interest (tax repealed)
    # NH taxable I&D: $10,000 - $2,400 = $7,600
    # NH tax: $7,600 * 0.00 = $0.00 (repealed)
    # Federal: AGI $60,000, std ded $15,750, taxable $44,250
    # Federal tax: $11,925 * 0.10 + $32,325 * 0.12 = $1,192.50 + $3,879 = $5,071.50
    TaxScenario(
        source="NH 2025 DP-10 (computed)",
        description="NH Single, $50,000 W2 + $10,000 interest (2025, repealed)",
        year=2025,
        state="NH",
        filing_status="Single",
        w2_income=50000.0,
        taxable_interest=10000.0,
        expected_federal_tax=5075.0,  # Tax Table row 44,250-44,300
        expected_state_tax=0.0,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # WV 2024 Single, middle income
    # Federal AGI: $40,000
    # Federal std deduction (2024): $14,600
    # WV AGI: $40,000
    # WV Taxable: $40,000 - $4,000 (2 exemptions @ $2,000) = $36,000
    # WV tax:
    #   First $10,000 @ 2.36%: $236.00
    #   $10,001-$25,000 ($15,000) @ 3.15%: $472.50
    #   $25,001-$36,000 ($11,000) @ 3.54%: $389.40
    #   Total: $1,097.90
    # Federal taxable: $25,400, Federal tax: $2,816 (formula)
    TaxScenario(
        source="WV 2024 Tax Brackets (computed)",
        description="WV Single, $40,000 income (2024)",
        year=2024,
        state="WV",
        filing_status="Single",
        w2_income=40000.0,
        dependent_exemptions=4000.0,
        expected_federal_tax=2819.0,  # Tax Table row 25,400-25,450
        expected_state_tax=1097.90,
        backend="graph",
    ),
    # WV 2024 Married/Joint, higher income
    # Federal AGI: $100,000
    # Federal std deduction (2024): $29,200
    # WV AGI: $100,000
    # WV Taxable: $100,000 - $4,000 (2 exemptions @ $2,000) = $96,000
    # WV tax:
    #   First $10,000 @ 2.36%: $236.00
    #   $10,001-$25,000 ($15,000) @ 3.15%: $472.50
    #   $25,001-$40,000 ($15,000) @ 3.54%: $531.00
    #   $40,001-$60,000 ($20,000) @ 4.72%: $944.00
    #   $60,001-$96,000 ($36,000) @ 5.12%: $1,843.20
    #   Total: $4,026.70
    # Federal taxable: $70,800, Federal tax: $8,032 (formula, graph backend)
    TaxScenario(
        source="WV 2024 Tax Brackets (computed)",
        description="WV MFJ, $100,000 income (2024)",
        year=2024,
        state="WV",
        filing_status="Married/Joint",
        w2_income=100000.0,
        dependent_exemptions=4000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_state_tax=4026.70,
        backend="graph",
    ),
    # WV 2024 Married/Sep, middle income
    # Federal AGI: $45,000
    # Federal std deduction (2024): $14,600
    # WV AGI: $45,000
    # WV Taxable: $45,000 - $2,000 (1 exemption @ $2,000) = $43,000
    # WV tax (MFS uses half brackets):
    #   First $5,000 @ 2.36%: $118.00
    #   $5,001-$12,500 ($7,500) @ 3.15%: $236.25
    #   $12,501-$20,000 ($7,500) @ 3.54%: $265.50
    #   $20,001-$30,000 ($10,000) @ 4.72%: $472.00
    #   $30,001-$43,000 ($13,000) @ 5.12%: $665.60
    #   Total: $1,757.35
    # Federal taxable: $30,400, Federal tax: $3,416 (formula: $1,160 + $2,256)
    TaxScenario(
        source="WV 2024 Tax Brackets (computed)",
        description="WV MFS, $45,000 income (2024)",
        year=2024,
        state="WV",
        filing_status="Married/Sep",
        w2_income=45000.0,
        dependent_exemptions=2000.0,
        expected_federal_tax=3419.0,  # Tax Table row 30,400-30,450
        expected_state_tax=1757.35,
        backend="graph",
    ),
    # WV 2024 Head_of_House, middle income
    # Federal AGI: $75,000
    # Federal std deduction (2024): $21,900
    # WV AGI: $75,000
    # WV Taxable: $75,000 - $6,000 (3 exemptions @ $2,000) = $69,000
    # WV tax:
    #   First $10,000 @ 2.36%: $236.00
    #   $10,001-$25,000 ($15,000) @ 3.15%: $472.50
    #   $25,001-$40,000 ($15,000) @ 3.54%: $531.00
    #   $40,001-$60,000 ($20,000) @ 4.72%: $944.00
    #   $60,001-$69,000 ($9,000) @ 5.12%: $460.80
    #   Total: $2,644.30
    # Federal taxable: $53,100, Federal tax: $6,041 (formula, graph backend)
    TaxScenario(
        source="WV 2024 Tax Brackets (computed)",
        description="WV HoH, $75,000 income (2024)",
        year=2024,
        state="WV",
        filing_status="Head_of_House",
        w2_income=75000.0,
        dependent_exemptions=6000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_state_tax=2644.30,
        backend="graph",
    ),
    # WV 2025 Single, middle income (reduced rates per SB 2033)
    # Federal AGI: $50,000
    # Federal std deduction (2025): $15,000
    # WV AGI: $50,000
    # WV Taxable: $50,000 - $4,000 (2 exemptions @ $2,000) = $46,000
    # WV tax:
    #   First $10,000 @ 2.22%: $222.00
    #   $10,001-$25,000 ($15,000) @ 2.96%: $444.00
    #   $25,001-$40,000 ($15,000) @ 3.33%: $499.50
    #   $40,001-$46,000 ($6,000) @ 4.44%: $266.40
    #   Total: $1,431.90
    # Federal taxable: $35,000, Federal tax: $3,961.50 (formula, graph backend)
    TaxScenario(
        source="WV 2025 Tax Brackets (computed)",
        description="WV Single, $50,000 income (2025)",
        year=2025,
        state="WV",
        filing_status="Single",
        w2_income=50000.0,
        dependent_exemptions=4000.0,
        expected_federal_tax=3875.0,  # Tax Table row 34,250-34,300
        expected_state_tax=1431.90,
        backend="graph",
    ),
    # ========== HI BLIND-DERIVED STATE SCENARIOS ==========
    # Record: docs/validation/state-fixtures/HI-2024-2025.md (b72.33).
    # Exemptions are $1,144 each: self, MFJ spouse, each dependent. Dependents
    # enter only through the dependent_exemptions TOTAL, 1,144 x (1 + MFJ +
    # deps); num_dependents is not mapped for HI (tenforty-aqx.4.1.6). No federal
    # expectation: the record derives only the Hawaii return.
    # Case A24: TI 44,456; row 44,450-44,500, S/MFS.
    TaxScenario(
        source="HI 2024 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Single, $50,000 wages, 0 dependents (2024)",
        year=2024,
        state="HI",
        filing_status="Single",
        w2_income=50000.0,
        expected_state_tax=2936.0,
        known_defects=(hi_table_gap(0.079, 44_456, 44_450, -0.40),),
        backend="graph",
    ),
    # Case B24: TI 76,624; row 76,600-76,650, MFJ.
    TaxScenario(
        source="HI 2024 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Married/Joint, $90,000 wages, 2 dependents (2024)",
        year=2024,
        state="HI",
        filing_status="Married/Joint",
        w2_income=90000.0,
        dependent_exemptions=4576.0,  # 1,144 x 4
        expected_state_tax=4896.0,
        known_defects=(hi_table_gap(0.079, 76_624, 76_600, 0.20),),
        backend="graph",
    ),
    # Case C24: TI 36,288; row 36,250-36,300, HoH.
    TaxScenario(
        source="HI 2024 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Head_of_House, $45,000 wages, 1 dependents (2024)",
        year=2024,
        state="HI",
        filing_status="Head_of_House",
        w2_income=45000.0,
        dependent_exemptions=2288.0,  # 1,144 x 2
        expected_state_tax=2051.0,
        known_defects=(hi_table_gap(0.076, 36_288, 36_250, 0.40),),
        backend="graph",
    ),
    # Case D24: TI 34,456; row 34,450-34,500, S/MFS.
    TaxScenario(
        source="HI 2024 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Married/Sep, $40,000 wages, 0 dependents (2024)",
        year=2024,
        state="HI",
        filing_status="Married/Sep",
        w2_income=40000.0,
        expected_state_tax=2150.0,
        known_defects=(hi_table_gap(0.076, 34_456, 34_450, -0.40),),
        backend="graph",
    ),
    # Case E24: TI 58,912; row 58,900-58,950, MFJ (QSS).
    TaxScenario(
        source="HI 2024 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Widow(er), $70,000 wages, 1 dependents (2024)",
        year=2024,
        state="HI",
        filing_status="Widow(er)",
        w2_income=70000.0,
        dependent_exemptions=2288.0,  # 1,144 x 2
        expected_state_tax=3537.0,
        known_defects=(hi_table_gap(0.076, 58_912, 58_900, 0.20),),
        backend="graph",
    ),
    # Case F24: TI 14,456; row 14,450-14,500, S/MFS.
    TaxScenario(
        source="HI 2024 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Single, $20,000 wages, 0 dependents (2024)",
        year=2024,
        state="HI",
        filing_status="Single",
        w2_income=20000.0,
        expected_state_tax=687.0,
        known_defects=(hi_table_gap(0.068, 14_456, 14_450, -0.40),),
        backend="graph",
    ),
    # Case G24: TI 144,456; Schedule I: 3,214 + 8.25% x 96,456 = 11,171.62.
    TaxScenario(
        source="HI 2024 Rate Schedule (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Single, $150,000 wages, 0 dependents (2024)",
        year=2024,
        state="HI",
        filing_status="Single",
        w2_income=150000.0,
        expected_state_tax=11172.0,
        known_defects=(hi_schedule_gap(-0.40, 11_171.62 - 11_172),),
        backend="graph",
    ),
    # Case A25: TI 44,456; row 44,450-44,500, S/MFS.
    TaxScenario(
        source="HI 2025 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Single, $50,000 wages, 0 dependents (2025)",
        year=2025,
        state="HI",
        filing_status="Single",
        w2_income=50000.0,
        expected_state_tax=2285.0,
        known_defects=(hi_table_gap(0.072, 44_456, 44_450, 0.20),),
        backend="graph",
    ),
    # Case B25: TI 76,624; row 76,600-76,650, MFJ.
    TaxScenario(
        source="HI 2025 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Married/Joint, $90,000 wages, 2 dependents (2025)",
        year=2025,
        state="HI",
        filing_status="Married/Joint",
        w2_income=90000.0,
        dependent_exemptions=4576.0,  # 1,144 x 4
        expected_state_tax=3683.0,
        known_defects=(hi_table_gap(0.072, 76_624, 76_600, 0.40),),
        backend="graph",
    ),
    # Case C25: TI 36,288; row 36,250-36,300, HoH.
    TaxScenario(
        source="HI 2025 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Head_of_House, $45,000 wages, 1 dependents (2025)",
        year=2025,
        state="HI",
        filing_status="Head_of_House",
        w2_income=45000.0,
        dependent_exemptions=2288.0,  # 1,144 x 2
        expected_state_tax=1308.0,
        known_defects=(hi_table_gap(0.068, 36_288, 36_250, -0.20),),
        backend="graph",
    ),
    # Case D25: TI 34,456; row 34,450-34,500, S/MFS.
    TaxScenario(
        source="HI 2025 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Married/Sep, $40,000 wages, 0 dependents (2025)",
        year=2025,
        state="HI",
        filing_status="Married/Sep",
        w2_income=40000.0,
        expected_state_tax=1571.0,
        known_defects=(hi_table_gap(0.068, 34_456, 34_450, 0.20),),
        backend="graph",
    ),
    # Case E25: TI 58,912; row 58,900-58,950, MFJ (QSS).
    TaxScenario(
        source="HI 2025 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Widow(er), $70,000 wages, 1 dependents (2025)",
        year=2025,
        state="HI",
        filing_status="Widow(er)",
        w2_income=70000.0,
        dependent_exemptions=2288.0,  # 1,144 x 2
        expected_state_tax=2461.0,
        known_defects=(hi_table_gap(0.068, 58_912, 58_900, 0.40),),
        backend="graph",
    ),
    # Case F25: TI 14,456; row 14,450-14,500, S/MFS.
    TaxScenario(
        source="HI 2025 official tax table (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Single, $20,000 wages, 0 dependents (2025)",
        year=2025,
        state="HI",
        filing_status="Single",
        w2_income=20000.0,
        expected_state_tax=292.0,
        known_defects=(hi_table_gap(0.055, 14_456, 14_450, 0.0),),
        backend="graph",
    ),
    # Case G25: TI 144,456; Schedule I: 8,391 + 7.9% x 19,456 = 9,928.024.
    TaxScenario(
        source="HI 2025 Rate Schedule (blind derivation)",
        state_evidence=HI_EVIDENCE,
        description="HI Single, $150,000 wages, 0 dependents (2025)",
        year=2025,
        state="HI",
        filing_status="Single",
        w2_income=150000.0,
        expected_state_tax=9928.0,
        known_defects=(hi_schedule_gap(0.20, 9_928.024 - 9_928),),
        backend="graph",
    ),
    # Legacy HI rows, kept for their federal tax/AGI checks only (unverified
    # legacy). Their state expectations omitted the mandatory $1,144
    # exemptions and were retired in b72.33; see HI-2024-2025.md, Part C.
    TaxScenario(
        source="HI 2024 Tax Brackets (computed)",
        description="HI Single, $60,000 income (2024)",
        year=2024,
        state="HI",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5219.0,  # Tax Table row 45,400-45,450
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    TaxScenario(
        source="HI 2024 Tax Brackets (computed)",
        description="HI MFJ, $100,000 income (2024)",
        year=2024,
        state="HI",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    TaxScenario(
        source="HI 2024 Tax Brackets (computed)",
        description="HI HoH, $75,000 income (2024)",
        year=2024,
        state="HI",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    TaxScenario(
        source="HI 2024 Tax Brackets (computed)",
        description="HI Single, $200,000 income (2024)",
        year=2024,
        state="HI",
        filing_status="Single",
        w2_income=200000.0,
        expected_federal_tax=37538.5,
        expected_federal_agi=200000.0,
        backend="graph",
    ),
    TaxScenario(
        source="HI 2025 Tax Brackets (computed)",
        description="HI Single, $60,000 income (2025)",
        year=2025,
        state="HI",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5075.0,  # Tax Table row 44,250-44,300
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    # Maine state scenarios
    # ME 2024 brackets: $26,050 @ 5.8%, $61,600 @ 6.75%, above @ 7.15%
    # ME 2025 brackets: $26,800 @ 5.8%, $63,450 @ 6.75%, above @ 7.15%
    # Personal exemption: $5,000 (2024), $5,150 (2025)
    # Note: exemptions not provided (defaults to 0)
    TaxScenario(
        source="ME 2024 Tax Brackets (computed)",
        description="ME Single, $60,000 income (2024)",
        year=2024,
        state="ME",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5219.0,  # Tax Table row 45,400-45,450
        # Taxable: $60,000 - $14,600 std = $45,400 (no exemption provided)
        # Tax: $26,050 x 5.8% = $1,510.90 + ($45,400 - $26,050) x 6.75% = $1,306.125
        # Total: $2,817.025
        expected_state_tax=2817.025,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
    TaxScenario(
        source="ME 2024 Tax Brackets (computed)",
        description="ME MFJ, $100,000 income (2024)",
        year=2024,
        state="ME",
        filing_status="Married/Joint",
        w2_income=100000.0,
        expected_federal_tax=8035.0,  # Tax Table row 70,800-70,850
        # Taxable: $100,000 - $29,200 std = $70,800 (no exemption provided)
        # Tax: $52,100 x 5.8% = $3,021.80 + ($70,800 - $52,100) x 6.75% = $1,262.25
        # Total: $4,284.05
        expected_state_tax=4284.05,
        expected_federal_agi=100000.0,
        backend="graph",
    ),
    TaxScenario(
        source="ME 2024 Tax Brackets (computed)",
        description="ME HoH, $75,000 income (2024)",
        year=2024,
        state="ME",
        filing_status="Head_of_House",
        w2_income=75000.0,
        expected_federal_tax=6044.0,  # Tax Table row 53,100-53,150
        # Taxable: $75,000 - $21,900 std = $53,100 (no exemption provided)
        # Tax: $39,050 x 5.8% = $2,264.90 + ($53,100 - $39,050) x 6.75% = $948.38
        # Total: $3,213.28
        expected_state_tax=3213.28,
        expected_federal_agi=75000.0,
        backend="graph",
    ),
    TaxScenario(
        source="ME 2024 Tax Brackets (computed)",
        description="ME Single, $150,000 income (2024)",
        year=2024,
        state="ME",
        filing_status="Single",
        w2_income=150000.0,
        expected_federal_tax=25538.5,
        # Taxable: $150,000 - $14,600 = $135,400 (no exemption provided)
        # Tax: $26,050 x 5.8% + ($61,600 - $26,050) x 6.75% + ($135,400 - $61,600) x 7.15%
        # = $1,510.90 + $2,399.63 + $5,276.70 = $9,187.23
        expected_state_tax=9187.23,
        expected_federal_agi=150000.0,
        backend="graph",
    ),
    TaxScenario(
        source="ME 2025 Tax Brackets (computed)",
        description="ME Single, $60,000 income (2025)",
        year=2025,
        state="ME",
        filing_status="Single",
        w2_income=60000.0,
        expected_federal_tax=5075.0,  # Tax Table row 44,250-44,300
        # Taxable: $60,000 - $15,750 std (OBBBA) = $44,250 (no exemption provided)
        # Tax: $26,800 x 5.8% = $1,554.40 + ($44,250 - $26,800) x 6.75% = $1,177.88
        # Total: $2,732.28
        expected_state_tax=2732.28,
        expected_federal_agi=60000.0,
        backend="graph",
    ),
]
