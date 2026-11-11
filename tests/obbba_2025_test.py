"""2025 OBBBA federal changes: SALT cap phase-down, Schedule 1-A, age 65+.

Every expected value below is an **independent derivation**, copied from the blind
source-only inventory

    docs/validation/derivations/obbba-2025.md

(derived 2026-11-02 from IRS 2025 forms and instructions without access to tenforty,
all sources retrieved 2026-11-02), as qualified by its "Adjudication" section and the
independent IRS-only spot-check obbba-2025-review.md beside it. Each case
names its inventory example id (S-1, T-1, ...) and the IRS form line it rests on.
Where a case carries an example's form-line amount to Form 1040 line 15, the step is
the form's own arithmetic, shown inline: line 14 = 12e + 13a + 13b and
line 15 = 11b - 14 (f1040 2025 p.2), with 12e from the 2025 standard deduction
amounts (f1040 2025 p.2 margin; i1040gi 2025 PDF p.35 age-65 chart) and, for forced
itemizers with no other Schedule A entries, 12e = Schedule A line 17 = line 5e.

Common facts for every case (inventory Section 3 assumptions): W-2 wages are the only
income, so AGI = 1040 line 11a = 11b = MAGI (no Puerto Rico, Form 2555 or 4563
exclusions); no adjustments, credits, dependents, QBI or other Schedule 2 taxes;
every person has a valid SSN; nobody is blind or claimable as a dependent; qualified
tips and FLSA overtime premium are already inside W-2 wages.

Tolerances: Schedule 1-A tips/overtime round down and car-loan interest rounds up,
explicitly on the form (lines 11, 19, 28), and every example here has whole-dollar
inputs and whole-dollar results with no fractional intermediate, so amounts are
asserted to the cent. The Tax Table (mandatory under $100,000 of taxable income) is a
published whole-dollar figure, also asserted to the cent.

Not testable through the public API, so deliberately absent:
- Line 1i nontaxable combat pay staying out of AGI: there is no combat-pay input.
- T-4 (self-employed tips limited to net income) and O-3 (the /3 overtime estimation
  method): published illustrations of qualified-amount judgment, not computations;
  qualified amounts are caller inputs.
- Self-employed / 1099 tips and overtime (Schedule 1-A lines 5, 14b), non-SSN
  filers, and the Schedule 8812 CTC example K-1: no inputs for them here.
"""

import pytest

from tenforty import evaluate_return, evaluate_returns

INVENTORY = "obbba-2025-inventory.md (independent derivation, retrieved 2026-11-02)"

CENTS = 0.01

BACKENDS = [
    pytest.param("graph", marks=pytest.mark.requires_graph, id="graph"),
    pytest.param("ots", id="ots"),
]

# Read from the vendored OTS 2025 source (taxsolve_US_1040_2025.c, OTS 23.06), not
# from running it. Per AGENTS.md these are upstream defects: report upstream, record
# here as strict xfail. raises=AssertionError keeps a missing input (TypeError) red.
OTS_MFS_SALT_WORKSHEET_TRIGGER = (
    "OTS 2025 enters the Schedule A SALT worksheet only when line 11b > $500,000 for "
    "every status; for MFS the threshold is $250,000 (i1040sca 2025 PDF p.7, "
    "worksheet line 5), so MFS between $250,000 and $500,000 keeps the flat $20,000 cap"
)
OTS_CAR_LOAN_PHASEOUT = (
    "OTS 2025 Schedule 1-A Part IV truncates line 28 instead of rounding up and "
    "multiplies by $300 instead of $200 at line 29 (f1040s1a 2025 lines 28-29)"
)


def _ots_defect(reason):
    return pytest.mark.xfail(strict=True, raises=AssertionError, reason=reason)


def _case(*values, case_id, ots_defect=None):
    return pytest.param(*values, ots_defect, id=case_id)


def _expect_ots_defect(request, backend, ots_defect):
    """Mark the OTS run of a case strict xfail; the graph run of it must pass."""
    if backend == "ots" and ots_defect is not None:
        request.applymarker(_ots_defect(ots_defect))


def _evaluate(backend, **inputs):
    return evaluate_return(year=2025, backend=backend, **inputs, allow_unvalidated=True)


# --- SALT cap with MAGI phase-down (Schedule A line 5e; i1040sca 2025 PDF p.7) ----
#
# Columns: filing status, wages (= AGI), Schedule A lines 5a-5d total,
# expected line 5e, expected 1040 line 15 = AGI - 5e (forced itemizer, nothing else
# on Schedule A). Itemizing is forced because several 5e amounts fall below the
# standard deduction.
SALT_CASES = [
    # S-1: WS6 = 60,000; WS7 = 18,000; WS8 = 22,000; 5e = 22,000.
    _case("Married/Joint", 560_000.0, 52_500.0, 22_000.0, 538_000.0, case_id="S-1-MFJ"),
    # S-2: WS8 = 40,000 - 42,000 < 10,000 floor; 5e = 10,000.
    _case(
        "Single", 640_000.0, 60_000.0, 10_000.0, 630_000.0, case_id="S-2-single-floor"
    ),
    # S-3: WS8 = 25,000; MFS halves line 9 at line 10; 5e = 12,500.
    _case(
        "Married/Sep",
        300_000.0,
        25_000.0,
        12_500.0,
        287_500.0,
        case_id="S-3-MFS-in-range",
        ots_defect=OTS_MFS_SALT_WORKSHEET_TRIGGER,
    ),
    # S-4: WS8 = -5,000 -> floor 10,000; half = 5,000.
    _case(
        "Married/Sep",
        400_000.0,
        30_000.0,
        5_000.0,
        395_000.0,
        case_id="S-4-MFS-floor",
        ots_defect=OTS_MFS_SALT_WORKSHEET_TRIGGER,
    ),
    # S-5: AGI exactly 500,000 skips the worksheet; 5e = min(45,000, 40,000).
    _case(
        "Head_of_House",
        500_000.0,
        45_000.0,
        40_000.0,
        460_000.0,
        case_id="S-5-HOH-at-500k",
    ),
    # S-5: MFJ at 600,000 reaches the floor exactly. The example gives no 5d; any
    # 5d >= 10,000 gives 10,000 (Codex spot-check), here 52,500 as in S-1.
    _case(
        "Married/Joint",
        600_000.0,
        52_500.0,
        10_000.0,
        590_000.0,
        case_id="S-5-MFJ-at-600k",
    ),
]


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize(
    ("filing_status", "wages", "salt_paid", "line_5e", "taxable_income", "ots_defect"),
    SALT_CASES,
)
def test_salt_cap_phase_down(
    request,
    backend,
    filing_status,
    wages,
    salt_paid,
    line_5e,
    taxable_income,
    ots_defect,
):
    """Schedule A line 5e follows the 2025 State and Local Tax Deduction Worksheet.

    Independent derivation, inventory Section 3.1 (examples S-1 to S-5), 2025
    Schedule A line 5e and i1040sca 2025 PDF p.7 worksheet lines 1-10.
    """
    _expect_ots_defect(request, backend, ots_defect)
    result = _evaluate(
        backend,
        filing_status=filing_status,
        w2_income=wages,
        state_and_local_taxes=salt_paid,
        standard_or_itemized="Itemized",
    )

    assert result.federal_adjusted_gross_income == pytest.approx(wages, abs=CENTS)
    assert wages - result.federal_taxable_income == pytest.approx(line_5e, abs=CENTS)
    assert result.federal_taxable_income == pytest.approx(taxable_income, abs=CENTS)


@pytest.mark.parametrize("backend", BACKENDS)
def test_other_itemized_deductions_sit_outside_the_salt_cap(backend):
    """`itemized_deductions` adds to Schedule A line 17 beside the capped line 5e.

    Independent derivation from inventory S-2 (line 5e = 10,000 at AGI 640,000,
    Single) plus $5,000 of other itemized deductions, which the 2025 Schedule A adds
    after line 5e (lines 16-17): line 17 = 15,000, line 15 = 640,000 - 15,000.
    """
    result = _evaluate(
        backend,
        filing_status="Single",
        w2_income=640_000.0,
        state_and_local_taxes=60_000.0,
        itemized_deductions=5_000.0,
        standard_or_itemized="Itemized",
    )

    assert result.federal_taxable_income == pytest.approx(625_000.0, abs=CENTS)


# --- Schedule 1-A Parts II-V -> Form 1040 line 13b (f1040s1a 2025) ---------------
#
# Under-65 standard deduction (f1040 2025 p.2): Single/MFS 15,750, MFJ/QSS 31,500,
# HOH 23,625. Age-65 chart (i1040gi 2025 PDF p.35): Single 1 box 17,750; MFJ 2 boxes
# 34,700; QSS 1 box 33,100; MFS 1 box 17,350.
#
# Columns: filing status, wages (= AGI), keyword inputs, expected Schedule 1-A
# line 38 (= 1040 line 13b), 1040 line 12e standard deduction. Expected 1040
# line 15 = AGI - 12e - 13b.
SCHEDULE_1A_CASES = [
    # Part II tips (i1040gi 2025 pp.101-105). T-1: L10 12,500; L11 floor 12; L13 18,800.
    _case(
        "Single",
        162_500.0,
        {"qualified_tips": 20_000.0},
        18_800.0,
        15_750.0,
        case_id="T-1-tips-single-in-range",
    ),
    # T-2: L7 combined cap 25,000; L12 26,000; L13 0.
    _case(
        "Married/Joint",
        560_000.0,
        {"qualified_tips": 30_000.0},
        0.0,
        31_500.0,
        case_id="T-2-tips-MFJ-above-range",
    ),
    # T-3: L10 1,999; L11 floor(1.999) = 1; L13 7,900.
    _case(
        "Head_of_House",
        151_999.0,
        {"qualified_tips": 8_000.0},
        7_900.0,
        23_625.0,
        case_id="T-3-tips-HOH-truncation",
    ),
    # Part III overtime (i1040gi 2025 pp.106-108). O-1: L15 25,000; L19 25; L21 22,500.
    _case(
        "Married/Joint",
        325_750.0,
        {"qualified_overtime": 30_000.0},
        22_500.0,
        31_500.0,
        case_id="O-1-overtime-MFJ-in-range",
    ),
    # O-2: L18 125,000; L20 12,500 > L15 12,000; L21 0.
    _case(
        "Single",
        275_000.0,
        {"qualified_overtime": 12_000.0},
        0.0,
        15_750.0,
        case_id="O-2-overtime-single-above-range",
    ),
    # O-4: MFS may not claim overtime, "whatever the amounts"; amounts chosen here.
    _case(
        "Married/Sep",
        100_000.0,
        {"qualified_overtime": 10_000.0},
        0.0,
        15_750.0,
        case_id="O-4-overtime-MFS-disallowed",
    ),
    # Part IV car-loan interest (i1040gi 2025 pp.108-110). C-1: L27 12,300; L28
    # ceil 13; L29 2,600; L30 1,900.
    _case(
        "Single",
        112_300.0,
        {"vehicle_loan_interest": 4_500.0},
        1_900.0,
        15_750.0,
        case_id="C-1-car-loan-single-in-range",
        ots_defect=OTS_CAR_LOAN_PHASEOUT,
    ),
    # C-2: L24 cap 10,000; L27 15,000; L28 15; L29 3,000; L30 7,000.
    _case(
        "Married/Joint",
        215_000.0,
        {"vehicle_loan_interest": 12_000.0},
        7_000.0,
        31_500.0,
        case_id="C-2-car-loan-MFJ-cap",
        ots_defect=OTS_CAR_LOAN_PHASEOUT,
    ),
    # C-3: MFS eligible at the $100,000 threshold; L28 60; L29 12,000; L30 0.
    _case(
        "Married/Sep",
        160_000.0,
        {"vehicle_loan_interest": 9_000.0},
        0.0,
        15_750.0,
        case_id="C-3-car-loan-MFS-above-range",
    ),
    # Part V seniors (i1040gi 2025 p.110). E-2: L35 4,200 per spouse; L37 8,400.
    _case(
        "Married/Joint",
        180_000.0,
        {"filer_65_or_older": True, "spouse_65_or_older": True},
        8_400.0,
        34_700.0,
        case_id="E-2-senior-MFJ-both-in-range",
    ),
    # E-3: L34 6,600 > 6,000; L37 0.
    _case(
        "Married/Joint",
        260_000.0,
        {"filer_65_or_older": True, "spouse_65_or_older": True},
        0.0,
        34_700.0,
        case_id="E-3-senior-MFJ-both-above-range",
    ),
    # E-4: L34 6,300; L37 0.
    _case(
        "Single",
        180_000.0,
        {"filer_65_or_older": True},
        0.0,
        17_750.0,
        case_id="E-4-senior-single-above-range",
    ),
    # E-5: QSS uses the $75,000 single threshold; L35 4,500.
    _case(
        "Widow(er)",
        100_000.0,
        {"filer_65_or_older": True},
        4_500.0,
        33_100.0,
        case_id="E-5-senior-QSS-single-threshold",
    ),
    # E-6: MFS may not claim the senior deduction; the age-65 standard deduction
    # (MFS, 1 box) still applies.
    _case(
        "Married/Sep",
        50_000.0,
        {"filer_65_or_older": True},
        0.0,
        17_350.0,
        case_id="E-6-senior-MFS-disallowed",
    ),
]


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize(
    ("filing_status", "wages", "inputs", "line_13b", "line_12e", "ots_defect"),
    SCHEDULE_1A_CASES,
)
def test_schedule_1a_deduction_reaches_line_15(
    request, backend, filing_status, wages, inputs, line_13b, line_12e, ots_defect
):
    """Schedule 1-A line 38 enters 1040 line 13b below AGI, beside the standard deduction.

    Independent derivation, inventory Sections 3.2-3.5 (ids in the case ids), 2025
    Schedule 1-A Parts II-V and Form 1040 lines 11b-15. AGI is unchanged: the
    deduction is not an adjustment to income (inventory Section 2.5).
    """
    _expect_ots_defect(request, backend, ots_defect)
    result = _evaluate(backend, filing_status=filing_status, w2_income=wages, **inputs)

    assert result.federal_adjusted_gross_income == pytest.approx(wages, abs=CENTS)
    assert result.federal_taxable_income == pytest.approx(
        wages - line_12e - line_13b, abs=CENTS
    )


@pytest.mark.parametrize("backend", BACKENDS)
def test_senior_deduction_single_whole_return(backend):
    """E-1: Single, born 1955, AGI 95,000, standard deduction, ordinary income only.

    Independent derivation, inventory Section 3.2 example E-1: Schedule 1-A L33
    20,000, L34 1,200, L37 4,800; 1040 line 12e 17,750 (i1040gi 2025 PDF p.35);
    line 14 22,550; line 15 72,450. Line 16 is the published 2025 Tax Table row
    72,450-72,500 Single (i1040gi 2025 PDF p.76) = 10,859. Per the adjudication, the
    whole-return figure holds for W-2 income only with no QBI, credits or other
    taxes, so line 24 = line 16.
    """
    result = _evaluate(
        backend, filing_status="Single", w2_income=95_000.0, filer_65_or_older=True
    )

    assert result.federal_adjusted_gross_income == pytest.approx(95_000.0, abs=CENTS)
    assert result.federal_taxable_income == pytest.approx(72_450.0, abs=CENTS)
    assert result.federal_total_tax == pytest.approx(10_859.0, abs=CENTS)


@pytest.mark.parametrize("backend", BACKENDS)
def test_combined_schedule_1a_return_with_amt_add_back(backend):
    """F-1: Single, born 1959, $90,000 W-2 incl. $12,000 tips and $3,000 FLSA premium.

    Independent derivation, inventory Section 3.6 example F-1 (confirmed by the
    Codex spot-check): AGI stays 90,000; 12e 17,750; Schedule 1-A L13 12,000,
    L21 3,000, L37 5,100, L38 20,100; line 14 37,850; line 15 52,150; line 16 from
    the published 2025 Tax Table row 52,150-52,200 Single (i1040gi 2025 PDF p.74) =
    6,393; no credits or other taxes, so line 24 = 6,393. Form 6251 (2025) line 1a
    = 37,850 - 5,100 adds back only the senior deduction: AMTI 75,000 < exemption
    88,100, so AMT = 0.
    """
    result = _evaluate(
        backend,
        filing_status="Single",
        w2_income=90_000.0,
        qualified_tips=12_000.0,
        qualified_overtime=3_000.0,
        filer_65_or_older=True,
    )

    assert result.federal_adjusted_gross_income == pytest.approx(90_000.0, abs=CENTS)
    assert result.federal_taxable_income == pytest.approx(52_150.0, abs=CENTS)
    assert result.federal_total_tax == pytest.approx(6_393.0, abs=CENTS)
    assert result.federal_amt == pytest.approx(0.0, abs=CENTS)


@pytest.mark.parametrize("backend", BACKENDS)
def test_batch_api_accepts_the_new_inputs(backend):
    """evaluate_returns carries the new inputs row by row (zip mode).

    Independent derivation: row 1 is inventory T-1 (line 15 = 127,950), row 2 is
    inventory E-2 (line 15 = 136,900), as in the Schedule 1-A cases above.
    """
    result = evaluate_returns(
        year=2025,
        filing_status=["Single", "Married/Joint"],
        w2_income=[162_500.0, 180_000.0],
        qualified_tips=[20_000.0, 0.0],
        filer_65_or_older=[False, True],
        spouse_65_or_older=[False, True],
        backend=backend,
        mode="zip",
    )

    assert result["federal_taxable_income"].to_list() == pytest.approx(
        [127_950.0, 136_900.0], abs=CENTS
    )


# --- Cases added during implementation ---------------------------------------------
#
# Two further OTS 2025 source defects turned out to be reachable through the public
# inputs. Their expected values are independent derivations by the implementing agent
# (Claude Opus 5.5, 2026-11-02) from the inventory's transcription of the IRS form
# lines (Sections 2.5c, 2.8 and 2.1), without running either backend on these cases.

OTS_MFS_SKIPS_CAR_LOAN = (
    "OTS 2025 runs Schedule 1-A Parts II-V only when the status is not MFS, so MFS "
    "never gets Part IV car loan interest, which MFS may claim (i1040gi 2025 p.109: "
    "'all other filing statuses -- $100,000')"
)
OTS_AMT_SENIOR_ADD_BACK = (
    "OTS 2025 Form 6251 line 1a subtracts a local `sched1a_l38` that is never set "
    "(always 0), so the Schedule 1-A line 37 senior deduction is not added back "
    "(f6251 2025 line 1a; i6251 2025 p.1)"
)


@pytest.mark.parametrize("backend", BACKENDS)
def test_married_filing_separately_claims_car_loan_interest(request, backend):
    """MFS, AGI 100,000, $5,000 of qualified car loan interest.

    Independent derivation (inventory Section 2.5c): MFS may claim Part IV with the
    $100,000 threshold. Line 24 = min(5,000, 10,000) = 5,000; line 27 = 100,000 -
    100,000 = 0, so line 30 = line 24 = 5,000 (the form's "if zero or less" branch).
    Line 15 = 100,000 - 15,750 - 5,000 = 79,250.
    """
    _expect_ots_defect(request, backend, OTS_MFS_SKIPS_CAR_LOAN)
    result = _evaluate(
        backend,
        filing_status="Married/Sep",
        w2_income=100_000.0,
        vehicle_loan_interest=5_000.0,
    )

    assert result.federal_taxable_income == pytest.approx(79_250.0, abs=CENTS)


@pytest.mark.parametrize("backend", BACKENDS)
def test_amt_adds_back_the_senior_deduction(request, backend):
    """Single, born 1959, W-2 90,000, a $200,000 ISO exercise spread (AMT binds).

    Independent derivation (inventory Sections 2.1, 2.5d, 2.8): 12e 17,750; Schedule
    1-A L33 15,000, L34 900, L37 = L38 = 5,100; 1040 line 14 = 22,850. Form 6251
    line 1a = 22,850 - 5,100 = 17,750; line 1b = 90,000 - 17,750 = 72,250; line 2a =
    12e = 17,750; ISO 200,000; line 4 AMTI = 290,000; line 5 exemption 88,100 (AMTI
    below 626,350); line 6 = 201,900, under the 239,100 26% breakpoint, so line 7 =
    0.26 x 201,900 = 52,494.00. With AMT owed, line 24 = line 16 + AMT = the
    tentative minimum tax, whatever the Tax Table gives for line 16; Medicare wages
    of 90,000 are under the 8959 threshold and there are no other taxes or credits.
    Without the add-back, AMTI would be 284,900 and line 24 = 51,168.00.
    """
    _expect_ots_defect(request, backend, OTS_AMT_SENIOR_ADD_BACK)
    result = _evaluate(
        backend,
        filing_status="Single",
        w2_income=90_000.0,
        incentive_stock_option_gains=200_000.0,
        filer_65_or_older=True,
    )

    assert result.federal_taxable_income == pytest.approx(67_150.0, abs=CENTS)
    assert result.federal_amt > 0
    assert result.federal_total_tax == pytest.approx(52_494.0, abs=CENTS)


# --- Year guard --------------------------------------------------------------------

SCHEDULE_1A_INPUTS = ["qualified_tips", "qualified_overtime", "vehicle_loan_interest"]


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize("name", SCHEDULE_1A_INPUTS)
@pytest.mark.without_ots
def test_schedule_1a_inputs_are_refused_before_2025(backend, name):
    """Schedule 1-A first exists for 2025 (P.L. 119-21 §§70201-70203); 2024 refuses it."""
    with pytest.raises(ValueError, match=name):
        evaluate_return(
            year=2024,
            filing_status="Single",
            w2_income=100_000.0,
            backend=backend,
            **{name: 1_000.0},
            allow_unvalidated=True,
        )


@pytest.mark.parametrize("backend", BACKENDS)
@pytest.mark.parametrize(
    ("filing_status", "wages", "flags", "taxable_income"),
    [
        # Chart: Single, 1 box = $16,550.
        pytest.param(
            "Single",
            60_000.0,
            {"filer_65_or_older": True},
            60_000.0 - 16_550.0,
            id="2024-single-65",
        ),
        # Chart: Married filing jointly, 2 boxes = $32,300.
        pytest.param(
            "Married/Joint",
            100_000.0,
            {"filer_65_or_older": True, "spouse_65_or_older": True},
            100_000.0 - 32_300.0,
            id="2024-MFJ-both-65",
        ),
    ],
)
def test_age_65_flags_raise_the_2024_standard_deduction(
    backend, filing_status, wages, flags, taxable_income
):
    """In 2024 the age flags act only through the additional standard deduction.

    Published table row: "Standard Deduction Chart for People Who Were Born Before
    January 2, 1960, or Were Blind", 2024 Instructions for Form 1040, printed p.34,
    https://www.irs.gov/pub/irs-prior/i1040gi--2024.pdf, retrieved 2026-11-02 for
    this test (not from the OBBBA inventory). No senior deduction exists for 2024.
    """
    result = evaluate_return(
        year=2024,
        filing_status=filing_status,
        w2_income=wages,
        backend=backend,
        **flags,
        allow_unvalidated=True,
    )

    assert result.federal_taxable_income == pytest.approx(taxable_income, abs=CENTS)
