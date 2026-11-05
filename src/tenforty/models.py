"""Configuration to map from taxpayer space to tax-return space.

This module contains a set of classes used for the configuration and setup of the
tenforty application. The classes are designed to be self-explanatory and are
used primarily for structuring and organizing configuration data.
"""

# ruff: noqa: D101
import json
import math
import warnings
from collections.abc import Callable
from enum import Enum
from functools import partial

from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    FiniteFloat,
    PrivateAttr,
    TypeAdapter,
    computed_field,
    model_validator,
)

from . import _ots_form_models

try:
    from enum import StrEnum
except ImportError:

    class StrEnum(str, Enum):
        def __str__(self):  # noqa: D105
            return self.value


class OTSErrorPolicy(StrEnum):
    RAISE = "raise"
    WARN = "warn"
    IGNORE = "ignore"


class OTSError(Exception):
    """Raised when OTS returns a non-zero exit code."""

    def __init__(  # noqa: D107
        self,
        exit_code: int,
        year: int,
        form: str,
        message: str | None = None,
    ):
        self.exit_code = exit_code
        self.year = year
        self.form = form
        super().__init__(
            message or f"OTS returned exit code {exit_code} for {year}/{form}"
        )


class OTSParseError(Exception):
    """Raised when OTS output cannot be parsed."""

    def __init__(self, message: str, raw_output: str | None = None):  # noqa: D107
        self.raw_output = raw_output
        super().__init__(message)


class UnsupportedReturnError(ValueError):
    """Raised before evaluation when no requested backend can compute the return.

    It covers an unsupported year, state or input concept for the selected
    backend, and is raised whatever ``on_error`` says: it describes the request,
    not a failed computation.
    """


class UnsupportedInputError(UnsupportedReturnError, NotImplementedError):
    """Raised when a backend does not compute a nonzero input it was given."""


class BackendUnavailableError(ImportError, RuntimeError):
    """Raised when the selected backend's compiled extension is not installed."""


def validate_on_error(on_error: str) -> OTSErrorPolicy:
    """Return the policy for ``on_error``, rejecting anything not a policy."""
    try:
        return OTSErrorPolicy(on_error)
    except ValueError:
        choices = ", ".join(repr(policy.value) for policy in OTSErrorPolicy)
        raise ValueError(
            f"Unknown on_error policy: {on_error!r}; expected one of {choices}"
        ) from None


class OutputFieldSpec(BaseModel):
    """Specification for validating an OTS output field."""

    name: str
    ots_key: str
    required: bool = True
    min_value: float | None = None
    max_value: float | None = None


_BASE_1040_OUTPUT_FIELDS = [
    OutputFieldSpec(name="taxable_income", ots_key="L15", required=True, min_value=0),
    OutputFieldSpec(name="total_tax", ots_key="L24", required=True, min_value=0),
    OutputFieldSpec(
        name="tax_bracket",
        ots_key="tax_bracket",
        required=False,
        min_value=0,
        max_value=37,
    ),
    OutputFieldSpec(
        name="effective_tax_rate",
        ots_key="effective_tax_rate",
        required=False,
        min_value=0,
        max_value=100,
    ),
]


def federal_1040_output_fields(year: int) -> list[OutputFieldSpec]:
    """Return 1040 output field specs with the correct AGI key for the given year."""
    agi_key = "L11b" if year >= 2025 else "L11"
    return [
        OutputFieldSpec(name="agi", ots_key=agi_key, required=True, min_value=0),
        *_BASE_1040_OUTPUT_FIELDS,
    ]


class OTSYear(Enum):
    YEAR_2018 = 2018
    YEAR_2019 = 2019
    YEAR_2020 = 2020
    YEAR_2021 = 2021
    YEAR_2022 = 2022
    YEAR_2023 = 2023
    YEAR_2024 = 2024
    YEAR_2025 = 2025


class OTSFilingStatus(StrEnum):
    SINGLE = "Single"
    MARRIED_JOINT = "Married/Joint"
    HEAD_OF_HOUSEHOLD = "Head_of_House"
    MARRIED_SEPARATE = "Married/Sep"
    WIDOW_WIDOWER = "Widow(er)"


class OTSDeductionType(StrEnum):
    AUTO = "Auto"
    STANDARD = "Standard"
    ITEMIZED = "Itemized"


# Graph encoding of the election on us_1040_DeductionChoice: negative forces
# the standard deduction, positive forces Schedule A, zero takes the larger.
DEDUCTION_CHOICE_ENCODING = {"Auto": 0.0, "Standard": -1.0, "Itemized": 1.0}

# Public inputs that reach the OTS federal return through a computed field,
# so they can be emptied when the standard deduction is forced.
OTS_INPUT_CARRIERS = {
    "itemized_deductions": "schedule_a_other_deductions",
    "state_and_local_taxes": "schedule_a_state_and_local_taxes",
}

# OTS input keys that a year's program accepts but never uses, so a natural
# mapped to one is dropped. The 2018 and 2019 US_1040 programs keep an AMTwsN
# line only when 3 <= N < 3, which no N satisfies, and their `amtws[3]`
# assignment is commented out; 2020 onward read AMTws3 with GetLine. AMTws3 is
# where incentive_stock_option_gains is mapped (Form 6251 line 2i).
OTS_UNREAD_INPUT_KEYS: dict[tuple[int, str], frozenset[str]] = {
    (2018, "US_1040"): frozenset({"AMTws3"}),
    (2019, "US_1040"): frozenset({"AMTws3"}),
}


class OTSState(Enum):
    NONE = None

    AL = "AL"
    AR = "AR"
    AZ = "AZ"
    CA = "CA"
    CO = "CO"
    CT = "CT"
    DC = "DC"
    DE = "DE"
    GA = "GA"
    HI = "HI"
    IA = "IA"
    ID = "ID"
    IL = "IL"
    IN = "IN"
    KS = "KS"
    KY = "KY"
    LA = "LA"
    MA = "MA"
    MD = "MD"
    ME = "ME"
    MI = "MI"
    MS = "MS"
    MN = "MN"
    MO = "MO"
    MT = "MT"
    NC = "NC"
    ND = "ND"
    NE = "NE"
    NH = "NH"
    NJ = "NJ"
    NM = "NM"
    NY = "NY"
    OH = "OH"
    OK = "OK"
    OR = "OR"
    PA = "PA"
    RI = "RI"
    SC = "SC"
    UT = "UT"
    VA = "VA"
    VT = "VT"
    WV = "WV"
    WI = "WI"

    # No income-tax states are easy to support! :)
    AK = "AK"
    FL = "FL"
    NV = "NV"
    SD = "SD"
    TN = "TN"
    TX = "TX"
    WA = "WA"
    WY = "WY"


STATE_TO_FORM = {
    OTSState.NONE: None,
    #
    OTSState.AL: "AL_40",
    OTSState.AR: "AR_AR1000F",
    OTSState.AZ: "AZ_140",
    OTSState.CA: "CA_540",
    OTSState.CO: "CO_Form104",
    OTSState.CT: "CT_1",
    OTSState.DC: "DC_D40",
    OTSState.DE: "DE_PIT_RES",
    OTSState.GA: "GA_500",
    OTSState.HI: "HI_N11",
    OTSState.IA: "IA_IA1040",
    OTSState.ID: "ID_FORM40",
    OTSState.IL: "IL_1040",
    OTSState.IN: "IN_IT40",
    OTSState.KS: "KS_K40",
    OTSState.KY: "KY_740",
    OTSState.LA: "LA_IT540",
    OTSState.MA: "MA_1",
    OTSState.MD: "MD_502",
    OTSState.ME: "ME_1040ME",
    OTSState.MI: "MI_1040",
    OTSState.MS: "MS_80105",
    OTSState.MN: "MN_M1",
    OTSState.MO: "MO_1040",
    OTSState.MT: "MT_Form2",
    OTSState.NC: "NC_D400",
    OTSState.ND: "ND_1",
    OTSState.NE: "NE_1040N",
    OTSState.NH: "NH_DP10",
    OTSState.NJ: "NJ_1040",
    OTSState.NM: "NM_PIT1",
    OTSState.NY: "NY_IT201",
    OTSState.OH: "OH_IT1040",
    OTSState.OK: "OK_511",
    OTSState.OR: "OR_40",
    OTSState.PA: "PA_40",
    OTSState.RI: "RI_1040",
    OTSState.SC: "SC_1040",
    OTSState.UT: "UT_TC40",
    OTSState.VA: "VA_760",
    OTSState.VT: "VT_IN111",
    OTSState.WV: "WV_IT140",
    OTSState.WI: "WI_Form1",
    # No income-tax states
    OTSState.AK: None,
    OTSState.FL: None,
    OTSState.NV: None,
    OTSState.SD: None,
    OTSState.TN: None,
    OTSState.TX: None,
    OTSState.WA: None,
    OTSState.WY: None,
}


class OTSFieldTerminator(Enum):
    SEMICOLON = "semicolon"
    NEWLINE = "newline"


class OTSField(BaseModel):
    """Typically, represents one line of input in a tax form."""

    key: str
    default: int | float | str | None = None
    terminator: OTSFieldTerminator = OTSFieldTerminator.SEMICOLON


class OTSForm(BaseModel):
    form_id: str
    year: int
    fields: list[OTSField]


class NaturalFormMapping(BaseModel):
    form_id: str
    year: int
    input_map: dict[str, Callable | str]
    output_map: dict[str, str]
    fed_import_map: dict[str, str] = {}
    # Applied to a natural output after `output_map` names it, for a quantity the
    # form defines from an OTS line rather than reporting the line itself.
    output_transforms: dict[str, Callable[[float], float]] = {}


class SubordinateFormConfig(BaseModel):
    year: int
    form_id: str
    phase: int
    input_map: dict[str, Callable | str]
    export_map: dict[str, str] = {}
    fed_import_map: dict[str, str] = {}
    output_map: dict[str, str] = {}
    total_tax_key: str | None = None
    defaults: dict[str, str] = {}
    # Naturals this form consumes through `fed_import_map` rather than directly.
    # Activation is decided on the inputs a form consumes, and a value arriving
    # from the federal return is invisible to `input_map` — so without this a
    # filer whose only investment income is capital gains would never trigger
    # Form 8960 at all.
    activation_naturals: list[str] = []


OTS_FORM_CONFIG = dict(
    ((form.year, form.form_id), form)
    for form in (OTSForm(**e) for e in _ots_form_models.OTS_FORM_CONFIG)
)


SCHEDULE_1A_FIRST_YEAR = 2025
SCHEDULE_1A_INPUTS = ("qualified_tips", "qualified_overtime", "vehicle_loan_interest")
_SPOUSE_BOX_STATUSES = frozenset(
    {OTSFilingStatus.MARRIED_JOINT, OTSFilingStatus.MARRIED_SEPARATE}
)


class TaxReturnInput(BaseModel):
    model_config = ConfigDict(allow_inf_nan=False)

    _state_facts: dict[str, object] = PrivateAttr(default_factory=dict)
    """The state facts active for this return (tenforty.state_facts), set by the
    public API after validation; read by the graph backend's fact lowerings."""

    year: OTSYear = OTSYear.YEAR_2025
    state: OTSState = OTSState.NONE
    filing_status: OTSFilingStatus = OTSFilingStatus.SINGLE
    num_dependents: int = Field(
        default=0,
        description=(
            "Total number of dependents claimed on the return (the Form 1040 "
            "Dependents section): qualifying children and qualifying relatives "
            "alike. Each is either a child tax credit qualifying child (see "
            "num_ctc_dependents) or gets the $500 credit for other dependents "
            "(Schedule 8812 lines 6-7). Both credits share the phase-out and the "
            "tax-liability limit, and together are federal_child_tax_credit (Form "
            "1040 line 19). Applied for 2023-2025 on the OTS backend and 2024-2025 "
            "on the graph backend; for 2018-2022 OTS reads the count but applies "
            "neither credit. Not modeled: the refundable additional child tax "
            "credit (a payment, not part of total tax), the earned income credit "
            "and the dependent care credit. Some state returns also read this "
            "count as their dependent count (graph: CA, IL, MS, NY, OH, OR and WI; OTS: the state "
            "forms that map it); graph returns for other states take dependents "
            "through dependent_exemptions. The OR graph uses it for its regular "
            "exemption credit and derives the taxpayer/spouse base from filing "
            "status; dependent_exemptions is not mapped for OR, and additional "
            "Oregon exemptions, including severe-disability exemptions, are "
            "unsupported. The NY graph uses the same total independently for its "
            "household credit. The OH graph uses the total for MAGI-tiered "
            "personal/dependent exemptions and the exemption credit, adding the "
            "filing-status taxpayer/spouse base; dependent_exemptions is not "
            "mapped for OH. OH claimable-filer and business-income adjustments "
            "are unsupported. The MS graph adds $1,500 per total dependent to "
            "the additional exemption amount, with the form's half-share rule "
            "for MFS. Conversion to NY line 36 exemption dollars is not "
            "mapped. Natural NY graph MFS returns require spouse AGI/dependents "
            "available only as raw graph inputs and are unsupported. Claimable "
            "filer and A6 household-credit controls are also raw graph only."
        ),
    )
    num_ctc_dependents: int | None = Field(
        default=None,
        ge=0,
        description=(
            "How many of num_dependents are qualifying children for the federal "
            "child tax credit: under 17 at the end of the year, each with a valid "
            "SSN, and meeting the qualifying-child tests (Schedule 8812 line 4; "
            "$2,000 each for 2024, $2,200 for 2025). Unset means every dependent "
            "is one. Set it lower when some dependents only get the $500 credit "
            "for other dependents: children 17 or older (including students), "
            "dependents without an SSN valid for employment (e.g. with an ITIN or "
            "ATIN), and qualifying relatives such as a parent. For example, two "
            "children aged 10 and 18 are num_dependents=2, num_ctc_dependents=1. "
            "The NC graph uses this same qualifying-child count for D-400 line "
            "10a and its income-tiered line 10b child deduction; other dependents "
            "do not qualify. The default to num_dependents applies there too. "
            "Must not exceed num_dependents (ValueError). For 2025, OBBBA also "
            "requires an SSN for the filer (at least one spouse on a joint "
            "return) to claim the child tax credit; that is not checked."
        ),
    )
    standard_or_itemized: OTSDeductionType = Field(
        default=OTSDeductionType.AUTO,
        description=(
            "The federal Form 1040 deduction election. 'Auto' (the default) takes "
            "the larger of the standard deduction and Schedule A. 'Standard' "
            "forces the standard deduction even when Schedule A is larger; "
            "'Itemized' forces Schedule A even when it is smaller or zero (IRC "
            "63(e); Schedule A line 18). Before 'Auto' existed, 'Standard' meant "
            "the larger of the two. The election sets Form 6251 line 2a. It does "
            "not choose a state's deduction method: states apply their own rules "
            "to the same inputs, or import the federal result. On the OTS "
            "backend, 'Standard' with a nonzero Schedule A input raises "
            "UnsupportedInputError for AZ, CA and NY, whose OTS programs build "
            "their own itemized deduction from the federal Schedule A."
        ),
    )
    w2_income: float = 0.0
    taxable_interest: float = 0.0
    qualified_dividends: float = 0.0
    ordinary_dividends: float = 0.0
    short_term_capital_gains: float = 0.0
    long_term_capital_gains: float = 0.0
    self_employment_income: float = 0.0
    qbi_w2_wages: float = Field(default=0.0, ge=0.0)
    qbi_ubia: float = Field(default=0.0, ge=0.0)
    qbi_is_sstb: bool = False
    rental_income: float = 0.0
    schedule_1_income: float = 0.0
    itemized_deductions: float = 0.0
    state_and_local_taxes: float = Field(
        default=0.0,
        ge=0.0,
        description=(
            "State and local income (or general sales), real estate and personal "
            "property taxes paid: the Schedule A line 5d total, entered on line 5a "
            "(the backends treat lines 5a-5c alike, so the split does not matter). "
            "The federal return limits it on line 5e: for 2018-2024 to $10,000 ($5,000 "
            "married filing separately). For 2025 the State and Local Tax Deduction "
            "Worksheet starts from $40,000 for every status, reduces it by 30% of "
            "modified AGI over $500,000 ($250,000 MFS), floors it at $10,000, and "
            "only then halves the result for MFS: equivalently $20,000 less 15% of "
            "modified AGI over $250,000, not below $5,000. "
            "Modified AGI is taken to be AGI (no Puerto Rico, Form 2555 or Form 4563 "
            "exclusions). It only lowers taxable income when Schedule A is used, "
            "chosen automatically or by standard_or_itemized='Itemized', and the "
            "capped amount is added back for the AMT. itemized_deductions stays "
            "outside the cap. State returns do not read it."
        ),
    )
    qualified_tips: float = Field(
        default=0.0,
        ge=0.0,
        description=(
            "Qualified tips already included in w2_income (W-2 box 7), for the 2025 "
            "Schedule 1-A Part II deduction: up to $25,000, reduced by $100 per whole "
            "$1,000 of modified AGI over $150,000 ($300,000 married filing jointly); "
            "married filing separately cannot claim it. The deduction enters Form "
            "1040 line 13b, below AGI, with the standard or itemized deduction. "
            "Eligibility (a listed tipped occupation, valid SSN, not an SSTB) is the "
            "caller's fact. Self-employed tips are not modeled. 2025 only; a nonzero "
            "value for an earlier year raises ValueError."
        ),
    )
    qualified_overtime: float = Field(
        default=0.0,
        ge=0.0,
        description=(
            "Qualified overtime compensation already included in w2_income: the FLSA "
            "section 7 premium above the regular rate, not the whole overtime pay. "
            "For the 2025 Schedule 1-A Part III deduction: up to $12,500 ($25,000 "
            "married filing jointly), reduced by $100 per whole $1,000 of modified "
            "AGI over $150,000 ($300,000 MFJ); married filing separately cannot "
            "claim it. Enters Form 1040 line 13b. 2025 only; a nonzero value for an "
            "earlier year raises ValueError."
        ),
    )
    vehicle_loan_interest: float = Field(
        default=0.0,
        ge=0.0,
        description=(
            "Qualified passenger vehicle loan interest paid (Schedule 1-A line 23, "
            "the column (iii) total: loans after 2024 for a new, US-assembled "
            "personal-use vehicle, net of any interest deducted on Schedule C, E or "
            "F). For the 2025 Schedule 1-A Part IV deduction: up to $10,000, reduced "
            "by $200 per $1,000 (or part of $1,000) of modified AGI over $100,000 "
            "($200,000 married filing jointly). Married filing separately may claim "
            "it. Enters Form 1040 line 13b. 2025 only; a nonzero value for an "
            "earlier year raises ValueError."
        ),
    )
    filer_65_or_older: bool = Field(
        default=False,
        description=(
            "The filer was 65 or older at the end of the year (born before January "
            "2, 1960 for 2024; January 2, 1961 for 2025). Adds the additional "
            "standard deduction for age ($1,950 unmarried / $1,550 married for 2024; "
            "$2,000 / $1,600 for 2025; OTS applies its own year's chart for "
            "2018-2023) and, for 2025, the Schedule 1-A senior "
            "deduction: $6,000 reduced by 6% of modified AGI over $75,000 ($150,000 "
            "married filing jointly), not for married filing separately. The senior "
            "deduction is taken with either the standard or itemized deduction and "
            "is added back for the AMT. Blindness and dependent-filer limits are not "
            "modeled. The IL graph also adds the $1,000 age-65 exemption allowance, "
            "subject to its federal-AGI limit. The MS graph adds the $1,500 "
            "age exemption before the MFS half-share rule."
        ),
    )
    spouse_65_or_older: bool = Field(
        default=False,
        description=(
            "The spouse was 65 or older at the end of the year (see "
            "filer_65_or_older). Married filing jointly: adds the spouse's "
            "additional standard deduction and, for 2025, the spouse's senior "
            "deduction. Married filing separately: adds only the spouse's additional "
            "standard deduction, which the IRS allows when the spouse had no income, "
            "is not filing and cannot be claimed as a dependent; the senior "
            "deduction is not allowed. Any other filing status has no spouse box, "
            "so True raises ValueError. The IL graph adds the spouse's $1,000 "
            "age allowance only for MFJ, subject to its federal-AGI limit. "
            "The MS graph counts the spouse's $1,500 age exemption for MFJ "
            "and MFS, applying the form's half-share rule for MFS."
        ),
    )
    state_adjustment: float = Field(
        default=0.0,
        description=(
            "Deprecated legacy escape hatch; avoid in new code. Its meaning depends "
            "on the state and the backend, and a nonzero value emits a "
            "DeprecationWarning (tenforty-yu51). OTS CA: Schedule CA Part II "
            "itemized-deduction addition (CA540_P2_Add_6). Graph CA: Schedule CA "
            "Part I income addition (ca_schedule_ca_A22_24), so the same value moves "
            "CA taxable income the opposite way. Graph AL: the Form 40 standard "
            "deduction amount (al_40_L12_std). Everywhere else OTS ignores it and "
            "the graph refuses it. Named single-meaning inputs will replace it "
            "(tenforty-tj2.1, tenforty-tj2.2)."
        ),
    )
    incentive_stock_option_gains: float = 0.0
    dependent_exemptions: float = Field(
        default=0.0,
        description=(
            "State exemption dollars, not a dependent count. The IL, MS, OH and OR "
            "graphs do not map this field; use num_dependents for IL personal/dependent "
            "allowances, MS additional exemptions, OH exemptions/credit and the OR "
            "regular exemption credit. "
            "For NJ, VA, HI, IN, KS, "
            "MD, ME, VT, and LA 2024 graph returns, this is the total including the "
            "mandatory personal and spouse baseline (and KS's head-of-household "
            "extra), not additional-only dollars. The effective total is "
            "max(baseline, explicit total); an omitted or below-baseline amount "
            "receives the baseline. For HI, IN, KS, MD and VT the total must include "
            "dependents' exemptions; num_dependents is not mapped there. For ME the "
            "explicit total is the pre-phase-out amount (worksheet line 6); the graph "
            "then applies the income phase-out, so do not pass an already-phased "
            "figure. For MD the baseline follows the Chart 10A federal-AGI tiers. An "
            "explicit MD total is the final Form 502 Line 19 amount after Chart 10A: "
            "the caller tiers the taxpayer, spouse and dependent amounts by federal "
            "AGI before supplying it, and adds any $1,000 age/blind exemptions "
            "unreduced. The library does not reduce an explicit total again, so the "
            "caller must recompute it whenever federal AGI changes. HI, ME and VT "
            "returns where the filer or spouse can be claimed as another's dependent "
            "(a total below the baseline) are unsupported. Maryland "
            "dependent-taxpayer filers, whose exemption is $0, are unsupported. Other "
            "state/year and backend support varies. OTS maps it for no state or "
            "year, and refuses a nonzero value."
        ),
    )

    @computed_field  # type: ignore[prop-decorator]
    @property
    def ma_contribution_deduction(self) -> float:
        """Massachusetts Form 1 line 11a for the OTS MA_1 form.

        Line 11 is the Social Security and Medicare paid, "not more than $2,000"
        per person (2024 instructions PDF p.10, 2025 PDF p.11; M.G.L. c.62
        3.B(a)(3)-(4)). For W-2 wages that is 6.2% + 1.45% = 7.65% of wages; the
        $2,000 cap binds near $26,144 of wages, below every year's Social
        Security wage base, so the base never enters. `w2_income` is a household
        aggregate, so a joint return takes the one-earner reading (all on 11a),
        as `schedule_se_ss_wages` does and as the graph's line 11 does; a two-earner
        couple is then at most $100 high, or $180 once the 4% surtax applies. MA_1 maps
        no self-employment income, so its SE-tax share is not added here.
        """
        return min(2_000.0, 0.0765 * max(0.0, float(self.w2_income)))

    @computed_field  # type: ignore[prop-decorator]
    @property
    def schedule_se_ss_wages(self) -> float:
        """The filer's own W-2 social security wages, for Schedule SE line 8a.

        Line 8a feeds line 9 — what is LEFT of the social security wage base ($168,600
        in 2024) once the filer's own wages are counted — and line 9 caps the 12.4% OASDI
        charge on line 10. Leaving 8a empty charges that 12.4% on self-employment earnings
        the filer's wages have already carried past the base.

        `w2_income` is a HOUSEHOLD aggregate, but Schedule SE is a per-person form. For
        Married/Joint we cannot tell whose wages are whose, so this stays zero and the full
        OASDI charge stands — the conservative reading, and the behaviour that
        `test_se_tax_mfj_w2_above_ss_base` locks in. Every other filing status has exactly
        one person, so the aggregate simply IS that person's wages.

        Zero when there is no self-employment income, so that this never causes Schedule SE
        to be resolved or evaluated for a return that would not otherwise have filed one.
        """
        if not self.self_employment_income:
            return 0.0
        if self.filing_status == OTSFilingStatus.MARRIED_JOINT:
            return 0.0
        return float(self.w2_income)

    @computed_field  # type: ignore[prop-decorator]
    @property
    def schedule_8812_qualifying_children(self) -> int:
        """Child tax credit qualifying children, Schedule 8812 line 4.

        num_ctc_dependents, or every dependent when it is unset.
        """
        if self.num_ctc_dependents is None:
            return self.num_dependents
        return self.num_ctc_dependents

    @computed_field  # type: ignore[prop-decorator]
    @property
    def schedule_8812_other_dependents(self) -> int:
        """Dependents who get the credit for other dependents, Schedule 8812 line 6."""
        return self.num_dependents - self.schedule_8812_qualifying_children

    @computed_field  # type: ignore[prop-decorator]
    @property
    def schedule_a_other_deductions(self) -> float:
        """itemized_deductions as OTS receives it: none when Standard is forced.

        OTS has no switch that forces the standard deduction; it takes the
        larger unless itemizing is forced, so an empty Schedule A forces it.
        """
        if self.standard_or_itemized == OTSDeductionType.STANDARD:
            return 0.0
        return self.itemized_deductions

    @computed_field  # type: ignore[prop-decorator]
    @property
    def schedule_a_state_and_local_taxes(self) -> float:
        """state_and_local_taxes as OTS receives it: none when Standard is forced."""
        if self.standard_or_itemized == OTSDeductionType.STANDARD:
            return 0.0
        return self.state_and_local_taxes

    @model_validator(mode="after")
    def refuse_more_ctc_dependents_than_dependents(self) -> "TaxReturnInput":
        """num_ctc_dependents counts a subset of num_dependents."""
        if (
            self.num_ctc_dependents is not None
            and self.num_ctc_dependents > self.num_dependents
        ):
            raise ValueError(
                f"num_ctc_dependents={self.num_ctc_dependents} exceeds "
                f"num_dependents={self.num_dependents}: num_dependents is the total "
                "number of dependents, and the child tax credit children are among "
                "them."
            )
        return self

    @model_validator(mode="after")
    def refuse_inputs_the_year_lacks(self) -> "TaxReturnInput":
        """Reject Schedule 1-A amounts before 2025 and a spouse box without a spouse."""
        first_year = SCHEDULE_1A_FIRST_YEAR
        if self.year.value < first_year:
            for name in SCHEDULE_1A_INPUTS:
                if getattr(self, name):
                    raise ValueError(
                        f"{name} feeds Schedule 1-A, which first exists for "
                        f"{first_year} (P.L. 119-21); {self.year.value} has no such "
                        f"deduction. Pass {name}=0 for {self.year.value}."
                    )
        if self.spouse_65_or_older and self.filing_status not in _SPOUSE_BOX_STATUSES:
            raise ValueError(
                "spouse_65_or_older applies only to Married/Joint and Married/Sep; "
                f"filing status {self.filing_status.value!r} has no spouse box."
            )
        return self

    @model_validator(mode="after")
    def warn_deprecated_state_adjustment(self) -> "TaxReturnInput":
        """Warn on a nonzero state_adjustment, whose meaning is backend-specific."""
        if self.state_adjustment:
            warnings.warn(
                "state_adjustment is deprecated: its meaning depends on the state and "
                "the backend (CA: an itemized-deduction addition on OTS but an income "
                "addition on graph; AL graph: the standard deduction amount). See "
                "docs/migrating-to-graph-default.md.",
                DeprecationWarning,
                stacklevel=2,
            )
        return self

    @model_validator(mode="after")
    def ensure_ordinary_includes_qualified(self) -> "TaxReturnInput":
        """Handle case where only qualified dividends is provided."""
        # On the 1040, the "ordinary dividends" box, recently Line 3b, includes
        # qualified dividends, recently Line 3a, so we must ensure that ordinary
        # dividends is at least as much as qualified dividends. E.g. if only
        # qualified dividends is specified, ordinary dividends would be zero,
        # and that will give an incorrect result.
        if self.qualified_dividends > self.ordinary_dividends:
            self.ordinary_dividends = self.qualified_dividends
        return self


TAX_RETURN_FLOAT_FIELDS = tuple(
    name
    for name, field in TaxReturnInput.model_fields.items()
    if field.annotation is float
)

FINITE_FLOAT_COLUMNS = TypeAdapter(dict[str, list[FiniteFloat]])


def canonical_json(value: object) -> str:
    """Deterministic project JSON: sorted keys, compact, UTF-8, no NaN.

    A stable serialization for metadata columns; not an RFC 8785 claim.
    """
    return json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=False,
        allow_nan=False,
    )


class Assumption(BaseModel):
    """A convention that entered the selected engine's computation of a return.

    Recorded because no input supplied the value. ``id`` and ``value`` are
    semantic and stable; ``text`` explains and may be reworded. ``input_path``
    names the public input that would replace the convention, or is None when
    no such input exists yet. ``value`` is a JSON scalar.
    """

    model_config = ConfigDict(frozen=True, extra="forbid", strict=True)

    id: str
    state: str | None
    input_path: str | None
    value: bool | int | FiniteFloat | str | None
    text: str

    def to_json(self) -> str:
        """Canonical JSON of the complete record."""
        return canonical_json(self.model_dump())

    @classmethod
    def from_json(cls, encoded: str) -> "Assumption":
        """Decode a record written by ``to_json``."""
        return cls.model_validate_json(encoded)


class InterpretedTaxReturn(BaseModel):
    total_tax: float = 0.0

    federal_adjusted_gross_income: float = 0.0
    federal_effective_tax_rate: float | None = None
    federal_tax_bracket: float | None = None
    federal_taxable_income: float = 0.0
    federal_qbi_deduction: float = 0.0
    federal_amt: float = 0.0
    federal_income_tax: float = 0.0
    federal_se_tax: float = 0.0
    federal_niit: float = 0.0
    federal_additional_medicare_tax: float = 0.0
    federal_child_tax_credit: float = Field(
        default=0.0,
        description=(
            "Nonrefundable child tax credit plus credit for other dependents: "
            "Schedule 8812 line 14, entered on Form 1040 line 19. Already "
            "subtracted in federal_total_tax. Excludes the refundable additional "
            "child tax credit (line 28), which is a payment."
        ),
    )
    federal_total_tax: float = 0.0

    state_adjusted_gross_income: float = 0.0
    state_taxable_income: float = 0.0
    state_total_tax: float = 0.0
    state_tax_bracket: float | None = None
    state_effective_tax_rate: float | None = None

    assumptions: tuple[Assumption, ...] | None = Field(
        default=(),
        description=(
            "Recorded state conventions that entered this computation because no "
            "input supplied them (see tenforty.state_facts). () when none was "
            "recorded, which is not a claim that the return makes no assumptions; "
            "None on a failed return."
        ),
    )

    @classmethod
    def failed(cls) -> "InterpretedTaxReturn":
        """Return a failed computation: every number NaN, never 0; no metadata."""
        return cls(**dict.fromkeys(NUMERIC_OUTPUT_FIELDS, math.nan), assumptions=None)

    @model_validator(mode="after")
    def compute_federal_income_tax(self) -> "InterpretedTaxReturn":  # noqa: D102
        if self.federal_income_tax == 0.0 and self.federal_total_tax > 0.0:
            self.federal_income_tax = (
                self.federal_total_tax
                - self.federal_se_tax
                - self.federal_niit
                - self.federal_additional_medicare_tax
            )
        return self


def capital_gains(term: str, amount: float) -> str:
    """Generate OTS-compatible capital gains clause."""
    return (
        "CapGains-A/D",
        f"""
    0 various-{term}
    {amount} various-{term}
    ~ ~
    """,
    )


def _force_itemize(value):
    return ("A18", "Y" if value == "Itemized" else "N")


def _filer_65_box(value):
    return ("You_65+Over?", "Y" if value else "N")


def _oregon_regular_spouse_box(
    filing_status: OTSFilingStatus | str,
) -> tuple[str, str]:
    return (
        "CkL6bRegular",
        "Yes" if filing_status == OTSFilingStatus.MARRIED_JOINT else "No",
    )


def _spouse_65_box(value):
    return ("Spouse_65+Over?", "Y" if value else "N")


def _restricted_filing_status(
    form_id: str, supported: frozenset[OTSFilingStatus]
) -> Callable[[str], tuple[str, str]]:
    """Map filing_status to a state template's Status line, refusing what OTS cannot compute.

    Our filing-status values are the tokens OTS's parsers match, so a supported
    status passes through unchanged. An unsupported one raises here rather than
    reaching OTS, where it would either be priced under no status at all or
    abort with a bare exit code.
    """

    def status_line(filing_status: str) -> tuple[str, str]:
        status = OTSFilingStatus(filing_status)
        if status not in supported:
            accepted = ", ".join(sorted(s.value for s in supported))
            raise ValueError(
                f"OTS {form_id} cannot compute filing status {status.value!r}; "
                f"it supports {accepted}"
            )
        return ("Status", status.value)

    return status_line


def _ma_adjusted_gross_income(total_5_percent_income: float) -> float:
    """MA AGI from Form 1 line 10, as the No Tax Status AGI worksheet computes it.

    Worksheet line 1 is Form 1 line 10 floored at zero (OTS computes it as
    ``ws[1] = NotLessThanZero(L[10])`` in every MA_1 year). Its remaining
    lines (Schedule Y deductions, exempt bank interest, line 20 interest and
    dividends) are zero for every input the MA_1 input map sets, so the floored
    line 10 is the worksheet's AGI. OTS fills the worksheet only for the
    statuses eligible for No Tax Status, but prints line 10 for all of them.
    """
    return max(0.0, total_5_percent_income)


# MA_1 parses Widow(er) but its exemption switch has no case for it, so the
# personal exemption falls to $0; the No Tax Status worksheet is gated on
# Single, Head_of_House and Married/Joint, so the return completes silently.
_ma_filing_status = _restricted_filing_status(
    "MA_1",
    frozenset(
        {
            OTSFilingStatus.SINGLE,
            OTSFilingStatus.MARRIED_JOINT,
            OTSFilingStatus.MARRIED_SEPARATE,
            OTSFilingStatus.HEAD_OF_HOUSEHOLD,
        }
    ),
)

# VA_760 rejects Widow(er) at parse time and Head_of_House at its standard
# deduction switch, exiting non-zero on either.
_va_filing_status = _restricted_filing_status(
    "VA_760",
    frozenset(
        {
            OTSFilingStatus.SINGLE,
            OTSFilingStatus.MARRIED_JOINT,
            OTSFilingStatus.MARRIED_SEPARATE,
        }
    ),
)


def capital_gains_pre2021(term: str, year: int, amount: int) -> str:
    """Generate OTS-compatible capital gains clause, for pre-2021 returns."""
    if term == "short":
        start_date = f"01-01-{year}"
    else:
        start_date = f"01-01-{year - 1}"
    end_date = f"10-10-{year}"
    return (
        "CapGains-A/D",
        f"""
    0 {start_date}
    {amount} {end_date}
    """,
    )


_NATURAL_FORM_CONFIG = [
    # 2024
    {
        "year": 2024,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1a",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "self_employment_income": "S1_3",
            "rental_income": "S1_5",
            "schedule_1_income": "S1_8z",
            "schedule_a_other_deductions": "A6",
            # Schedule A line 5d is reached through line 5a; OTS sums 5a-5c and
            # applies the line 5e cap to the sum.
            "schedule_a_state_and_local_taxes": "A5a",
            "filer_65_or_older": _filer_65_box,
            "spouse_65_or_older": _spouse_65_box,
            "standard_or_itemized": _force_itemize,
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2024,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L13": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "MA_1",
        "input_map": {
            "filing_status": _ma_filing_status,
            "w2_income": "L3",
            "ma_contribution_deduction": "L11a",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "L10": "adjusted_gross_income",
            "L28": "total_tax",
        },
        "output_transforms": {"adjusted_gross_income": _ma_adjusted_gross_income},
    },
    {
        "year": 2024,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "NC_D400",
        "input_map": {
            "filing_status": "Status",
        },
        "output_map": {
            "L6": "adjusted_gross_income",
            "L14": "taxable_income",
            "L15": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "NJ_1040",
        "input_map": {
            "filing_status": "Status",
            "w2_income": "L15",
            "taxable_interest": "L16a",
            "ordinary_dividends": "L17",
        },
        "output_map": {
            "L29": "adjusted_gross_income",
            "L39": "taxable_income",
            "L54": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "PA_40",
        "input_map": {
            "filing_status": "Status",
            "w2_income": "L1a",
            "taxable_interest": "L2",
            "ordinary_dividends": "L3",
        },
        "output_map": {
            "L9": "adjusted_gross_income",
            "L11": "taxable_income",
            "L12": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "VA_760",
        "input_map": {"filing_status": _va_filing_status},
        "fed_import_map": {"L11": "L1"},
        "output_map": {
            "L9": "adjusted_gross_income",
            "L15": "taxable_income",
            "L18": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "OH_IT1040",
        "input_map": {},
        "fed_import_map": {"L11": "L1"},
        "output_map": {
            "L3": "adjusted_gross_income",
            "L5": "taxable_income",
            "L13": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "MI_1040",
        "input_map": {},
        "fed_import_map": {"L11": "L10"},
        "output_map": {
            "L14": "adjusted_gross_income",
            "L16": "taxable_income",
            "L17": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "OR_40",
        "input_map": {"filing_status": _oregon_regular_spouse_box},
        "output_map": {
            "L7": "adjusted_gross_income",
            "L19": "taxable_income",
            "L31": "total_tax",
        },
    },
    {
        "year": 2024,
        "form_id": "AZ_140",
        "input_map": {},
        "output_map": {
            "L42": "adjusted_gross_income",
            "L45": "taxable_income",
            "L52": "total_tax",
        },
    },
    # 2025
    {
        "year": 2025,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1a",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "self_employment_income": "S1_3",
            "rental_income": "S1_5",
            "schedule_1_income": "S1_8z",
            "schedule_a_other_deductions": "A6",
            # Schedule A line 5d is reached through line 5a; OTS sums 5a-5c and
            # runs the State and Local Tax Deduction Worksheet on the sum.
            "schedule_a_state_and_local_taxes": "A5a",
            # Schedule 1-A (OTS reads it when the return carries S1A_2a, which
            # the template always does): line 4a W-2 box 7 tips, line 14a
            # W-2 box 1 overtime, and line 22 column (iii) for one vehicle,
            # which line 23 totals. The senior deduction (Part V) comes from
            # the age-65 boxes.
            "qualified_tips": "S1A_4a",
            "qualified_overtime": "S1A_14a",
            "vehicle_loan_interest": "S1A_22aiii",
            "filer_65_or_older": _filer_65_box,
            "spouse_65_or_older": _spouse_65_box,
            "standard_or_itemized": _force_itemize,
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            # OTS 2025 output uses L11a/L11b; L11b is AGI after adjustments.
            "L11b": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2025,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L13": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "MA_1",
        "input_map": {
            "filing_status": _ma_filing_status,
            "w2_income": "L3",
            "ma_contribution_deduction": "L11a",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "L10": "adjusted_gross_income",
            "L28": "total_tax",
        },
        "output_transforms": {"adjusted_gross_income": _ma_adjusted_gross_income},
    },
    {
        "year": 2025,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "NC_D400",
        "input_map": {
            "filing_status": "Status",
        },
        "output_map": {
            "L6": "adjusted_gross_income",
            "L14": "taxable_income",
            "L15": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "NJ_1040",
        "input_map": {
            "filing_status": "Status",
            "w2_income": "L15",
            "taxable_interest": "L16a",
            "ordinary_dividends": "L17",
        },
        "output_map": {
            "L29": "adjusted_gross_income",
            "L39": "taxable_income",
            "L54": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "PA_40",
        "input_map": {
            "filing_status": "Status",
            "w2_income": "L1a",
            "taxable_interest": "L2",
            "ordinary_dividends": "L3",
        },
        "output_map": {
            "L9": "adjusted_gross_income",
            "L11": "taxable_income",
            "L12": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "VA_760",
        "input_map": {"filing_status": _va_filing_status},
        "fed_import_map": {"L11b": "L1"},
        "output_map": {
            "L9": "adjusted_gross_income",
            "L15": "taxable_income",
            "L18": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "OH_IT1040",
        "input_map": {},
        "fed_import_map": {"L11b": "L1"},
        "output_map": {
            "L3": "adjusted_gross_income",
            "L5": "taxable_income",
            "L13": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "MI_1040",
        "input_map": {},
        "fed_import_map": {"L11b": "L10"},
        "output_map": {
            "L14": "adjusted_gross_income",
            "L16": "taxable_income",
            "L17": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "OR_40",
        "input_map": {"filing_status": _oregon_regular_spouse_box},
        "output_map": {
            "L7": "adjusted_gross_income",
            "L19": "taxable_income",
            "L31": "total_tax",
        },
    },
    {
        "year": 2025,
        "form_id": "AZ_140",
        "input_map": {},
        "output_map": {
            "L42": "adjusted_gross_income",
            "L45": "taxable_income",
            "L52": "total_tax",
        },
    },
    # 2023
    {
        "year": 2023,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1a",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "self_employment_income": "S1_3",
            "rental_income": "S1_5",
            "schedule_1_income": "S1_8z",
            "schedule_a_other_deductions": "A6",
            "schedule_a_state_and_local_taxes": "A5a",
            "filer_65_or_older": _filer_65_box,
            "spouse_65_or_older": _spouse_65_box,
            "standard_or_itemized": _force_itemize,
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2023,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2023,
        "form_id": "MA_1",
        "input_map": {
            "filing_status": _ma_filing_status,
            "w2_income": "L3",
            "ma_contribution_deduction": "L11a",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "L10": "adjusted_gross_income",
            "L28": "total_tax",
        },
        "output_transforms": {"adjusted_gross_income": _ma_adjusted_gross_income},
    },
    {
        "year": 2023,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    {
        "year": 2023,
        "form_id": "NC_D400",
        "input_map": {
            "filing_status": "Status",
        },
        "output_map": {
            "L6": "adjusted_gross_income",
            "L14": "taxable_income",
            "L15": "total_tax",
        },
    },
    {
        "year": 2023,
        "form_id": "NJ_1040",
        "input_map": {
            "filing_status": "Status",
            "w2_income": "L15",
            "taxable_interest": "L16a",
            "ordinary_dividends": "L17",
        },
        "output_map": {
            "L29": "adjusted_gross_income",
            "L39": "taxable_income",
            "L54": "total_tax",
        },
    },
    {
        "year": 2023,
        "form_id": "PA_40",
        "input_map": {
            "filing_status": "Status",
            "w2_income": "L1a",
            "taxable_interest": "L2",
            "ordinary_dividends": "L3",
        },
        "output_map": {
            "L9": "adjusted_gross_income",
            "L11": "taxable_income",
            "L12": "total_tax",
        },
    },
    {
        "year": 2023,
        "form_id": "VA_760",
        "input_map": {"filing_status": _va_filing_status},
        "fed_import_map": {"L11": "L1"},
        "output_map": {
            "L9": "adjusted_gross_income",
            "L15": "taxable_income",
            "L18": "total_tax",
        },
    },
    {
        "year": 2023,
        "form_id": "OH_IT1040",
        "input_map": {},
        "fed_import_map": {"L11": "L1"},
        "output_map": {
            "L3": "adjusted_gross_income",
            "L5": "taxable_income",
            "L13": "total_tax",
        },
    },
    # 2022
    {
        "year": 2022,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1a",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "self_employment_income": "S1_3",
            "rental_income": "S1_5",
            "schedule_1_income": "S1_8z",
            "schedule_a_other_deductions": "A6",
            "schedule_a_state_and_local_taxes": "A5a",
            "filer_65_or_older": _filer_65_box,
            "spouse_65_or_older": _spouse_65_box,
            "standard_or_itemized": _force_itemize,
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2022,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2022,
        "form_id": "MA_1",
        "input_map": {
            "filing_status": _ma_filing_status,
            "w2_income": "L3",
            "ma_contribution_deduction": "L11a",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "L10": "adjusted_gross_income",
            "L28": "total_tax",
        },
        "output_transforms": {"adjusted_gross_income": _ma_adjusted_gross_income},
    },
    {
        "year": 2022,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2021
    {
        "year": 2021,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains, "short"),
            "long_term_capital_gains": partial(capital_gains, "long"),
            "self_employment_income": "S1_3",
            "rental_income": "S1_5",
            "schedule_1_income": "S1_8z",
            "schedule_a_other_deductions": "A6",
            "schedule_a_state_and_local_taxes": "A5a",
            "filer_65_or_older": _filer_65_box,
            "spouse_65_or_older": _spouse_65_box,
            "standard_or_itemized": _force_itemize,
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2021,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L65": "total_tax",
        },
    },
    {
        "year": 2021,
        "form_id": "MA_1",
        "input_map": {
            "filing_status": _ma_filing_status,
            "w2_income": "L3",
            "ma_contribution_deduction": "L11a",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "L10": "adjusted_gross_income",
            "L28": "total_tax",
        },
        "output_transforms": {"adjusted_gross_income": _ma_adjusted_gross_income},
    },
    {
        "year": 2021,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2020
    {
        "year": 2020,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains_pre2021, "short", 2020),
            "long_term_capital_gains": partial(capital_gains_pre2021, "long", 2020),
            "self_employment_income": "S1_3",
            "rental_income": "S1_5",
            "schedule_1_income": "S1_8",  # This assumes 'schedule_1_income' consolidates all other income reported in Schedule 1
            "schedule_a_other_deductions": "A6",
            "schedule_a_state_and_local_taxes": "A5a",
            "filer_65_or_older": _filer_65_box,
            "spouse_65_or_older": _spouse_65_box,
            "standard_or_itemized": _force_itemize,
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L11": "adjusted_gross_income",
            "L15": "taxable_income",
            "L24": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2020,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L65": "total_tax",
        },
    },
    {
        "year": 2020,
        "form_id": "MA_1",
        "input_map": {
            "filing_status": _ma_filing_status,
            "w2_income": "L3",
            "ma_contribution_deduction": "L11a",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "L10": "adjusted_gross_income",
            "L28": "total_tax",
        },
        "output_transforms": {"adjusted_gross_income": _ma_adjusted_gross_income},
    },
    {
        "year": 2020,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2019
    {
        "year": 2019,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1",
            "taxable_interest": "L2b",
            "qualified_dividends": "L3a",
            "ordinary_dividends": "L3b",
            "short_term_capital_gains": partial(capital_gains_pre2021, "short", 2019),
            "long_term_capital_gains": partial(capital_gains_pre2021, "long", 2019),
            "self_employment_income": "S1_3",
            "rental_income": "S1_5",
            "schedule_1_income": "S1_8",  # Assumes 'schedule_1_income' consolidates all Schedule 1 income types
            "schedule_a_other_deductions": "A6",  # Assumes deductions are itemized; otherwise, standard deduction is used
            "schedule_a_state_and_local_taxes": "A5a",
            "filer_65_or_older": _filer_65_box,
            "spouse_65_or_older": _spouse_65_box,
            "incentive_stock_option_gains": "AMTws3",
        },
        "output_map": {
            "L8b": "adjusted_gross_income",
            "L11": "taxable_income",
            "L16": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2019,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2019,
        "form_id": "MA_1",
        "input_map": {
            "filing_status": _ma_filing_status,
            "w2_income": "L3",
            "ma_contribution_deduction": "L11a",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "L10": "adjusted_gross_income",
            "L28": "total_tax",
        },
        "output_transforms": {"adjusted_gross_income": _ma_adjusted_gross_income},
    },
    {
        "year": 2019,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
    # 2018
    {
        "year": 2018,
        "form_id": "US_1040",
        "input_map": {
            "filing_status": "Status",
            "num_dependents": "Dependents",
            "w2_income": "L1",  # Wages, salaries, tips
            "taxable_interest": "L2b",  # Taxable Interest
            "qualified_dividends": "L3a",  # Qualified Dividends
            "ordinary_dividends": "L3b",  # Ordinary Dividends
            "short_term_capital_gains": partial(capital_gains_pre2021, "short", 2018),
            "long_term_capital_gains": partial(capital_gains_pre2021, "long", 2018),
            "self_employment_income": "S1_3",
            "rental_income": "S1_5",
            "schedule_1_income": "S1_21",  # Assuming 'Other income' covers Schedule 1 income types
            "schedule_a_other_deductions": "A6",  # Other taxes (if itemized; if standard, this field is not used)
            "schedule_a_state_and_local_taxes": "A5a",
            "filer_65_or_older": _filer_65_box,
            "spouse_65_or_older": _spouse_65_box,
            "incentive_stock_option_gains": "AMTws3",  # Other adjustments under AMT
        },
        "output_map": {
            "L7": "adjusted_gross_income",
            "L10": "taxable_income",
            "L15": "total_tax",
            "Your Alternative Minimum Tax": "amt",
        },
    },
    {
        "year": 2018,
        "form_id": "CA_540",
        "input_map": {
            "num_dependents": "L10",
            "state_adjustment": "CA540_P2_Add_6",
        },
        "output_map": {
            "L15": "adjusted_gross_income",
            "L19": "taxable_income",
            "L64": "total_tax",
        },
    },
    {
        "year": 2018,
        "form_id": "MA_1",
        "input_map": {
            "filing_status": _ma_filing_status,
            "w2_income": "L3",
            "ma_contribution_deduction": "L11a",
            "num_dependents": "Dependents",
        },
        "output_map": {
            "L21": "taxable_income",
            "L10": "adjusted_gross_income",
            "L28": "total_tax",
        },
        "output_transforms": {"adjusted_gross_income": _ma_adjusted_gross_income},
    },
    {
        "year": 2018,
        "form_id": "NY_IT201",
        "input_map": {},
        "output_map": {
            "L33": "adjusted_gross_income",
            "L37": "taxable_income",
            "L46": "total_tax",
        },
    },
]


NATURAL_FORM_CONFIG = dict(
    ((form.year, form.form_id), form)
    for form in (NaturalFormMapping(**e) for e in _NATURAL_FORM_CONFIG)
)


def _se_income_to_8959_l8(se_income):
    # Schedule SE line 4c stops below $400, so line 6 (Form 8959 line 8) is zero.
    se_earnings = se_income * 0.9235
    return ("L8", str(round(se_earnings)) if se_earnings >= 400 else "0")


_SUBORDINATE_FORM_CONFIG = [
    # 2023 Schedule 8812 — Phase 2 (needs AGI and tax from a preliminary
    # 1040). See the 2024 entry.
    {
        "year": 2023,
        "form_id": "Form_8812",
        "phase": 2,
        "input_map": {
            "schedule_8812_qualifying_children": "L4",
            "schedule_8812_other_dependents": "L6",
            "filing_status": "Status",
        },
        "fed_import_map": {"L11": "L1", "L18": "L13"},
        "export_map": {"L14": "L19"},
        "output_map": {"L14": "child_tax_credit"},
    },
    # 2024 Schedule SE — Phase 1 (no federal dependency)
    {
        "year": 2024,
        "form_id": "US_1040_Sched_SE",
        "phase": 1,
        "input_map": {
            "self_employment_income": "L2",
            # Line 8a: the filer's own social security wages, which fill the wage base
            # before self-employment earnings do. See TaxReturnInput.schedule_se_ss_wages.
            "schedule_se_ss_wages": "L8a",
        },
        "export_map": {"L12": "S2_4", "L13": "S1_15"},
        "output_map": {"L12": "se_tax"},
    },
    # 2024 Form 8959 — Phase 1 (no federal dependency)
    {
        "year": 2024,
        "form_id": "Form_8959",
        "phase": 1,
        "input_map": {
            "w2_income": "L1",
            "filing_status": "Status",
            "self_employment_income": _se_income_to_8959_l8,
        },
        "export_map": {"L18": "S2_11"},
        "output_map": {"L18": "additional_medicare_tax"},
    },
    # 2024 Form 8995/8995-A — Phase 2 (preliminary 1040 feeds the deduction
    # back into the final 1040). OTS supplies the independent simplified-form
    # components; Python orchestration applies the missing 8995-A limitation.
    {
        "year": 2024,
        "form_id": "Form_8995",
        "phase": 2,
        "input_map": {},
        "export_map": {"L15": "L13"},
        "activation_naturals": [
            "self_employment_income",
            "qbi_w2_wages",
            "qbi_ubia",
            "qbi_is_sstb",
        ],
        "output_map": {"L15": "qbi_deduction"},
    },
    # 2024 Schedule 8812 — Phase 2, after Form 8995 so the credit limit sees
    # the QBI deduction. Line 13 is Credit Limit Worksheet A, which
    # is Form 1040 line 18 less Schedule 3 credits no tenforty input reaches,
    # so it is line 18. Line 4 takes the CTC children, line 6 the other
    # dependents (both counts derived from num_dependents and
    # num_ctc_dependents). Line 14 is the nonrefundable credit, Form 1040 line 19; the refundable ACTC (line
    # 27, Form 1040 line 28) is a payment and does not change line 24.
    {
        "year": 2024,
        "form_id": "Form_8812",
        "phase": 2,
        "input_map": {
            "schedule_8812_qualifying_children": "L4",
            "schedule_8812_other_dependents": "L6",
            "filing_status": "Status",
        },
        "fed_import_map": {"L11": "L1", "L18": "L13"},
        "export_map": {"L14": "L19"},
        "output_map": {"L14": "child_tax_credit"},
    },
    # 2024 Form 8960 — Phase 3 (needs AGI from 1040)
    {
        "year": 2024,
        "form_id": "Form_8960",
        "phase": 3,
        "input_map": {
            "taxable_interest": "L1",
            "ordinary_dividends": "L2",
            "rental_income": "L4a",
            "filing_status": "Status",
        },
        # L5a is "net gain from disposition of property" — Form 8960 takes it
        # from Form 1040 line 7, which is the Schedule D result with the
        # section 1211(b) $3,000 loss limitation already applied and both
        # holding periods already netted. Importing it is what the form
        # instructs; reconstructing it from the gain naturals would drop the
        # limitation and have to re-net the two terms by hand.
        #
        # Line 5a intentionally carries ALL of line 7. Gain from property held
        # in an active trade or business is excluded from net investment income
        # on line 5b, not by filtering 5a. We map no 5b because no input can
        # produce business-property gain; add both together if one ever does.
        "fed_import_map": {"L11": "L13", "L7": "L5a"},
        "activation_naturals": [
            "short_term_capital_gains",
            "long_term_capital_gains",
        ],
        "output_map": {"L17": "niit"},
        "total_tax_key": "L17",
        "defaults": {
            "Entity": "Individual",
            "Sec6013g": "No",
            "Sec6013h": "No",
            "Sec1141_10g": "No",
        },
    },
    # 2025 Schedule SE — Phase 1
    {
        "year": 2025,
        "form_id": "US_1040_Sched_SE",
        "phase": 1,
        "input_map": {
            "self_employment_income": "L2",
            # Line 8a: the filer's own social security wages, which fill the wage base
            # before self-employment earnings do. See TaxReturnInput.schedule_se_ss_wages.
            "schedule_se_ss_wages": "L8a",
        },
        "export_map": {"L12": "S2_4", "L13": "S1_15"},
        "output_map": {"L12": "se_tax"},
    },
    # 2025 Form 8959 — Phase 1
    {
        "year": 2025,
        "form_id": "Form_8959",
        "phase": 1,
        "input_map": {
            "w2_income": "L1",
            "filing_status": "Status",
            "self_employment_income": _se_income_to_8959_l8,
        },
        "export_map": {"L18": "S2_11"},
        "output_map": {"L18": "additional_medicare_tax"},
    },
    # 2025 Form 8995/8995-A — Phase 2; see the 2024 entry above.
    {
        "year": 2025,
        "form_id": "Form_8995",
        "phase": 2,
        "input_map": {},
        "export_map": {"L15": "L13a"},
        "activation_naturals": [
            "self_employment_income",
            "qbi_w2_wages",
            "qbi_ubia",
            "qbi_is_sstb",
        ],
        "output_map": {"L15": "qbi_deduction"},
    },
    # 2025 Schedule 8812 — Phase 2, after Form 8995 so the credit limit sees
    # the QBI deduction. See the 2024 entry; AGI is L11b from 2025.
    {
        "year": 2025,
        "form_id": "Form_8812",
        "phase": 2,
        "input_map": {
            "schedule_8812_qualifying_children": "L4",
            "schedule_8812_other_dependents": "L6",
            "filing_status": "Status",
        },
        "fed_import_map": {"L11b": "L1", "L18": "L13"},
        "export_map": {"L14": "L19"},
        "output_map": {"L14": "child_tax_credit"},
    },
    # 2025 Form 8960 — Phase 3
    {
        "year": 2025,
        "form_id": "Form_8960",
        "phase": 3,
        "input_map": {
            "taxable_interest": "L1",
            "ordinary_dividends": "L2",
            "rental_income": "L4a",
            "filing_status": "Status",
        },
        # See the 2024 Form 8960 entry for why line 5a is imported from the
        # 1040 rather than mapped from the gain naturals. The 2025 1040 splits
        # the capital-gain line, so the gain is on L7a here, not L7 — the same
        # year shift as L11 -> L11b for AGI.
        "fed_import_map": {"L11b": "L13", "L7a": "L5a"},
        "activation_naturals": [
            "short_term_capital_gains",
            "long_term_capital_gains",
        ],
        "output_map": {"L17": "niit"},
        "total_tax_key": "L17",
        "defaults": {
            "Entity": "Individual",
            "Sec6013g": "No",
            "Sec6013h": "No",
            "Sec1141_10g": "No",
        },
    },
]


def _build_subordinate_config() -> dict[int, list[SubordinateFormConfig]]:
    configs = [SubordinateFormConfig(**e) for e in _SUBORDINATE_FORM_CONFIG]
    result: dict[int, list[SubordinateFormConfig]] = {}
    for cfg in configs:
        result.setdefault(cfg.year, []).append(cfg)
    for year in result:
        result[year].sort(key=lambda c: c.phase)
    return result


SUBORDINATE_FORM_CONFIG = _build_subordinate_config()


METADATA_OUTPUT_FIELDS: tuple[str, ...] = ("assumptions",)
"""InterpretedTaxReturn fields that describe the computation rather than report a
number; None on a failed return."""

NUMERIC_OUTPUT_FIELDS: tuple[str, ...] = (
    "total_tax",
    "federal_adjusted_gross_income",
    "federal_effective_tax_rate",
    "federal_tax_bracket",
    "federal_taxable_income",
    "federal_qbi_deduction",
    "federal_amt",
    "federal_income_tax",
    "federal_se_tax",
    "federal_niit",
    "federal_additional_medicare_tax",
    "federal_child_tax_credit",
    "federal_total_tax",
    "state_adjusted_gross_income",
    "state_taxable_income",
    "state_total_tax",
    "state_tax_bracket",
    "state_effective_tax_rate",
)
"""The numeric outputs, NaN on a failed return. Written out, not derived, so a
field added to InterpretedTaxReturn must be classified (state_facts_test)."""
