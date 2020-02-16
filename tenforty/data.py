# -*- coding: utf-8 -*-
import collections


def float_or_float_list():
    return True


def yes_no():
    return True


def one_of(x):
    return True


def nonnegative_integer():
    return True


Field = collections.namedtuple(
    "Field",
    "key aliases valid_fn default terminator",
    defaults=(None, [], float_or_float_list, None, ";"),
)
US_1040 = (
    Field(key="Title:", aliases="title", terminator="\n",),
    Field(
        key="Status",
        aliases=("status", "filing_status"),
        valid_fn=one_of(
            frozenset(
                ("Single", "Married/Joint", "Head_of_House", "Married/Sep", "Widow(er)")
            )
        ),
        default="Single",
        terminator="\n",
    ),
    Field(
        key="You_65+Over?",
        aliases="you_are_over_65",
        valid_fn=yes_no,
        default="N",
        terminator="\n",
    ),
    Field(
        key="You_Blind?",
        aliases="you_are_blind",
        valid_fn=yes_no,
        default="N",
        terminator="\n",
    ),
    Field(
        key="Spouse_65+Over?",
        aliases="spouse_is_over_65",
        valid_fn=yes_no,
        default="N",
        terminator="\n",
    ),
    Field(
        key="Spouse_Blind?",
        aliases="spouse_is_blind",
        valid_fn=yes_no,
        default="N",
        terminator="\n",
    ),
    Field(
        key="HealthCoverage?",
        aliases="have_health_coverage",
        valid_fn=yes_no,
        default="Y",
        terminator="\n",
    ),
    Field(
        key="Dependents",
        aliases="num_dependents",
        valid_fn=nonnegative_integer,
        default=1,
        terminator="\n",
    ),
    Field(key="L1", aliases=("wages", "wages_salaries_tips", "w2_box_1")),
    Field(key="L2a", aliases=("tax_exempt_interest",)),
    Field(key="L2b", aliases=("interest", "1099_int_box_1", "1099_interest",)),
    Field(key="L3a", aliases=("qualified_dividends", "1099_div_box_1b",)),
    Field(key="L3b", aliases=("ordinary_dividends", "1099_div_box_1a",)),
    Field(key="L4a", aliases=("ira_distributions",)),
    Field(key="L4b", aliases=("taxable_ira_distributions",)),
    Field(
        key="L5a",
        aliases=("social_security_benefits", "ss_benefits", "ssa_1099_box_5"),
    ),
    Field(key="L9", aliases=("qualified_business_income_deduction",),),
    Field(key="L12a", aliases=("child_tax_credit",),),
    Field(
        key="L16", aliases=("withheld", "withholding", "federal_income_tax_withheld",),
    ),
    Field(key="L17a", aliases=("eic", "earned_income_credit",),),
    Field(key="L17b", aliases=("child_tax_credit", "schedule_8812_credit"),),
    Field(key="L17c", aliases=("education_credit", "form_8863_credit"),),
    Field(
        key="CapGains-A/D",
        aliases=(
            "capital_gains_a_d",
            "cap_gains_a_d",
            "capital_gains_basis_reported",
            "form_8949_check_a_or_d",
        ),
    ),
    Field(
        key="CapGains-B/E",
        aliases=(
            "capital_gains_b_e",
            "cap_gains_b_e",
            "capital_gains_basis_not_reported",
            "form_8949_check_b_or_e",
        ),
    ),
    Field(
        key="CapGains-C/F",
        aliases=("capital_gains_c_f", "cap_gains_c_f", "form_8949_check_c_or_f"),
    ),
    Field(key="D4"),
    Field(key="D5"),
    Field(key="D6"),
    Field(key="D11"),
    Field(key="D12"),
    Field(key="D13"),
    Field(key="D14"),
    Field(key="Collectibles"),
    Field(key="S1_10"),
    Field(key="S1_11"),
    Field(key="S1_12"),
    Field(key="S1_14"),
    Field(key="S1_17"),
    Field(key="S1_18"),
    Field(key="S1_19"),
    Field(key="S1_21"),
    Field(key="S1_23"),
    Field(key="S1_24"),
    Field(key="S1_25"),
    Field(key="S1_26"),
    Field(key="S1_27"),
    Field(key="S1_28"),
    Field(key="S1_29"),
    Field(key="S1_30"),
    Field(key="S1_31a"),
    Field(key="S1_32"),
    Field(key="S1_33"),
    Field(key="A1"),
    Field(key="A5a"),
    Field(key="A5b"),
    Field(key="A5c"),
    Field(key="A6"),
    Field(key="A8a"),
    Field(key="A8b"),
    Field(key="A8c"),
    Field(key="A9"),
    Field(key="A11"),
    Field(key="A12"),
    Field(key="A13"),
    Field(key="A15"),
    Field(key="A16"),
    Field(key="B7a", valid_fn=yes_no, default="N"),
    Field(key="B7aa"),
    Field(key="B7b"),
    Field(key="B8", valid_fn=yes_no, default="N"),
    Field(key="AMTws2c"),
    Field(key="AMTws2g"),
    Field(key="S2_46"),
    Field(key="S3_48"),
    Field(key="S3_49"),
    Field(key="S3_50"),
    Field(key="S3_51"),
    Field(key="S3_53"),
    Field(key="S3_54"),
    Field(key="S4_57"),
    Field(key="S4_58"),
    Field(key="S4_59"),
    Field(key="S4_60a"),
    Field(key="S4_60b"),
    Field(key="S4_61"),
    Field(key="S4_62"),
    Field(key="S4_63"),
    Field(key="S5_66"),
    Field(key="S5_70"),
    Field(key="S5_71"),
    Field(key="S5_72"),
    Field(key="S5_73"),
    Field(key="S5_74"),
    Field(key="Your1stName:", terminator="\n"),
    Field(key="YourLastName:", terminator="\n"),
    Field(key="YourSocSec#:", terminator="\n"),
    Field(key="Spouse1stName:", terminator="\n"),
    Field(key="SpouseLastName:", terminator="\n"),
    Field(key="SpouseSocSec#:", terminator="\n"),
    Field(key="Number&Street:", terminator="\n"),
    Field(key="Apt#:", terminator="\n"),
    Field(key="TownStateZip:", terminator="\n"),
)
