"""Gold standard scenarios: IRS Direct File validated test fixtures."""

from .tax_scenario import TaxScenario

IRS_DIRECT_FILE_SCENARIOS = [
    # ATS-1: Susan Miranda - Single filer in Massachusetts
    # Source: direct-file/backend/src/test/resources/facts/scenarios/ats-1.json
    # Expected values from IRS Direct File test suite
    TaxScenario(
        source="IRS Direct File ATS-1 (Susan Miranda)",
        description="MA Single filer with two W2s, $39,674 total wages",
        year=2023,
        state="MA",
        filing_status="Single",
        w2_income=39674.0,
        expected_federal_tax=2879.0,
        expected_federal_agi=39674.0,
    ),
    # ATS-2: Samuel Smith + Judy Johnson - MFJ in Florida with 1 dependent
    # Source: direct-file/backend/src/test/resources/facts/scenarios/ats-2.json
    # Note: This scenario includes CTC ($500) and EITC ($2,468) credits
    # tenforty computes pre-credit tax ($998), not post-credit ($498)
    TaxScenario(
        source="IRS Direct File ATS-2 (Smith/Johnson)",
        description="FL MFJ filers with $37,693 W2, 1 dependent, includes EITC/CTC",
        year=2023,
        state=None,  # FL has no state income tax
        filing_status="Married/Joint",
        w2_income=37693.0,
        num_dependents=1,
        expected_federal_tax=998.0,  # Pre-credit tax
        expected_federal_agi=37693.0,
    ),
    # Single filer with two W2s totaling $110k
    # Source: direct-file/backend/src/test/resources/facts/allFacts_accepted_singleTwoW2s.json
    # IRS expects $17,128, OTS computes $17,134 (+$6 rounding)
    TaxScenario(
        source="IRS Direct File (singleTwoW2s)",
        description="Single filer with $110,000 total W2 income",
        year=2022,
        state=None,
        filing_status="Single",
        w2_income=110000.0,
        expected_federal_tax=17128.0,
        expected_federal_agi=110000.0,
        known_failure="OTS computes $17,134 vs IRS expected $17,128 (+$6 rounding)",
    ),
    # Single filer with $70k income
    # Source: direct-file/backend/src/test/resources/facts/allFacts_accepted_singleChrisValues.json
    # IRS expects $8,168, OTS computes $8,174 (+$6 rounding)
    TaxScenario(
        source="IRS Direct File (singleChrisValues)",
        description="Single filer with $70,000 W2 income",
        year=2022,
        state=None,
        filing_status="Single",
        w2_income=70000.0,
        expected_federal_tax=8168.0,
        expected_federal_agi=70000.0,
        known_failure="OTS computes $8,174 vs IRS expected $8,168 (+$6 rounding)",
    ),
]
