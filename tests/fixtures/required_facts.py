"""Required state facts for tests whose quantity does not depend on them."""

from tenforty.state_facts import OH, StateFacts


def credit_neutral_facts(state: str | None, filing_status: str) -> StateFacts | None:
    """Facts a graph return requires, for a check the fact cannot affect.

    OH Married/Joint on graph requires the joint filing credit fact. A bracket
    or taxable-income check does not depend on that credit, so these tests pass
    False, the value the graph computes today (tenforty-b72.53).
    """
    if state == "OH" and filing_status == "Married/Joint":
        return OH(spouses_both_have_qualifying_income=False)
    return None
