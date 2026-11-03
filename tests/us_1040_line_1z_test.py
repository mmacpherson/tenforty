"""Form 1040 line 1z leaves out line 1i (the nontaxable combat pay election).

Evidence kind: **published form text**. Form 1040 (2024) and Form 1040 (2025) line 1z
both read "Add lines 1a through 1h"; line 1i is an entry used only for the earned
income credit and Schedule 8812 (2025 Instructions for Form 1040, PDF p.25, line 1i).
Sources: https://www.irs.gov/pub/irs-prior/f1040--2024.pdf and
https://www.irs.gov/pub/irs-pdf/f1040.pdf (2025), both page 1, retrieved 2026-11-02.

There is no public combat-pay input, so the check sets the graph node directly. With
only wages on lines 1a and 1i, AGI (line 11 in 2024, line 11a in 2025) is the wages.
"""

import pytest

pytestmark = pytest.mark.requires_graph


@pytest.mark.parametrize("year", [2024, 2025])
def test_combat_pay_election_stays_out_of_line_1z_and_agi(year):
    """Wages 50,000 on line 1a and 20,000 on line 1i: line 1z and AGI are 50,000."""
    from tenforty.backends.graph import _load_resolved_graph
    from tenforty.graphlib import FilingStatus, Runtime

    evaluator = Runtime(_load_resolved_graph(year), FilingStatus.from_str("single"))
    evaluator.set("us_1040_L1a_wages", 50_000.0)
    evaluator.set("us_1040_L1i_nontaxable_combat_pay", 20_000.0)

    assert evaluator.eval("us_1040_L1z_total_wages") == pytest.approx(50_000.0)
    assert evaluator.eval("us_1040_L11_agi") == pytest.approx(50_000.0)
