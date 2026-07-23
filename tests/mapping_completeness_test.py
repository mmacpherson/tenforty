"""Mapping-completeness inventory: every consumer edge is declared, both backends.

The audit's worst bug class was the silent missing edge: a form input that
neither backend fills, invisible to parity because both agree. This test
pins an explicit inventory of (form, natural input) consumer edges per
backend — "mapped", or "missing" with its tracking issue — and introspects
the real mapping tables against it. Adding or fixing an edge without
updating the inventory fails; so does a regression that drops one.
"""

import pytest

from tenforty.mappings import NATURAL_TO_NODES
from tenforty.models import SUBORDINATE_FORM_CONFIG

OTS_FORM_IDS = {
    "schedule_se": "US_1040_Sched_SE",
    "form_8959": "Form_8959",
    "form_8960": "Form_8960",
    "form_8995": "Form_8995",
}
GRAPH_PREFIXES = {
    "schedule_se": "us_schedule_se_",
    "form_8959": "us_form_8959_",
    "form_8960": "us_form_8960_",
    "form_8995": "us_form_8995_",
}

# Python-side derived fields stand in for the natural inputs they consume.
DERIVED_FROM = {"schedule_se_ss_wages": "w2_income"}

# The declared truth. "mapped" = the edge exists on that backend;
# "missing:<finding>" = known absent (docs/taxcalc-differential-audit.md).
# A fix updates this table in the
# same PR that adds the edge (and flips the corresponding burn-in xfail).
INVENTORY = {
    ("schedule_se", "self_employment_income"): {"ots": "mapped", "graph": "mapped"},
    ("schedule_se", "w2_income"): {"ots": "mapped", "graph": "mapped"},
    ("form_8959", "w2_income"): {"ots": "mapped", "graph": "mapped"},
    ("form_8959", "self_employment_income"): {"ots": "mapped", "graph": "mapped"},
    ("form_8960", "taxable_interest"): {"ots": "mapped", "graph": "mapped"},
    ("form_8960", "ordinary_dividends"): {"ots": "mapped", "graph": "mapped"},
    ("form_8960", "long_term_capital_gains"): {"ots": "mapped", "graph": "mapped"},
    ("form_8960", "short_term_capital_gains"): {"ots": "mapped", "graph": "mapped"},
    ("form_8995", "self_employment_income"): {
        "ots": "missing:F3",  # no Form 8995 config exists at all
        "graph": "mapped",
    },
}


def _ots_mapped(form_key: str, natural: str, year: int = 2024) -> bool:
    form_id = OTS_FORM_IDS[form_key]
    for cfg in SUBORDINATE_FORM_CONFIG.get(year, []):
        if cfg.form_id != form_id:
            continue
        # A form consumes a natural either directly through `input_map` or
        # indirectly through `fed_import_map`, in which case it declares the
        # natural in `activation_naturals`. Both are real consumer edges; only
        # counting the direct one would report Form 8960 as having lost its
        # capital-gain edge when it was moved to the 1040 line 7 import.
        for input_key in (*cfg.input_map, *cfg.activation_naturals):
            if input_key == natural or DERIVED_FROM.get(input_key) == natural:
                return True
    return False


# The output node whose derivative reveals whether a natural reaches a form,
# and a base return that puts that output in its active region. Used when a
# concept reaches a form through a spec-level `importForm` rather than a
# mapping-table entry (Form 8960 line 5a imports the 1040 capital-gain line;
# Form 8959 line 8 imports Schedule SE) — invisible to the mapping dict but a
# real, differentiable edge.
GRAPH_FORM_OUTPUT = {
    "schedule_se": "us_schedule_se_L10_se_tax",
    "form_8959": "us_form_8959_L18_total_additional_medicare",
    "form_8960": "us_form_8960_L17_niit",
    "form_8995": "us_form_8995_L16_qbi_deduction",
}
# High wages clear the NIIT and Additional-Medicare thresholds; the QBI forms
# want self-employment income without the wage limitation biting.
GRAPH_ACTIVE_BASE = {
    "schedule_se": {"self_employment_income": 60_000.0},
    "form_8959": {"w2_income": 250_000.0},
    "form_8960": {"w2_income": 250_000.0},
    "form_8995": {"self_employment_income": 60_000.0},
}


def _graph_flows(form_key: str, natural: str) -> bool:
    """Report whether the form's output actually moves with the natural.

    The derivative is the honest test of a consumer edge: it is nonzero
    exactly when the concept reaches the form's computation, whether it
    arrived by a mapping entry or a spec import, and it stays zero for a
    wrong-destination wiring. Requires the graph backend and the fan-out-aware
    autodiff from #294.
    """
    from tenforty.backends import GraphBackend
    from tenforty.models import TaxReturnInput

    backend = GraphBackend()
    if not backend.is_available():
        return _graph_mapped(form_key, natural)

    case = {"year": 2024, "filing_status": "Single", **GRAPH_ACTIVE_BASE[form_key]}
    case[natural] = case.get(natural, 0.0) + 100_000.0
    gradient = backend.gradient(
        TaxReturnInput(**case), GRAPH_FORM_OUTPUT[form_key], natural
    )
    return gradient is not None and abs(gradient) > 1e-9


def _graph_mapped(form_key: str, natural: str) -> bool:
    prefix = GRAPH_PREFIXES[form_key]
    for natural_name, nodes in NATURAL_TO_NODES.items():
        if natural_name != natural and DERIVED_FROM.get(natural_name) != natural:
            continue
        if any(node.startswith(prefix) for node in nodes):
            return True
    return False


@pytest.mark.parametrize(("form_key", "natural"), sorted(INVENTORY))
def test_consumer_edge_matches_inventory(form_key, natural):
    """The real mapping state must equal the declared inventory, per backend."""
    declared = INVENTORY[(form_key, natural)]
    actual = {
        "ots": _ots_mapped(form_key, natural),
        "graph": _graph_mapped(form_key, natural) or _graph_flows(form_key, natural),
    }
    for backend, state in declared.items():
        expected = state == "mapped"
        assert actual[backend] == expected, (
            f"{backend}: ({form_key}, {natural}) is "
            f"{'mapped' if actual[backend] else 'unmapped'} but inventory says "
            f"{state!r} — update the inventory in the same PR as the mapping change"
        )
