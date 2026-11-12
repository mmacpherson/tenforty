"""Evaluate the workflow's concurrency expressions across event contexts."""

import re
from pathlib import Path
from types import SimpleNamespace

import pytest
import yaml


@pytest.fixture
def workflow():
    """Load the actual workflow policy without mirroring its expressions."""
    return yaml.safe_load(
        (Path(__file__).resolve().parents[1] / ".github/workflows/ci.yml").read_text(
            encoding="utf-8"
        )
    )


def test_workflow_name_matches_concurrency_guard(workflow):
    """Keep the caller-name guard tied to the workflow name."""
    assert workflow["name"] == "ci"


def _expression(source, context):
    """Translate the string/boolean expression subset used by this policy."""
    source = source.replace("&&", " and ").replace("||", " or ")
    source = re.sub(r"!(?!=)", " not ", source)
    return eval(
        source.strip(),
        {"__builtins__": {}, "github": context, "startsWith": str.startswith},
    )


def _policy(workflow, event, ref, run_id, caller="ci"):
    context = SimpleNamespace(
        workflow=caller, event_name=event, ref=ref, run_id=str(run_id)
    )
    policy = workflow["concurrency"]
    group = re.sub(
        r"\$\{\{(.*?)\}\}",
        lambda match: str(_expression(match.group(1), context)),
        policy["group"],
        flags=re.DOTALL,
    )
    cancel = _expression(policy["cancel-in-progress"][3:-2], context)
    return group, cancel


@pytest.mark.parametrize(
    ("event", "ref", "caller", "coalesces"),
    [
        ("push", "refs/heads/feature", "ci", True),
        ("push", "refs/heads/main", "ci", False),
        ("push", "refs/tags/2025.15", "ci", False),
        ("workflow_call", "refs/heads/feature", "release", False),
        ("push", "refs/tags/2025.15", "release", False),
        ("push", "refs/heads/feature", "release", False),
        ("workflow_dispatch", "refs/heads/feature", "ci", False),
        ("workflow_dispatch", "refs/heads/main", "ci", False),
    ],
)
def test_event_policy(workflow, event, ref, caller, coalesces):
    """Only ordinary branch pushes may cancel or replace another pending run."""
    first, cancel = _policy(workflow, event, ref, 101, caller)
    second, _ = _policy(workflow, event, ref, 102, caller)
    assert cancel is coalesces
    assert (first == second) is coalesces
    assert first == f"{caller}-{ref}-{'branch' if coalesces else '101'}"


@pytest.mark.parametrize(
    "events", [("workflow_dispatch", "push"), ("push", "workflow_dispatch")]
)
def test_dispatch_and_push_do_not_replace_each_other(workflow, events):
    """Protect both an explicit wheel build and an ordinary validation run."""
    first, _ = _policy(workflow, events[0], "refs/heads/feature", 101)
    second, _ = _policy(workflow, events[1], "refs/heads/feature", 102)
    assert first != second
