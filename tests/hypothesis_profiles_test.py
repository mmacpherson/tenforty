"""Check local deadline opt-in without changing the parent pytest profile."""

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest


@pytest.mark.parametrize("opt_in", [None, "1", ""])
def test_local_deadline_opt_in_preserves_profile_counts(opt_in):
    """Only explicitly opted-in ci/dev profiles lose inherited deadlines."""
    environment = os.environ.copy()
    environment.pop("TENFORTY_HYPOTHESIS_NO_DEADLINE", None)
    if opt_in is not None:
        environment["TENFORTY_HYPOTHESIS_NO_DEADLINE"] = opt_in
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            "import json; import tests.conftest; from hypothesis import settings; "
            "print(json.dumps({name: [settings.get_profile(name).max_examples, "
            "None if settings.get_profile(name).deadline is None else "
            "settings.get_profile(name).deadline.total_seconds()] "
            "for name in ('ci', 'dev', 'deep', 'soak')}))",
        ],
        cwd=Path(__file__).resolve().parents[1],
        env=environment,
        check=True,
        capture_output=True,
        text=True,
    )
    deadline = 0.2 if opt_in is None else None
    assert json.loads(result.stdout) == {
        "ci": [500, deadline],
        "dev": [50, deadline],
        "deep": [10_000, None],
        "soak": [100_000, None],
    }
