Run complete quality assurance checks for graph backend (Haskell + Rust) before committing.

Prerequisites:
- Changes involve Haskell code in `tenforty-spec/` or state tax forms
- Working on graph backend or Rust code in `crates/tenforty-graph/`

0. Check `git status` to understand current state
1. Format Haskell code: `make spec-fmt`
   - May alphabetize imports or reformat code
   - Note any formatting changes for staging
2. Lint Haskell code: `make spec-lint`
   - Should report "No hints"
   - Fix any hints before proceeding
3. Generate JSON graphs: `make spec-graphs`
   - Watch for compilation errors or warnings
   - Note any unexpected changes to existing forms
4. Sync graphs to Python: `make forms-sync`
   - Reports number of graphs synced
5. Rebuild Rust extension: `make env-full`
   - Critical: keeps `.so` in sync with Rust source
   - Prevents test failures from out-of-date extension
6. Run pre-commit hooks: `make run-hooks`
   - Must pass before committing
   - Re-stage files if auto-fixed
7. Run test suite: `uv run pytest tests/ -q --tb=line`
   - Expected: 203+ passed, some skipped/xfailed
   - All graph tests must pass
8. Review staging with `git status`
   - Check for unexpected changes (modified forms, etc.)
   - Remove orphaned JSONs without Haskell sources if any
   - Stage formatting changes from step 1 if needed
9. For state-specific work, run focused test:
   - `uv run pytest tests/regression_test.py -k "WI" -v` (replace state)
10. Report results and any files needing staging decisions

Common issues:
- If `graphlib_link_year_test.py` fails: `.so` out of sync, run `make env-full`
- If unexpected JSON changes: review with `git diff`, decide to include/separate/discard
- If untracked JSON files appear: likely stale, safe to remove

Always prefer make targets over running commands directly.
