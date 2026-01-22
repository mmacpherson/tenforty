Refresh Claude Code's memory on branch changes.

1. Run `git diff --stat main...HEAD` and `git diff --name-status main...HEAD`
2. Parse output to detect renames (exclude from counts)
3. Auto-select strategy:
   - Small (<10 files): Load all changed files
   - Medium (10-25 files): Load core full, list test names only
   - Large (>25 files): Use diffs for files >500 lines
4. Execute:
   - Core files (src/tenforty/*.py): Read full
   - Test files: List test names only
   - Generated files (*.cpp, *.pyx): Show diff only
   - OTS source: Skip (too large)
5. Read files in parallel
6. Summarize changes by type
