Prepare current code for a pull request.

Prerequisites:
- Check `gh --version` to verify GitHub CLI is installed
- If not available, inform user to install it (https://cli.github.com/) and stop

0. Verify branch is not `main` - warn and confirm if so
1. Review `git status` and `git diff` for all changes
2. Ask user which files to include if unclear
3. Stage relevant files with `git add <paths>` (never `git add .`)
4. Run `pytest` - fix failures or report and stop
5. Create commit with concise message ending with:

   Generated with [Claude Code](https://claude.com/claude-code)

   Co-Authored-By: Claude <noreply@anthropic.com>

6. Handle pre-commit hooks - fix issues, never use --no-verify
7. Review `git log` and `git diff main...HEAD`
8. Ask user to confirm PR creation
9. Push (with -u if no tracking branch)
10. Create PR with `gh pr create`
