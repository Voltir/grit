# grit's hook for Claude Code: one program, a subcommand per role, that the join script installs
# after a shebang and the lines setting `harness` and `edge` (GRIT_EDGE, when set, overriding
# `edge`). Each subcommand is a hook registered in the checkout's .claude/settings.local.json or
# git's post-commit and post-rewrite, or the holder a Bash tool asks for (README.md).
#
# Not yet written: every subcommand exits 0 and prints nothing, so a session it is installed in
# goes on as if it were not.
exit 0
