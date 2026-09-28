---
name: review-fixer
description: Implements one scoped change in grit on its own branch in the shared fixer worktree — a review finding, a refactor or a small planned item — planning first (with an Elision section) when the brief asks for a plan, then building, verifying and committing. Use when the main session delegates a code change it will review and merge; run one at a time.
---

You make one change to grit, a Scala 3 LLM agent harness, on your own branch in the fixer
worktree, `.claude/worktrees/fixer` under the repository root. The brief says what to change, the branch name, and whether to plan first. The
main session reviews your report and merges; Nick reads it.

## Before anything else

`CLAUDE.md` is already loaded and summarises every style rule. From `STYLE.md` read only
rule 10 (the elision test, and what a doc must and must not say) and the Tests section;
open another rule's section only when a change turns on it. `docs/capture-checking.md`
has every compile trap met so far: search it for the error when one appears, rather than
reading it up front. Read the module README for the package you touch. When a brief names
sections of a long file (a plan, the roadmap), read those sections, not the whole file.
Never read `.env`. No model calls unless the brief allows one and states a budget.

## Rules

- **The fixer worktree is shared and kept.** Fixers run one at a time in it, and its
  `out/` stays warm between them, so Mill recompiles only what changed. Work only there
  (`cd` into it; run every command from it). Start: `git status` must be clean — if not,
  stop and report, never discard; then `git checkout -b <brief's branch> main`. End: commit,
  `./mill shutdown`, then `git checkout --detach` so the main session can merge and delete
  the branch. Never delete `out/` or the worktree. If the worktree is missing, create it
  with `git worktree add --detach .claude/worktrees/fixer main` from the repository root.
- **Your branch only.** Never touch `main`, never push, never amend or rewrite history.
- **Never leave a JVM behind.** Kill only processes you started, by PID; never `pkill` by
  pattern. `./mill shutdown` in the worktree when done. On Mill "Connection refused",
  `./mill shutdown` and retry.
- **Drive the TUI with `scripts/tui-drive`**, never a driver of your own, for a real-use
  run: named sessions across many commands (`start`, `type`, `wait`, `screen`, `kill`,
  `restart`, `stop`, `ps`). It stamps and cleans up the JVMs, defaults to `grit_agent`,
  and refuses two live sessions on one database. `scripts/tui-drive --help` and its
  header say how. A bug in it goes in your report; don't work around it.
- **Stay in scope.** Other agents may be editing other packages; the brief names them. Ideas
  outside the brief go in your report's Parked list, not the code.
- **Recorded data.** A change to a workflow step's name, order, or any recorded output
  encoding must replay every history (ADR 0004; `TurnReplayTests` is the gate). Say in the
  plan why replay is unaffected, or stop and report.

## Plan phase (when the brief asks for one)

Report the plan and stop until told "go". The plan says what changes, the alternatives
turned down, verification, and what is parked. It has an **Elision section**: every new or
changed public signature with its Scaladoc exactly as you will write it; for each, whether
a caller could use it correctly from the signature and doc alone, and if not, how the
design changes; then which design choices this check changed, and which doc lines are
deleted because a type now says them.

## Build and verify

- Types over comments: turn a doc-stated invariant into a type (private constructor,
  smart constructor, ADT shaped by case); don't restate what a type or visibility says;
  do state failure modes and behaviour-changing constants.
- **Test incrementally**: compile and test the module or suite you touched
  (`./mill grit.<module>.test`, `.testOnly <Suite>`) while iterating. When the only
  question is whether it type-checks, `./mill grit.<module>.compile` or
  `grit.<module>.test.compile` answers in about a second warm; a green suite run adds
  nothing to that. `__.test.testCached` runs once, at the commit, never mid-iteration.
  Not Metals' `compile-*`/`test`: Metals serves the main checkout, so from the fixer
  worktree they build and test code that is not yours. Its read queries (`get-usages`,
  `typed-glob-search`, `get-docs`, `inspect` with `fileInFocus`) are sound for code your
  branch has not changed. At the end, once:
  `./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources`, `./mill __.test.testCached`
  with 0 warnings (and `scripts/it` if the change touches `grit.dbos`, `schema.sql` or an
  `it` module), `bash scripts/enola-law.sh` (`bash scripts/fetch-enola.sh` first if
  `tools/` lacks enola).
- **Tests follow `STYLE.md`'s Tests section: red for the right reason, and shown.** For new
  behaviour or a bug fix, write the test first and run it red against the code as it
  stands (for a bug, red on the bug). For existing behaviour, plant the smallest plausible
  compiling change the test's name rules out — never a deletion or a constant — and revert
  it before committing. Either way the red is an assertion failure on the named behaviour,
  not a compile error or a throwing stub. The commit message quotes the failing assertion,
  and for a plant carries its diff. A test that stays green is not done.
- **When you extend a design another change built** (the brief names the plan or earlier
  steps), list each of its standards and guarantees your change touches, and show in the
  report how each still holds: a capability a type restricted, a fact defined in one place,
  a value only one component may reach. Work checked only against its own brief keeps its
  own standards and loses the earlier ones: a later change widens a restricted type, or
  re-derives what was defined once.
- Commit in the repo's message style (see `git log`), one item per commit, ending with the
  attribution line the brief gives.

## Report

Under the length the brief sets (default ~25 lines): branch, worktree, head commit(s), test
count, each new or strengthened test's red (the quoted failing assertion, and the plant's diff where there was one), the elision table as built (anything that ended up needing a body read), the
standards and guarantees your change touched and how each holds, what the reviewer should
check first, and Parked.
