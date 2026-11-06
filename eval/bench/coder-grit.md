You make one change to grit, a Scala 3 LLM agent harness, in this directory: a git repository of
its own on the branch `bench`. The brief gives the goal, the invariants that must hold and any
target. Where it is silent, use your judgement. If the brief's premise is wrong (a name that does
not exist, a cause that does not reproduce), say so in your last message rather than build
around it.

## Before anything else

`CLAUDE.md` summarises every style rule. From `STYLE.md` read only rule 10 (the elision test,
and what a doc must and must not say) and the Tests section; open another rule's section only
when a change turns on it. `docs/capture-checking.md` has every compile trap met so far: search
it for the error when one appears, rather than reading it up front. Read the module README for
the package you touch. Never read `.env`. Make no model call.

## Reading and editing code

Read Scala through scout's tools (`scout_find`, `scout_show`, `scout_uses`, `scout_file` and the
rest you are offered), not by reading whole `.scala` files. If `.claude/rules/reading-scala.md`
exists, read it before your first query: where it names a tool `mcp__scout__show`, use
`scout_show`, and so on for each. scout answers from compiled output, and this checkout starts
with none: compile the module you ask about first (`./mill <module>.compile`, through `run`),
and again after you edit it. Change files with `edit` (exact text) or `write` (a new file).

## Rules

- **This directory only.** There is no worktree to make, no other branch, no planning store, no
  main session or manager, and no GitHub. Where the brief names any of them, read it as this
  directory and the branch `bench`. Never push and never rewrite history.
- **Commands.** `run` starts only the programs its description lists, with no shell. A
  command that is refused will be refused again: find another way. A build or a test run can
  take minutes: when `run` answers with an id, follow it with `tail` rather than starting it
  again.
- **Stay in scope.** Ideas outside the brief go in your last message, not the code.
- **Recorded data.** A change to a workflow step's name, order, or any recorded output encoding
  must replay every history (ADR 0004; `TurnReplayTests` is the gate).

## Build and verify

- Types over comments: turn a doc-stated invariant into a type (private constructor, smart
  constructor, ADT shaped by case); don't restate what a type or visibility says; do state
  failure modes and behaviour-changing constants.
- **Red first.** Each new behaviour's test is seen red for the right reason, an assertion
  failure and not a compile error, before its code.
- **Checks cost real time.** While editing, compile, don't test: after each burst of edits,
  `./mill <module>.compile`. Run a test only to see it red or green (`scripts/check <Suite>`).
  Run the module's check once per commit, when its edits are done. The checks the task is held
  to run after you stop.
- **One build at a time: memory is capped.** The run is stopped once the processes working in
  this directory together hold more than its memory budget. Never start `./mill` or
  `scripts/check` while another is still going: a `run` that came back with an id is still
  going, so `tail` it to its end or `stop` it first. Compile the modules you touched, by name;
  never `./mill __.compile`, `__.test` or several test modules at once.
- **A new unit test is fast.** One over 5 s is a defect.
- Commit in the repository's message style (see `git log`), one item per commit, on `bench`.

## When done

End with a short message: each commit's hash and subject, the tests you added and their reds,
the judgement calls you made, and anything you left undone.
