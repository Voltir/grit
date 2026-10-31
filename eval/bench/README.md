# The bench's frozen tasks

Each directory under `tasks/` is one past change, frozen for the coder bench
(`grit.eval.harness.bench`, read by `Task.of`):

- `task.json`: `base`, the commit the change started from; `commits`, the commits that made it, in
  order, applied by `Clone.picked` for the known diff; `checks`, each a runner (`check` for
  `scripts/check`, `it` for `scripts/it`) and its arguments. The law is every task's check, run
  after these. A check names the module holding the suites the commits named, not the suite: a
  suite new in the known diff would fail a coder who named its tests otherwise.
- `brief.md`: what the change's coder was given. A checkpoint's brief is its text as the store held
  it just before the first commit, with every artifact it cites appended, as of that time, under
  `## Cited:`. Text written after the first commit is left out, so the brief tells no more than its
  coder knew. The proving-ground deployment is named only as that.

A task is kept only when its commits apply, in order, to its base and give the known commit's tree.

## The Claude Code arm

`scripts/eval bench` runs `claude -p` in the task's checkout, its prompt a fixed preamble and the
task's brief, as the agent `bench-coder` (`coder.md` here, passed with `--agents` and `--agent`),
under `--restricted` and `--permission-prompts none`: file tools stay inside the checkout, only the
settings the rig passes load, and a call no rule allows is refused and recorded with the trial
(`Rig.Allowed`: reads and edits, scout, `./mill`, `scripts/check`, and git's `add`, `commit`,
`status`, `diff` and `log`). The model, Claude Code's budget and its turns are required on every
run. The baseline is therefore Claude Code under grit's rule, not Claude Code as a person runs it
with its own settings, and commands that only read (`ls`, `cat`, `grep`) still run without a rule.
The run is billed to the Anthropic API key the rig passes, priced by grit's own rates beside
Claude Code's estimate. A run a cap stopped is a trial not passed.
