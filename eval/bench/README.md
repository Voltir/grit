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
