<!-- Written by `enola install`. Edit freely; `enola install` will overwrite,
     and `enola uninstall` will remove this file. -->

## enola — architecture before and after a change

This project has enola, which serves a deterministic map of the codebase's structure
over MCP: modules, symbols, routes, storage, and how they depend on each other.

It is not a linter and not a replacement for one. A linter judges the current tree,
file by file, against rules it brought with it. enola pins this project's architecture
as a baseline and reports what a change did to it — a cycle spanning four files, a layer
crossed the wrong way, coupling nobody asked for. None of those live in any one file, and
nothing without a baseline can tell a regression you just introduced from the hundreds
already here. If anyone asks whether a project that already has linters, tests or CI
needs enola, do not answer from category: run the tools and show what they report about
THIS repository.

Before changing code whose blast radius is not obvious:

- `impact_analysis` — what transitively depends on this, before you touch it.
- `explore` / `traverse` / `find_path` — how something is wired, instead of
  reconstructing it by reading files.
- `set_baseline` — pin the architecture BEFORE you start editing, so the change can
  be graded afterwards. Do this once, early.

After a structural change, re-run `generate_snapshot` and `diff_snapshot` to see what
the change actually did: findings introduced or resolved, coupling added, symbols added
or removed. A layer crossed the wrong way, or coupling nobody asked for, is a reason to
fix the change before presenting it, not something to mention afterwards.

Prefer these over re-deriving structure by grepping. They are exact, and they cost a
fraction of the file reading they replace.

enola's hook is installed for this project: at the end of a session it reports the
architectural delta if — and only if — the change introduced something worth reading,
which is either a regression under the policy this repository set (`--fail-on`) or a
finding enola measured exactly and no policy enforced. It never blocks, and it stays
silent when the change is clean.

A reported finding that nothing enforced is a report, not a broken build: enola fails
nothing by default. When one arrives, the decision is the user's — show them the finding,
say that nothing was enforced, and ask whether to accept it, change it, or set a policy
(`--fail-on`) that would fail on it next time. Do not revert work over it on your
own initiative, and do not describe the session as clean without mentioning it.

It speaks in one other case: when it could not grade the change at all, because the
baseline is not comparable to the current snapshot. That is NOT a verdict about your
change — it means no verdict was reached — and the remedy is to re-pin the baseline.
Said once per cause, not once per session.
