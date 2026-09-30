---
name: grit-test-cycle
description: Running grit's tests and gates while changing its code — scripts/check for a suite, a module or the commit gate, red-first tests, the integration tier, replay histories, Mill in a git worktree, and leaving no JVM behind.
---

# grit's test cycle

## Run it with `scripts/check`

```bash
scripts/check grit.core.speech.SpeechTests   # one suite, while iterating
scripts/check grit.core                      # one module's unit tests
scripts/check grit.dbos.it                   # an it module or suite (Docker)
scripts/check                                # the commit gate: unit, law, lint
scripts/check --it                           # the gate and the integration tier
```

It prints each failing test, every compile error and warning, and one totals line per tier;
the full logs are in `out/check/`. `scripts/check --help` has the options (`--timeout`,
`--no-lint`). Quote the totals lines in the commit message. Don't pipe Mill through
`tail`/`grep` yourself: a pipe loses Mill's exit code, and a run a compile error or a timeout
cut short prints a smaller total that looks green.

- **While iterating**, run the suite or module you touched. When the only question is
  whether it type-checks, `./mill grit.<module>.compile` (or `grit.<module>.test.compile`)
  answers in about a second warm.
- **At a commit point**, once: `./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll
  __.sources`, then bare `scripts/check`. Its tiers are the unit tier (`__.test.testCached`:
  a module's suites rerun only when their inputs changed; a failure is never cached), the
  law (`scripts/enola-law.sh`; `bash scripts/fetch-enola.sh` first if `tools/` lacks enola)
  and, last, **lint**: every module compiled with `-Wunused:all` in its own output
  directory. An unused import passes a targeted run and fails lint.
- **A cached suite can miss a file it reads**: a file a test reads by path is not an input
  unless `build.mill` makes its content one (`grit.turn.test`'s `histories` is the pattern).

## Red first

The rule and its reasons: [STYLE.md, Tests](../../../STYLE.md#tests). The procedure:

- **New behaviour or a bug fix**: write the test first and run it red against the code as
  it stands (for a bug, red on the bug).
- **Existing behaviour**: plant the smallest plausible compiling change the test's name
  rules out — never a deletion or a constant — run it red, and revert it before committing.
- Either way the red is an assertion failure on the named behaviour, not a compile error
  or a throwing stub. The commit message quotes the failing assertion, and for a plant
  carries its diff. A test that stays green is not done.

## The integration tier (`--it`)

Run it before committing a change to `grit.dbos`, `schema.sql`, an `it` module or the
engine's live paths, and when a milestone closes. It needs Docker; there is no skip.

## Replay histories

`bash scripts/capture-history.sh <workflow-id>` writes a recorded workflow as a fixture under
`grit/turn/test/histories/<epoch>/`. A change to a workflow's step names, order or output
encodings must replay every history of its epoch
([ADR 0004](../../../docs/decisions/0004-workflows-evolve-by-patch-within-a-compatibility-epoch.md));
`TurnReplayTests` and `LifecycleReplayTests` are the gate.

## In a git worktree

Build and test with Mill. Metals serves the main checkout, so its `compile-*`/`test` there
build code that is not yours; its read queries (`get-usages`, `typed-glob-search`,
`get-docs`, `inspect` with `fileInFocus`) are sound for code your branch has not changed. A
CLI `./mill` and Metals never block each other (separate build directories).

## JVMs

- One JVM-heavy job at a time on this machine: no parallel Mill, test or grit runs.
- Stamp what you start; kill only your own, by PID. Never `pkill` by pattern: Nick may
  have grit or a demo open elsewhere.
- On Mill "Connection refused": `./mill shutdown` and retry.
- At the end: `./mill shutdown` in your checkout. `scripts/check` shuts down the lint
  tier's daemon itself, and on a timeout the timed-out tier's.
