---
name: grit-test-cycle
description: Running grit's tests and gates while changing its code — scripts/check for a suite, a module or the commit gate, red-first tests, capture probes, the integration tier and the milestone run, replay histories, Mill in a git worktree, and leaving no JVM behind.
---

# grit's test cycle

## Run it with `scripts/check`

```bash
scripts/check grit.core.speech.SpeechTests   # one suite, while iterating
scripts/check grit.core                      # one module's unit tests
scripts/check grit.dbos.it                   # an it module or suite (Docker)
scripts/check grit.core.durable.SeparationTests  # a probe suite
scripts/check                                # the commit gate: unit, law, probes a change reaches, format, clock, unsafe, docs, lint
scripts/check --it                           # the gate and the integration tier
scripts/check --milestone                    # the gate, every probe and the integration tier
```

It prints each failing test, every compile error and warning, and one totals line per tier;
the full logs are in `out/check/`. `scripts/check --help` has the options (`--timeout`,
`--no-lint`). Quote the totals lines in the commit message. Don't pipe Mill through
`tail`/`grep` yourself: a pipe loses Mill's exit code, and a run a compile error or a timeout
cut short prints a smaller total that looks green.

- **A flake hunt**: `./mill __.test` reruns every unit suite, cached or not.
- **While iterating**, run the suite or module you touched. When the only question is
  whether it type-checks, `./mill grit.<module>.compile` (or `grit.<module>.test.compile`)
  answers in about a second warm.
- **At a commit point**, once: `./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll
  __.sources`, then bare `scripts/check`. Its tiers are the unit tier (`__.test.testCached`:
  a module's suites rerun only when their inputs changed; a failure is never cached), the
  law (`scripts/enola-law.sh`; `bash scripts/fetch-enola.sh` first if `tools/` lacks enola),
  the capture probes a change reaches (below), format (`bash scripts/fetch-scalafmt.sh` first
  if `tools/` lacks scalafmt), clock, unsafe (`scripts/unsafe-check`: each `caps.unsafe` use
  has its proof beside it), docs (`scripts/doc-length`) and, last, **lint**: the modules holding a changed Scala
  source and every module depending on one, compiled with `-Wunused:all` in their own output
  directory. An unused import passes a targeted run and fails lint.
- **The unused pass may run after the commit, in the background**, when the manager says so: the
  builder ends with `scripts/check --no-unused` (every other tier), and the manager then runs
  `scripts/check --only-unused` as a background shell command, not a subagent, while it reviews
  and writes the next brief. Nothing needs its result until the next build, so the next builder
  starts only after it finishes, and any failure goes into that builder's brief. It is still one
  JVM-heavy job at a time.
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

## Capture probes

A module's `probes` tests compile probe sources against it, through `Probes.batch`, to pin
what capture and separation checking reject. The gate runs a module's probes when a change
touches capture syntax (`^`, `caps.`, `SharedCapability`, `Pure`) or `private[grit]` in its main sources or a
module it depends on (the probe modules: `grit.core.probes`, `grit.act.probes`,
`grit.turn.probes`, `grit.edge.probes`, `grit.prose.probes`, `grit.knowledge.probes`,
`grit.tools.probes`, `grit.tui.probes`, `grit.kit.probes`), and every probe when `build.mill` or `.mill-version` changes. When a
plan's capture section names a probe, run its suite by name, red on its plant and green, and
once more before the commit; `scripts/check --probes` runs them all.

## The integration tier (`--it`) and the milestone run (`--milestone`)

The live suites, which run against Postgres and DBOS, are in `it` modules (`grit.dbos.it`,
`grit.app.it`, `grit.eval.it`, `grit.eval.harness.it`) and run through `scripts/it`: one
throwaway Postgres shared by every `it` JVM (`TestPostgres`), built from the same
`docker/postgres/Dockerfile` compose builds, `postgres:18` plus `pg_textsearch` (ADR 0005).
Run `--it` before committing a change to `grit.dbos`, `schema.sql`, an `it` module or the
engine's live paths. It needs Docker; there is no skip. When a milestone closes, run
`--milestone`: the gate, every probe and the integration tier.

## Replay histories

`bash scripts/capture-history.sh <workflow-id>` writes a recorded workflow as a fixture under
`core/turn/test/histories/<epoch>/`. A change to a workflow's step names, order or output
encodings must replay every history of its epoch
([ADR 0004](../../../docs/decisions/0004-workflows-evolve-by-patch-within-a-compatibility-epoch.md));
`TurnReplayTests` and `LifecycleReplayTests` are the gate.

## In a git worktree

Build and test with Mill, and read the code with `scripts/scout --root .` from the
worktree: it answers from the worktree's own compiled output. Metals serves the main checkout
only. A CLI `./mill` and Metals never block each other (separate build directories).

A targeted `scripts/check` right after `--it` can leave two Mill daemons: the exiting one
deletes `out/mill-daemon/socketPort`, orphaning the live one, and the next `./mill` says
"server launch failed". Kill the orphan by PID, yours only: its working directory
(`/proc/<pid>/cwd`) is your checkout's `out/mill-daemon/sandbox`. Then rerun.

## JVMs

- One JVM-heavy job at a time on this machine: no parallel Mill, test or grit runs.
- Stamp what you start; kill only your own, by PID. Never `pkill` by pattern: Nick may
  have grit or a demo open elsewhere.
- On Mill "Connection refused": `./mill shutdown` and retry.
- At the end: `./mill shutdown` in your checkout. `scripts/check` shuts down the lint
  tier's daemon itself, and on a timeout the timed-out tier's.
