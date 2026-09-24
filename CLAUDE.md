# grit

A from-scratch LLM agent harness. Thesis: **no manual context-window management**. Every
turn is a durable DBOS workflow, and every turn's context window is assembled fresh from
long-term memory in Postgres: messages, end-of-turn summaries, and code context from the
LSP. Nothing is carried forward as a transcript. The same engine runs in three modes: a
local TUI harness, a cloud agent driven from Slack, and triggered tasks. Edges reach it
only through Postgres ([ADR 0002](docs/decisions/0002-edges-reach-the-engine-through-postgres.md)).

Scala 3 · Mill · `dev.dbos:transact` · Postgres 18 · capture checking on, and separation
checking everywhere but `grit.tui` ([ADR 0003](docs/decisions/0003-durable-is-exclusive-under-separation-checking.md)). Versions live in
`build.mill` and `.mill-version`.

## Architecture

Context is not a scrollback buffer; it is a queryable store, and what goes to the model is
a **computed projection** of that store. Three seams carry the whole design:

- `EntryStore` — append-only, never rewritten
- `ContextAssembler` — the projection; *the* seam the thesis hangs on
- `Provider` — the model call

Retrieval, summaries, relevance checks and LSP code context are all future
`ContextAssembler` implementations, so that signature is the real design work; everything
else swaps behind it. The database transaction is a scoped, non-escaping capability (`Tx`) — capture
checking rejects any attempt to let it outlive its block.

Mill modules, and what each may name:

| Module | Package | Depends on | Holds |
|---|---|---|---|
| `grit.core` | `grit.core` | — | the domain and the seams; no DBOS, no JDBC driver on its classpath |
| `grit.dbos` | `grit.dbos` | core | DBOS quarantine: DBOS, JDBC, Postgres, `schema.sql`; `Engine` is what `grit.app` opens |
| `grit.tui` | `grit.tui.{model,components,wire,runtime}.*` | core | the terminal UI: an `App` is three pure functions and a view tree (`Node`) that the runtime lays out, paints and routes input through; core only from `components`/`runtime` |
| `grit.tui.examples` | `grit.tui.examples` | tui | runnable demos; `Demo` is the target of every `scripts/tui-gate` scenario but `chat` and `reload`, which drive `grit.app` |
| `grit.turn` | `grit.turn` | core | the durable turn's body, written against `Durable` |
| `grit.models` | `grit.models` | core | `Provider`s: `StubProvider`, `OpenRouterProvider` (the JDK HTTP client lives here); later the relevance judge |
| `grit.assembly` | `grit.assembly` | core | `ContextAssembler`s: builds each turn's context window |
| `grit.app` | `grit.app` | everything | the composition root; `Main` is the chat TUI (`ChatScreen` + `ChatHost`), or a one-shot run with arguments |

Mill `moduleDeps` are transitive, so
`grit.app` sees `dev.dbos.*` through `grit.dbos` — enola's rule, not the compiler, guards
it. Working in `grit/tui/`? Read [`grit/tui/CLAUDE.md`](grit/tui/CLAUDE.md) first.

**Changing a workflow's steps** (names, order, output encodings) must replay every
history of the current epoch: guard the change with `Durable.patch`, or start a new
`Turn.Epoch` ([ADR 0004](docs/decisions/0004-workflows-evolve-by-patch-within-a-compatibility-epoch.md)).
`TurnReplayTests` is the gate.

## Style rules

Full rationale in [`STYLE.md`](STYLE.md).

These are not preferences; elision soundness depends on them. **A signature without a
capability is a promise of purity.**

1. Referentially transparent by default.
2. Effects are capabilities in the signature — `(using Tx)`, explicit `Provider`. No
   ambient singletons, no hidden I/O.
3. Expected failure in the return type (`Either`/sealed ADT). Exceptions only for the
   unrecoverable — **DBOS records a step that throws** and rethrows it on every replay,
   so a stray exception becomes that workflow's permanent result.
4. Total over partial — no `.get`, `.head`, `Map.apply`, inexhaustive matches.
5. Illegal states unrepresentable — sealed ADTs, opaque id types.
6. No `null` outside a quarantine module (rule 8).
7. Immutable data; mutation only in scoped locals.
8. **Java libraries are quarantined by role.** Each lives only in the module whose job
   needs it, translated into grit's conventions there; such modules depend only on core and
   meet only in `grit.app`. Nothing outside `grit.dbos` imports `dev.dbos.*` or `java.sql.*`,
   and nothing outside `grit.models` imports `java.net.http.*`.
9. Explicit capability parameters over clever inference.
10. **The elision test** — if assembly dropped the body and kept only the signature and
    its doc, could a competent agent still call it correctly? If not, fix the signature.
    Scaladoc says *what*, never *how*.

## Build and format

```bash
./mill __.test
./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources   # before committing
```

Braces, never significant indentation — `-no-indent` makes it a compile error.

`./mill __.test` needs Docker: the live suites (`grit.dbos.test`, `grit.app.test`) start
a throwaway Postgres with Testcontainers (`TestPostgres`), running the image
`docker-compose.yml` names. There is no skip.

**Capture and separation checking: [`docs/capture-checking.md`](docs/capture-checking.md)**
has every trap met so far (symptom, cause, fix), how to test that something does not
compile, and the Scala-upgrade checklist. The ones that bite most:

- A step body may not mention the `Durable` at all, not even `d.workflowId`-style reads.
  Compute what it needs before the step.
- scalafmt parses `^` only because of `runner.dialectOverride.allowCaptureChecking = true`;
  without it a file fails with *"`identifier` expected but `)` found"* and is silently skipped.
- upickle's `derives ReadWriter` crashes; write codecs by hand over `ujson`.
- `assertCompileError` never sees capture-checking errors; use the `Driver` probe in
  `SeparationTests`.

Tests share `GritTests` in `build.mill`, which silences Scala 3.9's false
`unused pattern variable` warnings for variables read only inside utest's `assert`. Every
other warning is real.

A CLI `./mill` and Metals never block each other (separate build directories). Metals
spinners, BSP crash loops and build-directory hygiene:
[`docs/editor-tooling.md`](docs/editor-tooling.md).

## The architecture gate (enola)

```bash
bash scripts/fetch-enola.sh    # pinned, sha256-verified, into tools/ (gitignored)
bash scripts/enola-law.sh      # regenerate, lint the declarations, check the law, fail on a new cycle
```

`mcp-arch.yaml` is the walk config, `enola-intent.yaml` is the law. It is also an MCP
server — prefer `impact_analysis`, `explore`, `traverse` and `find_path` over re-deriving
structure by grepping.

**The trust boundary is not negotiable.** enola's Scala extractor silently degrades on
capture syntax — drops and mis-nests symbols while reporting `parse_errors: 0`. Measured
on a controlled fixture, re-measured before every version bump (`scripts/fetch-enola.sh`).

- **Import-edge and module facts are sound** and may gate. Every rule in the law is of this kind.
- **Symbol facts are degraded.** Never gate on `exported-surface`, `complexity-outliers`,
  or any symbol count, and do not quote one as evidence about this codebase.

**A green gate is not automatically a checked gate.** A declaration that matches nothing
passes vacuously and `lint` still exits 0; `enola-law.sh` greps for that. A rule is not
adopted until it has been watched failing on a planted breach. The known traps are
commented in `enola-intent.yaml`. New rules are a planned session:
`.local/backlog/enola-law.md`.

## Working agreements

- **Verify against the source, not the summary.** Three claims in this project's original
  design synthesis turned out to be wrong because they summarized *around* the reference
  material instead of reading its APIs. `javap` the jar; read the current docs. A claim
  becomes a design constraint only after it has been checked.
- **Park, don't widen.** An idea that surfaces mid-task goes into the backlog, not into
  the current change.
- **A design decision that clears the ADR threshold gets a record** in
  [`docs/decisions/`](docs/decisions/README.md), the same turn it is made. Most don't clear
  it: their rationale belongs in a comment beside the code or in the commit message.
- **Plan before implementing.** A roadmap item, or any change to a seam, a library API
  or behaviour a person sees, starts with a short plan and waits for Nick's go. The plan
  says what changes, the alternatives turned down, how it will be verified (including the
  real-use run below), and what is parked. Mechanical edits, doc corrections and small
  fixes inside an agreed plan need none.
- **Package layout is designed, not accreted.** A new module, library or package starts
  with its layout in the plan: each package one idea, named for it; no source file at a
  group's root; the packages in a one-way dependency order, written down where the module
  is documented (`grit/tui/CLAUDE.md` is the pattern). A module that is one idea is one
  package; when it grows a second, it becomes subpackages, none of its files left at the
  root. A layout that has drifted gets redesigned, not patched. The law fails on a new
  import cycle; the rest is review.
- **Done includes a real-use run** for anything a person interacts with, such as the TUI,
  a CLI or an edge. Use the real model, restart mid-turn, and wait as long as a person
  would. Tests that read the model rather than the painted screen, or that run against the
  stub, have passed paint and latency bugs.
- **Commit points.** Commit when a planned item, or a self-contained step inside one, has
  its tests passing with no warnings and the law green. Don't ask first; report the hash.
  One item per commit, never two mixed. Never push, amend or rewrite history unless asked.
- **One home per fact.** State a fact in one place and link to it from the others. No
  counts in docs: test totals, check counts and rule counts go in commit messages, where
  they are true as of that commit.
- **LLM spend stays under a few cents per run** of anything that calls a real model, such
  as a live test, a probe or a real-use run. Use a cheap model and a small `max_tokens`,
  and say what a run cost. `./mill __.test` makes no model calls.
