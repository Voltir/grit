# grit

A from-scratch LLM agent harness. Thesis: **no manual context-window management**. Every
turn is a durable DBOS workflow, and every turn's context window is assembled fresh from
long-term memory in Postgres: messages, end-of-turn summaries, and code context from the
LSP. Nothing is carried forward as a transcript. The same engine runs in three modes: a
local TUI harness, a cloud agent driven from Slack, and triggered tasks. Edges reach it
only through Postgres ([ADR 0002](docs/decisions/0002-edges-reach-the-engine-through-postgres.md)).

Scala 3 · Mill · `dev.dbos:transact` · Postgres 18 · capture checking on. Versions live in
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
| `grit.dbos` | `grit.dbos` | core | DBOS quarantine: DBOS, JDBC, Postgres, `schema.sql`, the phase-0 proof run |
| `grit.tui` | `grit.tui.{model,components,wire,runtime}` | core | the terminal UI; core only from `components`/`runtime` |
| `grit.tui.examples` | `grit.tui.examples` | tui | runnable demos; `Demo2` is `scripts/tui-gate`'s target |
| `grit.turn` | — | core | *placeholder*: the durable turn's orchestration |
| `grit.models` | — | core | *placeholder*: the frontier provider, later the relevance judge |
| `grit.assembly` | — | core | *placeholder*: builds each turn's context window |
| `grit.app` | — | everything | *placeholder*: the composition root |

Placeholders have a `README.md` and no code. Mill `moduleDeps` are transitive, so
`grit.app` sees `dev.dbos.*` through `grit.dbos` — enola's rule, not the compiler, guards
it. Working in `grit/tui/`? Read [`grit/tui/CLAUDE.md`](grit/tui/CLAUDE.md) first.

## Style rules

Full rationale in [`STYLE.md`](STYLE.md).

These are not preferences; elision soundness depends on them. **A signature without a
capability is a promise of purity.**

1. Referentially transparent by default.
2. Effects are capabilities in the signature — `(using Tx)`, explicit `Provider`. No
   ambient singletons, no hidden I/O.
3. Expected failure in the return type (`Either`/sealed ADT). Exceptions only for the
   unrecoverable — **DBOS retries a step that throws**, so a stray exception becomes
   undesigned retry behaviour.
4. Total over partial — no `.get`, `.head`, `Map.apply`, inexhaustive matches.
5. Illegal states unrepresentable — sealed ADTs, opaque id types.
6. No `null` outside a quarantine module (rule 8).
7. Immutable data; mutation only in scoped locals.
8. **Java libraries are quarantined by role.** Each lives only in the module whose job
   needs it, translated into grit's conventions there; such modules depend only on core and
   meet only in `grit.app`. Nothing outside `grit.dbos` imports `dev.dbos.*` or `java.sql.*`.
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

**scalafmt only parses capture syntax because of
`runner.dialectOverride.allowCaptureChecking = true`** in `.scalafmt.conf`. Without it,
every file using `^` fails with *"`identifier` expected but `)` found"* and is silently
skipped. If that error appears after a version change, check the setting first.

**upickle's `derives ReadWriter` crashes under capture checking** (a `MatchError` on
`caps.internal.inferred` in its macro; upickle 4.4.3, Scala 3.9). Write codecs by hand
over `ujson`, as `grit.core.PayloadJson` does.

Tests share `GritTests` in `build.mill`, which silences Scala 3.9's false
`unused pattern variable` warnings for variables read only inside utest's `assert`. Every
other warning is real.

A CLI `./mill` and Metals never block each other (separate build directories). Metals
spinners, BSP crash loops and build-directory hygiene:
[`docs/editor-tooling.md`](docs/editor-tooling.md).

## The architecture gate (enola)

```bash
bash scripts/fetch-enola.sh    # pinned, sha256-verified, into tools/ (gitignored)
bash scripts/enola-law.sh      # regenerate, lint the declarations, check the law
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
- Do not commit or push unless asked.
