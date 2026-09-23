# grit

A from-scratch LLM coding harness. Thesis: **no manual context-window management** — a
local groomer LLM continuously compacts conversation and code-awareness data into
DBOS/Postgres tables, and the prompt sent to frontier models is assembled dynamically from
those tables.

Scala 3 · Mill · `dev.dbos:transact` · Postgres 18 · capture checking on. Versions live in
`build.mill` and `.mill-version`.

## Architecture

Context is not a scrollback buffer; it is a queryable store, and what goes to the model is
a **computed projection** of that store. Three seams carry the whole design:

- `EntryStore` — append-only, never rewritten
- `ContextAssembler` — the projection; *the* seam the thesis hangs on
- `Provider` — the model call

Grooming, compaction, retrieval, and LSP symbol linkage are all future `ContextAssembler`
implementations, so that signature is the real design work; everything else swaps behind
it. The database transaction is a scoped, non-escaping capability (`Tx`) — capture
checking rejects any attempt to let it outlive its block.

Mill modules, and what each may name:

| Module | Package | Depends on | Holds |
|---|---|---|---|
| `grit.core` | `grit.core` | — | the domain and the seams; no DBOS, no JDBC driver on its classpath |
| `grit.interop` | `grit.interop` | core | the quarantine: DBOS, JDBC, Postgres, `schema.sql` |
| `grit.tui` | `grit.tui.{model,components,wire,runtime}` | core | the terminal UI; core only from `components`/`runtime` |
| `grit.tui.examples` | `grit.tui.examples` | tui | runnable demos; `Demo2` is `scripts/tui-gate`'s target |

`grit.app`, the composition root, arrives with its first file. Mill `moduleDeps` are
transitive, so it will see `dev.dbos.*` through interop — enola's rule, not the compiler,
guards it. Working in `grit/tui/`? Read [`grit/tui/CLAUDE.md`](grit/tui/CLAUDE.md) first.

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
6. No `null` outside `grit.interop`.
7. Immutable data; mutation only in scoped locals.
8. **Interop is quarantined.** Nothing outside `grit.interop` imports `dev.dbos.*` or
   `java.sql.*`.
9. Explicit capability parameters over clever inference.
10. **The elision test** — if the groomer dropped the body and kept only the signature and
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
