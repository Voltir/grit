# grit

A from-scratch LLM coding harness. Thesis: **no manual context-window management** — a
local groomer LLM continuously compacts conversation and code-awareness data into
DBOS/Postgres tables, and the prompt sent to frontier models is assembled dynamically from
those tables.

Scala 3.8.4 · Mill · `dev.dbos:transact:1.0.0` · Postgres 18 · capture checking on.

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

## Formatting

Braces. Never significant indentation — `-no-indent` makes it a compile error. Run
`./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources` before committing.

**Capture syntax needs `runner.dialectOverride.allowCaptureChecking = true`.** The stock
`scala3` dialect rejects `(using Tx^)`, `(Tx^) ?=> A`, and `Connection^{tx}` with
*"`identifier` expected but `)` found"*, which silently skips every seam file. The
override is already in `.scalafmt.conf`; it needs scalameta >= 4.13, so scalafmt must
stay >= 3.9 (3.8.3 fails with `NoSuchMethodException`). If a parse error reappears after
a version change, check that setting before assuming the syntax is unsupported.

## Build and editor tooling

**Mill's lock is per-build-directory, and Metals does not share ours.** Mill takes an
exclusive lock on a build directory for the duration of an evaluation, but the `mill --bsp`
server VSCode starts builds into `.bsp/out/`, with its own `.bsp/out/mill-out-lock`. Verify
any time with `fuser -v out/mill-out-lock .bsp/out/mill-out-lock` -- different holders. So
a CLI `./mill` run cannot block Metals, and Metals cannot block it. Only two CLI
invocations contend with each other.

**A "compiling" spinner that never resolves is almost never a lock.** It is usually a stale
Metals progress item -- a failed or superseded compile can orphan the notification, and
VSCode keeps counting against a task that already returned. Check before restarting
anything:

```bash
grep 'time: compiled' .metals/metals.log | tail    # real compiles finish in seconds
tail -40 .metals/metals.log
```

If the log shows `done compiling` and a recent `buildTargetCompile took 0 msec` heartbeat,
the build is idle and green. Clear the spinner with **Metals: Restart build server**. Long
silent stretches in the log -- no requests at all for tens of minutes -- mean the VSCode
extension host stalled, not that Mill was blocked.

**Keep the editor out of the build directories.** `out/` is ~150MB and `.bsp/out/` churns
on every compile; watching them is what stalls the extension host. `.vscode/settings.json`
excludes them. Mill uses `out/`, **not** `target/` -- an sbt-style `**/target/**` exclude
matches nothing here and silently leaves both trees watched.

`.bsp/out/mill-no-daemon/` accumulates a sandbox directory per BSP process and is never
reaped by Mill. It has run into five figures in the spike. If it grows into the thousands,
stop the build server first, then clear it -- deleting them under a live server kills it:

```bash
ls .bsp/out/mill-no-daemon | wc -l
rm -rf .bsp/out/mill-no-daemon/*
```

`.bloop/` is a leftover from the pre-Mill-BSP setup and is not used.

**If the build server crash-loops (SIGTERM/SIGKILL cycles in `.metals/metals.log`)**, it is the
Mill 1.x BSP kill-other behavior dueling with Metals reconnects — not a build error. A
global wrapper (`~/.local/bin/mill-bsp-wrapper`, activated via `MILL_EXECUTABLE_PATH` in
`~/.profile`) injects `--bspNoKillOther` to defuse it. Full diagnosis, symptom signatures,
and undo instructions in [`mill-bsp-nokill-workaround.md`](mill-bsp-nokill-workaround.md).

**When the TUI lands from the spike**, long-lived evaluations need
`./mill --no-daemon --no-build-lock` so a CLI compile can still run while the app is up.
Two cautions carried over from `~/Projects/Spikes/tui-spike-jline`: a compile that lands
mid-run rewrites `out/.../compile.dest/classes` underneath the live JVM, so restart the app
after recompiling; and a JLine app in raw mode **ignores SIGTERM**, so `timeout N ./mill ...`
does not bound a run -- it leaves a headless JVM alive indefinitely. Use `timeout -k`, or
`pkill -9 -f` by main class afterwards.

## The architecture gate (enola)

`tools/enola` maps this repository into `.enola/` and grades changes against a pinned
baseline. Fetch it with `bash scripts/fetch-enola.sh` (pinned 0.4.15, sha256-verified,
project-local and gitignored). Run the gate with:

```bash
bash scripts/enola-law.sh      # regenerate, lint the declarations, check constraints
```

`mcp-arch.yaml` is the walk config, `enola-intent.yaml` is the law. It is also an MCP
server (`.mcp.json`) — prefer `impact_analysis`, `explore`, `traverse` and `find_path`
over re-deriving structure by grepping.

**The trust boundary is not negotiable.** grit compiles with
`-language:experimental.captureChecking` project-wide, and enola's Scala extractor
silently degrades on capture syntax: it drops symbols and mis-nests the survivors while
reporting `parse_errors: 0`. Measured on a controlled fixture — 40 symbols from
capture-annotated sources vs 46 from identical plain ones, unchanged between enola 0.4.8
and 0.4.15. So:

- **Import-edge and module facts are sound** and may gate. STYLE rule 8 is enforced this
  way today, as four `strict` rules.
- **Symbol facts are degraded.** Never gate on `exported-surface`, `complexity-outliers`,
  or any symbol count, and do not quote one as evidence about this codebase.

**A green gate is not automatically a checked gate.** A declaration that matches nothing
is not an error to enola — `constraints lint` prints "matches nothing" and exits 0, and
every rule or layer naming it then passes vacuously. `scripts/enola-law.sh` greps for that
string for exactly this reason. Two live traps in `enola-intent.yaml`: layer paths are
**service-relative** while component paths are **repo-relative**, and enola's module model
is directory-grained so a layer naming individual files resolves to nothing.

Adding rules or layers is a planned session, not incidental work: `.local/backlog/enola-law.md`.

## Working agreements

- **Verify against the source, not the summary.** Three claims in this project's original
  design synthesis turned out to be wrong because they summarized *around* the reference
  material instead of reading its APIs. `javap` the jar; read the current docs. A claim
  becomes a design constraint only after it has been checked.
- **Park, don't widen.** An idea that surfaces mid-task goes into the backlog, not into
  the current change.
- **Record decisions that constrain future code**, with the evidence that settled them,
  in the same turn you make them. Supersede earlier entries; never rewrite them.
- Do not commit or push unless asked.
