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
