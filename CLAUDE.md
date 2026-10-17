# grit

A from-scratch LLM agent harness. Thesis: **no manual context-window management**. Every
turn is a durable DBOS workflow, and every turn's context window is assembled fresh from
long-term memory in Postgres: messages, end-of-turn summaries, and code context from the
LSP. Nothing is carried forward as a transcript. The same engine runs in three modes: a
local TUI harness, a cloud agent driven from Slack, and triggered tasks. Edges reach it
only through Postgres ([ADR 0002](docs/decisions/0002-edges-reach-the-engine-through-postgres.md)).

Scala 3 · Mill · `dev.dbos:transact` · Postgres 18 with `pg_textsearch` · capture checking on, and separation
checking everywhere but `grit.tui` ([ADR 0003](docs/decisions/0003-durable-is-exclusive-under-separation-checking.md)). Versions live in
`build.mill` and `.mill-version`.

## Architecture

Context is not a scrollback buffer; it is a queryable store, and what goes to the model is
a **computed projection** of that store. Three traits carry the whole design:

- `EntryStore` — append-only, never rewritten
- `ContextAssembler` — the projection; *the* trait the thesis hangs on
- `Provider` — the model call

Retrieval, summaries and LSP code context are all future `ContextAssembler`
implementations; the relevance check is the turn's own move between the assembler's reads
([ADR 0040](docs/decisions/0040-a-turn-draws-its-window-in-recorded-steps-and-only-candidates-judged-relevant-are-packed.md)),
so that signature is the real design work; everything else swaps behind it. The database transaction is a scoped, non-escaping capability (`Tx`) — capture
checking rejects any attempt to let it outlive its block.

Mill modules, and what each may name. A module's folder follows ADR 0021's three questions
(`core/`, `extensions/`, `kit/`, `deployments/`, `eval/`; [`docs/extending.md`](docs/extending.md)),
while its Mill name, package and artifact stay `grit.<name>`. More on each: `scripts/outline
area`, and the README in its folder (package order, and what the code cannot say).

| Module | Packages | Depends on | Holds |
|---|---|---|---|
| `grit.core` | `grit.core.*` | — | the domain and the traits; no DBOS, no JDBC driver on its classpath |
| `grit.dbos` | `sql`, `workflow`, `engine`, `internal` | core | the DBOS quarantine: DBOS, JDBC, Postgres, `schema.sql`; `internal` is lent to the eval harness alone |
| `grit.prose` | `form`, `markdown` | — | prose as edge-neutral blocks, read from markdown; each edge renders them |
| `grit.tui` | `model`, `components`, `wire`, `runtime` | core | the terminal UI library; core only from `components`/`runtime` |
| `grit.tui.examples` | `grit.tui.examples` | tui | runnable demos |
| `grit.act` | `phase`, `moves` | core | what an actor's moves do (ADR 0034), the steps the turn, jobs and the lifecycle share; names core alone |
| `grit.turn` | `grit.turn` | core, act | the durable turn's body, and a turn read back from the steps it recorded |
| `grit.lifecycle` | `transcript`, `close`, `settle`, `stitch`, `triage`, `shadow`, `post` | core, act | the engine's workflows besides the turn; their asks are moves |
| `grit.job` | `run`, `clock` | core, act | jobs (ADR 0029): a job's run, and the clock edge that starts it; names no `grit.turn` |
| `grit.digest` | `grit.digest` | core | the hello-world plugin |
| `grit.remind` | `grit.remind` | core | the reminders plugin |
| `grit.knowledge` | `card`, `github`, `sync`, `plugin` | core | the plugin that keeps cards of a declared source (ADR 0028) |
| `grit.models` | `grit.models` | core | `Provider`s and `Classifier`s |
| `grit.host` | `grit.host` | core | `grit.core.host`'s capabilities over this machine |
| `grit.edge` | `grit.edge` | core | an edge's side of the engine, over core's traits alone |
| `grit.tools` | `grit.tools` | core | the coding tool set and grit's own tools |
| `grit.slack` | `event`, `text`, `client`, `edge` | core, prose, edge | the Slack edge (ADR 0023) and the Slack SDK's quarantine |
| `grit.mcp` | `wire`, `scope`, `github`, `client`, `drive`, `edge` | core, edge | MCP servers and Drive's REST API: a declared service's read-only tools at its service place (ADR 0017) |
| `grit.assembly` | `estimate`, `linear`, `retrieval` | core | the `ContextAssembler`s |
| `grit.eval` | `grit.eval` | core (its `it`: dbos, assembly, models) | the eval's labelled cases; its `it`, the assembly eval, reports and gates nothing |
| `grit.eval.harness` | `stats` … `main` | core, dbos, turn, assembly, lifecycle, models, eval | the eval harness, run only through `scripts/eval` |
| `grit.kit` | `deployment`, `environment`, `run` | core, dbos, act, turn, job, lifecycle, assembly, models, tools, host | what a deployment is built against (ADR 0021) |
| `grit.app` | `config`, `look`, `chat`, `main` | kit, digest, remind, knowledge, slack, mcp, tui, host, edge | the reference deployment: the chat TUI, `grit serve`, `grit backfill` |

Mill `moduleDeps` are transitive, so `grit.kit` and `grit.app` see `dev.dbos.*` through
`grit.dbos` — enola's rule, not the compiler, guards it. Working in `extensions/tui/`? Read
[`extensions/tui/CLAUDE.md`](extensions/tui/CLAUDE.md) first.

**Changing a workflow's steps** (names, order, output encodings) must replay every history
of the current epoch: guard it with `Durable.patch`, or start a new `Turn.Epoch`
([ADR 0004](docs/decisions/0004-workflows-evolve-by-patch-within-a-compatibility-epoch.md)).
`TurnReplayTests` and `LifecycleReplayTests` are the gate.

## Reading the code

Read Scala through `scripts/outline` first (`dev/outline`; also the `outline` MCP server). It
answers from the compiler's TASTy, in less text than a file read, and works in any worktree (`--root`; it runs from the checkout it is in).

- `area` — with no name, the areas and what each covers; `area <name>[,name…]`, a map of one
  part of grit: start here on an unfamiliar task.
- `find <pattern>` — when you don't know a name: `find '*Shelf*'`, `find '*.placed'`.
- `show <Sym>[,Sym…]` — signatures and docs, the types they name traced a level, and the tests
  that exercise them; `--body <members>` for bodies. Several symbols per call.
- `family <Trait>` — a trait and its implementations. `uses <Owner.member>` — every direct
  reference, compiler-exact. `tests <Suite>` — a suite's tests and helpers.
- The definition lines it prints are the file's own text: an Edit may copy them as `old_string`
  without a Read first. `scripts/outline <query> --help` says which lines are file text.
- Ask for private members (`--private`) only to edit them or to understand an implementation;
  otherwise trust the public interface and referential transparency.
- It reads compiled output: compile the module first if its TASTy is stale. Bash reads remain
  for what it can't answer. Metals is for the editor ([`docs/editor-tooling.md`](docs/editor-tooling.md)).

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
   meet only in `grit.kit` and `grit.app`. Nothing outside `grit.dbos` imports `dev.dbos.*` or `java.sql.*`,
   nothing outside `grit.models` and `grit.mcp` imports `java.net.http.*`, nothing outside `grit.slack` imports
   `com.slack.*`, and nothing outside `grit.host` starts a process.
9. Explicit capability parameters over clever inference.
10. **The elision test** — if assembly dropped the body and kept only the signature and
    its doc, could a competent agent still call it correctly? If not, fix the signature.
    Trustworthy signatures let the next change be built from them alone, in far fewer tokens.
    Scaladoc says *what*, never *how*. An invariant a doc states is a missing type; a doc
    never restates what the type or visibility says; it does state every failure a caller
    can see and every constant that changes behaviour.
11. **Escape hatches carry their proof.** Every `caps.unsafe` use states beside it why the
    untracked effect cannot be observed.

**Tests** ([`STYLE.md`](STYLE.md#tests)): a test is seen red for the right reason before
it is trusted (the procedure is the `grit-test-cycle` skill's); a name states a contract
and its assertion pins the value; a fake shares one spec with the implementation it stands
in for. The `test-janitor` agent reviews test files against these rules.

## Build and format

Build, test and gate with `scripts/check`: a suite or module while iterating
(`scripts/check grit.core.topic.TopicsTests`), bare once at a commit; `./mill grit.<module>.compile`
for whether it type-checks; format with `./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll
__.sources`. Everything else, and the procedure: the `grit-test-cycle` and `grit-local-db` skills.

Braces, never significant indentation — `-no-indent` makes it a compile error.

**Three kinds of suite.** `test` modules need nothing outside the JVM; `it` modules run
against Postgres and DBOS through `scripts/it`, which needs Docker, with no skip; `probes`
modules pin what capture and separation checking reject.

**Capture and separation checking:** [`docs/capture-checking.md`](docs/capture-checking.md)
has every trap met so far, how to test that something does not compile, and the upgrade
checklist: search it for the error when one appears.

Every warning the build enables is an error (`-Werror`), and a warning joins the set in its
own commit.

## The architecture gate (enola)

```bash
bash scripts/fetch-enola.sh    # pinned, sha256-verified, into tools/ (gitignored)
bash scripts/enola-law.sh      # regenerate, lint the declarations, check the law, fail on a cycle between production packages
```

`mcp-arch.yaml` is the walk config, `enola-intent.yaml` is the law. It is also an MCP
server — prefer `impact_analysis`, `explore`, `traverse` and `find_path` over re-deriving
structure by grepping.

**The trust boundary is not negotiable.** enola's Scala extractor dropped and mis-nested
symbols under capture syntax while reporting `parse_errors: 0`, up to 0.4.24. 0.4.29 reads the
controlled fixture correctly, but its receipt would be as silent about a new regression.
Re-measured before every version bump (`scripts/fetch-enola.sh`).

- **Import-edge and module facts are sound** and may gate. Every rule in the law is of this kind.
- **Symbol facts never gate** (`exported-surface`, `complexity-outliers`, any symbol count).
  They may guide exploration (`impact_analysis`, `explore`); a symbol-level claim is confirmed
  with outline before it is stated.

**A green gate is not automatically a checked gate.** A declaration that matches nothing
passes vacuously and `lint` still exits 0; `enola-law.sh` greps for that. A rule is not
adopted until it has been watched failing on a planted breach. The known traps are
commented in `enola-intent.yaml`. New rules are a planned session, not an aside.

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
- **Plan before implementing.** A roadmap item, or any change to a core trait, a library API
  or behaviour a person sees, starts with a short plan and waits for Nick's go. The plan
  says what changes, the alternatives turned down, how it will be verified (including the
  real-use run below), and what is parked. It has an **Elision section**: every new or
  changed public signature with its Scaladoc as it will be written, whether a caller could
  use it from those alone (rule 10), and what that check changed in the design. This one
  question has produced most of a refactor's design improvements. It also has a **Capture
  and separation section**: which values hold capabilities and which must be pure, what
  each new trait or type parameter may capture, where a step body or a transaction must
  not reach (the `Durable`, a `Tx` escaping), any `caps.unsafe` with its proof (rule 11),
  and for each claim the compiler is meant to enforce, the probe that shows it rejects the
  breach ([`docs/capture-checking.md`](docs/capture-checking.md)). It has a **Sharing
  section**: for each new type, trait or function, the existing one it reuses or the second
  caller it serves, and each near-copy of existing code the change would add, folded into one
  place instead. Of two designs that pass the elision test, the one that shares more code wins:
  one implementation that a turn, a job and a rehearsal all run beats two kept alike by hand.
  Mechanical edits, doc corrections and small fixes inside an agreed plan need none of these.
- **Package layout is designed, not accreted.** A new module, library or package starts
  with its layout in the plan: each package one idea, named for it; no source file at a
  group's root; the packages in a one-way dependency order, written down where the module
  is documented (`extensions/tui/CLAUDE.md` is the pattern). A module that is one idea is one
  package; when it grows a second, it becomes subpackages, none of its files left at the
  root. A layout that has drifted gets redesigned, not patched. The law fails on a new
  import cycle; the rest is review.
- **Done includes a real-use run** for anything a person interacts with, such as the TUI,
  a CLI or an edge. Use the real model, restart mid-turn, and wait as long as a person
  would. Tests that read the model rather than the painted screen, or that run against the
  stub, have passed paint and latency bugs.
- **Commit points.** Commit when a planned item, or a self-contained step inside one, has
  its tests passing with no warnings and the law green. Don't ask first; report the hash.
  One item per commit, never two mixed. Work reaches `main` through a pull request; never
  push to `main` directly, and never amend or rewrite history on it.
- **One home per fact.** State a fact in one place and link to it from the others. No
  counts in docs: test totals, check counts and rule counts go in commit messages, where
  they are true as of that commit.
- **LLM spend stays under a few cents per run** of anything that calls a real model, such
  as a live test, a probe or a real-use run. Use a cheap model and a small `max_tokens`,
  and say what a run cost. Neither test tier makes model calls.
