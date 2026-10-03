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
a **computed projection** of that store. Three seams carry the whole design:

- `EntryStore` — append-only, never rewritten
- `ContextAssembler` — the projection; *the* seam the thesis hangs on
- `Provider` — the model call

Retrieval, summaries, relevance checks and LSP code context are all future
`ContextAssembler` implementations, so that signature is the real design work; everything
else swaps behind it. The database transaction is a scoped, non-escaping capability (`Tx`) — capture
checking rejects any attempt to let it outlive its block.

Mill modules, and what each may name. A module's folder follows ADR 0021's three questions
(`core/`, `extensions/`, `kit/`, `deployments/`, `eval/`; [`docs/extending.md`](docs/extending.md)),
while its Mill name, package and artifact stay `grit.<name>`:

| Module | Package | Depends on | Holds |
|---|---|---|---|
| `grit.core` | `grit.core.{clock,id,place,prompt,model,message,classify,topic,period,retention,store,spend,triage,speech,review,plugin,durable,approval,context,provider,inbox,stitch,host,tool,edge}` | — | the domain and the seams; no DBOS, no JDBC driver on its classpath; package order in [`core/core/README.md`](core/core/README.md) |
| `grit.dbos` | `grit.dbos.{sql,workflow,engine}` | core | DBOS quarantine: DBOS, JDBC, Postgres, `schema.sql`; `Engine` is what `grit.app` opens; package order in [`core/dbos/README.md`](core/dbos/README.md) |
| `grit.prose` | `grit.prose.{form,markdown}` | — | prose as edge-neutral blocks, read from markdown; each edge renders them (`Renderer`); package order in [`core/prose/README.md`](core/prose/README.md) |
| `grit.tui` | `grit.tui.{model,components,wire,runtime}.*` | core | the terminal UI: an `App` is three pure functions and a view tree (`Node`) that the runtime lays out, paints and routes input through; core only from `components`/`runtime` |
| `grit.tui.examples` | `grit.tui.examples` | tui | runnable demos; `Demo` is the target of every `scripts/tui-gate` scenario but `chat` and `reload`, which drive `grit.app` |
| `grit.turn` | `grit.turn` | core | the durable turn's body, written against `Durable`: what it offers (`TurnOffer`, its prompt's words `TurnPrompt`), its hosted calls as requests (`TurnHosted`), and `TurnRecord`, a turn read after the fact from the steps it recorded |
| `grit.lifecycle` | `grit.lifecycle.{transcript,close,settle,stitch,triage,shadow,post}` | core | the engine's workflows besides the turn: `Close`, a period sealed with its closing entry, `Settle`, a quiet period asked whether anyone is waiting, `Posting`, closed periods posted to a plugin (ADRs 0011, 0012), `Stitch`, a thread's first message placed among its room's exchanges, one room at a time in the order heard (ADR 0023), and `Triage`, what a heard message is (ADR 0020) and whether grit drafts a reply to it (ADR 0022), and `Shadow`, a declared variant of triage's question asked of a message already triaged, recorded and never acted on; package order in [`core/lifecycle/README.md`](core/lifecycle/README.md) |
| `grit.digest` | `grit.digest` | core | `Digest`, the hello-world plugin: one line per closed period, and the `recent_activity` tool that reads them; [`extensions/digest/README.md`](extensions/digest/README.md) |
| `grit.models` | `grit.models` | core | `Provider`s: `StubProvider`, `OpenRouterProvider` (the JDK HTTP client lives here); `Classifier`s: `JevClassifier`; [`extensions/models/README.md`](extensions/models/README.md) |
| `grit.host` | `grit.host` | core | the local host: `LocalWorkspace`, `LocalEdits`, `LocalShell`, `LocalInstructions` (`grit.core.host`'s capabilities over this machine's files and processes; a command sees only an allowlisted environment), `LocalMachine` (this process's `ProcessIdentity`); the only module that starts a process or reads which process and machine this is; [`extensions/host/README.md`](extensions/host/README.md) |
| `grit.edge` | `grit.edge` | core | an edge's side of the engine, over core's traits alone: `Server` (claims, runs and answers the tool requests addressed to the places it hosts, ADR 0017, with the `Tools` it is given), `Run` (a request run by a toolbox, as its permit says), `PlaceFragments` (a place's instruction files, or the text its edge was declared with, as the prompt's Place layer) |
| `grit.tools` | `grit.tools` | core | the coding tool set (`Coding`): read, list, search, write, edit and run, each written once as a `Hosted` description the engine offers and the `Tool` over `grit.core.host`'s capabilities an edge runs (ADR 0017); `Tuning`: `propose_model_setting`, a measured setting of a model kept once a person approves it; `Probes`: `probe_pair`, a battery of calls measuring a (model, upstream) pair; `About`: `about`, what grit is and how it works, from docs shipped in its resources; [`extensions/tools/README.md`](extensions/tools/README.md) |
| `grit.slack` | `grit.slack.{event,text,client,edge}` | core, prose, edge | the Slack edge (ADR 0019): the Slack SDK's quarantine (`SocketSlack`, over Socket Mode), Slack's events and text read into grit's terms, a reply's prose as Slack rich text, and `SlackEdge`, which records a thread's messages as turns, posts their replies, serves `slack_post` where a deployment allows, and posts a review's prompts and keeps its rater's reactions; package order in [`extensions/slack/README.md`](extensions/slack/README.md) |
| `grit.mcp` | `grit.mcp.{wire,client,edge}` | core modules (incl. `grit.edge`) | the MCP client (revision 2026-07-28, Streamable HTTP): a declared server's read-only tools, offered by an edge hosting them at a service place (ADR 0017); `wire`, the protocol over ujson, pure; `edge`, `McpEdge`, the `ServedEdge` a deployment declares; the JDK HTTP client's second egress beside `grit.models`; package order in [`extensions/mcp/README.md`](extensions/mcp/README.md) |
| `grit.assembly` | `grit.assembly.{estimate,linear,retrieval}` | core | `ContextAssembler`s: builds each turn's context window; package order in [`core/assembly/README.md`](core/assembly/README.md) |
| `grit.eval` | `grit.eval` | core (its `it`: dbos, assembly, models) | the eval's labelled cases (`Cases`, in the format `Case` reads) and their `Layout` in a database, written by `Load` over core's traits; its integration sources (`grit.eval.it`) are the assembly eval: every assembler over those cases in a throwaway Postgres; a report, not a gate |
| `grit.eval.harness` | `grit.eval.harness.{stats,corpus,label,log,pull,jev,reply,run,score,report,main}` | core, dbos (`Reader`, and an `Engine` to write the synthetic reference), turn, assembly, lifecycle, models, eval | the eval harness: corpora captured from a database through the shipped builders, and the turns it recorded, as text-free files under `.local/eval/`, their windows rebuilt as of their assembly; `grit.eval`'s cases as a synthetic reference; run only through `scripts/eval`; package order in [`eval/harness/README.md`](eval/harness/README.md) |
| `grit.kit` | `grit.kit.{deployment,environment,run}` | core, dbos, turn, lifecycle, assembly, models, tools, digest, host | what a deployment is built against (ADR 0021): the `Deployment` value, `Secrets`, and `Kit.serve`/`Kit.catchUp`, which run a deployment's `ServedEdge`s beside its engine; package order in [`kit/README.md`](kit/README.md); which modules are core, extension or deployment, and what a deployment can supply, in [`docs/extending.md`](docs/extending.md) |
| `grit.app` | `grit.app.{config,look,chat,main}` | kit, slack, mcp, tui, host, edge (everything, transitively) | the reference deployment; `Main` is the chat TUI (`ChatScreen` + `ChatHost`), `grit serve` (the Slack edge, and GitHub's MCP tools when its token is set, through `Kit.serve`), `grit backfill`, or a one-shot run with arguments; package order in [`deployments/app/README.md`](deployments/app/README.md) |

Mill `moduleDeps` are transitive, so
`grit.kit` and `grit.app` see `dev.dbos.*` through `grit.dbos` — enola's rule, not the compiler, guards
it. Working in `extensions/tui/`? Read [`extensions/tui/CLAUDE.md`](extensions/tui/CLAUDE.md) first.

**Changing a workflow's steps** (names, order, output encodings) must replay every
history of the current epoch: guard the change with `Durable.patch`, or start a new
`Turn.Epoch` ([ADR 0004](docs/decisions/0004-workflows-evolve-by-patch-within-a-compatibility-epoch.md)).
`TurnReplayTests` is the gate, and `LifecycleReplayTests` for the close, settle and posting.

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

```bash
scripts/check grit.core.topic.TopicsTests   # one suite, while iterating
scripts/check grit.core                     # the module you touched
./mill grit.core.compile                    # does it type-check
./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources   # before committing
scripts/check                               # the commit gate: unit tier, law, format, lint; once, last
scripts/check --it                          # and the integration tier (below)
./mill __.test                              # every unit suite rerun: a flake hunt
scripts/sql grit_agent "SELECT …"           # a read-only query on a local database
```

The procedure: the `grit-test-cycle` and `grit-local-db` skills.

Braces, never significant indentation — `-no-indent` makes it a compile error.

**Two tiers.** `test` modules need nothing outside the JVM. The live suites, which run
against Postgres and DBOS, are in `it` modules (`grit.dbos.it`, `grit.app.it`,
`grit.eval.it`, `grit.eval.harness.it`) and run through `scripts/it`: one throwaway Postgres shared by every `it`
JVM (`TestPostgres`), built from the same `docker/postgres/Dockerfile` compose builds:
`postgres:18` plus `pg_textsearch`
([ADR 0005](docs/decisions/0005-entries-are-ranked-with-bm25-inside-postgres-through-pg-textsearch.md)).
It needs Docker; there is no skip.

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

Every warning the build enables is an error (`-Werror`); `build.mill`'s `scalacOptions`
lists the set in force, and a warning joins it in its own commit. `-Wunused:all` is on only
in `scripts/check`'s lint tier.

Metals spinners, BSP processes, build-directory hygiene and Metals' MCP set-up:
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
commented in `enola-intent.yaml`. New rules are a planned session, not an aside.

## Metals

Metals' MCP server (`grit-metals` in `.mcp.json`; port from `.metals/mcp.json`) is for:

- **who uses X** — `get-usages`, compiler-exact where grep and enola's symbol facts are not;
- **what exactly X's type is** — `typed-glob-search`, `inspect`, `get-docs`;
- **compiling or testing one module mid-edit, in this checkout** — `compile-module`,
  `test`; it builds into `.bsp/out`, so it never blocks a CLI `./mill`. It serves this
  checkout only (a worktree: the `grit-test-cycle` skill).

Not for understanding a package's behaviour: read the files. Metals hands context out one
symbol at a time; in a measured comparison, file reads answered edge-case questions
perfectly at a third fewer tokens and an eighth of the calls. Quirk: `inspect` ignores `module` —
**always pass `fileInFocus`** (a source file in the symbol's module); without it the result
is empty although its footer names the module you passed. For a generic class it lists
only the companion, so use `get-docs`. Set-up, the MCP port and BSP failure modes:
[`docs/editor-tooling.md`](docs/editor-tooling.md#metals-for-grits-own-development).

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
  real-use run below), and what is parked. It has an **Elision section**: every new or
  changed public signature with its Scaladoc as it will be written, whether a caller could
  use it from those alone (rule 10), and what that check changed in the design. This one
  question has produced most of a refactor's design improvements. Mechanical edits, doc corrections and small
  fixes inside an agreed plan need none.
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
