# grit

An LLM agent harness, written from scratch in Scala 3, built around one claim: **no
manual context-window management.**

Every turn is a durable [DBOS](https://docs.dbos.dev/) workflow, and every turn's context
window is assembled fresh from long-term memory in Postgres: messages and end-of-turn
summaries today, code context from the language server to come. Nothing is carried forward as a transcript; there is nothing
to compact, trim or reset by hand. A turn that is interrupted, by a crash or a restart,
resumes where it stopped without calling the model twice.

## The design in one paragraph

Context is not a scrollback buffer; it is a queryable store, and what goes to the model is
a computed projection of that store. Three traits carry the design: `EntryStore`
(append-only, never rewritten), `ContextAssembler` (the projection), and `Provider` (the
model call). Retrieval is a `ContextAssembler` today, and relevance checks and code
context will be further ones, so that one signature is the real design work and everything
else swaps behind it. Effects are capabilities in signatures, checked by Scala's capture checking: the
database transaction is a scoped value the compiler will not let outlive its block.

One engine is designed to run in three modes: a local terminal harness, a cloud agent
driven from Slack, and triggered tasks. Edges reach the engine only through Postgres
([ADR 0002](docs/decisions/0002-edges-reach-the-engine-through-postgres.md)).

## State

grit is under active development; its APIs and its database schema change without
migration. What works today:

- **The chat TUI**: a terminal chat over the current checkout, with tools to read, search,
  edit and run, each write or command approved first.
- **The Slack edge** (`grit serve`): grit answers in Slack threads, recording each thread's
  messages as turns.
- **The MCP client**: a declared server's read-only tools, offered to the model (GitHub's,
  under `grit serve`).

A deployment outside this repository, Bort, runs on grit in Slack.

## Layout

The top-level folders answer [ADR 0021](docs/decisions/0021-grit-owns-six-semantics-and-a-deployment-is-a-value-built-against-its-kit.md)'s
questions: `core/`, `extensions/`, `kit/`, `deployments/` and `eval/`. What each holds,
where new code goes, which traits a deployment can supply, and how a module is added:
[`docs/extending.md`](docs/extending.md). A module's `README.md`, where it has one, gives its packages and their order.

## Getting started

Prerequisites:

- A JDK. `build.mill` runs everything with the one `GRIT_JDK_HOME` names, and is developed
  on JDK 26.
- Docker, for the local Postgres (`postgres:18` plus `pg_textsearch`, built from
  `docker/postgres/`) and for the integration tests.

`./mill` is Mill's bootstrap script: it fetches the version `.mill-version` pins, and Mill
fetches Scala and every library. Versions live in `build.mill`.

```sh
docker compose up -d postgres
cp .env.example .env                 # then fill in what you need; the real environment wins
scripts/grit                         # the chat TUI
scripts/grit hello "what is 2+2?"    # one-shot: each argument a message, one turn each
scripts/grit serve                   # the Slack edge; needs the Slack tokens in .env
```

Without `OPENROUTER_API_KEY` the model is a stub that calls nothing and costs nothing.
`.env.example` documents every setting: models, token budgets, the tools offered, a daily
spending cap, the database. `scripts/grit` has Mill build a launcher and exit, then runs it
with plain `java`, so restart grit after recompiling.

Building and testing:

```sh
./mill grit.core.compile             # does one module type-check
scripts/check grit.core              # one module's unit tests
bash scripts/fetch-enola.sh          # once: the architecture checker the gate runs
scripts/check                        # the gate: unit tests, the architecture law, lint
scripts/check --it                   # and the integration tier, against a throwaway Postgres (Docker)
```

The unit tier needs nothing outside the JVM; neither tier calls a model.

## Where things are written down

- [`docs/decisions/`](docs/decisions/README.md): the architecture decision records, and the
  threshold a decision must clear to get one.
- [`STYLE.md`](STYLE.md): the programming style, and why it is load-bearing: a signature
  without a capability is a promise of purity.
- [`docs/capture-checking.md`](docs/capture-checking.md): every capture- and
  separation-checking trap met so far, with its symptom, cause and fix.
- [`docs/extending.md`](docs/extending.md): extending grit, inside and outside this
  repository.
- [`CLAUDE.md`](CLAUDE.md): the working agreements, terse, as an agent reads them.
