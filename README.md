# grit — durable context harness

A from-scratch LLM coding harness on Scala 3.8 + DBOS (JVM), built around one claim:

> **No manual context-window management.** A local LLM continuously grooms and compacts
> conversation and code-awareness data into DBOS/Postgres tables, and the prompt sent to
> frontier models is assembled dynamically from those tables — *contentual compaction*.

First-class LSP/BSP integration as a code-awareness source. Tool calling as code, not
JSON.

## Architecture in one paragraph

Context is not a scrollback buffer; it is a queryable store, and what goes to the model is
a **computed projection** of that store. That yields three seams — `EntryStore`
(append-only, never rewritten), `ContextAssembler` (the projection), and `Provider`. The
groomer, compaction, retrieval, and LSP symbol linkage are all future `ContextAssembler`
implementations, so getting that one signature right is the real design work; everything
else is a swap behind it.

## Status

**Phase 0 — making the thesis testable.** The exit criterion is one conversational turn
persisting to Postgres through a durable DBOS workflow, where invoking the same workflow
id twice calls the provider *exactly once*. That proves durable execution earns its
complexity on turn one.

Done so far: Mill project on Scala 3.9.0 with `dev.dbos:transact:1.0.0`, local Postgres 18
via docker-compose, and a durable no-arg workflow landing a `SUCCESS` row in
`dbos.workflow_status`.

## Documents

| File | What it is |
|---|---|
| [`STYLE.md`](STYLE.md) | Programming style. Referential transparency as a context-compression strategy |
| [`CLAUDE.md`](CLAUDE.md) | Terse working agreements, loaded every session |

Planning artifacts — the roadmap, its decisions log, and the feature backlog — are kept as
local working documents and are not distributed with the repository.

## Influences

grit is designed against three bodies of prior art, studied locally rather than vendored
here:

- **pi** — a working coding-agent harness; the source of the append-only session tree,
  the projection-based context build, and compaction-as-checkpoint.
- **dbos4s** — a Scala wrapper over `dev.dbos:transact`; the source of the interop idioms.
- **Papers** — *Executable Code Actions* (CodeAct), *The Bitter Lesson of Tool Calling*,
  and *Securing Agents With Tracked Capabilities* (tacit), which supplies the
  capture-checking capability-safety model that `Tx` is the first instance of.

## Running

```sh
docker compose up -d postgres
./mill grit.app.run a b a
```

Each argument is a message id, answered by one durable turn with a stub model. A repeated
id is the same turn; run it again and finished turns replay without calling the model.

`GRIT_DATABASE_URL` (a `jdbc:postgresql:` URL), `GRIT_DATABASE_USER` and
`GRIT_DATABASE_PASSWORD` override the compose database; each defaults to it when unset.
