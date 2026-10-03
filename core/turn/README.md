# grit.turn

The durable turn: one workflow per user-visible turn — weigh its root by the answers triage
kept, decide what it offers (its tools and its system prompt as fragments, ADRs 0016, 0017),
shaped by its deployment's recipe (ADR 0025), place its message among the
conversation's topics (ADR 0008), assemble, call the model, send its hosted tool calls to
the edge serving its directory and wait for their answers, append, summarise.
Written against `grit.core`'s seams and `Durable`, never DBOS, so its tests run it over
core's in-memory fakes (`InMemoryDurable`, `InMemoryEntryStore`).

Design: `roadmap/mechanisms/durable-turn.md`.
