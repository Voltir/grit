# grit.turn

The durable turn: one workflow per user-visible turn — place its message among the
conversation's topics (ADR 0008), assemble, call the model, append, summarise.
Written against `grit.core`'s seams and `Durable`, never DBOS, so its tests run it over
core's in-memory fakes (`InMemoryDurable`, `InMemoryEntryStore`).

Design: `roadmap/mechanisms/durable-turn.md`.
