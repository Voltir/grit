# grit.turn

The durable turn: one workflow per user-visible turn — assemble, call the model, append.
Written against `grit.core`'s seams and `Durable`, never DBOS, so its tests run it over
core's in-memory fakes (`InMemoryDurable`, `InMemoryEntryStore`).

Design: `roadmap/mechanisms/durable-turn.md`.
