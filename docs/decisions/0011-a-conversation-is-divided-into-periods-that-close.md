# 0011. A conversation is divided into periods that close, and derived data is built from closing entries

Status: accepted (2026-09-26)

Context: every entry is kept, append-only, for the life of a conversation, and a
conversation never ends. An open-ended log grows without bound and has to be carried
through every migration. grit also means to derive data from conversations (knowledge,
digests), through features a deployment can opt into and that are expected to be replaced.
The choices:

- **What a period is.** A period could be a conversation, a later message starting a new
  one for the same origin; that was turned down because the place (a thread, a session)
  then has no row of its own to carry locality and labels.
- **When it closes.** On idleness alone (the boundary is arbitrary), on an explicit signal
  alone (people forget, and the log is unbounded again), or on size (it splits work
  mid-thought).
- **What derived data is built from.** The raw log, with each consumer able to hold back
  deletion until it has read it (like consumer groups), or only what the close writes.
  Holds were turned down: one stuck consumer would stop all deletion.

Decision:

- **A conversation holds periods.** The conversation is a lasting place; a period is a
  numbered stretch of its time. A closed period never reopens: later activity opens the
  next period, which starts from the previous period's closing entry.
- **Close is a deadline.** Activity pushes it to now plus an idle window. An explicit
  signal pulls it in to now plus a shorter grace window, never to zero; activity during
  grace cancels the signal. A turn in flight is activity. The period records why it closed:
  resolved after a signal, or lapsed.
- **Close writes a closing entry**: a small, fixed set of structured sections plus prose,
  covering its own period, versioned from the first. The seal and its reason belong to the
  period, not the entry.
- **Raw entries and their workflow history have a hard retention window** that nothing
  extends. Closing entries are kept, and their growth is measured, until a rollup bounds
  them.
- **Derived data is built only from closing entries**, each consumer in its own durable
  workflow, triggered by a close, outside the turn's steps, and in its own tables. A
  consumer enabled late, or rebuilt, replays closing entries.

Consequences:

- Storage is bounded by open periods plus closing entries, and only those need migrating.
- A consumer can be swapped, disabled or rebuilt without touching the turn, so no change to
  one needs a patch or an epoch (ADR 0004).
- What a consumer can ever know is what the closing entry holds, or what it records itself
  inside the raw window. That makes the closing entry's sections the data model's most
  expensive part to change.
- Closing entries grow without bound until the rollup exists.
- Nothing enforces this yet; the store and the consumer interface are to be built.
