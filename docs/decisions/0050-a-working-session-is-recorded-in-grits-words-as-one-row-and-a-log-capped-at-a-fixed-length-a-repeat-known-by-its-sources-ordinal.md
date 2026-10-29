# 0050. A working session is recorded in grit's words as one row and a log capped at a fixed length, a repeat known by its source's ordinal

Status: accepted (2026-10-28); revised (2026-10-28)

Context: grit keeps a record of each working session, a grit turn's or a harness's beside it,
so that later work (compacting a session's context from the store, coordinating sessions) can
read what each did without its transcript. Events arrive from hooks that fire concurrently, are
batched, and may be delivered twice. Turned down:

- Keeping the harness's own events (its hook payloads): core would name one harness's
  vocabulary, and every harness would need its own reader.
- A log with no cap: one runaway session could fill the disk.
- A soft cap that stops logging reads, or drops the oldest ones: the first loses the reads that
  compaction wants, and the second deletes rows outside the collector.
- A high-water mark for repeats (an event at or below the last ordinal is ignored): it drops a
  concurrent hook's late event silently, and puts an ordering contract on every writer.

Decision:

- **A session is one row** (`grit.sessions`): who ran it, its label, the writer's own room when
  it has one, and where it worked (a harness session's worktree, a turn's conversation's place:
  a place with no label meaning, so that who is working where is one equality read), when it
  started, last did something and ended, its closing summary, and counters of its tool calls, edits,
  commits and bytes read. A session that never says it ended counts as ended a fixed time after
  its last event.
- **Its log is lines in grit's words** (`grit.session_events`): `started`, `read`, `edited`,
  `ran`, `commit`, `closing`, `ended`, each with a small shape of what it was and never its
  content. Up to a fixed number of lines, the last an `overflow` standing for every event after
  it; past it nothing more is logged or counted.
- **A repeat is known by the source's own ordinal**, unique within the session: recording the
  same events again is a no-op. Each recording locks the session's row, so concurrent ones
  number their lines without gaps.
- **Both are deleted by the collector** (ADR 0014): the sweep writes a tombstone on a session's
  log a month after it ends and on its row half a year after, and the collector spares one
  recorded on since.

Consequences:

- A harness is mapped onto grit's words at its edge; core never names a hook.
- A session longer than the cap keeps its first events and loses the rest; if measured sessions
  mostly pass the cap, a ring of recent reads or a column of recent files is the next step.
- Enforced by the types (`Event.Kind` holds no overflow; `Event.Shape` and `Event.of` refuse
  what the tables would), and by `SqlSessionsTests` and `CollectorLiveTests`.
