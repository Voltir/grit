# 0011. A conversation is divided into periods that close, each closing carrying the conversation's balance

Status: accepted (2026-09-26), revised (2026-09-26); when a period closes superseded by 0012

Context: every entry is kept, append-only, for the life of a conversation, and a
conversation never ends. An open-ended log grows without bound and has to be carried
through every migration. grit also means to derive data from conversations (knowledge,
digests), through features a deployment can opt into and that are expected to be replaced.
The choices:

- **What a period is.** A period could be a whole conversation, with a later message
  starting a new one for the same origin. That was turned down because the place (a thread,
  a session) would then have no row of its own to carry locality and labels.
- **What the next period starts from.** The last few per-period summaries were built and run
  first. An item left open dropped out of view while still open, topic state was lost with
  the raw entries, and a summary written from earlier summaries compounded their loss. The
  other options were a cumulative summary rewritten at each close, which compounds loss
  faster, and tiered rollups, which re-summarise at every tier.
- **What derived data is built from.** Either the raw log, with each consumer able to hold
  back deletion until it has read it, or only what the close writes. Holds were turned down:
  one stuck consumer would stop all deletion.

Decision:

- **A conversation holds periods.** A closed period never reopens. Later activity opens the
  next period, which starts from the previous closing entry.
- **A closing entry has flows and a balance.** The *flows* are what happened in its own
  period. The *balance* is the conversation's state after that period: open items, standing
  decisions and topics, as keyed lines. Each line records the period that added it and the
  last period that touched it, as period numbers.
- **The balance changes only by edits.** The closing writer sees the previous balance as
  already known. It proposes adds, resolves, drops and touches, and never rewrites a line.
  A line no edit names is copied verbatim. A line is added only if it appears in its
  period's flows, and it must read alone, without depending on its context. Absence is an
  open item, never a fact.
- **The balance has a size cap, and the flows are bounded by the summary's token limit.**
  Over the cap, lines are evicted in a total order: least recently touched first, then
  oldest, then by id. The flows list the evicted lines. A period's own adds never evict
  the lines it touched. The cap is data.
- **Raw entries and their workflow history have a hard retention window**, and verdicts
  go with them.
- **Closings, period rows and usage have a longer window**, just as hard. The exception
  is a conversation's latest closing, which is its whole opening state. A conversation quiet
  for longer than this window is removed entirely.
- **Derived data is built only from closing entries.** Each consumer runs in its own
  durable workflow, triggered by a close, outside the turn's steps. A consumer declares its
  documents as either a cache, rebuilt from closings and dropped with them, or a ledger,
  with a window and bound of its own. Only a ledger consumer keeps anything past the
  closings' window.

Consequences:

- An open item stays in view until it is resolved or evicted, and topic state outlives the
  purge. A carried line reads the same in its fortieth period as in its first.
- Storage is bounded: close rate × window × (cap + flows), plus one closing per live
  conversation. Only open periods and kept closings need migrating.
- A long open period can post a checkpoint with the same writer without sealing it. Its
  flows cover what happened since the last checkpoint.
- A consumer can be swapped, disabled or rebuilt without touching the turn. No change to
  one needs a patch or an epoch (ADR 0004).
- What was evicted, and not kept by a ledger consumer inside the window, is eventually
  forgotten.
- Built so far: periods, the raw window, consumers built from closings, the balance and its
  cap, and verdicts going with the raw purge. Still to build: the closings' window and
  consumer classes.
