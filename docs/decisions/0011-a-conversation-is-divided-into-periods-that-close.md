# 0011. A conversation is divided into periods that close, each closing carrying the conversation's balance

Status: accepted (2026-09-26); revised (2026-10-06)

Context: every entry is kept, append-only, and a conversation never ends. An open-ended log
grows without bound and has to be carried through every migration. grit also means to derive
data from conversations (knowledge, digests), through features a deployment opts into and
that are expected to be replaced. The choices:

- **What a period is.** A whole conversation, with a later message starting a new one for the
  same origin, was turned down: the place (a thread, a session) would then have no row of its
  own to carry locality and labels.
- **What the next period starts from.** The last few per-period summaries were run first: an
  item left open dropped out of view while still open, topic state was lost with the raw
  entries, and a summary of summaries compounded their loss. A cumulative summary rewritten
  at each close compounds it faster; tiered rollups re-summarise at every tier.
- **What derived data is built from.** The raw log, each consumer able to hold back deletion
  until it has read it, was turned down: one stuck consumer would stop all deletion.
- **When a period closes.** People do not say they are done, so an explicit signal (`/done`,
  a ✅) leaves nearly every period to lapse after a day; a shorter idle window closes periods
  mid-thought.

Decision:

- **A conversation holds periods.** A closed period never reopens. Later activity opens the
  next period, which starts from the previous closing entry.
- **A closing entry has flows and a balance.** The *flows* are what happened in its own
  period. The *balance* is the conversation's state after it: open items, standing decisions
  and topics, as keyed lines, each recording the period that added it and the last that
  touched it.
- **The balance changes only by edits.** The closing writer sees the previous balance as
  already known, proposes adds, resolves, drops and touches, and never rewrites a line. A line
  no edit names is copied verbatim. A line is added only if it appears in its period's flows,
  and it must read alone. Absence is an open item, never a fact. What grounds a Standing line
  is ADR 0018's.
- **The balance has a size cap** (data, in bytes of its lines), and the flows are bounded by
  the summary's token limit. Over the cap, lines are evicted least recently touched first,
  then oldest, then by id, and the flows list them. A period's own adds never evict the lines
  it touched.
- **Raw entries and closings are kept under hard windows**, the latest closing kept as the
  conversation's whole opening state; how they are deleted is ADR 0014's.
- **Derived data is built only from closing entries**, each consumer in its own durable
  workflow triggered by a close, outside the turn's steps; what a consumer keeps is ADR
  0027's.

**When a period closes.** Two windows run from a period's newest activity: a short *settle*
window, after which the classifier is asked, once per quiet stretch, whether anyone is
waiting on anything (the person, something else, or nobody); and the *idle* window, after
which the period lapses. A verdict of nobody at or above a threshold closes it at once,
resolved, with that probability as its confidence; any other leaves the idle deadline. New
activity makes the verdict stale, and a new quiet stretch a new question. Every verdict is
kept, a failed classifier's included, so nothing is asked twice without new activity, and a
period is asked at most `asks` times. The settle window, the threshold (`resolveAt`; 1 turns
judging off) and `asks` are data (`LifecycleSettings`), and when a period is due and when it
is asked are defined once (`Deadline`), never mirrored in SQL. A job's run's period (ADR
0029) is never asked: nobody waits at a task place, and it closes on its idle deadline, its
closing written with no model from the run's opening and its reply (`CloseReason.Ran`). A
heard period that earned nothing closes the same way (`CloseReason.Unearned`, ADR 0020).

Consequences:

- An open item stays in view until it is resolved or evicted, and topic state outlives the
  purge. A carried line reads the same in its fortieth period as in its first.
- Storage is bounded: close rate × window × (cap + flows), plus one closing per live
  conversation. Only open periods and kept closings need migrating.
- A consumer can be swapped, disabled or rebuilt without touching the turn, and no change to
  one needs a patch or an epoch (ADR 0004).
- What was evicted, and kept by no consumer, is eventually forgotten.
- A person does nothing to close a period, and one nobody waits on closes within the settle
  window. Each quiet stretch costs at most one classifier call. A wrong verdict of nobody
  closes a period early; its next message opens the next period from the closing, as after
  any close.
- Enforced by `ClosingTests` and the replay gate (`LifecycleReplayTests`).
