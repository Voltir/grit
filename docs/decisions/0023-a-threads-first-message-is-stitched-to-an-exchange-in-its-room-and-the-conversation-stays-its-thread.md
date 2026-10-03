# 0023. A thread's first message is stitched to an exchange in its room; the conversation stays its thread

Status: accepted (2026-09-30)

Context: people do not start a thread for each message. In a listened channel a top-level
reply ("Is this a real question", "lol") began a conversation of its own (ADR 0019), so
triage, the speech rule and the window weighed it without the message it answered. The
choices were:

- whether to move such a message into the conversation it continues. Turned down: a thread
  would stop being a conversation, another thread's activity would move a period's deadline,
  and a model call would sit in the edge's write path.
- whether to keep a topic as an object of its own (a room-level conversation, or a topics
  table). Turned down: membership would have two homes, rewritten when two topics turn out
  to be one.
- whether to record the join as an entry. Turned down: an entry is activity, and would move
  a deadline.
- whether membership is transitive. Turned down: over a week, chains of stitches join
  unrelated exchanges.

Decision:

- **The conversation stays its thread; stitching adds a link.** When a stitchable
  conversation's first message arrives (`Origin.stitchable`: a Slack thread, whose first
  message was said at a channel's top level), a classifier judges whether it continues an
  exchange in its room. If it does, a stitch is kept: the conversation follows that
  exchange's root. A **strand** is a root and the conversations that follow it directly, a
  star, never a chain: a stitch always names a root.
- **Who places, and when.** The heard message's triage, or an addressed message's turn, in a
  step of its own before its other classifier calls; a first message is placed once. The
  exchanges offered are those of its room within a horizon (a week), in the scope in force:
  the most recently spoken in, and the best matched by BM25 on the message's own words. Each
  is shown by its opening message, its latest messages and its record's headline. Every
  placement keeps what the classifier was shown, why each exchange was offered, the
  probabilities, and the tuning in force (`Stitching.Tuning`), for later tuning.
- **Kept beside its entry.** A stitch is a row beside the first message, deleted with it, or
  with the conversation it follows. It writes no entry, so it is never a period's activity
  and moves no deadline; reading a strand writes nothing, so it reopens nothing.
- **Readers read the strand as context, not recall,** through one read that takes the scope
  (`Along.read`), and cut it by one rule (`Stitching.excerpt`: the opening, then the
  messages nearest the reader): the window, as a `[strand]` section paid before the tail and
  never ranked (a member whose raw entries are purged shown by its record); triage's thread
  and the judge's thread, the strand first. Closings are unchanged: each thread closes into
  its own record.
- **A person's reply no longer holds an unprompted draft** (revising ADR 0022): the judge
  sees every reply so far, in the thread and its strand, and `worth` decides. The one hard
  hold is the assistant's own reply after the heard message, in the thread or its strand,
  so grit never posts twice.

Consequences:

- A top-level reply is triaged and judged with the message it answers, and a window shows
  what its thread continues, with no change to conversations, periods or closings.
- Same label, not same reach: a strand stays in one room, which is one label while only
  public channels are served (ADR 0019), but it shows another thread's heard messages
  unranked, where `[afar]` shows them only when they rank. The lattice question stays open.
- Each top-level message with a candidate costs one more classifier call and one BM25 query.
- A reply whose own triage has not yet stitched it is not in its strand when a draft is
  settled: the race is one classifier call against a draft and a judge.
- Enforced by `StrandTests`, `StitchingTests`, `StitchJsonTests`, `StitchContract` (in
  memory and `SqlStitchTests`), `AlongTests`, `TriageTests`, `TurnStitchTests`,
  `TurnJudgeTests`, `SpeechTests`, `TurnSpeechTests`, `RetrievalAssemblerTests`,
  `StitchLiveTests`, and the replay gate.
