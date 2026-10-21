# 0023. A Slack thread is a conversation, its first message may be stitched to an exchange in its room, and a strand is a star

Status: accepted (2026-09-30); revised (2026-10-21)

Context: grit's first edge for several people is Slack. A channel is a stream of unrelated
questions; a thread is one exchange, and Slack already shows it as one; a conversation per
person would split an exchange two people share. But people do not start a thread for each
message: a top-level reply ("lol", "is this a real question") began a conversation of its
own, so triage, the speech rule and the window weighed it without the message it answered.
Turned down:

- moving such a message into the conversation it continues: a thread would stop being a
  conversation, another thread's activity would move a period's deadline, and a model call
  would sit in the edge's write path;
- a topic as an object of its own (a room-level conversation, or a topics table):
  membership would have two homes, rewritten when two topics turn out to be one;
- recording the join as an entry: an entry is activity, and would move a deadline;
- transitive membership: over a week, chains of stitches join unrelated exchanges.

Decision:

- **A thread is a conversation.** A Slack thread's origin is
  `Origin.Slack(team, channel, threadTs)` and its place `slack:{team}/{channel}/{thread}`. A
  top-level mention starts a thread under itself.
- **Stitching adds a link, never moves a message.** When a stitchable conversation's first
  message arrives (`Origin.stitchable`: a thread whose first message was said at a channel's
  top level), a classifier judges whether it continues an exchange in its room. If it does, a
  stitch is kept: the conversation follows that exchange's root. A **strand** is a root and
  the conversations that follow it directly, a star, never a chain: a stitch always names a
  root.
- **Who places, and when.** A workflow of its own per first message (`Stitch`), queued by
  the inbox as the message is recorded, on a queue partitioned by room that runs one
  placement at a time in the order queued. The message's triage, or an addressed message's
  turn, waits for it before its other classifier calls; a reply waits for nothing. A
  placement reads every link queued before it in its room, so what it is offered depends on
  what was said and the order it reached grit, not on scheduling. A first message is placed
  once. The exchanges offered are its room's within the horizon: the most recently spoken in,
  and the best BM25 matches on its own words. Every placement keeps what the classifier was
  shown, why each exchange was offered, the probabilities and the tuning in force
  (`grit.core.stitch.Tuning`).
- **Kept beside its entry.** A stitch is a row beside the first message, deleted with it or
  with the conversation it follows. It writes no entry, so it moves no deadline; reading a
  strand writes nothing.
- **Readers read the strand as context, not recall,** through one read within its room
  (`Along.read`), cut by one rule (`Stitching.excerpt`: the opening, then the messages
  nearest the reader): the window's `[strand]` section, paid before the tail and never
  ranked; triage's thread and the judge's, the strand first. Each thread still closes into
  its own record.
- **A person's reply does not hold an unprompted draft**: the judge sees every reply in the
  thread and its strand, and `worth` decides (ADR 0022). The one hard hold is the
  assistant's own reply after the heard message, so grit never posts twice.

Consequences:

- A top-level reply is triaged and judged with the message it answers, with no change to
  conversations, periods or closings.
- A strand stays in one room, so its members share the room's label (ADR 0030), but it shows
  another thread's heard messages unranked, where `[afar]` shows them only when they rank.
- Each top-level message with a candidate costs one more classifier call and one BM25 query.
- A room's first messages are placed one at a time: one whose classifier call hangs holds
  the room's later ones until the classifier's timeout. Live, a redelivery or two messages
  crossing within seconds can place a first message before an earlier-said one.
- Enforced by `StitchContract` (in memory and in SQL), `StrandTests`, and the replay gate.
