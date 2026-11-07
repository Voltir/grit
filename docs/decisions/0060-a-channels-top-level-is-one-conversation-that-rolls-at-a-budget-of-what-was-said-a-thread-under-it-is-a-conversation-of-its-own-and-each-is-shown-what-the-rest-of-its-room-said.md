# 0060. A channel's top level is one conversation that rolls at a budget of what was said, a thread under it is a conversation of its own, and each is shown what the rest of its room said

Status: accepted (2026-11-06); revised (2026-11-07)

Context: a channel is where a team talks, mostly at the top level, in one-line turns whose
topics interleave within minutes and come back days later. A conversation per thread left
nearly every top-level message a conversation of one message and its close, and a turn could
not see what was said a minute before in another thread. Measured on a team channel over a
week, of the earlier messages of a message's topic, its thread showed 8%, the room's last 25
messages 81%, the whole week 100% at about 7k tokens. Turned down:

- **a thread per conversation, its first message stitched** to an exchange of its room by a
  classifier: a call per top-level message, and a strand read only at a boundary;
- **a window gated by a topic chosen as each message arrives**: 81% joined right, an early
  wrong join carried forward, and it showed no more than a tail of 25;
- **the room's last day**: a time limit bounds nothing at volume;
- **threads as entries of the top level's conversation**: a reply would move the room's
  period, and a thread would close into nothing of its own;
- **closing a room's period when it settles**: a lull would clear the window (ADR 0052) and
  write a summary several times a day.

Decision:

- **A channel's top level is one conversation** (`Origin.Room`), its place the room. Every
  message said there is a turn of it: heard, said to grit, or grit's post (`Inbox.posted`,
  deduplicated by its source).
- **A thread is a conversation of its own** (`Origin.Thread`), begun by its first reply,
  under the top level's turn it replies to, which its window shows first while its period is
  its first. A reply is said to grit when it mentions grit or its root was said to grit.
- **Every conversation of a room is shown what the rest of the room said** since the top
  level's open period opened: newest first within `Rolling.shown`, unranked, never a
  candidate, in the strand's place. A thread is also shown the top level's newest kept
  closing by its record. On a carried turn, what was said since the last part joins the
  append (ADR 0052).
- **The top level's period rolls** (ADR 0011): it closes when what its people and grit said
  reaches `Rolling.chars`, at the turn that reached it, later turns opening the next; or once
  it has been open `Rolling.most`. It is never asked and never lapses. The budget stays under
  what a window's tail holds, so a boundary draws the open period whole.
- **Its closings are each a candidate** (ADR 0013), as are its earlier periods' kept turns
  to its own windows.
- **No message of a room is stitched.** A Slack thread recorded before rooms
  (`Origin.Slack`) keeps its rule: a reply joins it, its first message may have been stitched
  to an exchange of its room (`Origin.stitchable`), and its strand, a root and the threads that
  follow it directly, is read as context (`Along.read`), until its entries age out.
- **Whether an unprompted reply goes at the top level or in its thread** is ADR 0059's.
- **Rooms are a deployment's switch**, `GRIT_SLACK_CONVERSATIONS=rooms`, off by default and
  honoured by backfill; a message heard under a thread before the switch is not heard again.

Consequences:

- What a room said in the last hours is in every window of the room at no classifier call;
  triage is the only call a top-level message costs.
- A top level's turns, triages and closes share one queue partition, so a long turn delays
  triage behind it, and only one unprompted draft of the room is pending at a time.
- A busy room brings every thread in it to its boundary sooner: what the room says grows
  each append.
- At ten times the busiest hour measured, a room rolls about hourly; at today's volume,
  about weekly. Its closings are kept the ledger window, 180 days.
- Topics are made after the fact and never decide what a window shows.
- Enforced by `OriginTests`, `RetrievalAssemblerTests` and the replay gate.
