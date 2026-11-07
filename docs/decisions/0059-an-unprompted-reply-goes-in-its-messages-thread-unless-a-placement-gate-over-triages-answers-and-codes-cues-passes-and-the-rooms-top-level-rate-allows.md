# 0059. An unprompted reply goes in its message's thread unless a placement gate over triage's answers and code's cues passes and the room's top-level rate allows

Status: accepted (2026-11-07)

Context: grit posted every unprompted reply in its message's thread. In a channel where people
talk at the top level, nearly every thread is one message, a reply under it is easily missed,
and people who follow it up come back at the top level. A post at the top level is louder:
everyone sees it. Turned down:

- **always the thread**: buries answers the room needed;
- **where the message was said**: most heard messages are said at the top level, so grit
  would post there nearly every time;
- **the drafting model choosing**: a classifier labels features and code decides; a draft is
  not trusted to steer where grit writes;
- **deciding after the judge**, by a third question: the draft could not be told where it
  goes, and a post at the top level must stand alone.

Decision:

- **The edge says where a reply could go**: `Reach.replyTo`, a `ReplyTo` of its thread's
  address and, where the edge has a top level to post at, its room's (`ReplyTo.room`).
- **The deployment says when the top level**: `Limits.toRoom`, a `ToRoom` of a gate (ADR
  0022's forms) and a rate of its own; none, the default, is always the thread. A gate reading
  a question live triage does not ask is refused at declaration (`PlacementUnread`).
  `ToRoom.Suggested` reads `for-room` (v7's one new question: a reply for the whole channel,
  such as a heads-up, something several people asked, or an ask to tell everyone) and two
  facts code reads, 0 when absent so they only raise: `AtTop` (said at the top level) and
  `Calm` (how few messages people said there in the hour before). One top-level post a room
  in 2 hours.
- **Decided with the draft.** After every hold of ADR 0022 has passed, `Speech.decide`
  places the draft (`Decision.Drafting(turn, venue)`): `Venue.Room(to)` when its message has
  a room address, the gate passes, and the room's top-level posts within the rate are fewer
  than its count; else `Venue.Thread`. Placement never holds a message: a gate that fails or
  is unread, a spent rate or no address is the thread.
- **A message put to grit by name is answered in its thread** (`Answering`), whatever
  placement would read.
- **The draft and the judge are told** where it goes: the draft to say first what it answers,
  the judge to ask that of it. A draft in its thread is told and judged as before.
- **Counted again when posted**, in the transaction that writes the reply
  (`SpeechStore.placing`): a rate spent since is the thread, kept so. The post counts in the
  thread, room and deployment rates as any post.
- **Recorded.** The speech row keeps the room address while the draft goes there; the turn's
  `offer` records the venue, the thread when absent, so replay reads where it went, never the
  store.
- **Recorded once.** A reply at the top level is its turn's reply in its turn's conversation,
  as every reply is (ADR 0060); the edge records it nowhere else, so the room's other windows
  show it once.

Consequences: every top-level post traces to the bounds and the rate that allowed it; the
bounds are tuned by replaying `Speech.decide` over kept tags and cues, as ADR 0022's are.
Harder: a heard message keeps two addresses, and an edge with a top level says how to post
there and how to find a post it may have made. Enforced by `SpeechTests`, the speech store's
contract and the replay gate.
