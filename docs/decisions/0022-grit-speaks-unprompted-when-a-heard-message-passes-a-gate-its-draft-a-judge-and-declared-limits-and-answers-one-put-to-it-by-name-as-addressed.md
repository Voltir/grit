# 0022. grit speaks unprompted when a heard message passes a gate, its draft a judge, and declared limits; one put to it by name is answered as addressed

Status: accepted (2026-09-30); revised (2026-11-07)

Context: a listened channel (ADR 0020) is kept, but grit only answered what was said to it.
A deployment also wants grit to join in when it is worth interrupting. Turned down: a
workflow of its own for speaking (it would duplicate the turn's machinery); the edge deciding;
gating on triage alone (it let "yep" through); judging in triage (triage and the turn share a
queue slot, so triage cannot wait for a draft); the drafting model as its own judge; offering
a heard turn every reached service, or none (a writing tool would act on a draft nobody may
see; none hid a database a deployment wants drafts to read).

Decision:

- **The edge says where grit may answer**: it hears each message with a `Reach`, where a
  reply would go and whom the message names. A past message has no address.
- **The deployment says whether grit speaks**: `Speaking.Off`, `Shadow` (draft and judge,
  never post) or `Within(Limits)`. `Limits` holds `drafts`, a gate over triage's answers;
  `postAt`; freshness; post rates per thread, room and deployment; and a daily speech cap.
  A gate (`grit.core.triage.Gate`) is a closed set of forms a deployment composes: a bound on
  a named answer's reading, all of some gates, or any. A gate reading a question live triage does not ask is
  refused at declaration (`SpeechUnread`).
- **Triage considers each heard message it tagged** (`Speech.decide`), a few seconds after it
  was said (`Triage.Wait`), holding on the first check that fails, in `Silence`'s order: off,
  no address, believed below `Outside` (ADR 0036), stale, its author said more (the later
  message is weighed instead), untagged, the gate (`Gated` with every failed bound; `Unasked`
  when its answers cannot decide it), already answered by grit in its turn, the thread under
  it or its strand, an unanswered unprompted turn in its conversation (in a channel's top level,
  ADR 0060, one draft of the room at a time), a rate, the speech cap, the budget. It fails
  closed. A message said to grit is never considered: it is answered because it was
  addressed (ADR 0025).
- **Each decision keeps the cues code read** (`Cues`): whom it names, grit's own reply, its
  author's next message, the followed exchange's settle verdict, its best record match. The
  gate reads some as facts (`Reading.Fact`) beside triage's probabilities, 0 when absent, so a
  fact only raises a branch. Bounds are tuned by replaying `Speech.decide` over kept cues.
- **A message put to grit by name is answered as said to grit.** When triage reads it as
  directed at grit (`Tags.directed`, ADR 0021), the decision is
  `Answering`, held only as an addressed message could be: no address, the gate (none under
  `Off`), or the budget. Its turn's root is `ByName`: offered, told and reached as
  addressed, its reply the turn's own message (`Replied`), counted in no rate or speech spend:
  the one point where a classifier's reading decides addressing. Drafting it was turned down:
  answerable questions `pass`.
- **Otherwise the turn drafts** (root `Heard`): told it was not addressed, to add what the
  thread lacks or reply `pass`. It is offered its workspace's tools by the recipe, and of
  each service a `Reaches` links its place to, only when that link lets a heard turn read it
  (`Reaches.Heard.ReadOnly`; `Never` by default), and then only the tools whose declarations
  write nothing out of grit and ask no one. A draft is never shown, never searched, and not a
  period's activity.
- **A classifier judges the draft** against the heard message, its thread, and what the turn
  recalled or its own calls read: is it `grounded` in those, and `worth` posting? The
  weaker is its score (`Judged.score`). A pass, or a draft with nothing recalled, is settled
  without asking; a document above the judge's clearance leaves it unjudged, so not posted.
- **The draft is posted** where its decision placed it (ADR 0059) only when it reaches `postAt`,
  speaking is `Within`, its turn is believed at `Outside` or above (one that read rows the
  public typed, ADR 0057, is kept `Untrusted`), and grit has not replied to it, in its turn or
  the thread under it, or in a strand (ADR 0060), checked in the transaction that writes the
  reply.
- **Replay.** Whether a turn drafts, its tools and where it posts are in its `offer`
  (`root`, `reached`, `venue`), never read live.
- **Every decision, score and outcome is kept** (`grit.speech`) as long as its period's
  usage. A heard turn failed before its offer keeps `Failed`.

**Measuring the gate.** People label by reaction whether a reply was welcome
(`grit.core.review`), in rows keyed by entry id that hold no text.

Consequences: every unprompted post traces to the scores and cues that allowed it. Harder:
every heard message in a speaking deployment writes a decision row. Enforced by `SpeechTests`,
the reviews store's contract and `TurnReplayTests`.
