# 0022. grit speaks unprompted when a heard message passes a gate, its draft a judge, and declared limits; one put to it by name is answered as addressed

Status: accepted (2026-09-30); revised (2026-10-20)

Context: a listened channel (ADR 0020) is kept, but grit only answered what was said to it.
A deployment also wants grit to join in when it is worth interrupting. Turned down: a
workflow of its own for speaking (it would duplicate the turn's window, ledger, delivery and
recovery); the edge deciding (it would hold triage and the ledger); gating on triage alone, or on the kind "question" (one let "yep" through); judging in triage (triage and the turn share their
conversation's queue slot, so triage cannot wait for a draft); the drafting model as its
own judge (dearer, and it grades its own work).

Decision:

- **The edge says where grit may answer**: it hears each message with a `Reach`, where a
  reply would go and whom the message names. A past message has no address.
- **The deployment says whether grit speaks**: `Speaking.Off`, `Shadow` (draft and judge,
  never post) or `Within(Limits)`. `Limits` holds `drafts`, a gate over triage's answers;
  `postAt`; freshness; post rates per thread, room and deployment; and a daily speech cap.
  A gate (`grit.core.triage.Gate`) is a bound on a reading of a named answer, all of some
  gates, or any of them: a closed set of forms a deployment composes. The reference deployment drafts by live triage's own
  (`TriageQuestions.ShippedSpeak`). A gate reading a question live triage does not ask is
  refused at declaration (`SpeechUnread`).
- **Triage considers each heard message it tagged** (`Speech.decide`), a few seconds after it
  was said (`Triage.Wait`), holding on the first check that fails, in `Silence`'s order: off,
  no address, believed below `Outside` (ADR 0036), stale, its author said more (the later
  message is weighed instead), untagged, the gate (`Gated` with every failed bound; `Unasked`
  when its answers cannot decide it), already answered by grit in its thread or strand, an
  unanswered unprompted turn in the thread, a rate, the speech cap, the budget. It fails
  closed. A message said to grit is never considered: it is answered because it was
  addressed (ADR 0025).
- **Each decision keeps the cues code read** (`Cues`): whom it names, grit's own reply, its
  author's next message, the followed exchange's settle verdict, its best record match. The
  gate reads some as facts (`Reading.Fact`) beside triage's probabilities; a fact is 0 when
  its cue is absent, so it only raises a branch, and naming someone is a branch of the gate,
  not a hold. Bounds are tuned by replaying `Speech.decide` over kept tags and cues.
- **A message put to grit by name is answered as said to grit.** When triage reads it as
  directed at grit (`Tags.directed`, in the persona's name, ADR 0021), the decision is
  `Answering`, held only as an addressed message could be: no address, the gate (none under
  `Off`), or the budget. Its turn's root is `ByName`: offered, told and reached as
  addressed, its reply the turn's own message (`Replied`), posted by the edge, counted in no
  rate or speech spend. This is the one point where a classifier's reading decides
  addressing. Turned down: drafting it and letting it `pass`, which dropped answerable
  questions (`TurnOffer.Root.Named`, kept for offers that recorded it).
- **Otherwise the turn drafts** (root `Heard`): told it was not addressed, to add what the
  thread lacks or reply `pass`. A draft is never shown, never searched, and not a period's
  activity.
- **A classifier judges the draft** against the heard message, its thread, and what the turn
  recalled: sections from elsewhere, documents, its own record where the thread cuts what it
  covers, and what its own calls read: is it `grounded` in those, and `worth` posting? The
  weaker is its score (`Judged.score`). A pass, or a draft with nothing recalled, is settled
  without asking; a document above the judge's clearance leaves it unjudged, so not posted.
- **The draft is posted** in the message's thread only when its score reaches `postAt`,
  speaking is `Within`, and grit has not replied since, in the thread or its strand
  (ADR 0023), checked in the transaction that writes the reply.
- **Replay.** Whether a turn drafts is recorded in its `offer` (`root`), never read live.
- **Every decision, score and outcome is kept** (`grit.speech`) as long as its period's
  usage. A heard turn that fails before its offer keeps `Failed` (`record-failure`).

**Measuring the gate.** A person labels by reaction whether a reply would have been welcome
(`grit.core.review`). Every message a review considers is a row of `grit.reviews` keyed by
its entry's id with no foreign key, so labels and their population outlive the raw window
(ADR 0014); it never holds the message's text.

Consequences: every unprompted post traces to the scores and cues that allowed it. Harder:
every heard message in a speaking deployment writes a decision row. Enforced by `SpeechTests`,
the reviews store's contract and `TurnReplayTests`.
