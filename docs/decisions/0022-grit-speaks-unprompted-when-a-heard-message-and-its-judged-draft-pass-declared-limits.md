# 0022. grit speaks unprompted when a heard message passes triage, its draft passes a judge, and declared limits allow it

Status: accepted (2026-09-30), revised (2026-09-30); amended (2026-10-03): the gate over
triage's answers is the deployment's `Limits.drafts`; amended (2026-10-03): a gate is built
from bounds by "every one of" and "any one of", and is unread only when no failing bound
decides it; amended (2026-10-03): a message said to grit, weighed in its turn, is never
considered; amended (2026-10-04): a
message triage reads as directed at grit by name is answered as named; amended (2026-10-04):
a named draft is not judged

Context: a listened channel (ADR 0020) is kept, but grit only ever answered what was said to
it. A deployment also wants grit to join in, with context or insight a thread lacks, when that
is worth the interruption. Turned down:

- a workflow of its own for speaking: it would duplicate the turn's window, ledger, summary,
  delivery and recovery;
- the edge deciding: it would put triage and the ledger in the edge;
- gating on triage alone: measured, v1's `helps` passed about a quarter of what is heard,
  "yep" among it;
- gating on the kind "question": grit is to add insight, not only answer;
- judging in triage: triage and the turn share their conversation's queue slot, so triage
  cannot wait for a draft;
- the drafting model as its own judge: dearer, and it grades its own work;
- a timed wait before drafting: a wait holds the conversation's queue, and DBOS offers no
  delayed enqueue.

Decision:

- **The edge says where grit may answer.** It hears each message with a `Reach`: where a reply
  would go, in its own address form, and whom the message names. A past message has no
  address, so it is never answered.
- **The deployment says whether grit speaks, and within what**: `Speaking.Off`, `Shadow` (draft
  and judge, never post) or `Within(Limits)`. The limits are `drafts`, a gate over triage's
  answers, `postAt`, freshness, a rate of posts per thread, per room and per deployment, and
  a daily cap on speech spend. Speech is also counted in the deployment's budget.
  (Amended 2026-10-03: the gate was `helpsAt` and "not chatter", fixed to v1's answers.) A
  gate is a bound on a reading of a named answer (a yes/no's probability, a choice's weight
  on a key, or whether a key is its most weighted), every one of some gates, or any one of
  them: a closed set of forms, which a deployment composes from grit's named parts. The
  reference deployment speaks by live triage's own (`TriageQuestions.ShippedSpeak`: gap asks,
  still open, and either directed at grit by its persona's name (ADR 0026) or not directed at
  one person and not something no record could supply; amended 2026-10-04: before, a message
  naming grit read as directed at one person and was held). A gate reading a question live triage does
  not ask is refused at declaration (`SpeechUnread`): it would hold every message.
- **Triage considers each message it tagged.** It holds on the first check that fails: off,
  no address, stale, untagged, the gate (`Gated`, with every bound the answers failed and
  what each read; `Unasked`, when the gate cannot be decided without an answer the answers
  lack, as for a message triaged by an earlier set; an answer can only decide an unread gate,
  never reverse a decided one), naming someone else, an earlier unprompted turn in the thread
  not yet answered, a rate reached, the speech cap or the budget. Otherwise it starts the
  heard message's own turn. It fails closed. A hold an earlier build kept as `chatter` or
  `below` reads back as gated on v1's bound.
- **Only a heard message is considered** (amended 2026-10-03). A message said to grit that
  its turn weighs with live triage's set (ADR 0020, ADR 0025) keeps no tags and is never
  considered: it is answered because it was addressed, so its answers only shape its offer.
- **The turn drafts.** It is told it was not addressed, to add what the thread lacks from what
  it was shown, or to reply `pass`. (Amended 2026-10-04.) A message triage read as directed at
  grit (`Tags.V3.directed`: it asks, is still open, and `to-grit` is at least one half) roots
  a `Named` turn instead, recorded in its `offer` like any root: told the message reads as said
  to it, to answer what it asks, or to reply `pass` when it only talks about the assistant.
  It is offered as any heard turn. Turned down: routing it as said to grit, which would let a
  classifier's reading decide addressing (ADR 0020 leaves that to the edge) and skip the
  rates and the speech cap. Its answer is a draft: never shown, never searched, and
  not a period's activity.
- **A classifier judges the draft** against the thread and what the turn recalled: is it
  grounded in what was recalled, is it worth the interruption. The weaker of the two is its
  score. A third question, whether the draft adds what the thread lacks, was dropped: it
  scored a correct recalled answer to the thread's own question as a restatement, and chatter
  answered with trivia as adding. A pass, or a window that recalled no record and no other
  conversation, is settled without asking. (Amended 2026-10-04.) A named draft is not
  judged: like an addressed reply, it is posted unless it passes. Holding a bad answer
  after it was drafted saves nothing it cost, and `worth` scores the interruption an
  asked-for answer is not. It still passes the gate, the rates, the speech cap and the
  assistant's own reply before it. A named draft judged before this reads as posted or
  shadowed unjudged, its score dropped. Turned down: a judge of its own (whether it answers
  what was asked without making up facts), which this ADR first had; refining a draft before
  it posts would be a workflow of its own.
- **The draft is posted** in the message's thread, only when its score reaches `postAt` (a
  named draft has none to reach), speaking is `Within`, and the assistant has not already replied after the message, in the
  thread or its strand (ADR 0023). That is checked in the transaction that writes the reply
  and awaits its delivery. A person's reply does not hold it: the judge sees every reply so
  far, and `worth` decides.
- **Replay.** Whether a turn drafts is recorded in its `offer` (`root`), so replaying it never
  depends on live settings.
- **Every decision, score and outcome is kept** (`grit.speech`), with an excerpt of the draft,
  as long as its period's usage, so each unprompted reply can be stated with how it was
  approved.

Consequences: a listened thread can be answered unasked, within limits a deployment states,
and every unprompted post can be traced to the scores that allowed it, a named one to the
triage answers that rooted it. Harder: every heard message in a
speaking deployment writes a decision row, and a busy channel spends its speech cap on drafts
before its rates bind. Enforced by `SpeechTests` (the rule), `SpeechContract` (the ledger),
`TurnSpeechTests` and `TurnJudgeTests` (the draft and its judge), `TriageTests`,
`SlackEdgeTests` (reach), `UnpromptedLiveTests`, and `TurnReplayTests` and
`LifecycleReplayTests` for the new steps (`named-judged` and `named-posted`, a named turn
before and after its drafts went unjudged).
