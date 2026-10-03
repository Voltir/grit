# 0022. grit speaks unprompted when a heard message passes triage, its draft passes a judge, and declared limits allow it

Status: accepted (2026-09-30), revised (2026-09-30); amended (2026-10-03): the gate over
triage's answers is the deployment's `Limits.drafts`; amended (2026-10-03): a gate is built
from bounds by "every one of" and "any one of", and is unread only when no failing bound
decides it

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
  reference deployment speaks by live triage's own (`TriageQuestions.Shipped.speak`: gap asks, still open, not directed at one
  person, not something no record could supply). A gate reading a question live triage does
  not ask is refused at declaration (`SpeechUnread`): it would hold every message.
- **Triage considers each message it tagged.** It holds on the first check that fails: off,
  no address, stale, untagged, the gate (`Gated`, with every bound the answers failed and
  what each read; `Unasked`, when the gate cannot be decided without an answer the answers
  lack, as for a message triaged by an earlier set; an answer can only decide an unread gate,
  never reverse a decided one), naming someone else, an earlier unprompted turn in the thread
  not yet answered, a rate reached, the speech cap or the budget. Otherwise it starts the
  heard message's own turn. It fails closed. A hold an earlier build kept as `chatter` or
  `below` reads back as gated on v1's bound.
- **The turn drafts.** It is told it was not addressed, to add what the thread lacks from what
  it was shown, or to reply `pass`. Its answer is a draft: never shown, never searched, and
  not a period's activity.
- **A classifier judges the draft** against the thread and what the turn recalled: is it
  grounded in what was recalled, is it worth the interruption. The weaker of the two is its
  score. A third question, whether the draft adds what the thread lacks, was dropped: it
  scored a correct recalled answer to the thread's own question as a restatement, and chatter
  answered with trivia as adding. A pass, or a window that recalled no record and no other
  conversation, is settled without asking.
- **The draft is posted** in the message's thread, only when its score reaches `postAt`,
  speaking is `Within`, and the assistant has not already replied after the message, in the
  thread or its strand (ADR 0023). That is checked in the transaction that writes the reply
  and awaits its delivery. A person's reply does not hold it: the judge sees every reply so
  far, and `worth` decides.
- **Replay.** Whether a turn drafts is recorded in its `offer` (`root`), so replaying it never
  depends on live settings.
- **Every decision, score and outcome is kept** (`grit.speech`), with an excerpt of the draft,
  as long as its period's usage, so each unprompted reply can be stated with how it was
  approved.

Consequences: a listened thread can be answered unasked, within limits a deployment states,
and every post can be traced to the scores that allowed it. Harder: every heard message in a
speaking deployment writes a decision row, and a busy channel spends its speech cap on drafts
before its rates bind. Enforced by `SpeechTests` (the rule), `SpeechContract` (the ledger),
`TurnSpeechTests` and `TurnJudgeTests` (the draft and its judge), `TriageTests`,
`SlackEdgeTests` (reach), `UnpromptedLiveTests`, and `TurnReplayTests` and
`LifecycleReplayTests` for the new steps.
