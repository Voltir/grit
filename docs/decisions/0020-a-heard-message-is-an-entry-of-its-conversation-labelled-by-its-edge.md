# 0020. A heard message is an entry of its conversation, labelled by its edge and triaged, and supports no Standing line

Status: accepted (2026-09-28); amended (2026-10-03): triage asks a question set, its
answers kept by name; amended (2026-10-03): a message said to grit is asked the same set in
its turn, when its recipe reads the answers, and keeps no tags

Context: grit was answering only what was said to it. A deployment also wants the rest of a
listened place (a Slack channel's threads) kept, so a later question can be answered from
what colleagues said to each other. The choices:

- Who decides whether grit was addressed. The engine could inspect each edge's events (a
  mention, a thread grit started). That was turned down: it would put Slack's specifics in
  the engine.
- What a heard message becomes. It could be ingested as a plugin's raw input, but the
  plugin contract forbids raw entries so that no plugin holds back a deletion (ADR 0011). It
  could be a turn that answers nothing, but that would run a model call per message.
- What a heard remark establishes. Grounding it as a person's word (ADR 0018) was turned
  down. It was said to someone else, may be a joke, and may be overruled later.

Decision:

- **The edge labels each message it takes, addressed or heard.** An addressed message is a
  turn, as before. A heard one is recorded through `Inbox.hear` as an entry of its
  conversation (`Payload.Heard`) on a turn number of its own, which runs a workflow only when
  grit drafts a reply to it (ADR 0022). The engine never inspects an edge's specifics.
- **A heard message keeps the time it was said.** `Inbox.hear` takes it: the entry is dated
  then, and a period it opens opens then, so a message heard late is exactly as quiet as it
  was. A thread already past idle closes on the next sweep, never asked whether anyone is
  waiting. An addressed message is dated when it is ingested. The closing itself is dated
  when the sweep reaches it, not at its deadline.
- **What was said before grit listened is heard by a command, `grit backfill`**, run before
  `grit serve`, never as a catch-up inside it. It reads each listened channel's last days,
  shows what hearing them would cost at most, and asks first. It hears each message at its
  own time, a past mention of grit included (a past message is never answered), then sweeps
  until nothing due is left and every workflow it started has ended. Nothing caps what it
  spends: over what today's cap leaves, it only warns. A catch-up at every serve start was
  turned down: listening and catching up at once reorders a thread, and with nothing
  running beforehand, serve's first sweep would enqueue every due close at once.
- **Hearing is never refused over the daily cap.** What a heard period costs is its closing,
  which is in the ledger.
- **A heard line supports no Standing line.** The closing writer sees it labelled `h`,
  under its speaker's name. A Standing item resting only on heard lines is not kept.
  Standing comes only from what was said to grit, or shown by a tool.
- **A period grit only heard is closed with its record written as reported speech** ("Emily
  said the freeze moves to Thursday"): its prose and outcome, never Open or Standing lines.
  The policy's `heard` role writes it. In a period with anything said to grit, what was
  said to grit can still stand.
- **Each heard message is triaged once** by a classifier, in one call. Its tags are kept
  beside its entry and deleted with it. Triage writes no entry, so it never moves a period's
  deadline. (Amended 2026-10-03.) It asks a question set, `TriageQuestions.Shipped`, and
  keeps every answer as given, under its question's name, in the order asked: v2's `gap`
  (what the message leaves open: it asks, its author owes, it closes something, or nothing),
  whether that is still `open`, whether it is directed `to` one person, whether it is
  `durable`, whether it is something no record could supply (`anchor`), then one yes/no per
  knowledge source covering the message's conversation. Named answers, not columns: the
  per-source questions differ by deployment and place, and live and shadow rows become one
  shape. A message triaged before then keeps v1's: its kind (question, answer, decision,
  announcement or chatter), and whether someone waits on a reply, whether it is durable, and
  whether a reply from grit would help; a triage in flight across the change reads its
  recorded v1 answers back under those names.
- **A message said to grit is asked live triage's set in its own turn** (amended
  2026-10-03), only when its deployment's recipe offers a service it links by a knowledge
  source covering its place (ADR 0025): the turn's `weigh` step builds the state as for a
  heard message (`TriageInput.read`), after its opening's placement, waited for at most
  `Mentions.PlacedWithin`, and asks `TriageQuestions.Shipped` once, waiting at most
  `Mentions.AskWithin` for the answer. Its answers are the step's
  recorded output and its call a usage-ledger row of the turn; no tags are kept, so speech,
  earning, shadows and reviews never see a mention. A placement or classifier that fails or is
  late leaves it unweighed, offered everything, its step recording which by kind alone. It adds one classifier call to such a mention's
  latency, before its first model call. Turned down: a triage workflow for a mention, which
  would put a second workflow and a cross-workflow wait in every mention's path and keep a row
  that speech, shadows and reviews would each have to skip.
- **A period earns a written closing** when grit was addressed in it, or a heard message in
  it was tagged durable (at 0.5 or above), or one is untagged or unanswered. It fails open,
  so a triage outage costs money, never a record. `Earning.earns` is the one definition.
  (Amended 2026-10-03.) It reads `durable` by name (`Earning.Durable`), the name every set
  asks it under; answers without a `durable` yes/no earn, as an untagged message does, and a
  build whose live set does not ask it is refused at declaration (`DurableUnasked`).
- **A due period that does not earn closes `Unearned`**, whatever its deadline's reason, with
  no classifier or model call. Its closing's prose is a fixed line ("Heard 4 messages;
  nothing kept.") and its balance is carried, so retention and the next period work as for
  any close. A Resolved verdict on such a period is recorded as unearned; the verdict stays
  in `grit.verdicts`. An unearned closing is never shown from elsewhere, and keeps no
  digest line.

Consequences:

- A thread nobody addressed to grit leaves a record, so a question elsewhere can be answered
  from it as what someone said, never as a settled fact.
- A heard period costs one classifier call per message, and a written closing only when it
  earned one. Which messages grit may answer unprompted is derived from the answers by a
  gate (ADR 0022).
- A window shows a heard message as speech not said to grit.
- Enforced by:
  - the inbox contract (`InboxContract`: a heard message dated when it was said);
  - `CloseLiveTests` (a message heard days late is closed on the first sweep, never asked);
  - `BackfillTests` (backfill waits out every workflow before its last sweep);
  - `SlackEdgeTests` (what is addressed, heard or ignored);
  - `LabelledTests` and `ClosingSummaryTests` (heard lines ground nothing);
  - `CloseTests` (the heard pin, reported speech, and an unearned close);
  - `EarningTests` and `TriageTests` (what earns, and triage once per message);
  - `TriageInputTests` and `MentionsTests` (a mention asked as a heard message is, keeping
    nothing), `TurnWeighTests` (only when the recipe reads its answers);
  - `PeriodContract` (an unearned closing is never closed elsewhere) and `DigestTests`;
  - `LifecycleReplayTests`.
