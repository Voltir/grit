# 0020. A heard message is an entry of its conversation, labelled by its edge and triaged, and supports no Standing line

Status: accepted (2026-09-28)

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
  conversation (`Payload.Heard`) on a turn number of its own, which runs no workflow. The
  engine never inspects an edge's specifics.
- **Hearing is never refused over the daily cap.** What a heard period costs is its closing,
  which is in the ledger.
- **A heard line supports no Standing line.** The closing writer sees it labelled `h`,
  under its speaker's name. A Standing item resting only on heard lines is not kept.
  Standing comes only from what was said to grit, or shown by a tool.
- **A period grit only heard is closed with its record written as reported speech** ("Emily
  said the freeze moves to Thursday"): its prose and outcome, never Open or Standing lines.
  The policy's `heard` role writes it. In a period with anything said to grit, what was
  said to grit can still stand.
- **Each heard message is triaged once** by a classifier, in one call: what kind of message
  it is (question, answer, decision, announcement or chatter), and whether someone waits on
  a reply, whether it states something worth keeping (durable), and whether a reply from
  grit would help. Its tags are kept beside its entry and deleted with it. Triage writes no
  entry, so it never moves a period's deadline. The kind decides nothing yet.
- **A period earns a written closing** when grit was addressed in it, or a heard message in
  it was tagged durable (at 0.5 or above), or one is untagged or unanswered. It fails open,
  so a triage outage costs money, never a record. `Earning.earns` is the one definition.
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
  earned one. What `helps` measures is recorded for when grit may speak unprompted.
- A window shows a heard message as speech not said to grit.
- Enforced by:
  - the inbox contract (`InboxContract`);
  - `SlackEdgeTests` (what is addressed, heard or ignored);
  - `LabelledTests` and `ClosingSummaryTests` (heard lines ground nothing);
  - `CloseTests` (the heard pin, reported speech, and an unearned close);
  - `EarningTests` and `TriageTests` (what earns, and triage once per message);
  - `PeriodContract` (an unearned closing is never closed elsewhere) and `DigestTests`;
  - `LifecycleReplayTests`.
