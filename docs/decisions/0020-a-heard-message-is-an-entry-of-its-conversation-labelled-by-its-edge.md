# 0020. A heard message is an entry of its conversation, labelled by its edge, and supports no Standing line

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

Consequences:

- A thread nobody addressed to grit leaves a record, so a question elsewhere can be answered
  from it as what someone said, never as a settled fact.
- Every heard period costs a closing for now. Which heard periods earn one is decided by
  triage, which is not built.
- A window shows a heard message as speech not said to grit.
- Enforced by:
  - the inbox contract (`InboxContract`);
  - `SlackEdgeTests` (what is addressed, heard or ignored);
  - `LabelledTests` and `ClosingSummaryTests` (heard lines ground nothing);
  - `CloseTests` (the heard pin, and reported speech);
  - `LifecycleReplayTests`.
