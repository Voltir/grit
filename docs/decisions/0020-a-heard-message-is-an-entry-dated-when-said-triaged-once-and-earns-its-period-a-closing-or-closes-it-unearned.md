# 0020. A heard message is an entry of its conversation, dated when said, triaged once, and earns its period a closing or closes it unearned

Status: accepted (2026-09-28); revised (2026-10-16)

Context: grit was answering only what was said to it. A deployment also wants the rest of a
listened place (a Slack channel's threads) kept, so a later question can be answered from
what colleagues said to each other. The choices:

- Who decides whether grit was addressed. The engine could inspect each edge's events (a
  mention, a thread grit started). Turned down: it would put Slack's specifics in the engine.
- What a heard message becomes. It could be ingested as a plugin's raw input, but the
  plugin contract forbids raw entries so that no plugin holds back a deletion (ADR 0011). It
  could be a turn that answers nothing, but that would run a model call per message.

Decision:

- **The edge labels each message it takes, addressed or heard.** An addressed message is a
  turn. A heard one is recorded through `Inbox.hear` as an entry of its conversation
  (`Payload.Heard`) on a turn number of its own, which runs a workflow only when grit drafts
  a reply to it (ADR 0022). The engine never inspects an edge's specifics. What an edge hears
  is every room its bot is a member of (ADR 0033).
- **A heard message keeps the time it was said.** Its entry is dated then, and a period it
  opens opens then, so a message heard late is exactly as quiet as it was; a thread already
  past idle closes on the next sweep. An addressed message is dated when it is ingested; a
  closing is dated when the sweep reaches it.
- **What was said before grit listened is heard by a command, `grit backfill`**, run before
  `grit serve`, never as a catch-up inside it. It reads the last days of each room, shows
  what hearing them would cost at most and asks first, hears each message at its own time (a
  past mention is never answered), then sweeps until nothing due is left and every workflow
  it started has ended. Over what today's cap leaves, it only warns. A catch-up at every
  serve start was turned down: listening and catching up at once reorders a thread, and
  serve's first sweep would enqueue every due close at once.
- **Hearing is never refused over the daily cap.** What a heard period costs is its closing,
  which is in the ledger.
- **A heard line grounds nothing** (ADR 0018). A period grit only heard is closed as reported
  speech ("Emily said the freeze moves to Thursday"), written under the policy's `heard`
  role.
- **Each heard message is triaged once**, by a classifier in one call, asking live triage's
  question set (`TriageQuestions.shipped`). Every answer is kept as given, under its
  question's name, in the order asked, beside the entry and deleted with it: named answers,
  not columns, since per-source questions differ by deployment and place. A shipped set is
  fixed: a new or reworded question is a new set under a new name, so every recorded answer
  keeps its meaning and an older set stays one fixed set for shadows and corpora. Triage
  writes no entry, so it never moves a period's deadline. A message said to grit is weighed
  in its own turn instead (ADR 0025).
- **A period earns a written closing** when grit was addressed in it, or a heard message in
  it was tagged `durable` at `Earning.DurableAt` or above, or one is untagged or unanswered.
  It fails open, so a triage outage costs money, never a record. `Earning.earns` is the one
  definition; a deployment whose live set does not ask `durable` is refused at declaration
  (`DeploymentRefusal.DurableUnasked`).
- **A due period that does not earn closes `Unearned`**, with no classifier or model call: a
  fixed line of prose and its balance carried, so retention and the next period work as for
  any close. An unearned closing is never shown from elsewhere and keeps no digest line.

Consequences:

- A thread nobody addressed to grit leaves a record, so a question elsewhere can be answered
  from it as what someone said, never as a settled fact.
- A heard period costs one classifier call per message, and a written closing only when it
  earned one. Which messages grit may answer unprompted is derived from the answers by a
  gate (ADR 0022).
- Enforced by: the inbox contract (`InboxContract`), `CloseTests` (reported speech and an
  unearned close), and the replay gate (`LifecycleReplayTests`).
