# 0008. Topics are flat, recorded as events, and placed before assembly

Status: accepted (2026-09-24); revised (2026-09-26)

Context: grit is to narrow each turn's context by what a conversation is about, so each
message needs a topic. Several choices were open:

- **Topic structure.** A stack of topics, with asides, subtopics and returns, was turned
  down: classifying the move was the weak point in the spike.
- **Storage.** Topics could live in a mutable table, beside the entries, or be recorded as
  events.
- **Who judges.** The main model on every turn, a cheap classifier alone, or both.

The spike measured a cheap classifier's yes/no ("is this about the current topic?") right on
93 of 100 labelled messages, never wrongly saying a message had changed topic, at a fraction
of a cent and about 100 ms a call.

Decision:

- **Flat topics.** A conversation's topics are flat, ordered by recency. Each user message
  is placed in exactly one topic, and a return is just placement in an earlier topic.
- **Placed before assembly.** The turn's `classify` step places the message before its
  window is assembled, through a `Classifier` (core; implemented in `grit.models`), and
  `record-topic` records it. p(same) ≥ 0.8 stays; below 0.2, the classifier chooses among the
  earlier topics and a new one; between them, the message stays for now and the main model is
  asked.
- **The main model, only when unsure.** It is asked through a `topic` tool offered beside the
  turn's own; its answer is kept by `record-verdict`, never among the conversation's messages.
- **Events, not a table.** Topics are append-only `TopicEvent`s (opened, placed, described),
  stored as entries (`Payload.Topic`). What stands now is a pure fold (`Topics.fold`).
- **Carried across a close.** A closing's balance (ADR 0011) keeps the topics as lines, each
  by its name, with its one-line summary beside it (not part of the line's identity). A
  period's topics are a fold over the balance of the closing it opened from and its own
  events alone (`EntryTopics`), never an earlier period's, so they stand the same before and
  after the raw entries are purged. A carried topic's id comes from its line
  (`TopicId.carried`), and before a period places any message the topic spoken in last is
  current. The close adds a topic named in its period and touches one spoken in again, from
  the events, with no model, carrying the summary the period last described it with. A topic
  never named is not carried.
- **Placements are weights.** A placement is weights over topics, with an explicit remainder
  `elsewhere`. The message is in the heaviest topic, and a turn's latest placement wins, the
  earlier ones kept.
- **Naming.** The summariser names a topic once and keeps its one-line summary current.
- **Topics never fail a turn.** No classifier, or one that fails, leaves the message where
  it is, recorded as unclassified.

Consequences:

- Nothing is ever rewritten. A new rule for placement or naming needs no migration: it reads
  the same events.
- A carried topic keeps its last summary, so the classifier's choice among earlier topics
  sees it as it was when it was last spoken in.
- Recording weights rather than a label lets later work treat topics as weighted labels (for
  example, recall ranked by overlap of weights) on the rows that exist now.
- The ledger prices the classifier and the verdict on their own entries.
- Narrowing context by topic is a later decision; assembly does not read topics yet.
- Every consumer of entries must ignore `Payload.Topic`, which the exhaustive matches
  enforce, and a topic's name is fixed once given.
- Enforced by `TopicsTests`, `TurnTopicsTests` and the replay gate.
