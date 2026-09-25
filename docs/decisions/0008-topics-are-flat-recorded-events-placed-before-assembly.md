# 0008. Topics are flat, recorded as events, and placed before assembly

Status: accepted (2026-09-24)

Context: grit is to narrow each turn's context by what a conversation is about, so each
message needs a topic. Several choices were open:

- **Topic structure.** A stack of topics, with asides, subtopics and returns, was turned
  down. Classifying the move was the weak point in the spike (`spike/auto-topics`).
- **Storage.** Topics could live in a mutable table, beside the entries, or be recorded as
  events.
- **Who judges.** The main model on every turn, a cheap classifier alone, or both.

The spike measured Jev over 100 labelled messages. Its yes/no ("is this about the current
topic?") was right on 93 of 100 and never wrongly said a message had changed topic. Its
choice among earlier topics was right on 37 of 41 real changes, at about $0.00002 and
100 ms a call.

Decision:

- **Flat topics.** A conversation's topics are flat, ordered by recency. Each user message
  is placed in exactly one topic, and a return is just placement in an earlier topic.
- **Placed before assembly.** The turn's `classify` step places the message before its
  window is assembled, through a `Classifier` (core; Jev in `grit.models`). p(same) ≥ 0.8
  stays; below 0.2, the classifier chooses among the earlier topics and a new one; between
  them, the message stays for now and the main model is asked.
- **The main model, only when unsure.** It is asked through a `topic` tool, which
  `ModelRequest` can now carry. After it calls the tool, the model is called again for the
  reply, with at most one plain call as a fallback. The tool exchange is kept in the journal
  and the verdict event, never among the conversation's messages.
- **Events, not a table.** Topics are append-only `TopicEvent`s (opened, placed,
  described), stored as entries (`Payload.Topic`), like queries and windows. What stands
  now is a pure fold (`Topics.fold`).
- **Placements are weights.** A placement is weights over topics, with an explicit
  remainder `elsewhere`. The message is in the heaviest topic, and a turn's latest placement
  wins, the earlier ones kept.
- **Naming.** The summariser names a topic once and keeps its one-line summary current.
- **Topics never fail a turn.** No classifier, or one that fails, leaves the message where
  it is, recorded as unclassified.

Consequences:

- Nothing is ever rewritten. A new rule for placement or naming needs no migration: it
  reads the same events.
- Recording weights rather than a label lets later work treat topics as weighted labels
  (for example, recall ranked by overlap of weights) on the rows that exist now.
- The ledger prices the classifier and the verdict on their own entries.
- Assembly is unchanged, apart from the note and the tool in the grey band (shadow mode).
  Narrowing context by topic is a later decision.
- Two patches (`topics`, `verdict`) keep every earlier history replayable (ADR 0004).
- What enforces this: `TopicsTests`, `TurnTopicsTests`, `TurnVerdictTests`, the replay
  gate, and the gate's `topics` scenario.
- What is harder: every consumer of entries must ignore `Payload.Topic`, which the
  exhaustive matches enforce, and a topic's name is fixed once given.
