# 0008. A conversation's agenda is flat, recorded as events, and placed before assembly

Status: accepted (2026-09-24); revised (2026-10-18)

Context: grit is to narrow each turn's context by what a conversation is about, so each
message needs a place on its conversation's agenda: the conversation is where, the agenda is
what it is about. (This record called the agenda's items topics until "topic" came to name
the knowledge that spans conversations; the stored `"topic"` keys and ids belong to the
agenda.) Several choices were open:

- **Agenda structure.** A stack of items, with asides, sub-items and returns, was turned
  down: classifying the move was the weak point in the spike.
- **Storage.** The agenda could live in a mutable table, beside the entries, or be recorded
  as events.
- **Who judges.** The main model on every turn, a cheap classifier alone, or both.

The spike measured a cheap classifier's yes/no ("is this about the current topic?") right on
93 of 100 labelled messages, never wrongly saying a message had changed topic, at a fraction
of a cent and about 100 ms a call.

Decision:

- **A flat agenda.** A conversation's agenda is flat, its items ordered by recency. Each user
  message is placed in exactly one item, and a return is just placement in an earlier item.
- **Placed before assembly.** The turn's `classify` step places the message before its
  window is assembled, through a `Classifier` (core; implemented in `grit.models`), and
  `record-topic` records it. p(same) ≥ 0.8 stays; below 0.2, the classifier chooses among the
  earlier items and a new one; between them, the message stays for now and the main model is
  asked.
- **The main model, only when unsure.** It is asked through a `topic` tool offered beside the
  turn's own; its answer is kept by `record-verdict`, never among the conversation's messages.
- **Events, not a table.** The agenda is append-only `Agenda.Event`s (opened, placed,
  described), stored as entries (`Payload.Agenda`, kind `"topic"`). What stands now is a pure
  fold (`Agenda.fold`).
- **Carried across a close.** A closing's balance (ADR 0011) keeps the agenda as lines (the
  section `"topics"`), each item by its name, with its one-line summary beside it (not part
  of the line's identity). A period's agenda is a fold over the balance of the closing it
  opened from and its own events alone (`EntryAgenda`), never an earlier period's, so it
  stands the same before and after the raw entries are purged. A carried item's id comes from
  its line (`Agenda.ItemId.carried`), and before a period places any message the item spoken
  in last is current. The close adds an item named in its period and touches one spoken in
  again, from the events, with no model, carrying the summary the period last described it
  with. An item never named is not carried.
- **Placements are weights.** A placement is weights over items, with an explicit remainder
  `elsewhere`. The message is in the heaviest item, and a turn's latest placement wins, the
  earlier ones kept.
- **Naming.** The summariser names an item once and keeps its one-line summary current.
- **The agenda never fails a turn.** No classifier, or one that fails, leaves the message
  where it is, recorded as unclassified.

Consequences:

- Nothing is ever rewritten. A new rule for placement or naming needs no migration: it reads
  the same events.
- A carried item keeps its last summary, so the classifier's choice among earlier items sees
  it as it was when it was last spoken in.
- Recording weights rather than a label lets later work treat items as weighted labels (for
  example, recall ranked by overlap of weights) on the rows that exist now.
- The ledger prices the classifier and the verdict on their own entries.
- Narrowing context by the agenda is a later decision; assembly does not read it yet.
- Every consumer of entries must ignore `Payload.Agenda`, which the exhaustive matches
  enforce, and an item's name is fixed once given.
- Enforced by `AgendaTests`, `TurnAgendaTests` and the replay gate.
