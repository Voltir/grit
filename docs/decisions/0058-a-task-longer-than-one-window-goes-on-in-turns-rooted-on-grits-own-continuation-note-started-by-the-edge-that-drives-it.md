# 0058. A task longer than one window goes on in turns rooted on grit's own continuation note, started by the edge that drives it

Status: accepted (2026-11-05)

Context: a turn at its window's limit ends, its last call forced, and the next turn draws a
boundary (ADR 0052). A task longer than one window, such as `grit work` given an hour of
coding, therefore needs a next turn, and something must say it. Turned down: the edge saying
a fixed line as the person (it puts words in a person's mouth, shown and escaped as theirs,
searched, and stored as text in every row); a model's summary of the turn so far (ADR 0052
turned compaction by summary down; the forced last reply, written on the cached prefix, is
the hand-off); clearing the window inside the turn (the slide ADR 0052 turned down); the
engine continuing every turn that ends at its limit (going on unattended, spending, is the
policy of whoever drives the conversation, and a person in a Slack thread may not want it).

Decision:

- **The root is grit's own note.** A continuation is its own kind of entry
  (`Payload.Continuation`), naming the turn that began its task and its turn number (the
  task's first turn is turn 1). It holds no text: its words are grit's, from core constants,
  shown to the model as grit's note (`Fenced.noted`, `Shown.Continuing`), never escaped as a
  person's text, and never searched. Authorship is its type, not a field.
- **It acts for the task's asker.** It is recorded through the inbox
  (`Inbox.continued`) only by whoever wrote the task's first message, under that account, so
  its turn's asker and clearance are the task's (ADR 0030); it is believed no higher than the
  task's first message, so grit's authorship raises no turn's level.
- **The edge that drives the conversation starts it**, through the inbox, as the clock edge
  starts a job (ADR 0002, ADR 0029); never the engine. `grit work` goes on while a turn ends
  at its limit (`TurnEnding.AtLimit`, read from the turn's outcome), up to a stop for a loop
  that cannot end (`GRIT_WORK_TURNS`), each turn under a source fixed by the task and its
  number, so a restart finds where the chain stopped.
- **What the next turn is shown** is ADR 0052's boundary: the conversation's messages, the
  forced reply among them, no tool calls or results, then the note.

Consequences: a task of several windows is a conversation of turns, each one cached,
append-only request ending in a hand-off, with no edited prefix. A new driver (a Slack
thread) decides its own policy for going on and reuses the note and the inbox. Every match
over `Payload` names the new case. The note's words are code, so changing them changes no
stored row. Enforced by `InboxContract` (who may continue, as whom, at what belief) and
`ShownTests` (the note is grit's, unescaped).
