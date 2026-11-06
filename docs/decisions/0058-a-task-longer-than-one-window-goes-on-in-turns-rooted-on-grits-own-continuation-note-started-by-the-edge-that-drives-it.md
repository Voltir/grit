# 0058. A task longer than one window goes on in turns rooted on grit's own continuation note, started by the edge that drives it

Status: accepted (2026-11-05); revised (2026-11-05)

Context: a turn at its window's limit ends, its last call forced, and the next turn draws a
boundary (ADR 0052). A task longer than one window, such as `grit work` given an hour of
coding, therefore needs a next turn, and something must say it. Turned down: the edge saying
a fixed line as the person (it puts words in a person's mouth, shown and escaped as theirs,
searched, and stored as text in every row); a model's summary of the turn so far (ADR 0052
turned compaction by summary down; the last reply, written on the cached prefix, is
the hand-off); clearing the window inside the turn (the slide ADR 0052 turned down); the
engine continuing every turn that ends at its limit (going on unattended, spending, is the
policy of whoever drives the conversation, and a person in a Slack thread may not want it).

Decision:

- **The root is grit's own note.** A continuation is its own kind of entry
  (`Payload.Continuation`), naming the turn that began its task and its turn number (the
  task's first turn is turn 1), and why it went on (`WentOn`). It holds no words of grit's,
  only its reason: its words are grit's, from core constants, shown to the model as grit's
  note (`Fenced.noted`, `Shown.Continuing`, `Shown.checkFailed`), never escaped as a person's
  text, and never searched. A failed check's command and output are kept bounded and shown as
  material, never as grit's words. Authorship is its type, not a field.
- **It acts for the task's asker.** It is recorded through the inbox
  (`Inbox.continued`) only by whoever wrote the task's first message, under that account, so
  its turn's asker and clearance are the task's (ADR 0030); it is believed no higher than the
  task's first message, so grit's authorship raises no turn's level.
- **The edge that drives the conversation starts it**, through the inbox, as the clock edge
  starts a job (ADR 0002, ADR 0029); never the engine. `grit work` goes on while a turn ends
  at its limit (`TurnEnding.AtLimit`, read from the turn's outcome), or its task's check fails
  (below; revised 2026-11-05), up to a stop for a loop
  that cannot end (`GRIT_WORK_TURNS`), each turn under a source fixed by the task and its
  number, so a restart finds where the chain stopped.
- **Whether the task goes on is the model's declared state** (revised 2026-11-05). A turn its
  limit cut is offered one more tool on its last call with tools and its last, `task_status`:
  `done` with its answer, or `more` with what the next turn needs. Its last call asks for that
  tool alone, forced where the provider can force one tool; Anthropic's models, whose thinking
  refuses a forced choice, are asked for it in words with tool choice `auto`, since under
  `none` a model told to call it was seen to answer nothing. The loop never runs it: a reply that calls it alone,
  or any reply to the last call that calls it, ends the turn, its handoff the reply's text, so
  no tool call reaches the conversation. `done` ends the task; `more`, or no declaration at
  all, ends the turn at its limit, and the driver may go on. Defaulting to "go on" costs at
  most a needless turn; defaulting to "done" would leave a task half finished as if it were
  finished. Turned down: reading "done" from the reply's words (a guess where a type can say
  it), and treating every cut turn as at its limit (a finished task got an extra turn).
- **A task its turn ends is checked** when its driver names an acceptance command (revised
  2026-11-05). A turn that ends with a reply and no request to go on (`TurnEnding.Answered`)
  is followed by the command, run by the driver in the task's directory with a time limit; a
  pass ends the task, and a failure goes on in a new turn whose note carries the command, its
  exit code and the end of its output. A failure with the same output as the one before
  stops, since another turn is unlikely to change it. Before checking a finished turn, the
  driver looks for the next turn already recorded, so a restart re-runs no check that has
  already decided. Turned down: the model's word alone (a live run showed a wrong path
  declared done); the engine running the check (the driver's policy, as going on is); a
  separate entry kind for a failed check (a second root beside an identical one).
- **What the next turn is shown** is ADR 0052's boundary: the conversation's messages, the
  forced reply or the handoff among them, no tool calls or results, then the note.
- **A continuation's boundary holds its task first, reserved as a brief is** (revised
  2026-11-05). The task's first message is packed by the turn after the brief, its cost
  reserved from what the assembler draws (`AssemblyRequest.task`, `Window.task`), and no
  assembler draws or finds it, so it stays in view however many of the task's turns the tail
  has dropped, shown once, in the part a provider caches as the task's. It is an own entry to
  the turn's gate, hidden as its stub like any other.

Consequences: a task of several windows is a conversation of turns, each one cached,
append-only request ending in a hand-off, with no edited prefix. Offering `task_status` on a
cut turn's last call with tools changes its tool set, so that call writes its prefix to the
cache afresh. A new driver (a Slack
thread) decides its own policy for going on and reuses the note and the inbox. Every match
over `Payload` names the new case. The note's words are code, so changing them changes no
stored row. Enforced by `InboxContract` (who may continue, as whom, at what belief),
`ShownTests` (the note is grit's, unescaped, a check's output fenced as material),
`PayloadJsonTests` (a reason's stored form and its bounds), `WorkTests` (when `grit work`
checks, goes on and stops), and `TurnLoopTests` and `TurnLoopTurnTests` (who is
offered `task_status`, what each declaration ends in, and the task held at a boundary), and
`TurnGateTurnTests` (the task hidden as its stub).
