# 0058. A task longer than one window goes on in turns rooted on grit's own continuation note, started by the edge that drives it

Status: accepted (2026-11-05); revised (2026-11-10, 2026-11-26)

Context: a turn at its limit ends, its last calls told so, and the next turn draws a
boundary (ADR 0052). A task longer than one window (an hour of coding in `grit work`) needs a
next turn, and something must say it. Turned down: the edge saying a fixed line as the person
(words in a person's mouth, kept as theirs); a model's summary of the turn (ADR 0052 turned it
down; the cut turn's own handoff carries the task on); the engine going on after every cut
turn (spending unattended is the driver's policy).

Decision:

- **The root is grit's own note.** A continuation is its own kind of entry
  (`Payload.Continuation`): the turn that began its task, its turn number, why it went on
  (`WentOn`) and what its driver read of the task's checkout after the turn before
  (`CheckoutState`). It holds no words of grit's: they are core code, shown as grit's note
  (`Shown.continuation`), never escaped as a person's text, never searched. A failed check's
  command and output, and what was read of the checkout, are kept bounded and shown as
  material after the note; a note carrying either is believed no higher than a member, as a
  coding tool's result is. Authorship is its type, not a field.
- **It acts for the task's asker.** Only whoever wrote the task's first message records it
  (`Inbox.continued`), under that account, so its turn's asker and clearance are the task's
  (ADR 0030), and it is believed no higher than that message.
- **The edge that drives the conversation starts it**, through the inbox, as the clock edge
  starts a job (ADR 0002, ADR 0029); never the engine. `grit work` goes on while a turn ends at
  its limit (`TurnEnding.AtLimit`) or its task's check fails, up to `GRIT_WORK_TURNS`, each
  turn's source fixed by the task and its number, so a restart finds where it stopped.
- **Whether the task goes on is the model's declared state.** A turn briefed for one task is
  offered `task_status` on every call (`TurnLoop.offered`): `done` with its answer, or `more`
  with what the next turn needs. The loop never runs it. A call before the turn is ending is
  refused (`TaskStatus.Early`). A turn is ending on its budget's last two calls, whether its
  input limit cut its calls short or they ran out (`Rounds.endsAtLimit`), never on its first
  call. From its last call with tools on, a reply that calls it alone, or any reply to the last call that calls it, ends the turn, its
  handoff the reply's text, so no tool call reaches the conversation. Every call's tool choice
  is auto (ADR 0052); the last call's note (`Ending`) asks for it in words. `done` ends the
  task; `more`, or no declaration, ends the turn at its limit. Defaulting to "go on" costs at
  most a needless turn; "done" would pass a half-finished task as finished. Turned down:
  reading "done" from the reply's words (a guess where a type can say it), and every cut turn
  at its limit (a finished task got an extra turn).
- **A task its turn ends is checked** when its driver names an acceptance command. A turn that
  ends with a reply and no request to go on (`TurnEnding.Answered`) is followed by the command,
  run by the driver in the task's directory with a time limit: a pass ends the task, a failure
  goes on in a turn whose note carries the command, its exit code and its output's end. A
  failure repeating the last one's output stops. Turned down: the model's word alone (a live
  run declared a wrong path done); the engine running the check (the driver's policy); an entry
  kind for a failed check (a second root like the first).
- **What the next turn is shown** is a boundary: the task, then of the earlier turns only the
  newest, its handoff (its reply, when it declared none), not the note it answered, behind one
  gap line when turns are left out; then the session's digest and the note. No tool calls or
  results, and no earlier reply: each handoff is cumulative, and every turn shown would grow
  each boundary until the window slid; the newest alone keeps each boundary about one size, its
  prefix cached. Every assembler holds to it, recall included (`AssemblyRequest.drawable`).
  What the earlier turn learned of the checkout is carried by the note instead, so the next
  turn need not run commands again to find it out: the branch's diff stat against origin's
  default branch, uncommitted changes included, and the files git does not track, both read by
  git in the checkout; and, after a turn at its limit or a failed check, the turn's last run
  that failed with a line looking like a failure, as its recorded answer shows it, its lines
  picked by the rule `run` flags them by. A later run of the same command that passed drops it,
  as does the check's own command, whose output the note already shows. Each piece is cut to a
  bound (`CheckoutState`'s constants), so a continuation's row stays near 8 KB, at most one
  for each turn `GRIT_WORK_TURNS` allows a task. Turned down: running the tool loop's own runs
  again (they may be gone, and the engine runs no driver's policy), and a summary of the turn
  by a model (ADR 0052).
  After a turn that left no reply a driver going on gets the task alone (`grit work` stops).
  Turned down: the earlier turns' summaries (still growing, beside the digest), the last two
  handoffs (the older contradicts the newer), and folding earlier turns in large steps (each
  fold misses the cache).
- **A continuation's boundary holds its task first, reserved as a brief is.** The task's first
  message is packed after the brief, its cost reserved from the assembler's draw
  (`AssemblyRequest.task`), and no assembler draws or finds it, so it stays in view, cached,
  however many turns the tail drops. To the turn's gate it is an own entry, hidden as its stub
  like any other.

Consequences: a new driver (a Slack thread) sets its own policy for going on, over the same
note and inbox. The note's words are code, so changing them changes no stored row. Enforced by
the compiler (every match over `Payload` names the case), `InboxContract` (who may continue, as
whom) and `TurnLoopTurnTests` (the offer, each declaration's end, a boundary).
