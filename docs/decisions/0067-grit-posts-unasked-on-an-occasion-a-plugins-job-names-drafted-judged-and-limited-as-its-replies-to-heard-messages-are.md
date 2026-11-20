# 0067. grit posts unasked on an occasion a plugin's job names, drafted, judged and limited as its replies to heard messages are

Status: accepted (2026-11-19)

Context: grit spoke only after someone spoke (ADR 0022). A deployment also wants it to speak
first when something its plugins keep makes that worth it: a planned meeting coming up, a day
the team talked of and never settled. Turned down: a job that asks the model and posts its
reply itself, through its report (it has no judge, no shadow, no rates and no decision row,
and a report names one destination per schedule); a core sweep over the store (core cannot
name a plugin's meetings); a new edge operation to find a room's address (ADR 0002).

Decision:

- **An occasion is a new root of an unprompted turn.** A plugin's keeping job names it
  (`Occasion`: a room, a key, a reason, the plugin's document it is about, and an instant it
  lapses at), and it goes the way a heard message's draft goes: decided by speech, drafted,
  judged, and posted or shadowed at the room's top level. Its precedent is grit's own
  continuation note (ADR 0058).
- **It is a move, `Keeping.occasion`: spawn's first form (ADR 0068).** It starts a turn in
  another conversation, charged to the speech accounts, at most once per key, recorded as
  `occasion:{name}` with its phases `:decide` and `:start`. It is no call (the engine is no
  service place) and no keep body (no turn starts inside a transaction).
- **Its entry is grit's own note, recorded by id** (`Payload.Occasion`: the reason, the
  document's version, the key). It says nothing, counts nothing spoken, is never searched, is
  no line of a period's transcript, opens no exchange, and is shown only to its own turn: a
  shadowed occasion feeds nothing that is written from what was said.
- **Speaking says whether**: `Limits.occasions`, off unless set, `Shadow(room)` or
  `Live(room)`, the lesser of it and the speaking mode winning. `room` is a rate of occasions
  decided in a room, posted or shadowed alike.
- **The address is the room's newest kept heard reach with a room address within a week**,
  and the room is not quiet. No address is no occasion.
- **Labels.** A room at another label than the run's is refused. The entry is believed at
  what the job's input was believed at; below `Outside` it is held.
- **The decision holds on the first failing check**: off, no address, below `Outside`, lapsed,
  another unprompted turn drafting in the conversation (either way round, with heard drafts),
  the occasion rate, the room and deployment rates, the speech cap, the budget; those from the
  rates on are `Speech.decide`'s own. Off, no address, a quiet room, below `Outside` and lapsed
  hold before anything is recorded, so the key stays free for a later run; a hold after the
  entry spends its key for good.
- **The turn starts as triage starts a drafting turn**; a start that fails settles the
  decision `Failed`, and the move says so (`Occasioned.Unstarted`), its key spent.

Consequences: an unprompted post traces to its occasion's decision as a heard draft traces
to its message's. A job that names occasions spends speech, not its own allowance, and a
deployment switches the whole path on by one value. Harder: a key held after its entry is
lost, so a rate that holds a day's note loses that day. Enforced by `SpeechTests`,
`MovesContract`, a capture probe (a turn's moves hold no starter) and the replay gate.
