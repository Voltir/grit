# 0029. A job is versioned code run as a turn of its slot's conversation, a schedule is data, and a clock edge starts each due slot

Status: accepted (2026-10-05); revised (2026-10-16)

Context: some work is declared and recurring (keeping an index fresh, a nightly pass, a
daily standup per person); some is asked for in conversation (a reminder for Thursday). The
close alternative was a sweep per feature, as the close, settle and posting each have: every
recurring feature would add its own. A job as data and a scheduler in core were turned down
(ADR 0021: a job's plan is code, and cron is a market of its own).

Decision:

- **A job is a turn with a Scala planner, and it has a version.** It is contributed by a
  plugin (ADR 0027) or declared by the deployment, and runs under grit's epoch (ADR 0021).
  Jobs are never patched.
- **A schedule is a row** (`grit.schedules`): its job, by name; a slot rule (`SlotRule`),
  once at an instant or a recurrence from a small closed set (daily, on weekdays, weekly, at
  a time in a time zone), never a cron expression; parameters, as JSON the job reads; the
  principal it runs for, whose budget it spends; and where its runs report: kept in their own
  conversation, or posted at a destination that names its edge as well as its address.
- **Schedules come from two sources.** A deployment or plugin declares some; they run for the
  deployment's persona and are reconciled with the rows when the engine starts, a schedule no
  longer declared being ended. A tool writes others from a turn: once-only, run for the person
  who asked, recorded with that turn, theirs to cancel, reporting where that turn's reply was
  posted, and never touched by reconciliation. A turn whose reply nobody posts cannot write
  one.
- **A slot is a schedule and its nominal instant, and a run is a turn of the slot's own
  conversation** at the job's task place. The slot names the conversation and the version
  names its opening, so each (slot, version) is its own turn, and its key is the run's id, so
  the inbox refuses a second start. Deliveries, retention and the close treat it as any turn;
  its period closes mechanically, with no model or classifier call (ADR 0011, "When a period
  closes"), its closing never shown in another conversation's window.
- **The clock edge** reads due slots from Postgres and starts their runs through the inbox,
  as any edge starts a turn (ADR 0002); it hosts no place. After downtime, a once slot runs
  late within the grace its rule declares and is otherwise recorded as missed; a recurrence
  runs its latest missed slot alone. Each pass makes progress on both due slots and runs in
  flight.
- **A deploy does not by itself end a run.** The engine recovers every workflow of its epoch;
  a run started under another version than its job's current one ends itself superseded,
  writing no reply, and its slot is started again at the current version unless a later slot
  is due. Nothing is cancelled out from under DBOS. A run whose workflow ended without its
  reply was superseded under another version, and failed under the current one or with its
  job gone.
- **A run makes its moves as ADR 0034 says**, each recorded under the name its plan gives it
  with a digest of what it does. Its spend is recorded under the run's conversation, so its
  schedule's principal's; the daily cap refuses an ask, never a start; a call whose tool asks
  a person first is never sent. A run resumed under another version after its moves ends in
  error at its reply, and is superseded: a kept move is not reused across versions.

Consequences:

- Reminders, standups and index upkeep are one mechanism; a new one is a job and a schedule,
  not a sweep.
- Recurrences are as expressive as the closed set; a rule it lacks is a change to core.
- Who besides its author may cancel a person's schedule, and whether a person may write a
  recurring one, is the writing tool's to decide.
- Enforced by the replay gate (`JobReplayTests`, `run-superseded.json` among its histories).
