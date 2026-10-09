# 0029. A job is versioned code, a schedule is data, and a clock edge starts each due slot

Status: accepted (2026-10-05); amended (2026-10-06): a run is a turn of its slot's own
conversation; asked schedules are once-only and report at the asking turn's destination; a
run's period closes mechanically; a run ended without its reply is superseded or failed by its
version; amended (2026-10-08): a run's moves

Context: ADR 0021 decides that a job is a turn with a Scala planner and a version, and that a
trigger is an edge starting one run per slot at a task place; it does not say where slots
come from. Some work is declared and recurring (keeping an index fresh, a nightly pass, a
daily standup per person); some is asked for in conversation (a reminder for Thursday). The
close alternative was a sweep per feature, as the close, settle and posting each have: every
recurring feature would add its own. A job as data and a scheduler in core were turned down
by ADR 0021.

Decision:

- **A schedule is a row**: its job, by name; a slot rule, either once at an instant or a
  recurrence from a small closed set (daily, on weekdays, weekly, at a time in a time zone),
  never a cron expression; parameters, as JSON the job reads; the principal it runs for,
  whose budget it spends (ADR 0021); and where its runs report: kept in their own
  conversation, or posted at a destination that names its edge as well as its address
  (amended 2026-10-06).
- **Schedules come from two sources.** A deployment or plugin declares some; they run for the
  deployment's persona and are reconciled with the rows when the engine starts, a schedule no
  longer declared being cancelled. A tool writes others from a turn; they run for the person
  who asked, are recorded with that turn, are theirs to cancel, and reconciliation never
  touches them. (Amended 2026-10-06.) A schedule written from a turn is once-only, and
  reports at the asking turn's destination, where that turn's reply was posted; a turn whose
  reply nobody posts cannot write one.
- **A slot is a schedule and its nominal instant**, and its key is the run's id, so the inbox
  refuses a second start. The run is a conversation at the job's task place, closed like any
  other, so what it did is memory (ADR 0021). (Amended 2026-10-06.) Each run is a turn of its
  slot's own conversation, and its workflow is that turn's, so deliveries, retention and the
  close treat it as any turn. Its period is never asked whether anyone waits, and closes with
  no model or classifier call (ADR 0012), its closing never shown in another conversation's
  window.
- **The clock edge** reads due slots from Postgres and starts their runs through the inbox,
  as any edge starts a turn (ADR 0002); it hosts no place (ADR 0017). After downtime, a once
  slot runs late within a grace its schedule declares and is otherwise recorded as missed; a
  recurrence runs its latest missed slot alone. (Amended 2026-10-06.) A run whose workflow
  ended without its reply was superseded if it ran under another version than its job's
  current one, and failed if under the current one or if its job is gone. Each pass makes
  progress on both due slots and runs in flight.
- **A job is contributed by a plugin (ADR 0027) or declared by the deployment**, and runs
  under grit's epoch (ADR 0021).
- (Amended 2026-10-08.) A run makes its moves as ADR 0034 says. Its spend is recorded under the
  run's conversation, so its schedule's principal's (grit's, for a declared schedule); the
  daily cap refuses an ask, never a start; an ask sends what its job gives it to the catalog's
  summary model. A run resumed under another version after its moves ends in error at its
  reply, and is superseded.

Consequences:

- Reminders, standups and index upkeep are one mechanism; a new one is a job and a schedule,
  not a sweep.
- Recurrences are as expressive as the closed set; a rule it lacks is a change to core.
- Who besides its author may cancel a person's schedule, and whether a person may write a
  recurring one, is the writing tool's to decide.
