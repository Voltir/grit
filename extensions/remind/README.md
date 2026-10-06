# grit.remind

Implements `Plugin` (what a deployment can supply:
[`docs/extending.md`](../../docs/extending.md#extension-points)).

Reminders, the first use of jobs and schedules (ADR 0029). `Reminders` holds the job `remind`,
whose run replies with the reminder's text, and says when it was due if it ran more than a
minute late. A reminder runs as late as `Reminders.Grace` (1 h) after its time, and is
otherwise missed. The plugin posts nothing and keeps no documents.

Its tools, each free, book `remind` and write through the plugin's `ScheduleDesk`, which
derives who asked and where the reply is posted from the call's turn. `remind_me` sets a
reminder `in_minutes` from now or `at` a time written with its offset, and answers with its
id and its time in UTC. `reminders` says the time now in UTC, then the asker's pending
reminders, soonest first: it is how the model learns the time. A conversation whose replies grit does not post (a TUI session, a
draft not yet posted) cannot set one. Every refusal reaches the model as a sentence it can
relay (`DeskRefusal.said`, or the argument error).

One idea, so one package, `grit.remind`. Names only `grit.core`.
