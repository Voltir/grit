# grit.remind

Implements `Plugin` (what a deployment can supply:
[`docs/extending.md`](../../docs/extending.md#extension-points)).

Reminders, the first use of jobs and schedules (ADR 0029). `Reminders` holds the job `remind`,
whose run replies with the reminder's text, and says when it was due if it ran more than a
minute late. A reminder runs as late as `Reminders.Grace` (1 h) after its time, and is
otherwise missed. The plugin posts nothing and keeps no documents.

One idea, so one package, `grit.remind`. Names only `grit.core`.
