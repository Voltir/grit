---
name: grit-local-db
description: Reading, resetting or running grit against its local Postgres databases, and changing schema.sql — scripts/sql, scripts/reset-db, which database an agent uses and which are Nick's.
---

# grit's local databases

The compose Postgres (`docker compose up -d`) holds them.

| Database | Whose | May an agent… |
|---|---|---|
| `grit_agent` | agents' runs, probes, `scripts/tui-drive`, `scripts/tui-gate` | read, reset, run grit on it |
| `grit` | Nick's local sessions | read with `--nick-db` only |
| `grit_slack` | Nick's Slack deployment | read with `--nick-db` only |
| `grit_eval_<yyyymmdd>` | an eval corpus, restored by `scripts/eval capture` from a dump of another database | create, read, drop; never run grit on it |
| `grit_eval_shadow` | a disposable copy of a corpus that a shadow run's engine runs on | create, read, reset, drop, run grit on it |
| `grit_eval_synthetic` | the eval's hand-written cases, written by `scripts/eval reference-build`, which drops and recreates it | build, read, drop; never run grit on it |

- **`scripts/eval capture --from <db>`** only reads `<db>`: `pg_dump` is an MVCC read, safe
  beside the engine running on it.

- **Run grit on `grit_agent`**:
  `GRIT_DATABASE_URL=jdbc:postgresql://localhost:5432/grit_agent`; the gate creates it
  when missing. An engine started on a database someone else is using recovers their
  in-flight turns and races their session.
- **Read** with `scripts/sql <db> "<query>"` (read-only, `-At`; `-- <psql flags>` for more,
  e.g. `-- -x`). It refuses `grit` and `grit_slack` without `--nick-db`.
- **Reset** with `scripts/reset-db grit_agent`: drops the `grit` and `dbos` schemas, which
  grit recreates on its next start. It refuses while grit is running. The local databases
  are disposable until the first real deployment: reset rather than write code that copes
  with old rows. Reset `grit` only when Nick isn't running grit (`reset-db` checks); never
  `grit_slack`.
- **Schema changes don't reach an existing database.** `schema.sql` is `CREATE … IF NOT
  EXISTS`: a changed CHECK, column or table is never applied to a database that already has
  it (functions are `CREATE OR REPLACE` and are; a stored generated column keeps its old
  values). Reset `grit_agent`. For Nick's databases, diff `schema.sql` and give him the
  `ALTER` statements in your report; don't run them.
