# grit.dbos

The DBOS quarantine (ADR 0001): the only module that names DBOS, JDBC or the Postgres
driver, translated into core's traits here. `resources/schema.sql` is the schema. The types
and their docs are the code's (`scripts/scout area storage`, `scripts/scout show <Type>`);
this file holds the package order and the rules the code's signatures cannot state.

- **`sql`**: Postgres behind core's store traits, one `Sql*` class per trait.
  - Each transaction's clearance is resolved as it opens (`Opener`, ADR 0030), under the labels
    in force and what is recorded beside them, read whole as it opens; `Opener.asker` is the one
    asker rule.
  - `SqlClearance` is the one filter every read of labelled rows puts in its `WHERE`, inside any
    ranked or limited subquery; what is said in a direct message is read only in its own room.
  - `SqlLabels` is the one translation of labels through `LabelParts`.
  - A conversation is created at its direct message's person's clearance, else its room's label
    in force.
  - A person's id is minted by Postgres under a lock on the account (`SqlIdentities`); a claimed
    email's under a lock on the address taken before the account's row (`SqlVoucher`), so two
    vouchings never wait on each other in opposite orders.
  - Joins and leaves lock each room's row while they are ordered (`SqlJoins`); each admin
    command runs in one transaction, each change with one audit row (`SqlAdministration`).
  - Each period seal is numbered in commit order (`SqlPeriodStore`).
  - An engine, a link and a reader each run their short transactions, and their DBOS client's
    reads, on one `Pool` of their own. A connection that keeps session state past its
    transaction opens its own instead: the engine lock's, a desk's `LISTEN`, a rehearsal's.

  Imports nothing else in dbos.
- **`workflow`**: DBOS behind `Durable`. `DurableWorkflow` registers every body under the fixed
  class name `grit.workflow`, so moving one strands no workflow row. The close, settle and
  triage workflows and a job's run share the turns' queue under the conversation's partition,
  so none runs beside one of its turns; posting is partitioned by plugin, shadows run one at a
  time on a queue of their own, never the turns', and stitch placements one at a time per room
  in the order queued (ADR 0023). Imports nothing else in dbos.
- **`engine`**: both, composed. `Engine` is what `grit.app` starts under the database's one
  engine lock (ADR 0015), refused when the lock is held or its deployment drops a compartment
  the database ran under; closing it, or losing the lock, stops the sweep, then DBOS, waits for
  running bodies, and releases the lock last. Each engine start is recorded with the build it
  ran. `SqlInbox` records a message and enqueues its turn in one transaction, opening the
  conversation's next period when none is open. The sweep:
  - enqueues each open period's close attempt under an id naming its deadline, so a moved
    deadline is a new attempt and no workflow is ever deleted to run again, and each quiet
    period's settle question under an id naming its quiet stretch;
  - enqueues each enabled plugin's posting from its cursor, up to `PostRef.Attempts` runs from
    one cursor, and each declared shadow's oldest unshadowed messages within its day's cap,
    passing over any whose shadow ended keeping nothing;
  - marks for deletion every plugin no longer enabled that left a cursor, documents or terms,
    and then `Collector` collects each tombstone whose window has passed (ADR 0014): its
    workflows, unless one is still queued or running, then its rows.

  ← `sql`, `workflow`
- **`internal`**: what `grit.dbos` lends to the eval harness alone (ADR 0030; rule 1h in
  `enola-intent.yaml`): `Reader`, a database read without an engine or its lock, on sessions
  Postgres keeps read-only. ← `sql`, `workflow`, `engine`

`sql` and `workflow` are siblings and never name each other. No source file sits at the
root, and `scripts/enola-law.sh` fails on a new import cycle.
