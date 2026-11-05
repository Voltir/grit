# grit.databases

Will implement `ServedEdge` (its `edge` package), serving an application's Postgres database as
tools at a service place (what a deployment can supply:
[`docs/extending.md`](../../docs/extending.md#extension-points)).

An application's database read as a service, one database role to a place
([ADR 0057](../../docs/decisions/0057-an-applications-database-is-read-as-a-service-one-database-role-to-a-place-the-roles-grants-the-boundary-and-the-role-checked-on-every-query.md)).
A shipped extension (ADR 0021): it names core and `grit.edge` alone
(`databases-names-core-and-edge-alone`), and JDBC's second role beside `grit.dbos` (STYLE rule
8), in its `connect` package alone. The JDBC driver is the deployment's dependency, not this
module's: a deployment without one is refused at open.

In dependency order:

- **`declare`**: what a deployment declares, pure: a `Database` at a service, its URL's
  variable, its `Login` (a password, or the driver's own properties, plain apart from secret),
  the `DatabaseRole` it must be, and its `Limits`. Imports nothing in databases.
- **`connect`**: the JDBC quarantine, the one package naming `java.sql` and `Reveal`. Imports
  `declare`.

No source file sits at the module's root. The test tree mirrors it.

## What the code cannot say

- **The role's grants are the boundary.** Postgres enforces schema usage, column `SELECT`,
  row-level security and views; grit's read-only transaction, single statement, timeout and
  caps are a second line. A role made for this is `NOINHERIT`, a member of no role, and granted
  `SELECT` on exactly the columns its place's label admits.
- **The catalog is not compartmented.** Any role reads the system catalogs, so every table's and
  column's name and comment is visible to the lowest role: no confidential fact goes in one.
- **Nothing a query takes outlives it.** Each query's transaction is rolled back, which undoes
  every setting a statement makes; the session's advisory locks and `LISTEN`s, which a rollback
  leaves, are then released, or the connection is closed. An application may hold a worker's
  lease as an advisory lock, and grit's connection must never keep one.
- **The driver may find a password grit did not pass.** pgjdbc reads `~/.pgpass` (or
  `PGPASSFILE`) when no password is given. The role check catches a login that lands as
  another role, but not a password read from there for the declared role itself.
