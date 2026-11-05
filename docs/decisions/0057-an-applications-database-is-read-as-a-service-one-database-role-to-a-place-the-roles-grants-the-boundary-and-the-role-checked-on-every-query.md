# 0057. An application's database is read as a service, one database role to a place: the role's grants are the boundary, and the role is checked on every query

Status: accepted (2026-11-04)

Context: a deployment's people ask questions an application's own database answers ("the
latest leads, and why two got no offers"). Answering them needs general SQL, not a few fixed
queries, and the database holds personal data beside what the answer needs. Turned down:

- **An off-the-shelf database MCP server over the MCP edge.** No second JDBC quarantine, but
  the read-only guard, the caps and the refusal wording would be the server's, untested by
  grit. Kept for stores SQL does not reach.
- **Fixed, named queries.** Quick, but every new question waits for code.
- **Guarding by the SQL text.** A `READ ONLY` transaction alone was bypassed in a reference
  MCP server by `COMMIT;` and a write in one string; parsing SQL for safety is never complete.
- **One service choosing its role by the turn's clearance.** The edge would learn a label per
  request and decide visibility itself, which belongs to core (ADR 0030).

Decision:

- **A database is a service, and one database role is one place.** A deployment declares a
  database at a `Service` with its URL, its login and the role it is to be. The service
  place's label is what that role can read; two levels of one database are two roles and two
  services. The deployment declares, for each, the place's label, a `Cleared` for the service
  at that label (a tool that writes nothing is offered only where the turn may send to its
  service), a `Cap`, and a `Reaches`; nothing else in grit's visibility changes.
- **The role's grants are the boundary.** Postgres enforces schema usage, table and column
  `SELECT`, row-level security, and views read with their owner's rights. A role made for
  this is `NOINHERIT`, a member of no role, granted `SELECT` on exactly the columns its label
  admits and nothing else, so that a leaked credential reads only what the place already
  shows. grit's own guards are a second line: each query runs in its own read-only
  transaction, opened as SQL text so no driver setting turns it off, as one statement, with a
  timeout, its rows and bytes capped and the clip said.
- **The role is checked on every query.** Before the model's statement, in the same
  transaction, the edge reads `current_user` and `session_user` and refuses unless both are
  the declared role; also at start and on every reconnect. This catches a driver that fell
  back to ambient credentials (a cloud connector using the operator's own login) and a
  statement that changes the role.
- **Two tools per database**, named for its service: one lists the tables, views and columns
  the role may read, with their types, comments and foreign keys; one runs a statement and
  answers a text table. A failure reaches the model as a sentence with the server's primary
  message, which general SQL needs to correct itself (`column "x" does not exist`); its code,
  detail and hint (where row values are quoted), and the driver's exception, go to the
  operator's log only.
- **The application documents itself.** What a column means is a `COMMENT` kept beside it in
  the application's migrations; a recurring derivation is a view the application keeps. The
  deployment's instructions for the place point at them, and restate neither. An error a
  function or view raises is read at the place's label, so it never quotes an ungranted
  value.
- **The login holds no secret where the platform allows one.** A login is a password, read
  as a secret (ADR 0054), or the driver's own properties, plain ones apart from secret ones.
  A URL that carries a credential is refused at start.
- **The cap follows who wrote what the role reads** (ADR 0036): columns only the
  application writes, `member`; text a supplier wrote, such as an error reason, `outside`;
  text the public typed, `untrusted`. An untrusted result still lets an addressed turn reply
  in its thread, but no note is kept from it and its level carries forward, so a role that
  reads public text is the last resort, not the default.

Consequences:

- Personal data is excluded by grants, not by instructions, and a new personal column is
  excluded until someone grants it. The application can test that no column it marks as
  personal is granted to the role.
- **The catalog is not compartmented.** Any role reads the system catalogs, so table and
  column names and comments are visible at the lowest role's label. No confidential fact goes
  in a name or a comment; a database whose structure is itself confidential is its own
  database.
- Every query is in the turn's journal: who asked, in which room, the SQL, the role, the rows.
  No table is added; a result is bounded by the byte cap.
- A second SQL engine is one more implementation of the queries that differ by engine.
- JDBC now has two roles (ADR 0001): grit's own store in `grit.dbos`, an application's
  database in `grit.databases`, its `connect` package alone naming `java.sql` and `Reveal`.
- Enforced by the law (the JDBC quarantine rules, `only-quarantines-reveal`, and
  `databases-names-core-prose-and-edge-alone`), and by the module's integration tests, which
  attack the role check, the single statement and the read-only transaction.
