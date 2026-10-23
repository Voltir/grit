# 0030. Visibility is a Bell–LaPadula label lattice, and a turn reads for its room and its asker

Status: accepted (2026-10-06); revised (2026-10-16, 2026-10-18)

Context: grit first served several people with no visibility model: one database was one
trust boundary, so everyone who could write to the conversations a database held could see
everything it held. Every room's closings, documents and plugin tools could reach every other
room's window. That boundary is superseded here; visibility narrows it and never widens it.
Turned down:

- Visibility by place, a document kept at a workspace's root reaching every channel under it:
  the place tree says what a thing is about, not who may see it (ADR 0013 ranks by place and
  decides nothing: locality is not visibility), and a sharing set fits no place.
- Reader sets: they go stale as membership does.
- A label stored as a set of atoms, so a deployment could insert a level: such a set has more
  values than there are labels, and its join folds a malformed one into a valid one.

Decision:

- **A label is a level and a set of compartments** (`Label`). One dominates another when its
  level is at least the other's and it holds every compartment of the other; join and meet
  follow, and public with no compartment is the bottom. The levels are grit's and fixed:
  public < internal < confidential < restricted. The compartments are the deployment's,
  declared and closed; grit adds `unmapped`. The level names are never a compartment's.
  Anything that names a compartment (a plugin, an edge, a labeller, a schedule) declares it
  as a requirement, and a deployment that does not declare it is refused.
- **Code outside core is parametric in labels.** A plugin compares, joins and meets labels and
  keys what it derives by them, but never takes one apart: representation independence, not
  secrecy. It may learn what its transaction reads to choose which variant to serve, never to
  decide what may be read: the transaction filters, so a wrong choice cannot read up.
- **The simple security property and the *-property.** Every database transaction is opened
  for a subject (`Subject`) and carries that subject's clearance: it reads only rows its
  clearance dominates, and what it labels is kept at least at the subject's current level
  (`Clearance.floor`). Only `grit.dbos` opens a transaction as the trusted subject
  (`Maintenance`), for work that moves rows and derives nothing.
- **An edge acting for a room with no conversation** (`Subject.Room`: a review's channel)
  reads everywhere up to the label its place is written at outside grit (`Tx.writable`), and
  public rows alone where nothing writes. It is no trusted subject: it reads no more than the
  place may receive, and a post rechecks in its own transaction that the place still dominates
  what it posts, since a place's label may fall in between.
- **A room has one label in force**, set by one rule in core (ADR 0033). What is said in a
  room takes its label, so a reply is never above its room; a conversation keeps the label it
  was created at (strong tranquility).
- **A turn reads for room ∧ asker.** A room's members read everything grit derived in it, up to
  the room's label, since the room admitted them: a discretionary rule within the mandatory
  one. Everything else (other rooms, documents) a turn reads up to the meet of the room's label
  and the asker's clearance. A room is one by identity, never by containment. A person's
  clearance is the join of their groups' grants.
- **What is derived takes at least the join of its sources' labels.** Work that derives runs
  under a declared clearance and keeps its output there.
- **What a mapping cannot place fails high**: `unmapped`, joined to whatever part was mapped,
  readable only by clearances granted it.
- **Core owns the semantics**: labels, clearances, the filter, and the clearance every
  transaction carries. Edges and plugins map what they bring in onto labels, and own group
  memberships. The deployment owns its compartments.
- **People decide rarely and in bulk.** A deterministic core bounds the worst case; automated
  judges decide grey zones (an outbound call, a lower-labelled summary); people set standing
  policies, add compartments and audit samples.

Consequences:

- A document or a closing can be drawn on in any room whose clearance dominates its label.
- Derived data is kept per label: a plugin keeps a variant per label it derives at, and serves
  the one its reader may read.
- Under the default, with no compartment declared and every room public, nothing a reader
  reads changes.
- A judge's error leaks through one outbound call or one summary, never through a window.
- A start that drops a declared compartment is refused, since it could make a stored label
  readable to a clearance that could not read it.
- Enforced by the compiler (no transaction opens without a subject), the law
  (`only-dbos-names-maintenance`), and `LabelContract`, which binds dominance, join and meet
  in Scala to the same in SQL. Row-level security is the later enforcement in the database.
