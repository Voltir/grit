# 0030. Visibility is a label lattice: readers read down, writers write up, and a room admits its members to its own speech

Status: accepted (2026-10-06)

Context: ADR 0013 made place rank candidates rather than decide who sees them, and ADR 0019
made one database one trust boundary. Every room's closings, documents and plugin tools could
therefore reach every other room's window. Deciding visibility by place, a document kept at a
workspace's root reaching every channel under it, was proposed and turned down: the place tree
says what a thing is about, not who may see it, and a sharing set (a document shared with three
people) fits no place. Three lattices were weighed. Labels stored as a level and categories carry
a second field that a new level renumbers. Reader sets go stale as membership changes. A set of
declared atoms embeds the first exactly and leaves the second to the clearance computation.

Decision:

- A label is a level and a set of compartments. One label dominates another when its level is
  at least the other's and it holds every compartment of the other. Join and meet follow, and
  public (no compartment) is the bottom. The levels are grit's and fixed: public < internal <
  confidential < restricted. A deployment cannot add, replace or rename one. The compartments
  are the deployment's, closed: it declares each, and grit adds `unmapped`. The four level names
  are reserved and are never a compartment's. A new level is named to avoid every declared
  compartment, which the migration that adds it checks. Anything that names a compartment (a
  plugin, an edge, a labeller, a schedule) declares it as a requirement, and a deployment that
  does not declare it is refused.
- Code outside core is parametric in labels. A plugin compares, joins and meets labels, and
  keys what it derives by them, but never takes one apart. This buys representation
  independence, not secrecy. It may learn what its transaction reads in order to choose which
  variant to serve, never to decide what may be read: the transaction filters, so a wrong
  choice cannot read up. A deployment injects the rest as pure values and functions, which core
  verifies: its compartments, the labeller of its rooms, its groups and grants, and later its
  membership sources and authorities.
- Bell–LaPadula: no read up, no write down. Every database transaction is opened for a subject
  and carries the clearance that subject has: it reads only rows its clearance dominates, and
  what it labels is kept at least at the clearance's floor.
- A room has one label, given by the mapping its deployment declares. What is said in a room
  takes its label (over-classifying is the safe direction), so a reply is never above its room.
- A turn reads for room ∧ asker. A room's members read everything grit derived in it, whether
  or not the edge showed it to them, up to the room's label, since the room admitted them.
  Everything else (other rooms, documents) a turn reads up to the meet of the room's label and
  the asker's clearance. A room is one by identity, never by containment in another. A
  person's clearance is the join of their groups' grants.
- What is derived takes at least the join of its sources' labels. Work that derives runs under a
  declared clearance and keeps its output there. Only work that moves rows and derives nothing
  reads at maintenance.
- What a mapping cannot place fails high: the atom `unmapped`, joined to whatever part was
  mapped, readable only by clearances granted it.
- Core owns the semantics: labels, clearances, the filter, and the clearance every transaction
  carries. Edges and plugins own the mappings of what they bring in onto labels, and group
  memberships. The deployment owns its compartments.
- People decide rarely and in bulk; per-item decisions go to automated judgement. There are
  three layers. A deterministic core, never bypassed and never prompting, bounds the worst
  case. Automated judges decide grey zones, such as an outbound call or a lower-labelled
  summary. People set standing policies, add compartments, audit samples, and are asked within
  a hard budget per authority.

Consequences:

- A document or a closing can be drawn on in any room whose clearance dominates its label, so
  sharing across rooms no longer waits on deciding visibility by place.
- Every read of labelled rows must filter by its transaction's clearance. The compiler enforces
  that no transaction opens without a subject, the law confines the maintenance clearance to the
  module that opens connections, and contract tests bind dominance in Scala and in SQL. Row-level
  security is the later enforcement in the database.
- Derived data is kept per label: a plugin keeps a variant for each label it derives at, and
  serves the one its reader may read.
- Under the default, with no compartment declared and every room public, every label is
  public, and nothing a reader reads changes.
- A judge's error leaks through one outbound call or one summary, never through a window.
- Dropping or renaming a compartment could make a stored label readable to a clearance that
  could not read it. It is refused at start until the change is acknowledged as a
  declassification. A change to grit's levels is grit's own, with its own migration.
