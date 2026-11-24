# 0063. An artifact's id is a number its project mints, its name a handle, its order a rank among its siblings, and what it may sit under its method's

Status: accepted (2026-11-18); revised (2026-11-19, 2026-11-24)

Context: an artifact's key (ADR 0049) was a slug its first writer chose, `<project>/<local>`:
identity, name and, through its words, a guess at order. A milestone kept the number it was
planned under after it moved, its title's prefix carried the order, re-ordering retitled and keys
went stale. Nothing said what may sit under what.

Turned down:

- **Slugs as identity**, kept for new artifacts too: the problem above.
- **The handle as the store key**: the stale slug would be permanent, an artifact made without
  one could never take one, and a handle taken again would join two ids in one history.
  **Aliases for retired handles**: a second lookup, unbounded by renames.
- **Moving each document to a key of its id**: the store is append-only, so history, claims,
  briefs and waits would restart, or core would need a rename.
- **`GRIT-412`**, **numbers across projects**, **a method naming artifacts by id**: a second
  key syntax, and numbers that are not one project's.
- **Minting by a version-conditioned write**: `DocumentKeeper` has none.
- **Order as a list on the parent**: every re-order writes one shared document. **Float
  order**: runs out of precision.

Decision:

- **An id is `<project>/<n>`**, minted by the creating transaction under the project's lock, from
  the counter `next` in the project's index, never reused or changed. Every rewrite of the index (a
  create, the re-key, a migration) keeps who started the project and is believed at no more than the
  index was and its writer is, a version per create (ADR 0028). A create from a room above the
  index's label is refused, as a write of anything kept below the room is: a higher copy of the
  index would mint the same numbers again.
- **The store key is the key an artifact was first kept under**: its id for a new artifact, its old
  key for a re-keyed one. It is never shown and moves nothing when the handle does. The id and the
  handle are head fields; a document is an artifact's by its head, never by its key's form, and a
  claim is matched to its artifact by the loaded document's key. A withdrawn re-keyed artifact is
  found by id through `<project>#rekeyed-<k>`, which the re-key writes once: 256 numbers to a
  document, each kept at the label of what it maps.
- **A handle is an optional head field**: a slug led by a letter, held by at most one current
  artifact of its project. A keep sets, changes or removes it; a change away from a handle the
  pinned method's `carried` names is refused. A reference with a leading digit is an id; any other
  resolves as a current handle, then as a current artifact's store key, so an old key led by a
  letter keeps resolving; one led by a digit is reached by its `k-` handle. Stored heads and bodies
  cite ids alone; method data names artifacts by handle.
- **Order is a `Rank` among siblings**, changed by one write to the moved artifact; ties list by
  number. A new artifact ranks after its last sibling; where there is no room, the re-order
  re-spreads that parent's children.
- **Containment is method data** (`ArtifactKind.nesting`), judged where an artifact is placed: by
  the keep that makes it or gives it a parent, and by a migration (`Method.nested`). Reading a
  stored or exported project judges none, so one already out of place stays. The default, anywhere,
  digests as before.
- **A claim may cover what sits under its artifact**, as its role's view says (`RoleView.covers`; a
  role with no view covers nothing). A covering claim is refused when another holder's is at, above
  or under it, and a keep is refused a parent that would make two overlap, both judged under the
  project's lock. A cover refuses only another covering role: a role that covers nothing claims
  under it, by key or by `oldest`, so a manager's claim on a milestone leaves its checkpoints to
  its coders.
- **A large artifact is split** (`project_split`): each `##` section becomes a `section` under it,
  at its label. `section` is the one kind name grit's code names; a method without it has no split.
- **The export holds the id, handle and rank**, one file per artifact at its parents' numbers then
  its own (ADR 0049).

**The re-key** is one command, one transaction, believed at `Authority`, run as a step of the deploy
that ships it, with no inverse: a deploy that has sent it rolls back by its dump. It numbers what it
reads by first version's time, ties by key; each old key becomes its handle (`k-<key>` for one led
by a digit) and stays its store key; ranks follow the old order (siblings titled `M<n>` first, by
`n`); cites are rewritten to ids, one version each at its own label. A document with no id is in the
old form, judged per document; a cite it cannot read refuses the run, and a later run, cleared
higher, numbers what is left from `next`. Until it runs, a project whose index has no `next` refuses
every command but the re-key and the export, naming the re-key. The old-form reader goes once every
deployment is re-keyed.

Consequences: ADR 0049 is revised in place. Every method version keeps its file and digest:
`carried` reads as handles, and an example's slug-keyed artifact reads as that handle, numbered
by its place in `start`.

Enforced by: the compiler (each name's constructor refuses what its doc rules out), the
round-trip law over the export, `ProjectCommandsTests`, `NestingTests`, `CoveringTests` and
`SplitTests`.
