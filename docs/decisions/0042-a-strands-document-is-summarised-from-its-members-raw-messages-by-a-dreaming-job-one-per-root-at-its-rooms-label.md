# 0042. A strand's document is summarised from its members' raw messages by a dreaming job, one per root at its room's label

Status: proposed (2026-10-18)

Context: a room's topic is scattered over the threads ADR 0023 stitches into a strand, and a
turn's window ranks those threads one by one. A document per strand, a short summary with
search terms, lets a window and the relevance judge (ADR 0040) see the topic as one thing, and
lets the stitch's judge name what a strand's opening never says. Its source had to be chosen.
Turned down: summarising the members' closings, which keeps ADR 0027 as it was (a closing
exists only after its period lapses, a heard one is prose alone, and a summary of summaries
loses what the messages said); the engine writing the documents itself (it owns no documents,
and a plugin is where derived knowledge lives); a step returning the members' text for a
plain ask (the text would sit in a step's record, and come back believed at the run's own
integrity, laundering a guest's words up to grit's); every keeping job reading strands (a
job that never needs raw messages would hold the read).

Decision:

- **A dreaming job is a third kind of plugin job** (`DreamingJob`, handed `Dreaming`). Its
  keeps may also read the strands of the rooms its transaction reads, by ids, counts and a
  content digest of their messages (`StrandShelf`, `Strand.Seen`): nothing a model would read
  leaves a scan.
- **Core summarises a strand for it.** `Dreaming.summarise` is an ask whose step reads the
  members' messages itself (`StrandExcerpts`), shows them as a reader's excerpt within the
  summary's characters, and records only the JSON ask's own form under a digest of the
  strand's ids. Its reply is believed at the meet of every message read, so a guest's message
  caps the document. When the messages are not the ones the scan digested, it asks nothing.
- **One document per strand root, at its room's label.** A run is kept at one label (its
  schedule's clearance), writes only for rooms whose label in force is its floor, and
  withdraws a document whose room has moved to another; a deployment declares one schedule per
  label. A room at a label no schedule declares has none.
- **Members are read as raw messages until they age out**; a strand whose members' entries are
  purged loses its document. Nothing re-links strands: membership stays the stitch's.

Consequences:

- A plugin's code never holds a message's text; ADR 0027's rule holds for it, and ADR 0034's
  moves gain a read inside a keep and an ask core frames.
- A document's integrity is never above its least-believed message's, and its label never
  below its room's: the keeper joins with the floor, and the run reads no room above it.
- A strand that goes quiet past the stitch's horizon keeps its document until its members age
  out, and is found by search, not by the stitch.
- Enforced by the types (only `Dreaming` offers `scan` and `summarise`; `Strand.Seen` has no
  text) and capture probes (`ActCaptureTests`), and by `MovesContract` against the moves and
  their fake.
