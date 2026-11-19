# 0065. A room's discussions are cut into strands by code each night; a topic is a subject or one meeting, fixed when it opens; a live turn writes no knowledge

Status: proposed (the text is a draft, for Nick)

Context: a Slack channel's top level and the threads under it (ADR 0060) are conversations no
stitch links, so no strand of them reached the topics run, and none of a room's talk became
knowledge. Stitching them would need a live write per message, made with no hindsight: in a
labelled room, five messages had their subject defined by a later one. A replay of two labelled
rooms measured the alternatives: 5-minute runs of top-level messages, each thread whole, placed
best; per-message units, and runs shown their neighbours as context, placed worse (Jev joined a
unit to the document holding one of its neighbours). The same replay found a confidence mark on
the sort's choice cost more than it saved, and that a meeting talked about over several messages
is one occurrence with a day, a start and people, not a subject.

Turned down: stitching the top level live; the scan reading entries itself, which ADR 0042 keeps
out of a job's code; and a mark under which a chosen topic is set aside.

Decision:

- **Units are cut by code each night** (`StrandShelf.units`): a legacy stitched strand; a thread
  under a channel's top level, whole, opening with the message it hangs under; or a run of a
  channel's top-level messages less than `Strand.Cut.gap` apart, of at most `most`, offered once
  it has settled. A run is cut from its true start whatever the window reads it, is told apart
  by its first message (`Strand.Id`), and has no ref: settled, it never grows.
- **One choice places a unit**, among its shortlist plus `new_topic`, `new_meeting` and `aside`,
  and stands whatever its probability. A unit that opens a topic is written at once, so a later
  unit that night may join it.
- **A topic is a subject or one meeting** (`Topic.Kind`), fixed when it opens. A meeting opens
  proposed and is refined by each write's reply (`Meeting.refined`), a model's guess checked by
  code: a day too far back or ahead dropped, planned with no day proposed, held kept held.
- **A live turn writes no knowledge.** Only the dream writes, and a written passage is never
  re-placed.

Consequences: room talk becomes topics a night later, with a night's hindsight. A run's cut and
the meeting's head are code a test pins; the sort's and the write's wording are Nick's. A run's
identity is its first message, so changing `Cut` changes which runs later nights see; a run
already in a lineage is never offered again. Changing the cut or the sort changes the job, so
`TopicDream.version` moves with it (ADR 0029). Enforced by `StrandShelfTests` and
`TopicDreamTests`.
