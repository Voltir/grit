# grit.assembly

The `ContextAssembler`s: what draws a window at a boundary and appends to it on each later turn
(ADR 0052).

In dependency order:

- **`estimate`**: `CharEstimate`, the fallback `TokenEstimator`. Imports nothing in assembly.
- **`linear`**: `LinearAssembler`, the closing entries that open the turn's period (as
  many as the settings in force say, paid for first), then the most recent whole turns of
  the period that fit. The baseline every other assembler is measured against.
  ← `estimate`
- **`retrieval`**: `RetrievalAssembler`, the same closing entry, the recent tail, plus
  what a query finds, which the turn has a model write from `QueryWriter`'s request: the period's earlier turns, and the
  turns of other conversations' open periods its turn may read and their kept closings
  (ADR 0013), the enabled plugins' documents it may read, current at the turn's
  root (ADR 0028), and their kept files' pieces likewise, at most eight of one file (ADR 0046),
  ranked by an `EntrySearch`, a `DocumentSearch` and a `PieceSearch` in one pool, its own
  weighted up and each document or piece by its plugin's weight; those from elsewhere shown as their
  own sections, one per conversation; and a thread that begins with grit's post, the turn that
  asked for it. ← `linear`

Neither makes a call: a `ContextAssembler` is pure, its window drawn in reads a turn records
apart (`prepare`, `search`, `judgeText`, `assemble`), the query written and what it found judged
between them (ADR 0040).
Each draws its window as wide as it was built to, or within the budget (and, for retrieval,
the hits per search) a request's `Width` names. Neither reaches past its own period (ADR 0011): what came before the turn's is its closing
entry. Only retrieval draws on other conversations.

## What a retrieval window shows

Each candidate is costed as it is shown: a recalled own turn with a gap line (`Shown.Gap`), as is
a tail that leaves turns out; a section as `Shown.section`; a document as `Shown.document`; a piece
as `Shown.piece`; a person's message with its author's name line (`Shown.of`).

Own turns join the window in conversation order, never with the request's task
(`AssemblyRequest.task`), which is neither drawn nor found, and of a continuation's turns only
those `AssemblyRequest.drawable` keeps, in its tail, its gap line paid for first when that leaves
turns out, none of the others ever a candidate; packed documents and pieces in rank order. Turns
from elsewhere form one section per conversation (`Nearby`), in the order of its best candidate,
its turns in its own order. A conversation whose closings match is one candidate, shown as its
newest kept closing's record (`Shown.recorded`), unless any of its open turns is one: then those
alone are its section. The own closing is never a candidate.

A conversation in a strand (`grit.core.stitch.Strand`) is shown one `Nearby.Along` section per
other member, after any from afar: its root's opening message, then the messages nearest the turn,
newest first while they fit in `tuning.windowTokens`, paid after the closing and before the tail; a
member whose first message is gone, by its newest kept closing's record. A conversation that begins
with grit's post (`ConversationStore.postedBy`) is shown first, the turn that asked for it as one
`Nearby.Asked` section, while the turn's period is the conversation's first and both conversations
are colleagues' (`Audience.Colleagues`): its person's messages and grit's reply from the first, as
many as fit in the strand's allowance, paid from it before the strand. Neither a strand member nor
the asking conversation is ever also a candidate.

A channel's top level or a thread under it (ADR 0060) reads no strand: each other conversation of
its room is shown one `Nearby.Along` section, of what it said since the room's open period opened,
newest first within `Rolling.Default.shown`, and a thread first its room's newest kept closing and,
in its first period, the turn it is under (`Nearby.Asked`). An append adds what the room said since
its last part. A room's kept closings are each a candidate, but those the window shows; so are a top
level's earlier periods' turns, as its own.

## Layout

No source file sits at the module's root, and the test tree mirrors it. The assembly eval
lives in its own module, `grit.eval`.
