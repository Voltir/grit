# 0033. An edge hears where its bot is a member, and a room's label is set through grit by one rule in core

Status: accepted (2026-10-08); revised (2026-10-21)

Context: a deployment named the channels grit listened in, the channels `slack_post` could
post to, and the label of every room in code. Each new channel waited on a deploy, and a
labeller could not tell a public channel from a private one. The people who know what a room
holds are its members, and they invite grit there. Turned down: taking Slack's workspace
administrators as grit's (it couples core to one edge); compartments per channel (a channel's
audience is not a lasting area of work); relabelling existing conversations when a room is
raised (rows derived from them already exist at the old label); carrying a room's access in
each conversation's origin (it would change a conversation's identity); reading a place's label
lazily per call (every label check would become fallible); answering a command in the channel
(it needs grit's bot to be a member).

Decision:

- **An edge hears where its bot is a member**, public or private, and reports each room's
  access (`RoomAccess.Open`: anyone in the workspace may join; `Invited`: only whom its
  members invite). It records each join and leave in order, checked against its source's list
  whenever it opens. Being removed stops hearing at once. Channels grit's bot is not in, and
  group direct messages, are never served.
- **A room's label is set through grit**, by command, in the room it labels, whether or not
  grit is a member there; a place within one a service shares (ADR 0039) is labelled from
  any room, by `label … at <place>`, and any other place named so is refused. Core decides
  each change by one rule (`Authority.decide`, yielding `Authority.Allowed`) and keeps each
  change allowed with an audit row (`grit.visibility_changes`):
  - in an invited room, raising is free to any full member a trusted realm vouches;
  - a room not invited is relabelled only by an administrator: raising it would make it a
    destination higher rooms write down to (ADR 0031);
  - compartments and levels follow their stewards' and administrators' rules, and clearing a
    person for a compartment adds them to its declared group;
  - grit, and an account no trusted realm vouches a full member, change nothing.
- **The administering tools** (`Administering`: `label_room`, `quiet_room`) act on the turn's
  own room as its asker, re-decided by `Authority.decide` at approval. The model proposes, the
  person approves, and the audit row names the turn (NULL for a slash command's). They are
  offered only on a turn a person put to grit (`Addressed`, `ByName`) in a colleagues' room:
  never in a direct message, a job's run or the TUI, where every call would be refused after
  its ask; and not while a served edge shows no ask (ADR 0017).
- **Administrators and stewards are declared members** of groups the deployment names in code;
  people added through grit are never either.
- **A room's label in force** is, in order: a direct message's, its person's clearance; the
  label set through grit; the label declared at the room's own place; for a reported access,
  the deployment's `open` label or `unmapped` for an invited room (a room strictly within a
  place the deployment declares invited is one until an edge reports it); else the label at the
  longest declared place enclosing it, else `otherwise`. An unlabelled invited room fails high.
- **What grit records is read as each transaction opens** (`Tx.roomLabel`, `Tx.writable`,
  `Tx.quiet`), so every check in it sees one state. A conversation keeps its creation label
  (ADR 0030); labelling a room before inviting grit is the safe order.
- **When grit joins a room it hears what was said before**, bounded by days, by message count
  and by a day's joins; never answered. Skipped over the day's cap or for an unvouched inviter.

Consequences:

- Every message in a member room is recorded, and no notice is posted: inviting grit is the
  members' act. A window's names are the edge's display names, forgeable by anyone who can
  write there.
- **Posting.** grit posts outside its answer's thread only through `slack_post`, hosted at
  `service:slack` (ADR 0017): to a member channel not quiet, by id, within the deployment's
  rate, on turns addressed to grit, never asking first and never run again after a crash;
  narrowed per turn by ADR 0031. Its text is literal, so it never mentions or broadcasts, and
  it is never heard or answered. A top-level post starts its thread once a message there is recorded.
- A room can be made quiet by any full member: grit then posts nothing there unasked, neither
  unprompted speech (ADR 0022) nor `slack_post`, and still answers a mention.
- A room left more than a day ago is forgotten unless a person labelled it or made it quiet.
- The audit rows are kept; a retention window for them is a later decision.
- Enforced by the transaction's reads above, `AuthorityTests`, and `PostingTests`.
