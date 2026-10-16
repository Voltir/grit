# 0033. An edge hears where its bot is a member, and a room's label is set through grit by one rule in core

Status: accepted (2026-10-08); revised (2026-10-16)

Context: a deployment named the channels grit listened in, the channels `slack_post` could
post to, and the label of every room in code. Each new channel waited on a deploy, and a
labeller could not tell a public channel from a private one. The people who know what a room
holds are its members, and they invite grit there. Turned down: taking Slack's workspace
administrators as grit's (it couples core to one edge's flag); compartments per channel (a
channel's audience is not a lasting area of work); relabelling existing conversations when a
room is raised (rows derived from them already exist at the old label); carrying a room's
access in each conversation's origin (it would change a conversation's identity); reading a
place's label lazily per call (every label check would become fallible); answering a command
in the channel (that needs grit's bot to be a member, and a room is labelled best before grit
is invited).

Decision:

- **An edge hears where its bot is a member**, public or private, and reports each room's
  access (`RoomAccess.Open`, anyone in the workspace may join; `RoomAccess.Invited`, only whom its members
  invite). It records each join and leave in order, and checks them against its source's list
  whenever it opens. Being removed stops hearing at once. A channel grit's bot is not in, and a
  group direct message, are never served; a direct message is ADR 0032's.
- **A room's label is set through grit**, by command, in the room it labels, whether or not
  grit is a member there; a place within one a service shares (ADR 0039) is labelled from
  any room, by `label … at <place>`, and any other place named so is refused. Core decides each change by one rule (`Authority.decide`, the only
  way to the value applying it, `Authority.Allowed`) and keeps each change allowed with an
  audit row (`grit.visibility_changes`). The rule decides on what a change raises, lowers and
  removes:
  - in an invited room, raising is free to any person a trusted realm vouches a full member;
  - a room not invited is relabelled only by an administrator: raising it would make it a
    destination higher rooms write down to (ADR 0031);
  - removing a compartment needs its steward or an administrator; lowering a level, an
    administrator; replacing `unmapped`, an administrator or a steward of every compartment
    the new label holds;
  - clearing a person for a compartment, or removing them, needs its steward or an
    administrator, and adds them to the compartment's own declared group;
  - grit, and an account no trusted realm vouches a full member, change nothing.
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
- **When grit joins a room it hears what was said before**, bounded by days, by a number of the
  newest messages and by joins backfilled a day, each message at its own time and never
  answered; skipped over the day's cap or when the inviter is no vouched full member.

Consequences:

- Every message in a member room is recorded, and no notice is posted: inviting grit is the
  members' act. A person's name in a window is the edge's display name, so text that looks like
  another's name line can be forged by anyone who can write there.
- **Posting.** grit posts outside the thread it answers only through `slack_post`, hosted at
  `service:slack` (ADR 0017): to any member channel not quiet, by id, within a rate the
  deployment declares, on turns addressed to grit (never a heard one), never asking first and
  never run again after a crash; narrowed per turn by ADR 0031. Its text is literal, so it never
  mentions or broadcasts; it is grit's bot's message, so it is never heard or answered. A
  top-level post begins the thread under it once someone's message there is recorded.
- A room can be made quiet by any full member: grit then posts nothing there unasked, neither
  unprompted speech (ADR 0022) nor `slack_post`, and still answers a mention.
- A room left more than a day ago is forgotten unless a person labelled it or made it quiet.
- The audit rows are kept; a retention window for them is a later decision.
- Enforced by the transaction's reads above, `AuthorityTests`, and `PostingTests`.
