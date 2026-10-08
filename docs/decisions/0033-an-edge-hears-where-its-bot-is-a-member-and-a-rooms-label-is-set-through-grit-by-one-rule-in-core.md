# 0033. An edge hears where its bot is a member, and a room's label is set through grit by one rule in core

Status: accepted (2026-10-08)

Context: a deployment named the channels grit listened in, the channels `slack_post` could
post to, and the label of every room in code (ADR 0019, ADR 0030). Each new channel waited on
a deploy, and a labeller could not tell a public channel from a private one (ADR 0032), so a
private channel was either served at a label someone guessed in advance or not served. The
people who know what a room holds are its members, and they invite grit there. The
alternatives turned down:

- Taking Slack's workspace administrators as grit's: it couples core to one edge's flag.
- Compartments per channel: a channel's audience is not a lasting area of work. An implicit
  group per compartment at restricted: it would invent the level.
- Relabelling existing conversations when a room is raised: rows derived from them already
  exist at the old label.
- Carrying a room's access in each conversation's origin: it would change a conversation's
  identity. Passing it with each message heard: every edge would pay for one edge's fact.
- Reading a place's label lazily, per call, on the transaction's connection: every check that
  reads a label would become fallible. A cache keyed by a version row, or notifications across
  processes: still a read per transaction, plus invalidation. What grit records is read once,
  as each transaction opens.
- Answering a command inside the edge's acknowledgement deadline: attesting the asker and a
  transaction cannot be held to it. Answering in the channel as an ephemeral message: that
  needs grit's bot to be a member, and a room is labelled best before grit is invited.

Decision:

- An edge hears where its bot is a member, and reports each room's access: open (anyone in
  its workspace may join, a public channel) or invited (only whom its members invite, a
  private one). It records each join and leave in the order they happened, and checks what it
  holds against its source's list whenever it opens. Being removed stops hearing at once.
- A room's label is set through grit, by command, in the room it labels, whether or not grit
  is a member there. Core reads the command's words, decides it by one rule, and keeps each
  change allowed with an audit row naming who made it, when, and what it replaced. The rule
  decides on what a change raises, lowers and removes, never on which command asked:
  - In an invited room, raising (the new label dominates the one in force) is free to any
    person a trusted realm vouches a full member (ADR 0032).
  - A room that is not invited (open, or with no access reported) is relabelled only by an
    administrator: raising it would make it a destination higher rooms write down to
    (ADR 0031).
  - Removing a compartment needs its steward or an administrator; lowering a level needs an
    administrator. Replacing `unmapped` (an invited room's first label) needs an
    administrator, or a steward of every compartment the new label holds.
  - Clearing a person for a compartment, or removing them from it, needs its steward or an
    administrator. Clearing adds them to the compartment's own declared group, so that group
    must be declared with a grant holding the compartment.
  - Grit, and an account no trusted realm vouches a full member, change nothing. A direct
    message's room is never relabelled.
- Administrators are the declared members of the group the deployment names in code;
  a compartment's stewards are the declared members of the group it names for it. People
  added through grit are never either, so no one made a member by command can make another.
- A room's label in force is, in order: a direct message's, its person's clearance (ADR 0032);
  the label set through grit; the label the deployment declares at the room's own place; for a
  reported access, the deployment's `open` label for an open room and `unmapped` for an
  invited one; otherwise the label declared at the longest declared place enclosing it, else
  `otherwise`. An invited room left unlabelled therefore fails high: only a clearance granted
  `unmapped` reads it, and nothing writes to it from outside.
- What grit records (labels set, access reported, quiet, people cleared) is read as each
  transaction opens, so every check in it sees one state, and a change takes effect from the
  next transaction. A conversation keeps the label it was created at (ADR 0030); labelling a
  room before inviting grit is the safe order.
- A person may read their own clearance in full by command, wherever they ask, since the
  answer is shown to them alone; another person's only an administrator may read.
- When grit joins a room it hears what was said there before, bounded by days, by a number of
  the newest messages, and by joins backfilled a day, each message at its own time and never
  answered. It is skipped when the day's spend is over the cap, and when the person who
  invited grit is no full member a trusted realm vouches; a join with no inviter named (found
  while grit was down) is backfilled.

Consequences:

- A deployment sets up in tiers: naming its administrators alone (public rooms public,
  invited rooms unmapped until labelled); a realm group granted `internal` with an `open`
  label of `internal`; then compartments, each with its own group, a grant and optionally a
  steward. None of them names a channel.
- Releasing or sanitising one item is a later kind of change under the same rule: a steward
  removes their compartment, an administrator lowers a level.
- The audit rows are kept; a retention window for them is a later decision.
- A room can be made quiet, by any full member: grit then posts nothing there unasked,
  neither unprompted speech (ADR 0022) nor `slack_post`, and still answers a mention.
- A room left more than a day ago is forgotten unless a person labelled it or made it quiet,
  so a decision about a room survives its re-invitation, and the rooms kept are bounded by
  current memberships, the day's leaves, and the rooms people decided about.
- A room relabelled up keeps its existing threads at their old label (ADR 0031's
  consequence); relabelled down, they stay high.
- `slack_post` may post in every member channel that is not quiet, still narrowed per turn by
  ADR 0031's rule; the deployment declares only its rate.
- Enforced by the transaction (`Tx.roomLabel`, `Tx.writable`, `Tx.quiet`, which the static
  declaration no longer computes alone), by `Authority.decide` being the only way to the value
  applying a change takes (`Authority.Allowed`), by `Visibility.of` refusing a compartment's own
  group, which people cleared through grit join, as administrators or as another
  compartment's stewards, by the stored administration counting declared members alone, and
  by tests of the rule, the stored administration and the edge's memberships.
