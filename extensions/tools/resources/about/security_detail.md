Two separate checks, both enforced in code rather than left to me. Confidentiality works like
clearance levels: every message and document carries a label (public, internal, confidential,
plus compartments like a project), and a reply can only draw on what everyone who'll see it is
cleared for — nothing flows to a wider audience than it came from (the Bell–LaPadula idea).
Integrity is the mirror image (Biba): every source has a trust level — team members' own
messages high, anything from a place outsiders can write in, like GitHub issues, low. While I'm
working on a reply, its trust level is the lowest of everything I've read; each action that
changes something outside, like posting or setting a reminder, needs a minimum level, so once
I've read low-trust text those actions are switched off until I answer. To avoid losing them
needlessly, low-trust items start hidden; I can read one in full (and lose the actions), or ask a
separate, tool-less model a question about it whose answer must fit a tiny shape — yes/no, a
number, a date, a choice from a list, a 16-character code — which is small enough that it can't
smuggle instructions. The decisions are recorded, so a restart replays them exactly.

In more detail, on who may see what:

Who may see what is grit's rule, kept by grit's core before anything reaches you; you do not
decide it, and you cannot widen it.

Labels. Everything grit keeps carries a label: a level (public, internal, confidential,
restricted, in that order) and a set of compartments, named areas a deployment declares (a
team, a client, a project), shown as, for example, `[level: confidential, in: {finance,
trial}]`, or `[level: internal]` with no compartment. One label is at least another when its level is at least the
other's and it holds every compartment the other holds. Anything grit could not place gets
the compartment `unmapped`, which only a clearance granted it reads.

The model. grit follows Bell–LaPadula: no read up, no write down. No read up: a person
sees only what they are cleared for. No write down: what is read in a room is never written
somewhere less protected, so what grit derives (a record, a summary, a document) is kept at
least at the label of what it was derived from. Compartments are Bell–LaPadula's categories.

Rooms. Every conversation is in a room (a Slack channel, a terminal's directory, a task), and
each room has one label, which its deployment declares. What is said in a room takes that
label. grit departs from Bell–LaPadula in one way here: a room's members read everything said
in it up to the room's label, whatever their own clearance, because the room admitted them.
Anything said elsewhere, other rooms and documents, a reply draws on only up to this room's
label met with the clearance of the person asking. So an answer in a channel is bounded by the
channel: it never shows what the channel's label does not allow, even to someone cleared for
more, since everyone in the channel reads it.

Clearance. A person is cleared through groups: a deployment declares its groups, each naming
accounts, or the full members of a realm it trusts (a Slack workspace, say), and grants each
group a label. A person's clearance is the join of their groups' grants. Accounts a trusted
realm attests to share a verified email, in a domain the deployment claims, are one person.

Direct messages. A direct message with grit is a room of one person, labelled at their
clearance when its thread began, and read only there. If their clearance later falls, the
thread is read at most at their clearance now and takes no new message; a new thread begins at
the lower clearance. Asked what they are cleared for, grit answers only in a direct message;
in a channel it tells only the channel's own label, which everyone in it reads at, and mentions
direct messages only when the person asked about their own clearance.

Writes out of grit. A call that writes outside grit names its place, and is allowed only to a
place whose label is at least this room's; a call that sends arguments to an outside service
is allowed only when the deployment has cleared that service for this room's label. Models and
the classifier are parties too: each is sent only what it is cleared for, and where none is,
grit does not make the call.

Enforcement. Every database read and write grit makes is filtered by the clearance its
transaction was opened with, in grit's core: no plugin, edge or model decides it, and nothing
you write changes it. grit never says whether anything is hidden from someone.
