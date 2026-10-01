# 0019. A Slack thread is a conversation, and one database is one trust boundary

Status: accepted (2026-09-27), revised (2026-09-28), amended (2026-10-01): posting beyond
a thread

Context: grit's first edge for several people is Slack. Two choices shape every later
edge that serves more than one person.

What a Slack conversation is:
- A channel is a stream of unrelated questions.
- A thread is one exchange, and Slack already shows it as one.
- A conversation per person would split an exchange two people share.

Who may see what: grit has no visibility model yet. Labels, scopes and RLS are parked
(`mechanisms/lifecycle.md`). Meanwhile every window may draw on the whole database:
- other conversations' open periods, through `[afar]`, scope everywhere by default;
- retrieval;
- Digest's `recent_activity`.

Two alternatives were turned down:
- serving Slack from the database a developer's terminal uses, which would answer
  colleagues from that developer's private sessions;
- building visibility first, which delays any use by several people.

Decision:

- **A Slack thread is a conversation.** Its origin is `Origin.Slack(team, channel, thread)`,
  and its place is `slack:{team}/{channel}/{thread}`. A top-level mention starts a thread
  under itself.
- **One database is one trust boundary.** Everyone who can write to the conversations a
  database holds may see everything it holds. `grit serve` runs on a database of its own,
  serving one workspace of trusted colleagues. Only public channels are served: a private
  channel's thread would reach public ones through `[afar]` and `recent_activity`.
  Private channels, and more than one team on one database, wait for visibility.

Consequences:

- Nothing inside the boundary is hidden from anyone in it. A person's name in a window
  is Slack's display name, shown from `grit.inbound`'s author, so anyone can write text
  that looks like another person's name line. That forgery stays inside the boundary.
- A colleague's word grounds a Standing line as a person's (ADR 0018), the same as the
  asker's.
- Visibility, when it comes, narrows this boundary. It never has to widen it.
- A deployment names the channels grit listens in (ADR 0020). Every message in them is
  recorded inside the boundary, as a mention was. No notice is posted in the channel:
  which channels are listened in is the deployment's configuration. A private channel is
  never served, addressed or heard.
- grit posts outside the thread it answers only through `slack_post`, a tool the Slack
  edge serves at `service:slack` (ADR 0017's reach): to the channels the deployment
  declares, by id, within a rate it declares, on turns addressed to grit, never asking first
  and never run again after a crash. Anyone who can address the deployment may ask for a
  post, bounded by where it may post and by the rate. A post's text is literal, so it never
  mentions or broadcasts. A post is grit's bot's message, so it is never heard or answered,
  wherever it lands. It is not recorded in the conversation of the channel it lands in;
  the turn that made it keeps what it posted where.
- Enforced by:
  - `SlackEdge` ignoring any channel Slack does not report as public, and hearing only in
    the channels it is told to listen in (`SlackEdgeTests`);
  - `grit serve` refusing to attach to an engine another grit holds;
  - `slack_post` refusing a channel not declared, a link into another channel and a post
    past the rate (`PostingTests`), and a turn rooted on a heard message never being
    offered it (`TurnOfferTests`).
