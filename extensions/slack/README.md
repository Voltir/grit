# grit.slack

Implements `ServedEdge` and `CatchUp`, each its workspace's attester over a `RealmSource` of
its own, and serves `slack_post` as a `Hosted` (what a
deployment can supply: [`docs/extending.md`](../../docs/extending.md#extension-points)).

The Slack edge (ADR 0023): a Slack thread, or a thread of a person's direct message with grit,
is a conversation, brought into the engine and
answered through Postgres alone (ADR 0002). The Slack SDK's quarantine (STYLE rule 8):
`com.slack.*` is named in `client/SocketSlack.scala` and nowhere else in grit, and the law's
`only-slack-imports-*` rules hold it there. Depends on `grit.core`, `grit.prose` and
`grit.edge` (whose `Server` serves `slack_post`), never the turn
(`slack-names-core-prose-and-edge`).

Its types are the code's (`scripts/outline area edge`); this file holds the package order, what
the edge does that its signatures cannot say, and what the Slack app needs.

In dependency order:

- **`event`**: what Slack says, as grit reads it, live or listed from a channel's history under
  the same rules. A message's author is a user of their own team (its `user_team`, else its
  `team`, else the installing workspace), so a user of a workspace sharing a channel is never
  spelled as one of grit's. A group direct message is ignored. Imports nothing in slack.
- **`text`**: Slack's text, both ways: a person's message as grit stores it (mentions and links
  as names, markup written out), and a reply's prose as rich-text posts within Slack's limits.
  ← `event`
- **`client`**: the `Slack` trait, as grit uses Slack, and `SocketSlack`, the SDK over Socket
  Mode behind it. A user's standing is read by one function for `users.info` and `users.list`
  alike: a full member of the team asked about (not a guest, a member of another organisation,
  a bot or an app, invited or deactivated), with the address Slack verified when the app may
  read emails; outside otherwise, and when Slack knows no such user. ← `event`, `text`
- **`edge`**: `SlackEdge`, the edge itself, over core's traits and a `Slack`.
  - **Membership** (ADR 0033): it hears every channel its bot is a member of, changed only by
    what `grit.core.edge.Joins` returns. At open it reconciles with Slack's list; joins and
    leaves are recorded as they happen, ordered by when. A message from a channel not recorded
    a member asks Slack once: the join is recorded first when the bot is in it, the message
    ignored when not, and left for Slack to send again when Slack cannot be asked. The review's
    channel is never a member, and nothing said there is heard.
  - **Backfill**: what was said before a join is heard within a `Backfill`, less what the inbox
    has recorded, each message at its own time and never answered, since a past message keeps
    no reply address. A join whose inviter is no full member, made while the day's spend is
    over the cap, or after a day's joins were backfilled, is marked done unheard, and why is
    logged. Served, backfills run one join at a time on a thread of their own; closing waits
    for one under way at most `Backfill.StopWithin`, and one cut short resumes at the next open.
  - **Turns**: a message in a member channel is a turn of its thread's conversation when it
    mentions grit or is in a thread whose root did; otherwise it is heard, an entry with no
    turn. Each reply is posted in its thread once, found again by its tag after a crash. A
    message is marked `:eyes:` while grit works on it, until its reply is posted or its turn
    ends with nothing to post. A thread under a `slack_post` post begins with that post, read
    from Slack and recorded as the conversation's opening, so its windows show the turn that
    asked for it.
  - **`slack_post`**: served at `service:slack` within one rate across all channels, a writing
    tool (ADR 0031): each channel is offered under its name, with and without `#`, at its own
    place, so a turn is offered only the channels its room may write to, and a quiet room is
    written to by no one. The edge advertises its channels again after a join, a leave or a
    room made quiet or not, and the rate's count carries across.
  - **Review**: a review's place must be a channel of the bot's own team, or the edge does not
    open. A prompt names the message's channel and permalink alone, never its text, a draft's,
    why it was picked or what either gate decided, so the rater answers cold. A crash between
    posting a prompt and keeping it posts it again. Only the rater's reactions, and only the
    three verdict emoji, are kept. A deployment adds other reviews after triage's
    (`Prompting`, by `SlackReview.also`), over a plugin's documents put up for review: each is
    read for the review's place (`Subject.Room`), its prompt links a conversation it names by
    its thread when the thread is this workspace's, and it is posted and kept only while the
    place is written at a label dominating the document's, rechecked as it is posted.
  - **Attesting** (ADR 0032): the edge is its workspace's attester. An author is attested
    through core's `Attesting.before` unless Slack answered for them within a minute; one Slack
    cannot answer for and never has is not recorded, so Slack sends the message again, and a
    backfill stops there. The edge holds no answer, clock or domain of its own: when to ask,
    and which addresses count, are core's. How a deployment trusts the edge:
    [`docs/extending.md`](../../docs/extending.md) (Identities).
  - **Slash command**: acknowledged at once and run on a thread of its own, through core's
    `Administration`, as its asker's, in its room (its channel's, member or not, or from any
    direct message the asker's own direct room), answered to them alone. Any other command is
    ignored. The edge hands core words and accounts; it never sees a transaction or a label's
    parts.

  ← `client`, `text`, `event`

**Direct messages.** A person's message in their direct message with grit's bot is recorded
as a mention is, as a turn of its thread's conversation in their
direct room (`Origin.Direct`), which is labelled at their clearance and read only there (ADR
0032); a message in a thread begun when they were cleared for more is answered once with
`InboxError.SealedReply` and not recorded. Group direct messages are not heard, and direct
messages are never backfilled. App Home's Messages tab must be on, with "Allow users to send
messages", or no one can write to the bot.

**The Slack app.** Socket Mode, with an app-level token (`SLACK_APP_TOKEN`, scope
`connections:write`) and a bot token (`SLACK_BOT_TOKEN`) with these scopes, each for what the
edge calls or is sent:

- `app_mentions:read`, `channels:history`, `groups:history`, `im:history`: the messages it is
  sent (events `app_mention`, `message.channels`, `message.groups`, `message.im`), and a
  thread's replies and a channel's history read back (a thread's root, its own post found
  again by its tag, a backfill);
- `channels:read`, `groups:read`, `im:read`: what a conversation is, and its name, and which
  channels the bot is in (`users.conversations`), and its joins and leaves (events
  `member_joined_channel`, `member_left_channel`, `channel_left`, `group_left`);
- `chat:write`: its replies, refusals, `slack_post`'s posts and a review's prompts;
- `reactions:read`, `reactions:write`: a rater's reactions (events `reaction_added`,
  `reaction_removed`), and the `:eyes:` mark and a prompt's reactions it adds and removes;
- `users:read`, `users:read.email`: who an author is, and whether a full member, for the names
  windows show and for attesting (event `user_change`); without `users:read.email` no account is
  linked to another by email;
- `commands`: its slash command, registered under the name the deployment gives it (Socket
  Mode needs no request URL), with "Escape channels, users, and links sent to your app" on, so
  a person it names arrives as a mention; its usage hint may list what its `help` answers
  (`Command.Usage`).

A scope added takes effect only once the app is reinstalled to the workspace.

**The daily cap.** `grit serve` takes new messages until the day's recorded spend reaches
`GRIT_DAILY_USD` ($1.00 when unset); a message after that is not recorded, and its thread is
told, once, that grit can't take new messages right now (`Budget.Refusal`, which names no
cost). It is checked only as a message arrives, so a day can end above the cap by what the
turns already running spend (each up to `GRIT_TOOL_ROUNDS` model calls of about
`GRIT_WINDOW_TOKENS` in and `GRIT_MAX_TOKENS` out) and a closing summary per period they end.
A call its provider does not price counts as nothing: under such a provider the cap is never
reached, and `grit serve` says so at start. Days begin at this machine's midnight; OpenRouter
counts its own daily figure in UTC. A join's backfill is checked once, as it starts: one
started under the cap hears up to its `Backfill`'s messages, each triaged by one classifier
call, which the recorded spend does not count, as for any message heard, and each thread it
begins closed once, whose closing summary it does count.

No source file sits at the module's root, and the test tree mirrors it.
