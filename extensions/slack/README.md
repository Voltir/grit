# grit.slack

Implements `ServedEdge` and `CatchUp`, each its workspace's attester over a `RealmSource` of
its own, and serves `slack_post` as a `Hosted` (what a
deployment can supply: [`docs/extending.md`](../../docs/extending.md#extension-points)).

The Slack edge (ADR 0019): a Slack thread, or a thread of a person's direct message with grit,
is a conversation, brought into the engine and
answered through Postgres alone (ADR 0002). The Slack SDK's quarantine (STYLE rule 8):
`com.slack.*` is named in `client/SocketSlack.scala` and nowhere else in grit, and the law's
`only-slack-imports-*` rules hold it there. Depends on `grit.core`, `grit.prose` and
`grit.edge` (whose `Server` serves `slack_post`), never the turn
(`slack-names-core-prose-and-edge`).

In dependency order:

- **`event`** — what Slack says, as grit reads it: the opaque ids (`TeamId`, `ChannelId`,
  `UserId`, `Ts`), and `Event`, a person's message `Said` (at the time its ts names, by a user of their own team: its `user_team`, else its `team`, else the installing workspace, so a user of a workspace sharing a channel is never spelled as one of grit's), a
  person's direct message to grit `Told` (a group direct message is `Ignored`), a
  reaction to a message added or removed (`Reacted`), grit's bot joining a channel, with who added it (`Joined`), or leaving or removed from one (`Left`), each at when it happened, or `Ignored` with why, read from an Events API payload by `Events.read`, or from a `Listed`
  message of a channel's history by `Events.listed`, under the same rules; `Commanded`, a slash
  command read from its payload by `Events.command`, its answers going to its `ResponseUrl`; and
  `MessageLink`, the channel and thread a message's link names. Imports nothing in slack.
- **`text`** — Slack's text, both ways: `Incoming`, a person's message as grit stores it
  (mentions and channel links as names, markup written out, escapes undone), and `RichText`, grit's reply (a
  `grit.prose` doc) as Slack messages (`Post`s) of rich-text blocks, within Slack's limits.
  ← `event`
- **`client`** — Slack as grit uses it: the `Slack` trait (listen, a channel's history, post in a thread or at a channel's top level, find a post by its
  `Tag`, the message a thread begins with, a message's permalink, react, a user as a `Member` of a team, one or every one, whether a conversation is a channel grit's bot is in, every channel it is in, and a conversation's name, and answer a slash command to its asker alone at its response url), `SlackError`, the tokens, and
  `SocketSlack`, the SDK over Socket Mode behind it; and `Root`, the message a thread begins
  with, its author and its tag. A `Member` is the name a user shows and their standing as
  Slack states it, read from Slack's user by one function for `users.info` and `users.list`
  alike: a full member of the team asked about (not a guest, a member of another
  organisation, a bot or an app, invited or deactivated), with the address Slack verified
  when its user confirmed it and the app may read emails (`users:read.email`), whatever its
  domain; outside otherwise, and when Slack knows no such user. ← `event`, `text`
- **`edge`** — `SlackEdge`, the edge itself, over core's traits (`EdgeStores`: the inbox,
  the joins it records its bot's memberships through, the accounts it names, the replies it
  awaits) and a `Slack`. It hears every channel its bot is a member of (ADR 0033): which, it
  holds in `Members` (`private[slack]`), changed only by what `grit.core.edge.Joins` returns.
  At open it reads Slack's list (`Slack.channels`) and records each listed channel a member,
  each recorded member no longer listed gone, and forgets rooms left over a day ago
  (`reconcile`); `Event.Joined` and `Event.Left` are recorded as they happen, ordered by when
  they happened, a join with the access Slack reports and its inviter's account, checked first,
  so a join whose inviter is no full member skips its backfill, which is logged. A message from
  a channel not recorded a member (one racing its join) asks Slack once whether the bot is in it
  (`Slack.kind`): the join is recorded first when it is, the message ignored when not, and left
  for Slack to send again when Slack cannot be asked. The review's channel is never recorded a
  member, and nothing said there is heard. What was said before a join is heard
  (`backfillJoins`), within a `Backfill` (the days before the join, the newest messages of
  them, and the joins a day), less what the inbox has recorded, each message at its own time
  and never answered; then the join is marked done (`Joins.backfilled`). A join made while
  the day's spend is over the inbox's cap (`Inbox.overCap`), or after a day's joins were
  backfilled (`Joins.backfilledSince`), is marked done unheard, and why is said. Served, the
  backfills run on a thread of their own, one join at a time (`Backfilling`, `private[slack]`):
  those pending at open (a crash's or a close's included) as it opens, each join after from
  the next delivery; closing waits for one under way at most `Backfill.StopWithin`, and one
  cut short is heard from where it stopped at the next open. Each message heard is triaged
  once and its thread closed once; none is answered, since a past message keeps no reply
  address. A person's message in a member channel becomes a turn of its thread's conversation when it is addressed to grit (it
  mentions grit, or is in a thread whose root did), and each finished turn's reply is posted
  in its thread, once, found again by its tag after a crash. A message is marked `:eyes:`
  while grit works on it: one addressed to grit from when it is recorded, and one heard that
  triage answers as put to grit by name from that turn's acknowledgement
  (`grit.core.edge.Acknowledgements`, wanted by triage beside its awaited reply;
  `acknowledge`, run first in each delivery), each until its reply is posted or its turn ends
  with nothing to post. A message not addressed to grit is heard:
  an entry of its thread's conversation with no turn, never answered, dated when it was said.
  A thread under a post grit made with `slack_post` begins with that post: when the first
  message recorded or heard there arrives, the edge reads the root from Slack (`Slack.root`)
  and records it as the conversation's opening, made by the call its tag names, so the
  thread's windows show the turn that asked for it.
  `unheard` and `backfill` are `grit backfill`'s: a member channel's history since an
  instant (`Slack.history`, read by the rules a live message is, `Events.listed`), less what
  the inbox has recorded, then each of those heard at its own time, a past mention of grit
  included, since a past message is never answered. At start it logs the name grit's bot goes by in Slack
  (`displayName`); the assistant's name is the deployment's persona (ADR 0026), not Slack's.
  `SlackEdge.serving(command, backfill)` is the module's entry: the edge as a deployment
  serves it (`grit.core.edge.ServedEdge`, ADR 0021), its tokens read from `SLACK_BOT_TOKEN`
  and `SLACK_APP_TOKEN` as it opens, answering the slash command the deployment registered
  (`SlackCommand`, below), and bounding each join's backfill by `backfill`;
  `SlackEdge.serving(command, backfill, posting, review)`, given a rate as `posting`, also
  serves `slack_post` at `service:slack` (`SlackEdge.PostsAt`), a turn's post in any channel
  the bot is a member of that is not quiet, within that rate across them all (`Posting`), for
  the conversations a deployment links there (`grit.core.place.Reaches`). Its channels follow
  membership: the edge advertises them again at the delivery after a join, a leave, or a
  room made quiet or not through its slash command (`SlackEdge.reoffer`), and the rate's count
  carries across. It is a writing tool (`grit.core.tool.Writing`, ADR 0031): each channel is
  offered under its name, with and without `#`, at the place `slack:{team}/{id}`, so a turn is
  offered only the channels its room may write to (a quiet room is written to by no one), and
  the edge posts in the channel its request was checked to write to. Given `review`, it also
  answers a deployment's review (`SlackReview.of(place, rater)`: a place
  `slack:{team}/{channel id}`, refused otherwise, never heard, and in the bot's own
  team, or the edge does not open; `ServedEdge.reviewsAt`): each delivery posts the prompts
  the kit picked that its place may receive (`grit.core.review.Reviews.unposted`) whose
  messages were heard in
  this workspace (`ReviewPrompt`: the message's channel and permalink alone, never its text,
  a draft's, why it was picked or what either gate decided, so the rater answers cold), adds the three reactions a verdict is given with, then keeps the prompt posted; a
  crash between the post and keeping it posts the prompt again. A reaction the rater adds to a
  prompt, or removes, is kept or withdrawn as its verdict (`grit.core.review.Reviews`);
  anyone else's, and any other emoji, is ignored.
  `SlackEdge.backfill(days)` is `grit backfill`'s
  (`CatchUp`), what each channel its bot is a member of said over those days that grit has not
  recorded, its memberships first recorded as the served edge's open records them.
  The edge is its workspace's attester (`SlackAccounts.Attester`, ADR 0032), and both say
  so: before a message is recorded or heard, live or caught up, its author's account goes
  through core's `Attesting.before`, which asks the edge's source, a private
  `grit.core.edge.RealmSource` over `Slack.member` and `Slack.members`, unless Slack
  answered for them within a minute; an author Slack cannot answer for and never has is not
  recorded, so Slack sends the message again, and a backfill stops there. `user_change`
  (`Event.UserChanged`) asks again, and each look the kit asks for (`Open.attest`) lists the
  workspace once when any account is due. The edge holds no answer, clock or domain of its
  own: when to ask, and which addresses count, are core's. How a deployment trusts the edge:
  [`docs/extending.md`](../../docs/extending.md) (Identities); what the app needs for it:
  below.
  Its slash command (`SlackEdge.command`) changes and reads who may see what through core's
  `grit.core.admin.Administration`: Socket Mode acknowledges each command at once and hands
  it to the edge on a thread of its own, the edge reads its words with `Command.read` (a
  mention naming that user's account), checks its asker first as it checks an author, runs it
  as theirs in its room (its channel's, member or not, or, from any direct message, the
  asker's own direct room), and answers them alone at its response url (`Slack.respond`).
  Any other command is ignored. The edge hands core words and accounts and shows
  `Answer.text`; it never sees a transaction or a label's parts.
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
  a person it names arrives as a mention.

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
