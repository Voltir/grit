# grit.slack

Implements `ServedEdge` and `CatchUp`, and serves `slack_post` as a `Hosted` (what a
deployment can supply: [`docs/extending.md`](../../docs/extending.md#extension-points)).

The Slack edge (ADR 0019): a Slack thread is a conversation, brought into the engine and
answered through Postgres alone (ADR 0002). The Slack SDK's quarantine (STYLE rule 8):
`com.slack.*` is named in `client/SocketSlack.scala` and nowhere else in grit, and the law's
`only-slack-imports-*` rules hold it there. Depends on `grit.core`, `grit.prose` and
`grit.edge` (whose `Server` serves `slack_post`), never the turn
(`slack-names-core-prose-and-edge`).

In dependency order:

- **`event`** — what Slack says, as grit reads it: the opaque ids (`TeamId`, `ChannelId`,
  `UserId`, `Ts`), and `Event`, a person's message `Said` (at the time its ts names, by a user of their own team: its `user_team`, else its `team`, else the installing workspace, so a user of a workspace sharing a channel is never spelled as one of grit's), a
  reaction to a message added or removed (`Reacted`), or `Ignored` with why, read from an Events API payload by `Events.read`, or from a `Listed`
  message of a channel's history by `Events.listed`, under the same rules; and `MessageLink`, the
  channel and thread a message's link names. Imports nothing in slack.
- **`text`** — Slack's text, both ways: `Incoming`, a person's message as grit stores it
  (mentions and channel links as names, markup written out, escapes undone), and `RichText`, grit's reply (a
  `grit.prose` doc) as Slack messages (`Post`s) of rich-text blocks, within Slack's limits.
  ← `event`
- **`client`** — Slack as grit uses it: the `Slack` trait (listen, a channel's history, post in a thread or at a channel's top level, find a post by its
  `Tag`, the message a thread begins with, a message's permalink, react, a user as a `Member` of a team, one or every one, whether a conversation is a channel, and its name), `SlackError`, the tokens, and
  `SocketSlack`, the SDK over Socket Mode behind it; and `Root`, the message a thread begins
  with, its author and its tag. A `Member` is the name a user shows and their standing as
  Slack states it, read from Slack's user by one function for `users.info` and `users.list`
  alike: a full member of the team asked about (not a guest, a member of another
  organisation, a bot or an app, invited or deactivated), with the address Slack verified
  when its user confirmed it and the app may read emails (`users:read.email`), whatever its
  domain; outside otherwise, and when Slack knows no such user. ← `event`, `text`
- **`edge`** — `SlackEdge`, the edge itself, over core's traits (`EdgeStores`: the inbox,
  the accounts it names, the replies it awaits) and a `Slack`: a person's message in a public
  channel, or in a private one it listens in, becomes a turn of its thread's conversation when it is addressed to grit (it
  mentions grit, or is in a thread whose root did), and each finished turn's reply is posted
  in its thread, once, found again by its tag after a crash. A message is marked `:eyes:`
  while grit works on it: one addressed to grit from when it is recorded, and one heard that
  triage answers as put to grit by name from that turn's acknowledgement
  (`grit.core.edge.Acknowledgements`, wanted by triage beside its awaited reply;
  `acknowledge`, run first in each delivery), each until its reply is posted or its turn ends
  with nothing to post. In the
  channels it listens in
  (`listening`, the channels a deployment declares), a message not addressed to grit is heard:
  an entry of its thread's conversation with no turn, never answered, dated when it was said.
  A thread under a post grit made with `slack_post` begins with that post: when the first
  message recorded or heard there arrives, the edge reads the root from Slack (`Slack.root`)
  and records it as the conversation's opening, made by the call its tag names, so the
  thread's windows show the turn that asked for it.
  `unheard` and `backfill` are `grit backfill`'s: a listened channel's history since an
  instant (`Slack.history`, read by the rules a live message is, `Events.listed`), less what
  the inbox has recorded, then each of those heard at its own time, a past mention of grit
  included, since a past message is never answered. At start it logs the name grit's bot goes by in Slack
  (`displayName`); the assistant's name is the deployment's persona (ADR 0026), not Slack's.
  `SlackEdge.serving(channels)` is the module's entry: the edge as a deployment serves it
  (`grit.core.edge.ServedEdge`, ADR 0021), its tokens read from `SLACK_BOT_TOKEN` and
  `SLACK_APP_TOKEN` as it opens; `SlackEdge.serving(channels, posts)` also serves
  `slack_post` at `service:slack` (`SlackEdge.PostsAt`), a turn's post in the channels
  `Posts` declares, within its rate (`Posting`), for the conversations a deployment links
  there (`grit.core.place.Reaches`). It is a writing tool (`grit.core.tool.Writing`, ADR
  0031): each channel is offered under its name, with and without `#`, at the place
  `slack:{team}/{id}`, so a turn is offered only the channels its room may write to, and the
  edge posts in the channel its request was checked to write to. `SlackEdge.serving(channels,
  posts, review)` also answers a deployment's review (`SlackReview.of(place, rater)`: a place
  `slack:{team}/{channel id}` grit does not listen in, refused otherwise, and in the bot's own
  team, or the edge does not open; `ServedEdge.reviewsAt`): each delivery posts the prompts
  the kit picked that its place may receive (`grit.core.review.Reviews.unposted`) whose
  messages were heard in
  this workspace (`ReviewPrompt`: the message's channel and permalink alone, never its text,
  a draft's, why it was picked or what either gate decided, so the rater answers cold), adds the three reactions a verdict is given with, then keeps the prompt posted; a
  crash between the post and keeping it posts the prompt again. A reaction the rater adds to a
  prompt, or removes, is kept or withdrawn as its verdict (`grit.core.review.Reviews`);
  anyone else's, and any other emoji, is ignored.
  `SlackEdge.backfill(channels, days)` is `grit backfill`'s
  (`CatchUp`), what each channel said over those days that grit has not recorded.
  The edge is its workspace's attester (`SlackAccounts.Attester`, ADR 0032), and both say
  so: before a message is recorded or heard, live or caught up, its author's account goes
  through core's `Attesting.before`, which asks the edge's source, a private
  `grit.core.edge.RealmSource` over `Slack.member` and `Slack.members`, unless Slack
  answered for them within a minute; an author Slack cannot answer for and never has is not
  recorded, so Slack sends the message again, and a backfill stops there. `user_change`
  (`Event.UserChanged`) asks again, and each look the kit asks for (`Open.attest`) lists the
  workspace once when any account is due. The edge holds no answer, clock or domain of its
  own: when to ask, and which addresses count, are core's. Which scopes and event the app
  needs, and how a deployment trusts the edge: [`docs/extending.md`](../../docs/extending.md).
  ← `client`, `text`, `event`

**The daily cap.** `grit serve` takes new messages until the day's recorded spend reaches
`GRIT_DAILY_USD` ($1.00 when unset); a message after that is not recorded, and its thread is
told, once, that grit can't take new messages right now (`Budget.Refusal`, which names no
cost). It is checked only as a message arrives, so a day can end above the cap by what the
turns already running spend (each up to `GRIT_TOOL_ROUNDS` model calls of about
`GRIT_WINDOW_TOKENS` in and `GRIT_MAX_TOKENS` out) and a closing summary per period they end.
A call its provider does not price counts as nothing: under such a provider the cap is never
reached, and `grit serve` says so at start. Days begin at this machine's midnight; OpenRouter
counts its own daily figure in UTC.

No source file sits at the module's root, and the test tree mirrors it.
