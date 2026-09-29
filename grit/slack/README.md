# grit.slack

The Slack edge (ADR 0019): a Slack thread is a conversation, brought into the engine and
answered through Postgres alone (ADR 0002). The Slack SDK's quarantine (STYLE rule 8):
`com.slack.*` is named in `client/SocketSlack.scala` and nowhere else in grit, and the law's
`only-slack-imports-*` rules hold it there. Depends on `grit.core` and `grit.prose`, never
the turn (`slack-names-core-and-prose`).

In dependency order:

- **`event`** — what Slack says, as grit reads it: the opaque ids (`TeamId`, `ChannelId`,
  `UserId`, `Ts`), and `Event`, a person's message `Said` (at the time its ts names) or
  `Ignored` with why, read from an Events API payload by `Events.read`, or from a `Listed`
  message of a channel's history by `Events.listed`, under the same rules. Imports nothing in slack.
- **`text`** — Slack's text, both ways: `Incoming`, a person's message as grit stores it
  (mentions as names, markup written out, escapes undone), and `RichText`, grit's reply (a
  `grit.prose` doc) as Slack messages (`Post`s) of rich-text blocks, within Slack's limits.
  ← `event`
- **`client`** — Slack as grit uses it: the `Slack` trait (listen, a channel's history, post, find a post by its
  `Tag`, react, a person's name, whether a channel is public, and its name), `SlackError`, the tokens, and
  `SocketSlack`, the SDK over Socket Mode behind it. ← `event`, `text`
- **`edge`** — `SlackEdge`, the edge itself, over core's traits (`EdgeStores`: the inbox,
  the people it enrolls, the replies it awaits) and a `Slack`: a person's message in a public
  channel becomes a turn of its thread's conversation when it is addressed to grit (it
  mentions grit, or is in a thread whose root did), and each finished turn's reply is posted
  in its thread, once, found again by its tag after a crash. In the channels it listens in
  (`listening`, a deployment's `GRIT_SLACK_LISTEN`), a message not addressed to grit is heard:
  an entry of its thread's conversation with no turn, never answered, dated when it was said.
  `unheard` and `backfill` are `grit backfill`'s: a listened channel's history since an
  instant (`Slack.history`, read by the rules a live message is, `Events.listed`), less what
  the inbox has recorded, then each of those heard at its own time, a past mention of grit
  included, since a past message is never answered. At start it names the workspace's assistant as Slack names grit's
  bot (`introduce`), and each turn's prompt then says what the assistant is called there.
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
