# grit.slack

The Slack edge (ADR 0019): a Slack thread is a conversation, brought into the engine and
answered through Postgres alone (ADR 0002). The Slack SDK's quarantine (STYLE rule 8):
`com.slack.*` is named in `client/SocketSlack.scala` and nowhere else in grit, and the law's
`only-slack-imports-*` rules hold it there. Depends on `grit.core` and `grit.prose`, never
the turn (`slack-names-core-and-prose`).

In dependency order:

- **`event`** — what Slack says, as grit reads it: the opaque ids (`TeamId`, `ChannelId`,
  `UserId`, `Ts`), and `Event`, a person's message `Said` or `Ignored` with why, read from an
  Events API payload by `Events.read`. Imports nothing in slack.
- **`text`** — Slack's text, both ways: `Incoming`, a person's message as grit stores it
  (mentions as names, markup written out, escapes undone), and `RichText`, grit's reply (a
  `grit.prose` doc) as Slack messages (`Post`s) of rich-text blocks, within Slack's limits.
  ← `event`
- **`client`** — Slack as grit uses it: the `Slack` trait (listen, post, find a post by its
  `Tag`, react, a person's name, whether a channel is public), `SlackError`, the tokens, and
  `SocketSlack`, the SDK over Socket Mode behind it. ← `event`, `text`
- **`edge`** — `SlackEdge`, the edge itself, over core's traits (`EdgeStores`: the inbox,
  the people it enrolls, the replies it awaits) and a `Slack`: a person's message in a public
  channel becomes a turn of its thread's conversation when it mentions grit or is in a thread
  grit started, and each finished turn's reply is posted in its thread, once, found again by
  its tag after a crash. ← `client`, `text`, `event`

No source file sits at the module's root, and the test tree mirrors it.
