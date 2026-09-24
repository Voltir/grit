# grit.app

The composition root: wires implementations into the seams. The only module that depends
on every other one, and so the only place quarantine modules meet. Because Mill's
`moduleDeps` are transitive, DBOS is on its classpath — enola's `only-dbos-imports-*`
rules are what keep it out; it reaches Postgres only through `grit.dbos.engine.Engine`.
Run it with `scripts/grit`.

- `Main` — reads `.env` (`DotEnv`), opens the engine and launches the turn (OpenRouter
  with a key, the stub without), then runs the chat TUI, or with arguments answers each
  as a message.
- `ChatScreen` — the chat screen, a pure `grit.tui` app. Its transcript is a projection
  of the store: a submission leaves as `Effect.ToHost(Send)` and is shown when the store
  has it.
- `ChatHost` — the screen's engine side: `Send` ingests and starts a turn; `Load` follows
  the conversation, polling its entries and each open turn's status (`Follow`, pure), so
  replies to any turn appear, recovered ones included (ADR 0002).
