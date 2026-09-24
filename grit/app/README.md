# grit.app

The composition root: wires implementations into the seams. The only module that depends
on every other one, and so the only place quarantine modules meet. Because Mill's
`moduleDeps` are transitive, DBOS is on its classpath — enola's `only-dbos-imports-*`
rules are what keep it out; it reaches Postgres only through `grit.dbos.Engine`.

- `Main` — opens the engine and launches the turn (OpenRouter with a key, the stub
  without), then runs the chat TUI, or with arguments answers each as a message.
- `ChatScreen` — the chat screen, a pure `grit.tui` app. A submission leaves as
  `Effect.ToHost(Send)`.
- `ChatHost` — what the screen's requests mean against the engine: ingest, start the turn,
  wait for it, read the reply from the store (ADR 0002), each on a virtual thread,
  answering through the runtime's mailbox.
