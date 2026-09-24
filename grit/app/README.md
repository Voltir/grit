# grit.app

The composition root: wires implementations into the seams. The only module that depends
on every other one, and so the only place quarantine modules meet. Because Mill's
`moduleDeps` are transitive, DBOS is on its classpath — enola's `only-dbos-imports-*`
rules are what keep it out; it reaches Postgres only through `grit.dbos.engine.Engine`.
Run it with `scripts/grit`.

In dependency order:

- **`config`** — `DotEnv`: settings from a `.env` file under the real environment.
  Imports nothing in app.
- **`look`** — `Theme`, a palette by role (Frost the default; `GRIT_THEME` picks
  another), and `Look`, the chat screen's styles from a theme. Every theme is held to
  the same APCA contrast targets (`ThemeContrastTests`). Imports nothing in app.
- **`chat`** — the chat TUI. `ChatScreen` is a pure `grit.tui` app whose transcript is a
  projection of the store: a submission leaves as `Effect.ToHost(Send)` and is shown when
  the store has it. `ChatHost` is its engine side: `Send` ingests and starts a turn;
  `Load` follows the conversation, polling its entries and each open turn's status
  (`Follow`, pure), so replies to any turn appear, recovered ones included (ADR 0002).
  `Replies` reads replies back out of the store. ← `look`
- **`main`** — `Main`: reads the settings, opens the engine and launches the turn
  (OpenRouter with a key, the stub without), then runs the chat TUI, or with arguments
  answers each as a message. ← `config`, `look`, `chat`

No source file sits at the module's root, and the test tree mirrors it.
