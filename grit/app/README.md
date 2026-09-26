# grit.app

The composition root: wires implementations into the seams. The only module that depends
on every other one, and so the only place quarantine modules meet. Because Mill's
`moduleDeps` are transitive, DBOS is on its classpath — enola's `only-dbos-imports-*`
rules are what keep it out; it reaches Postgres only through `grit.dbos.engine.Engine`.
Run it with `scripts/grit`.

In dependency order:

- **`config`** — `DotEnv`: settings from a `.env` file under the real environment.
  `Prefs`: what grit remembers between runs (the theme last chosen), in
  `$XDG_CONFIG_HOME/grit/prefs` or `~/.config/grit/prefs`. Imports nothing in app.
- **`look`** — `Theme`, a palette by role (Frost the default; `GRIT_THEME` picks
  the one a run starts in), and `Look`, the chat screen's styles from a theme: who speaks is a rune as
  well as a colour (`Look.Runes`: ᛗ the user, ᚨ grit, ᚺ a failure, ᛁ idle, ᛭ between turns). Every theme is held to
  the same APCA contrast targets (`ThemeContrastTests`). `ProseLook` is the terminal's
  output mode for prose (`grit.prose`): a reply's markdown as transcript blocks, whole or
  still streaming. `Pill` is a tab's pill, lit on the accent while it is chosen; `Splash`, the
  welcome to an empty conversation. Imports
  nothing in app.
- **`chat`** — the chat TUI. `ChatScreen` is a pure `grit.tui` app whose transcript is a
  projection of the store: a submission leaves as `Effect.ToHost(Send)` and is shown when
  the store has it. It paints at once: a ward turns while the engine opens, the Futhark
  spins while a turn runs, both on one cancellable timer. `ChatHost` is its engine side:
  `Load` opens the engine (`ChatHost.Opener`, off the screen's thread), then follows the
  conversation, polling its entries and each open turn's status
  (`Follow`, pure), so replies to any turn appear, recovered ones included (ADR 0002);
  `Send` ingests and starts a turn once the engine is open. The panel beside the
  transcript has three tabs, switched by ctrl-t or a click on a pill: the turn
  (`TurnView`), the session so far (`SessionView`: its length, what it was billed,
  what search recalled, the models in each role), and its topics (`TopicsView`: each
  topic with its messages, the current one marked, and how the shown turn's message was
  placed: p(same) and its band, the classifier's choice, the model's verdict, a flag when
  the two disagreed, the heaviest weights), each built purely from what the host read.
  A conversation with nothing in it yet shows the splash in the transcript's place,
  once the host's first look has said so. `Replies` reads replies back out of the store, and a
  turn's tool loop as one faint line per step (`Replies.exchange`); the call the model is
  still writing shows as `calling read…`. `Commands` is the one table of slash
  commands: what the palette (`/` in an empty prompt, or ctrl-p) lists, what `/help`
  describes, and what a submitted `/` draft runs; such a draft never reaches the model, but one starting `//` does, as a message starting
  with `/` (one slash taken off; the palette stays shut).
  The theme is screen state, so `/theme` repaints everything live, and asks the host
  to keep it (`KeepTheme`): `Main` writes it to `Prefs`, and the next run starts in it
  unless `GRIT_THEME` says otherwise. `/summaries` shows
  each turn's summary, faint, under its reply (off by default: the transcript is the
  conversation, and a summary lands after its reply, so the rows below would move). ← `look`
- **`main`** — `Main`: reads the settings, opens the engine and launches the turn
  (OpenRouter with a key, the stub without; Jev placing messages among topics with
  `JEV_API_KEY`, the stub classifier with `GRIT_STUB_TOPICS=1`, none otherwise), offering
  each turn's model the coding tools over the checkout it runs in (`grit.tools` over
  `grit.host`): which ones as `GRIT_TOOLS` says (`Main.toolChoice`), the edits and shell
  built only when they are offered, then runs the chat TUI, or with arguments answers each as a message.
  `KeptFacts`, the fact book `propose_fact` keeps approved facts in; `PromoteFacts`, which
  prints the seed catalog with every approved fact laid over it, for a reviewed commit.
  ← `config`, `look`, `chat`

No source file sits at the module's root, and the test tree mirrors it.
