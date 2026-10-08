# grit.app

The reference deployment (ADR 0021): grit's own chat, `grit serve` and `grit backfill`, each
filled from `GRIT_*` variables into a `grit.kit` `Deployment` and run through the kit
([`kit/README.md`](../../kit/README.md)). The only module that depends on every other one,
and so the only place quarantine modules meet. Because Mill's
`moduleDeps` are transitive, DBOS is on its classpath — enola's `only-dbos-imports-*`
rules are what keep it out; it reaches Postgres only through `grit.dbos.engine.Engine`.
Run it with `scripts/grit`.

In dependency order:

- **`config`** — `Durations`: a duration as a setting writes it (`30s`, `3m`, `24h`, `30d`). `Lifecycle`:
  the lifecycle's settings as a person writes them, written from `GRIT_IDLE`, `GRIT_SETTLE`,
  `GRIT_RESOLVE_AT`, `GRIT_ASKS`, `GRIT_RETENTION`, `GRIT_LEDGER`, `GRIT_BALANCE`, `GRIT_SCOPE`
  and `GRIT_WEIGHT` over the database's on every start (logged as they stand), and changed
  one at a time by `/set` until the next start. `Budgets`: the daily cap on model spend,
  `GRIT_DAILY_USD` (`grit serve`'s default $1.00).
  `Claimed`: the email domains the deployment claims as its own, `GRIT_CLAIMED_DOMAINS`
  (comma-separated; ADR 0032).
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
  (`TurnView`), the session so far (`SessionView`: its length, what it was billed, what
  its recorded calls cost with its closes', what the whole database spent today against
  the daily cap, what search recalled, the models in each role), and its topics (`TopicsView`: each
  topic carried from the last close or spoken in since, with its messages, the current one marked, and how the shown turn's message was
  placed: p(same) and its band, the classifier's choice, the model's verdict, a flag when
  the two disagreed, the heaviest weights), each built purely from what the host read.
  A conversation with nothing in it yet shows the splash in the transcript's place,
  once the host's first look has said so. `Replies` reads replies back out of the store, and a
  turn's tool loop as one faint line per step (`Replies.exchange`); the call the model is
  still writing shows as `calling read…`. `Commands` is the one table of slash
  commands: what the palette (`/` in an empty prompt, or ctrl-p) lists, what `/help`
  describes, and what a submitted `/` draft runs; such a draft never reaches the model, but one starting `//` does, as a message starting
  with `/` (one slash taken off; the palette stays shut).
  `/set` shows the lifecycle's settings and grit's voice, or changes one (`/set idle 3m`,
  `/set resolve 0.9`, `/set voice colleague`, `/set voice <your own words>`), taking effect
  from the next sweep and turn; a lifecycle setting holds until grit next starts, which
  writes the `GRIT_` variables' settings again (the voice is kept), and `/set` says so. A closed period shows in the
  transcript as a double rule marked `closed`, then why it closed (`resolved (0.86)`, as
  judged with nobody waiting, or `lapsed`) and what it came to (`Replies.closed`).
  The theme is screen state, so `/theme` repaints everything live, and asks the host
  to keep it (`KeepTheme`): `Main` writes it to `Prefs`, and the next run starts in it
  unless `GRIT_THEME` says otherwise. `/summaries` shows
  each turn's summary, faint, under its reply (off by default: the transcript is the
  conversation, and a summary lands after its reply, so the rows below would move). ← `look`
- **`main`** — `Main`: reads the settings into the reference `Deployment`
  (`Main.deployment`), and serves it (`grit serve`: the Slack edge, `SlackEdge.serving`,
  in every channel its bot is invited to, answering the slash command
  `GRIT_SLACK_COMMAND` names (`/grit` when unset), and with `GITHUB_MCP_TOKEN` set, GitHub's
  read-only MCP tools at `service:github` for every Slack conversation, `McpEdge.serving`
  (`Main.github`), through `Kit.serve`), catches it up (`grit
  backfill`: `SlackEdge.backfill` over the last `GRIT_BACKFILL_DAYS`, through `Kit.catchUp`;
  serving Slack, it trusts Slack to say who the people of the workspace its bot token is
  installed in are, `SlackEdge.installedIn`, and no one otherwise, and the two end what its
  identities no longer trust as they start, where the chat and a run with arguments end nothing),
  or opens the engine and launches the turn
  (OpenRouter with a key, the stub without; Jev placing messages among topics with
  `JEV_API_KEY`, the stub classifier with `GRIT_STUB_TOPICS=1`, none otherwise), offering
  each turn's model the coding tools over the checkout it runs in (`grit.tools` over
  `grit.host`): which ones as `GRIT_TOOLS` says (`Main.toolChoice`), the edits and shell
  built only when they are offered, and launches the close and posting (`grit.lifecycle`)
  beside the turn, sweeping every `GRIT_SWEEP`, posting to the plugins `GRIT_PLUGINS` turns
  on (`digest`, `grit.digest`, whose `recent_activity` every turn's model is offered, as any plugin's tools are; `remind`, `grit.remind`'s reminders, posted in the conversation that asked for them); then runs the chat TUI, or with arguments answers each as a message.
  `LocalTools`, the chat's edge's tools over this checkout; `PromoteModelSettings`, which prints the seed catalog with every approved setting
  laid over it, for a reviewed commit.
  ← `config`, `look`, `chat`

No source file sits at the module's root, and the test tree mirrors it.
