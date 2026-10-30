# grit.chat

The chat TUI: a person's terminal onto a database's engine, which a deployment runs over the
`grit.core.edge.Console` that `Kit.console` opens. That console runs the engine when the
database's lock is free and attaches to the process holding it otherwise (ADR 0015), and the
chat reaches the engine through it alone, across Postgres (ADR 0002). Depends on `grit.core`,
`grit.prose`, `grit.edge`, `grit.tui`, and `grit.turn` for a turn read back from the steps it
recorded; never the kit or DBOS.

Its types are the code's (`scripts/scout area chat`); this file holds the package order and
what a person sets and sees.

In dependency order:

- **`settings`**: settings as a person writes them in the chat. A `/set` of a lifecycle setting
  (`Lifecycle`) holds until grit next starts, and `/set` says so. The theme last chosen is kept
  in `$XDG_CONFIG_HOME/grit/prefs` or `~/.config/grit/prefs` (`Prefs`). Imports nothing in chat.
- **`look`**: themes and the chat screen's styles. Who speaks is a rune as well as a colour.
  Every theme is held to the same APCA contrast targets (`ThemeContrastTests`). Imports nothing
  in chat.
- **`view`**: what the turn panel's tabs describe of the store: a turn, the session so far, and
  its agenda. Pure. Imports nothing in chat.
- **`screen`**: `ChatScreen` is a pure `grit.tui` app whose transcript is a projection of the
  store: a submission leaves as an effect to the host and is shown when the store has it, so
  replies to any turn appear, recovered ones included (ADR 0002). It paints at once, while the
  engine opens. The panel's tabs (ctrl-t, or a click on a pill) show the turn, the session so
  far, and its agenda. `Commands` is the one table of slash commands: what the palette (`/` in
  an empty prompt, or ctrl-p) lists, what `/help` describes, and what a submitted `/` draft
  runs. Such a draft never reaches the model; one starting `//` does, one slash taken off.
  `/theme` repaints live and is kept for the next run. `/summaries` is off by default: a summary
  lands after its reply, so the rows below would move. ← `settings`, `look`, `view`
- **`host`**: `ChatHost` is the screen's engine side: it opens the console off the screen's
  thread, then polls the conversation's entries and each open turn's status. ← `screen`,
  `view`, `settings`

No source file sits at the module's root, and the test tree mirrors it.
