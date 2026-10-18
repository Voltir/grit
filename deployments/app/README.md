# grit.app

The reference deployment (ADR 0021): grit's own chat, `grit serve` and `grit backfill`, each
filled from `GRIT_*` variables into a `grit.kit` `Deployment` and run through the kit
([`kit/README.md`](../../kit/README.md)). The only module that depends on every other one,
and so the only place quarantine modules meet. Because Mill's `moduleDeps` are transitive,
DBOS is on its classpath; enola's `only-dbos-imports-*` rules are what keep it out, and it
reaches Postgres only through `grit.dbos.engine.Engine`. Run it with `scripts/grit`. Its types
are the code's (`scripts/outline area app`); this file holds the package order and what a
person sets and sees.

In dependency order:

- **`config`**: settings as a person writes them. The lifecycle's (`GRIT_IDLE`, `GRIT_SETTLE`,
  `GRIT_RESOLVE_AT`, `GRIT_ASKS`, `GRIT_RETENTION`, `GRIT_LEDGER`, `GRIT_BALANCE`, `GRIT_SCOPE`,
  `GRIT_WEIGHT`) are written over the database's on every start, logged as they stand, and
  changed one at a time by `/set` until the next start. `GRIT_DAILY_USD` caps a day's model
  spend (`grit serve`'s default $1.00); `GRIT_CLAIMED_DOMAINS` (comma-separated) are the email
  domains the deployment claims (ADR 0032). The theme last chosen is kept in
  `$XDG_CONFIG_HOME/grit/prefs` or `~/.config/grit/prefs`. Imports nothing in app.
- **`look`**: themes and the chat screen's styles (`GRIT_THEME` picks the one a run starts in).
  Who speaks is a rune as well as a colour. Every theme is held to the same APCA contrast
  targets (`ThemeContrastTests`). Imports nothing in app.
- **`chat`**: the chat TUI. `ChatScreen` is a pure `grit.tui` app whose transcript is a
  projection of the store: a submission leaves as an effect to the host and is shown when the
  store has it, so replies to any turn appear, recovered ones included (ADR 0002). It paints at
  once, while the engine opens. `ChatHost` is its engine side: it opens the engine off the
  screen's thread, then polls the conversation's entries and each open turn's status. The
  panel's tabs (ctrl-t, or a click on a pill) show the turn, the session so far, and its agenda.
  `Commands` is the one table of slash commands: what the palette (`/` in an empty prompt, or
  ctrl-p) lists, what `/help` describes, and what a submitted `/` draft runs. Such a draft never
  reaches the model; one starting `//` does, one slash taken off. A `/set` of a lifecycle
  setting holds until grit next starts (the voice is kept), and `/set` says so. `/theme`
  repaints live and is kept for the next run unless `GRIT_THEME` says otherwise. `/summaries`
  is off by default: a summary lands after its reply, so the rows below would move. ← `look`
- **`main`**: `Main` reads the settings into the reference `Deployment` and runs one of:
  - `grit serve`: the Slack edge in every channel its bot is invited to, hearing what was said
    before each join within `GRIT_BACKFILL_DAYS`, `GRIT_BACKFILL_MESSAGES` and
    `GRIT_BACKFILL_JOINS_PER_DAY` (a value that is not a whole number above zero is refused,
    naming the variable); the slash command `GRIT_SLACK_COMMAND` names (`/grit` when unset);
    with `GITHUB_MCP_TOKEN` set, GitHub's read-only MCP tools at `service:github`; and with
    `GRIT_DRIVE_SA_KEY` set, Drive's files at `service:drive` (`Main.drive`). That variable
    names the absolute path of the service account's JSON key file, as
    `GOOGLE_APPLICATION_CREDENTIALS` does (`~` is not expanded): the deployment's runtime
    writes the file, a mounted secret or a deploy step, and passes its path. `GRIT_DRIVE_DOMAIN` names the domain
    whose addresses are Drive's people, one `GRIT_CLAIMED_DOMAINS` lists; `GRIT_DRIVE_WRITERS`
    lists, comma-separated, the exact addresses of accounts that are no person (another
    service's) whose edits are believed at member. `service:drive` is shared: each folder in
    it is unmapped, so nothing of it is shown, until a person labels it from Slack with
    `label <level> at <the folder's link or id>`.
  - `grit backfill`: the Slack edge's catch-up over the last `GRIT_BACKFILL_DAYS`.
  - Both trust Slack to say who the people of the workspace its bot token is installed in are,
    and no one otherwise, and end what the deployment's identities no longer trust as they
    start; the chat and a run with arguments end nothing.
  - Otherwise the engine and the turn: OpenRouter with a key, the stub without; Jev with
    `JEV_API_KEY`, the stub classifier with `GRIT_STUB_TOPICS=1`, none otherwise; the coding
    tools `GRIT_TOOLS` names, over the checkout it runs in, the edits and shell built only when
    offered; sweeping every `GRIT_SWEEP`; posting to the plugins `GRIT_PLUGINS` turns on
    (`digest`; `remind`; `knowledge`, over the repository `GRIT_GITHUB_OWNER`/`GRIT_GITHUB_REPO`
    at `GRIT_GITHUB_REF` when `GITHUB_MCP_TOKEN` is set, synced through the GitHub edge `grit
    serve` serves, and over Drive's shared drives when `GRIT_DRIVE_SA_KEY` is set, carded at
    each label `GRIT_DRIVE_CARD_LABELS` lists (written forms, comma-separated, none below
    internal), synced through the Drive edge; refused at start with neither, or with a part
    of either unset; `Main.knowledge`; `strands`, one document per strand of a room, dreamed
    daily at 04:30 at each label `GRIT_STRAND_LABELS` lists, refused at start without one;
    `Main.strands`; `topics`, one document per subject found across strands and cards,
    dreamed daily at 05:00 at each label `GRIT_TOPIC_LABELS` lists, reading the knowledge
    plugin's cards when `knowledge` is listed before it; `Main.topics`; `standing`, the person- and tool-established Standing lines of each close, but
    a reminder tool's, kept as one document); then the chat TUI, or with arguments each answered
    as a message.

  `PromoteModelSettings` prints the seed catalog with every approved setting laid over it, for a
  reviewed commit. ← `config`, `look`, `chat`

**Who may label what (setup tiers).** A room's label is set by slash command, under one rule
in core (`grit.core.admin.Authority`): in a private channel any full member may raise it; in a
public one, or to lower a level, only an administrator may; removing a compartment needs its
steward or an administrator. What a deployment writes, least first:

- **Tier 0, this deployment.** `GRIT_ADMINS`: Slack user ids (`U…` or `W…`, as a profile's
  "Copy member ID" gives them), comma-separated; one that is not is refused, naming the
  variable. They are the declared group `administrators` of `slack:{team}/{id}` accounts, the
  visibility's administrators (`Main.visibility`); no other group, grant or compartment is
  declared, so public channels are public and private ones unmapped until labelled. They alone
  set a room's baseline (`baseline <grade>`), the least grade its turns are routed to. Unset, no
  one can lower a room's label, label a public channel or set a baseline, and `grit serve` says
  so as it starts.
- **Tier 1, a deployment's code.** One realm group granted `Internal`, and
  `RoomLabels.of(…, open = Some(Labelled.Mapped(Internal)))`: every full member reads internal,
  and public channels are internal.
- **Tier 2, a deployment's code.** Compartments, a few lines each: the compartment, its own
  group, a grant, and optionally a `Steward(compartment, group)` whose declared members may
  clear and remove people for it. Declaring a deployment: [`docs/extending.md`](../../docs/extending.md).

No source file sits at the module's root, and the test tree mirrors it.
