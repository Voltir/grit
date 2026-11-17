# 0049. A project's method is versioned data whose checks are compiled in, and a project is one value held to its export by a round-trip law

Status: accepted (2026-10-27); revised (2026-10-29, 2026-11-17)

Context: the projects plugin keeps a project's artifacts (a change, a design, a task) and moves
each from state to state as the project's methodology allows. Methodologies differ by team and
change over time, and a project must keep the rules it was started under. An artifact is read
from the store, written to files a person edits and reads back, and later restored from those
files. Turned down:

- A methodology as code (a plugin per method): every change would be a release, and a project
  could not say which rules it holds to.
- A methodology wholly as data, its checks an expression language: such a language grows
  until it is a program the method's author maintains without a compiler.
- Checks that read the store: a replay over recorded heads and a live move could disagree.
- A method's examples as the deployment's own tests: a version could be declared without them
  run.
- A project as its stored rows, with export, import and restore each its own procedure: three
  readers of one format drift apart.

Decision:

- **A methodology is versioned data** (`Method`): its kinds of artifact, each kind's states and
  end states and the sections its body holds, the transitions between states, and what each
  role is shown (`RoleView`: what an artifact cites, at most three links deep, and the
  artifacts the role always carries whole). `Method.of` refuses an inconsistent
  method, every reason at once.
- **Its checks are compiled in.** A transition names its checks; each is a `TransitionCheck`
  the deployment compiles, judging one artifact over its project's heads and its method's kinds,
  pure; what it decides changing is a new version. What a transition does besides moving is
  grit's (`TransitionAction`, a closed set), never a method's code: nothing,
  or merging the moved artifact's `Delta: <name>` sections into the one artifact it cites of
  the kind the transition names.
- **A method's digest pins it**: it covers the method's data, whatever its declared order, and
  each named check's name and version, so a check whose decision changes under its name
  changes the digest. A project pins its method's name, version and digest (`Pin`).
- **A method is declared with worked examples** (`Example`): a starting project, then moves,
  each expected allowed or refused by a named check; a method its replay (`MethodBench`) does
  not bear out is refused. A replay and a live move make one move (`Method.move`).
  *Revised:* a method is kept in the store, not in code. `project_declare` takes a version as
  JSON (`MethodJson`), from a caller giving it directly and believed at `Authority`, and accepts
  it once its examples replay; each version is a document of the plugin's own under
  `<name>@<version>`, never changed (the same version again is a no-op under its digest, refused
  under another) and never withdrawn or swept. `Projects.of` takes only the compiled checks and
  the default method's name, and every command reads the version its project pins in its own
  transaction, so a process change is a declaration, with no rebuild and no restart.
- **A project is one value** (`Project`): its key, its pin and its artifacts, each a head
  (key, kind, state, title, parent, cites, author) and a body, with no link to an artifact it
  lacks and no parent cycle. A key is an id, never a title. A body is prose referring by key,
  so a rename breaks no sentence (`ProseJson`).
- **A project has its export and its reader** (`ProjectMarkdown`): a file per artifact, front
  matter then markdown, and a file holding the project's key and pin. The round-trip law holds
  them together: read after render keeps every head and link, and after one pass is a fixed
  point (markdown normalises once). *Revised:* a method has its own export and reader
  (`MethodJson`, which `project_methods` gives and `project_declare` takes), held together by a
  round-trip law of its own (`MethodJsonTests`: an equal digest and equal examples).
- **A project lives in the plugin's ledger documents** (ADR 0028), no table of its own: one per
  artifact, under its key, its head and stored body as data and refs naming its project, kind,
  state, parent and cites; and an index under the project's key holding its pin. Only the
  plugin's commands write them (ADR 0053), each in one transaction and recording its writer. A
  keep never changes a state; a move is refused when the pinned version is not declared, or its
  digest is not the pin's.
  Past the bound a write is refused, never a document withdrawn.
  A claim is one of the plugin's claims (`grit.core.claim.Claims`, the table `grit.claims`),
  never a document, so claiming writes no version: made as a role for a holder (the session or
  agent its brief is for) at its artifact's label, one live claim per artifact and one artifact
  per holder at each label. It lapses unless its holder's sessions keep working (ADR 0050), and
  is deleted by its release, by a claim taking it over once lapsed, or by the sweep.
  *Revised:* a harness's holder is one rule, `grit.commands.machine.ClaimHolders.of`, computed by the
  commands edge (ADR 0062) and nowhere else: `<machine>/<directory's last segment>-<hash>` for a
  session, and `/<agent type>/<starting session's hash>` after it for an agent that session
  starts, so a claims listing stays readable. A hashed session cannot be written by hand, so
  `GET /holder` is the one way to learn one, and it is no MCP tool: a harness's adapter has its
  start hook print the session's holder and the form for an agent type it starts. A manager can
  still claim before its agent exists: the machine, the directory, the type and its own session
  are known first.

Consequences:

- A team changes its method by declaring a new version, not by releasing grit; a project keeps
  its pinned version until `project_migrate` moves it, renaming kinds and states as asked.
- A check needing a parameter is a compiled check of its own until checks take arguments.
- OpenSpec and the elephant-goldfish model, written as method data, show what it cannot say: a
  section by pattern, a merge removing a section or below `##`, and a goldfish's freshness.
  *Revised:* who may make a transition is method data: a transition of `Movers.People` is made
  only by a caller giving the command directly and believed at `Authority` (ADR 0062's proxy,
  not an identity), and a worked example states it (`Expect.RefusedPersonOnly`). A turn, or a
  session believed lower, moves as an agent. `TurnCommands` says only which commands a turn is
  offered.
- Export, import and restore are one function pair for a project, and one for a method, so a
  backup is checked by reading it. A restore declares the methods first, from their files, then
  restores the projects that pin them.
- Enforced by the compiler (private constructors, `caps.Pure`), a probe
  (`TransitionCheckPurityTests`), the law rule `plugins-name-core-and-plugins`,
  `ProjectMarkdownTests`, `MethodJsonTests` and `DeclareTests`.
