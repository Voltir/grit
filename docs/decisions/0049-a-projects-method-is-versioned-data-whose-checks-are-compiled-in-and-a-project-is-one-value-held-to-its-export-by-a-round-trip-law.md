# 0049. A project's method is versioned data whose checks are compiled in, and a project is one value held to its export by a round-trip law

Status: accepted (2026-10-27); revised (2026-10-29, 2026-11-17, 2026-11-18)

Context: the projects plugin keeps a project's artifacts (a change, a design, a task) and moves
each from state to state as the project's methodology allows. Methodologies differ by team and
change over time, and a project must keep the rules it was started under. Turned down:

- A methodology as code (a plugin per method): every change would be a release, and a project
  could not say which rules it holds to.
- A methodology wholly as data, its checks an expression language: such a language grows
  until it is a program the method's author maintains without a compiler.
- Checks that read the store: a replay over recorded heads and a live move could disagree.
- A method's examples as the deployment's own tests: a version could be declared without them
  run.

Decision:

- **A methodology is versioned data** (`Method`): its kinds of artifact, each kind's states,
  end states and sections, the transitions between states, who may make each (`Movers`), and
  what each role is shown (`RoleView`). `Method.of` refuses an inconsistent method, every
  reason at once.
- **Its checks are compiled in.** A transition names its checks; each is a pure
  `TransitionCheck` the deployment compiles, judging one artifact over its project's heads. What
  a transition does besides moving is grit's (`TransitionAction`, a closed set), never a
  method's code.
- **A digest pins it**, covering the method's data in any order and each named check's name and
  version. A project pins its method's name, version and digest (`Pin`); a move is refused when
  the pinned version is not declared or its digest is not the pin's.
- **A method is declared, with worked examples, into the store.** `project_declare` takes a
  version as JSON (`MethodJson`) from a caller giving it directly and believed at `Authority`,
  and accepts it once its examples (`Example`) replay (`MethodBench`); a replay and a live move
  make one move (`Method.move`). Each version is a plugin document under `<name>@<version>`,
  never changed, withdrawn or swept: the same version again is a no-op under its digest, refused
  under another. `Projects.of(checks, default, turns, methodsAt)` names no method; every command
  reads the version its project pins in its own transaction, so a process change is a
  declaration, with no rebuild and no restart.
- **Every version is kept and read at one label, `methodsAt`**, so it has one digest whoever
  reads it. A declaration from a room that writes at another label is refused, and a caller who
  cannot read `methodsAt` finds no method declared: a deployment gives a label every caller of
  the plugin's commands reads.
- **A project is one value** (`Project`): its key, its pin and its artifacts, each a head (key,
  kind, state, title, parent, cites, author) and a body, with no link to an artifact it lacks and
  no parent cycle. A key is an id, never a title.
- **Each has an export and its reader, held together by a round-trip law**: a project's
  (`ProjectMarkdown`, a file per artifact plus one for its key and pin; read after render keeps
  every head and link, and is a fixed point after one pass), and a method's (`MethodJson`, which
  `project_methods` gives and `project_declare` takes; equal digest and examples).
- **A project lives in the plugin's ledger documents** (ADR 0028), no table of its own: one per
  artifact and an index holding its pin, written only by the plugin's commands (ADR 0053), each
  in one transaction. A keep never changes a state.
- **A claim is one of the plugin's claims** (`grit.core.claim.Claims`, ADR 0050), never a
  document. A harness's holder is one rule, `ClaimHolders.of`, computed by the commands edge
  (ADR 0062) alone.

Consequences:

- A team changes its method by declaring a new version, not by releasing grit; a project keeps
  its pinned version until `project_migrate` moves it, renaming kinds and states as asked.
- Who may make a transition is method data: one of `Movers.People` is made only by a caller
  giving the command directly and believed at `Authority` (ADR 0062's proxy, not an identity),
  and a worked example can state it (`Expect.RefusedPersonOnly`). A turn, or a session believed
  lower, moves as an agent.
- A check needing a parameter is a compiled check of its own until checks take arguments.
  OpenSpec and the elephant-goldfish model, as method data, cannot say a section by pattern, a
  merge below `##`, or a goldfish's freshness.
- The exports are for reading and diffing; the law keeps them a faithful picture. A backup is
  the database's, and the method documents travel inside it; a deployment that keeps its methods
  as files declares them from those files, as its deploy and restore test do.
- Enforced by the compiler (private constructors, `caps.Pure`), a probe
  (`TransitionCheckPurityTests`), the law rule `plugins-name-core-and-plugins`,
  `ProjectMarkdownTests` and `MethodJsonTests`.
