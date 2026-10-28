# 0049. A project's method is versioned data whose checks are compiled in, and a project is one value held to its export by a round-trip law

Status: accepted (2026-10-27)

Context: the projects plugin keeps a project's artifacts (a change, a design, a task) and moves
each from state to state as the project's methodology allows. Methodologies differ by team and
change over time, and a project must keep the rules it was started under. An artifact is read
from the store, written to files a person edits and reads back, and later restored from those
files. Turned down:

- A methodology as code (a plugin per method): every method change would be a release, and a
  project could not say which version of the rules it holds to.
- A methodology wholly as data, its checks an expression language: such a language grows
  until it is a program the method's author maintains without a compiler.
- Checks that read the store: a replay of a move over recorded heads and a live move could
  then disagree.
- A project as its stored rows, with export, import and restore each its own procedure: three
  readers of one format drift apart.

Decision:

- **A methodology is versioned data** (`Method`): its kinds of artifact, each kind's states and
  end states and the sections its body holds, the transitions between states, and what each
  role is shown (`RoleView`, at most three links deep). `Method.of` refuses data naming a name twice,
  a kind or state it does not declare or a check not compiled in, a transition out of an end
  state, or a state its kind's first cannot reach, reporting every reason at once.
- **Its checks are compiled in.** A transition names its checks; each is a `TransitionCheck`
  the deployment compiles, judging one artifact over its project's heads and its method's kinds.
  A check is pure, so it reaches no store, and deterministic, which review holds. A change to
  what a check decides is a new version. What a transition does besides moving is grit's
  (`TransitionAction`, a closed set), never a method's code.
- **A method's digest pins it**: it covers the method's data, whatever its declared order, and
  each named check's name and version, so a check whose decision changes under its name
  changes the digest. A project pins its method's name, version and digest (`Pin`).
- **A project is one value** (`Project`): its key, its pin and its artifacts, each a head
  (key, kind, state, title, parent, cites, author) and a body, with no link to an artifact it
  lacks and no parent cycle. An artifact's key is an id, never its title. Its body is prose
  whose references to other artifacts are notes on their spans, by key, so a rename breaks no
  sentence; its stored form is prose's (`ProseJson`), its version the body's.
- **It has one export and one reader of it** (`ProjectMarkdown`): a file per artifact, front
  matter then markdown, and a file holding the project's key and pin. The round-trip law holds
  them together: read after render keeps every head and link, and after one pass is a fixed
  point (markdown normalises once).

- **A project lives in the plugin's ledger documents** (ADR 0028), no table of its own: one per
  artifact, under its key, its head and stored body as data and refs naming its project, kind,
  state, parent and cites; and an index under the project's key holding its pin. Only the
  plugin's commands write them (ADR 0048), each in one transaction and recording its writer. A
  keep never changes a state; a move runs its transition's checks over the heads and its
  action, and is refused once the pinned method's digest has changed: a changed method is
  declared as a new version. Past the bound a write is refused, never a document withdrawn.

Consequences:

- A team changes its method by declaring a new version, not by releasing grit; a project keeps
  the version it pinned until moved to another.
- A check needing a parameter is a compiled check of its own until checks take arguments.
- Export, import and restore are one function pair, so a backup is checked by reading it.
- Enforced by the compiler (private constructors, `caps.Pure`), a probe
  (`TransitionCheckPurityTests`), the law rule `plugins-name-core-and-plugins`, and
  `ProjectMarkdownTests`.
