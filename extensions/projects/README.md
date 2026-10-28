# grit.projects

Projects: a project's artifacts (a change, a design, a task: whatever kinds its method
declares) kept as ledger documents of the `projects` plugin (ADR 0028), moved from state to
state only as the project's method allows, and read and written as one value, `Project`
(ADR 0049). Names `grit.core` and `grit.prose` alone: an artifact's body is prose, references
to other artifacts as notes on their spans.

Packages, in one-way order (each names only those before it):

- **`name`**: the names a method and a project use: `MethodName`, `KindName`, `StateName`,
  `CheckName`, `RoleName` (each a `Slug`), `ProjectKey`, `ArtifactKey` (`<project>/<artifact>`,
  an id, never a title), `Pin`, the method version a project holds to, and `BriefSlot`, what
  an artifact is claimed for.
- **`artifact`**: what a project is: `ArtifactHead` (everything but the body), `Body` (opaque:
  its prose and its stored form in one file, so a stored-form version bump is one migration),
  `Artifact` and `Project`, which holds no dangling link and no parent cycle; and `ArtifactJson`,
  an artifact and a project's index as ledger documents hold them (`Artifact.of` reads one
  back). Imports `name`.
- **`method`**: a methodology as versioned data: `Method` (its kinds, transitions and role
  views, every name declared and every check compiled in), `ArtifactKind`, `Transition`,
  `TransitionCheck` (compiled, pure, deterministic and versioned; the built-ins in
  `TransitionChecks`), `TransitionAction` (grit's, never a method's code) and `RoleView`.
  Imports `name` and `artifact`.
- **`render`**: a project for each consumer: `ProjectMarkdown`, the export (one file per
  artifact, front matter then body) and its reader, held to each other by the round-trip law
  (`ProjectMarkdownTests`); `AnswerText`, what the commands answer, the same for every edge;
  and `Brief`, what a session on one artifact is given, a pure function of the project's value
  (`BriefTests`). Imports `name`, `artifact` and `method` (a brief is shaped by a role view).
- **`plugin`**: `Projects`, the plugin, and its commands (ADR 0048): the only writers of its
  documents, each run in one transaction. Imports every package before it.

No source file sits at the module's root.

## What the code cannot say

- **A check is deterministic.** `TransitionCheck` is pure, so it reaches no store
  (`TransitionCheckPurityTests`), but the clock, `Random` and the environment are untracked:
  review holds a check to giving the same answer over the same artifact, heads and kinds. What a
  check decides changing is a new `version`, which changes every digest naming it.
- **Export normalises once.** `Markdown.write` writes for a reader, not for `Markdown.parse`,
  so a body read back from its export may differ from the one written; read again, it is
  the same. Every head and every link survives the first pass.
