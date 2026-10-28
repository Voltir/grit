---
paths:
  - "**/*.scala"
---

## Reading and editing Scala in this repository: scout first

scout answers from the compiler's TASTy, in less text than a file read, in a form an Edit can
match. It is this repository's tooling (`scripts/scout`; the script says where the binary lives),
and the one home of how Scala is read here: CLAUDE.md and the agent definitions point here.

**Tools.** The `scout` MCP server's `show`, `file`, `find`, `uses`, `tests`, `family` and
`area` (load them with ToolSearch `select:mcp__scout__show,…` when they are listed as
deferred), passing `root` as the checkout or worktree you work in; the server keeps what it has
read between calls. The CLI (`scripts/scout <query> … --root .`) is the fallback when the MCP
tools are missing; `scripts/scout <query> --help` gives each query's flags and says which
printed lines are the file's own text.

**Which query, for what** (MCP arguments; a list argument is a JSON array of quoted strings,
`["Turn"]`, never a bare word or one comma-joined string):

- an unfamiliar part of grit, or which areas exist: `area {"names": ["knowledge"]}`, `area {}`;
- a name you don't know: `find {"pattern": "*Shelf*"}`, `find {"pattern": "*.placed"}`;
- a signature and its doc, before calling it, every symbol you need in one call:
  `show {"symbols": ["Flow", "Step", "Flows"]}`;
- a body you are about to edit or need to understand, private or not, named in `body`:
  `show {"symbols": ["Turn"], "body": ["looping", "settle"], "private": true}` (`private`
  with no `body` lists every internal helper);
- where a file's imports end and its definitions lie, or where to add one: `file {"target": "TurnEnv"}`;
- every caller before changing a signature: `uses {"symbols": ["Moves.ask", "Moves.keep"]}`;
- a trait and its implementations: `family {"trait": "Durable"}`;
- a suite, a test or a helper: `tests {"suite": "TurnToolsTests", "test": "<name prefix>"}`,
  `tests {"suite": "TurnToolsTests", "body": ["helper"]}` (`show` finds a test-source name
  only when main sources lack it).

**What not to read.**

- No `cat`, `Read` or whole-file `sed` of a `.scala` file: `file` for its shape, `show … body`
  for a member.
- A line range scout printed is not a cue for `sed -n`: ask for the member by name. Nor is a
  `File.scala:N` in a plan or brief: it points at a definition; `show` that.
- Before your first edit, no `tests` with many `body` helpers or a raised `max_kb`: read the
  one test you imitate, by name prefix, when you write yours. A brief's reads are run in their
  two groups: the before-the-first-edit ones first, each point-of-use read when you reach the
  code it names.
- Adding a member to an implementation or a fake: `file` it for its members, then `show` it
  with `body: ["<the nearest sibling>"]`, `private: true`. Never read the implementation whole
  to learn its shape.
- Trust the public interface and referential transparency: ask for private members only to
  edit them or to understand an implementation.
- Bash remains right for text search (`grep`), a test file's imports, never-compiled code, and
  a region inside a body you have not edited.

**Editing.**

- An Edit's `old_string` may be copied from scout's file text (`show` with or without `body`,
  `family`, `tests` with `test` or `body`) without a Read first. Read a line range only when an
  Edit fails to match. Several changes a few lines apart are one Edit.
- Change source with Edit (Write for a new file), which fails loudly on a mismatch; never
  `sed -i`, python or a heredoc, which change nothing silently when the text differs. The one
  exception is a mechanical change across many sites (a rename): a script may make it, and its
  commit message shows the count of matches before and after and a clean compile.
- scout reads compiled output. After an edit it is current again for every file a compile that
  succeeds covered (`./mill <module>.compile`, `<module>.test.compile` or the suite's run): go
  back to scout then, not `sed`. Until then, the lines you edited are what your Edit's
  `new_string` says: copy the next `old_string` from it rather than reading them back. After a
  failed compile scout answers from the last good one, and an edited file prints only its
  compiled signature.

**Report** a query that failed to answer what you needed: the query, what you wanted, what came
back.
