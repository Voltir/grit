# outline

A command-line tool and MCP server that answers questions about a Scala 3 codebase from its
compiled TASTy. Signatures, Scaladoc and resolved references come from the compiler's output,
so names resolve the way the compiler resolves them. Bodies and docs are source text sliced at
TASTy's positions, so an answer can be used as edit input. Answers are grouped by file, with
line ranges taken from the Scaladoc.

## Queries

Each query has a CLI form and an MCP tool of the same name, taking the same arguments.
`scripts/outline <query> --help` prints that query's full help: its examples and every flag.
Every query takes `--max-kb N` (MCP `max_kb`), default 12: the answer is cut at N KB, between whole
entries, and a cut answer ends with `-- cut at 12 KB: 7 more entries, ~31 KB; rerun with --max-kb 31 or narrow`,
the KB every entry needs, so a rerun at that budget shows them all. In show, a type of two members or more is
cut between its members, its head first, then the members `--body` names, then the rest in file order: `-- cut at 12 KB after Turn.route: 31 more members, ~14 KB; rerun with --max-kb 26, or --body <member> for one`,
14 KB being what was cut, the `--body` hint only when none was given; what `--body` names prints whatever the budget; its closing lines print only when no member was cut.
In show, family, find, tests and uses' owner lines, a line naming a definition ends with its size,
`[N.N KB]`: the bytes of its whole text, doc included, to tell what a `--body` would cost.
In show and family, a class, trait, object or enum printed without its body ends with its closing lines, an
anchor for an Edit that adds a member to it (File text, below); no other definition has one.

**show**: To read or call a definition: its signature and Scaladoc, with one-line outlines of the
project types it names. Bodies only for members named with `--body`; a path there (`--body Settling.began`) reaches
a nested member, and a bare name a type's own members lack is looked for in its nested owners, printed from one owner
or named with each when several have it. A name defined only inside the body of a val or def shown or among a shown
type's members, such as an anonymous class's member, is named with its line and the `--body` that prints that body. A
name a shown type lacks that another type's member has, with the shown type in its signature (`Place --body readsFrom`,
for `Tx.readsFrom(source: Place)`), is named as `find` lists it, with the `show` printing it; with none, the note names
the `find` listing every definition of the name. A member of a shown type not
named in `--body` prints its doc's first sentence only, its size tag saying how much more there is;
`--docs` prints every doc whole (File text, below, says which of those lines are file text). After the definitions, one
line per named symbol counts the tests in test sources that exercise it:
`-- tests of Label: 41 in 12 suites (nearest: LabelTests, TxTests, InboxContract); --tests to list`.
The nearest suites are those under the symbol's own module directory first, then those holding the
most references. `--tests` lists them instead: a `## tests exercising <Sym>` line, then per file a
`== file  package` line and entries `NN-MM <Suite>: <test name>`, or `NN-MM <Suite>: helper <def>`
for a helper that itself holds a reference. `--no-tests` drops both. None of these lines is file text. A name main sources lack is looked up in test sources
and shown from there, under a `-- in test sources: <full name>` note, with no tests line. Every query reads a short name
the same way: when several definitions' full names end with it and only one lies outside test sources and the
`secondary` paths (Configuration, below), it means that one, under
`-- Part taken as grit.core.edge.Part, the only one outside test and secondary sources; also grit.eval.harness.capture.Part (secondary: eval/harness/): name it fully to read it`;
otherwise it means all of them, under `-- ambiguous Part, name one fully:` and one `--   ` line each, as `find` lists it,
primary ones first and the others tagged.

    scripts/outline show Sym[,Sym…] [--depth 0|1|2] [--body m[,m…]] [--private] [--docs] [--tests|--no-tests] [--max-kb N] [--root DIR]

**family**: To implement or change a trait: its implementations, contracts and their suites.
`--member` narrows each to one member. A short name reads as in show; one it leaves ambiguous answers only
the `-- ambiguous` list, no family picked from it. A name that is no trait or abstract class, such as an opaque type,
says what it is and names the `show` and `uses` for it.

    scripts/outline family Trait [--member m] [--body] [--max-kb N] [--root DIR]

**uses**: To find who calls or names a definition before changing it. `--in` and `--outside`
filter by path prefix. `X.apply` (or `X.new`), for a class or enum case with no `apply` in source, finds its
construction sites: `X(…)` and `new X(…)`. `X.unapply`, for a class or enum case with no `unapply` in source, finds
its pattern sites: `case X(…)` and `case _: X` (for a trait or a plain class, `case _: X` alone). `uses X` includes
`case X(…)` patterns and `X(…)` calls with its other uses. A name `X.apply` or `X.unapply` whose `X` is neither says
so in one line. Sites are grouped by file (`== file`), then by the
definition holding them: an `in <owner>` line (the member around the site, or `Suite: test name` for
a site in a test), then each site as its line number and source line. Over 20 sites in all, each file
prints as `== file  N sites` and each owner once with its count, with no site lines, and the answer ends
`-- 57 sites in 31 files; --sites to list them all, or --in/--outside to narrow`. `--sites` lists every site.

    scripts/outline uses Sym[,Sym…] [--in PREFIX] [--outside PREFIX] [--sites] [--max-kb N] [--root DIR]

**area**: To get a map of a part of the project: the areas `.outline.conf` declares, at level 0 one
line per package, at level 1 (the default) each definition with its doc's first sentence. With no
name, the declared areas, each with its purpose. A name no area declares, such as a module's (`area slack`),
gives the one area whose symbols lie under a package of that name, or else every main package of that
name (`area admin`: `grit.core.admin`), under a note saying which; several areas naming it are listed.

    scripts/outline area [name[,name…]] [--level 0|1] [--max-kb N] [--root DIR]

**find**: To find a definition when you don't know its name: one line per definition in main
sources, members and constructor vals included, as its full name, file, lines and collapsed signature. The pattern's
dot-separated segments match the last segments of a full name, each whole, `*` standing for any run
of characters within one segment: `find '*Shelf*'`, `find '*.placed'`, `find 'Moves.a*'`. When main
sources hold no match, test sources' matches follow a `-- no definition in main sources matches` note;
when neither does, a renaming import giving the name (`import a.X as Y`, or a braced selector `X as Y` or `X => Y`,
read from source text) is named, with `X`'s definition. A pattern whose owner is a method (`Read.read.*`) says `find`
lists no definition local to one, and names the `show` that prints its body, for each method whose body holds a word the
last segment matches (so `*.setLabel*` names only the methods that mention one); those notes are cut at `--max-kb` like matches. `--private` lists private members too (a
definition lent to the top package, `private[grit]`, counts as public; one lent to a narrower package as private); without it, a pattern only private names match says how many, in a
`-- N private matches: add --private` note. A `show`
that matches nothing suggests `find` with the name's last segment.

    scripts/outline find pattern [--private] [--max-kb N] [--root DIR]

**tests**: To add or change a test in a suite: its file's imports and its own loose lines as file text, its
head and closing lines as file text, its helpers' signatures, the test-source fixtures it uses with the members it
names, and its tests' names with line ranges. A fixture is a top-level definition in test sources, in another file,
that the suite's TASTy refers to; they print under one `fixtures` header, a line each naming its file, its ranges
(a class and its companion on one line) and the members the suite names. `--test NAME` prints each test whose name
starts with NAME, or with any of its comma-separated prefixes when NAME as written starts none; `--body h[,h…]` prints a helper's own lines, or, when no helper has the name, a used fixture
member's; `--fixtures` lists the used members' signatures. What is named is printed first when the answer is cut.

    scripts/outline tests Suite [--test NAME] [--body h[,h…]] [--fixtures] [--max-kb N] [--root DIR]

**file**: To edit a file's imports or place a new definition: its loose lines (package, imports, comments
outside every definition) as file text, each top-level definition and its members as `NN-MM kind name [N KB]`
lines, and its last lines under `NN-MM end of file`, file text. It takes a repo-relative path, a path suffix only
one file ends with, or a definition's name (main sources, then test sources); several matching files are NoMatch
naming each, and a file with no `.tasty` fails naming the command that compiles it.

    scripts/outline file Path|Name [--max-kb N] [--root DIR]

MCP tool names: `show`, `family`, `uses`, `area`, `find`, `tests`, `file`.

File text: in show and family, each definition's lines after its NN-MM header are the file's own lines, unchanged: its doc and signature, and with --body its body (family: with --member and --body). A header is `NN-MM <kind> <Owner.name>  [N.N KB]`, or `NN-MM  [N.N KB]` alone when the definition is top-level or shown under its type and the file text under it shows its kind and name (`def at`); a member shown alone (show Owner.member, family --member) keeps its name. A class, trait, object or enum printed without its body ends, after its members' entries, with its closing lines under an `NN-MM end of <name>` header line: its last line, or its last two when the last holds only closing brackets; those lines are file text, for an Edit that adds a member before them; a type whose head already reaches its last line has none, and no other definition has any. In show, a member of a shown type not named in --body has its doc cut to its first sentence, unless --docs: a doc whose first sentence is its whole first line prints that line, file text, then a `-- … N more doc lines` line; any other cut doc prints as one `-- <first sentence> …` line in its place; a doc of one line prints whole; the member's lines after its doc, from its signature on, are file text. In tests, the lines under each bare `NN-MM` line (the file's and the suite's loose lines, and the suite's head) and under `NN-MM end of <Suite>` are the file's own lines, and with --test each printed test is. In file, the lines under each bare `NN-MM` line (loose lines, outside the definitions there) and under `NN-MM end of file` are the file's own lines; its `NN-MM <kind> <name>` lines are not. Not file text: the ##, == and -- lines, the NN-MM header lines, the → lines, the [N KB] line, and tests' helper, fixture and test-name lines; a helper's or a fixture member's --body lines are file text. A size tag, `[N.N KB]` at the end of a definition's header or of a line naming one, is the bytes of the definition's whole text, doc included (what --body prints for it), to one decimal and at least 0.2; no file-text line carries one, and a definition past the end of its source has none. A family line that gives an implementation, a contract or a member its collapsed signature after the NN-MM header is not file text. An Edit may take file text as old_string without a Read first; Read the line range only when an Edit fails to match. Exceptions: a file tagged [stale: ...] may print lines that no longer match its source, and an answer that read any such file says so after its ## root line, in a `-- stale since the last compile:` line naming each module, its stale files and the command that compiles them; a file is tagged when it changed since its .tasty, as the number and length of its lines tell, so an edit that keeps every line's length is not tagged though its lines may not match; a --max-kb budget cuts whole entries, never inside one; byte-exactness is tested for LF files only.

## Running it

- `scripts/outline <query> …` runs a query from the current checkout.
- `scripts/outline mcp` runs a stdio MCP server. Client configuration, one entry:

      {"outline": {"command": "<checkout>/scripts/outline", "args": ["mcp"]}}

Builds are kept in `${XDG_CACHE_HOME:-~/.cache}/grit-outline/<key>`, one per version of outline's
sources: the key hashes every file under `dev/outline`, the `build.mill` lines that define the module
and `.mill-version`. Every checkout on the same outline code runs the same build, and a branch that
changes outline gets its own. A key with no build is built on first use, in the checkout, without a
daemon in its own Mill output directory (`out/outline`, so a running `./mill` daemon is never
disturbed), then kept; the build first says so on stderr, in one `outline: building` line. A kept
build is never rewritten, so neither a rebuild nor `./mill clean` reaches it. A key unused for 7 days
is removed.

An MCP server runs from its own copy of a kept build, in `grit-outline/mcp/<pid>`. When its key has
no build yet, it starts at once from the last build used, since a client waits about 30 s for it to
answer, and its own builds in the background for the next start; with none to start from, it builds
first and says so on stderr. `grit-outline/mcp.log` keeps when and by whom each server was started,
its stderr, how it ended, the background builds and the keys removed.

A request the server fails on gets a JSON-RPC error (-32603) naming the failure, and the server goes on
serving. Three requests in a row that fail to link, its classes gone or changed under it, end it; it
says on stderr why it ends, then or when stdin closes.

## Roots and freshness

- Every answer starts with a `## root` line naming its root, its branch and its HEAD's first
  8 characters, but for the MCP note below. Read it first: it says which checkout the answer describes.
- `--root DIR` picks the root; without it, the repository above the working directory. An MCP
  server's calls use its start-up root unless a call names one. Once a call has named another root,
  an answer to a call naming none opens, before its `## root` line, with a line naming the roots named
  so far, since one server answers every agent of a session and the caller may be working in one of them:
  `-- no root given, so this answers for the default checkout, <dir>, not <other>, which an earlier call named; if your work is there, pass root "<other>"`,
  or with several, `not <a> or <b>, which earlier calls named; pass as root the one your work is in`.
- A query reads only that root's build output, and refuses any TASTy whose sources lie outside
  the root.
- A source that changed since its `.tasty` was compiled is flagged as stale: the definition reflects
  an earlier compile. A source no newer than its `.tasty` is fresh. A newer one is fresh when its
  lines are as many and as long as the `.tasty` records, so a checkout, rebase or stash that rewrites
  a file with the same content leaves it fresh; it is judged against the `.tasty` compiled from it,
  not another file's in its package. **The known gap:** an edit that keeps every line's length, such
  as changing one digit, reads fresh until the next compile.
  The answer then says so on its second line, naming each module whose stale files it read, at most three
  file names a module and `+N` for the rest, and the command that compiles them:
  `-- stale since the last compile: grit.core (Label.scala, Tx.scala), grit.dbos (A.scala, B.scala, C.scala +2): ./mill grit.core.compile + grit.dbos.compile`.
  Only the files the query read are checked: a look at each of their sources, and a read of its
  package's `.tasty` headers only for a source newer than one of them. The tags and the line judge alike.
  A definition that ran past where its source now ends is shown by the compiler's signature, on a
  `-- not file text` line, with its lines as compiled and no doc or body.
- A `.tasty` the inspector cannot read, such as one left by a test source whose last compile failed, or one a
  build deleted while the query read its package, fails no query: it is left out, the rest are read, and the answer names each such file it needed in one `-- not loaded:`
  line, with the inspector's first error as `File.scala:line: message` (or the first line of its message, colour codes
  stripped), then the command compiling their modules:
  `-- not loaded: p/User.tasty (User.scala:2: undefined: new p.World); compile to read them: ./mill m.compile`.
  It is not read again until it changes.
- Nothing compiles on its own. With no compiled classes, the answer says how to compile the
  root, and the tool does not run the build.

## Configuration: `.outline.conf`

The file sits at the repository root. Lines starting with `#` are comments.

    hide vendor/generated
    secondary tools/
    area storage "Where records are kept.": com.example.store.Store com.example.store.Tx family:com.example.store.Repository
    test-call test

- `hide <path>`: source paths no query shows unless it names them fully.
- `secondary <path>`: main source paths a short name rarely means, such as tooling or a harness; a short name
  with one candidate outside them and outside test sources means that one (Queries, show). A plain prefix:
  end it with `/` to stop at a directory.
- `area <name> "<purpose>": <symbols…>`: a named set of symbols, and `family:<Trait>` entries for a
  trait's implementations and contracts; the quoted purpose, optional, is what `area` with no name
  lists.
- `test-call <name>`: the call `tests` reads test names from, as in `test("name") { … }`.

A line opening with one of these words but not in its form is refused, named by its line number in the error. A line
opening with any other word, a setting a newer outline may know, is ignored, and every answer names it after its
`## root` line: `-- .outline.conf lines this outline does not know, ignored (a newer outline may read them): line 2: …`.

## Layout

One-way dependency order: model ← locate ← read; model ← trace ← render; all ← query ← cli, mcp.

- `model`: definitions, line ranges, uses, test cases, staleness.
- `locate`: a root's build output and sources, what a `.tasty` records of its source, and each checkout's branch and HEAD.
- `read`: TASTy files into definitions, each with its source slices.
- `trace`: the repository types reached from named roots within a number of hops.
- `render`: the text of a family and of a traced type.
- `query`: the queries, their caches, and the `.outline.conf` reader.
- `cli`: argument parsing and subcommands.
- `mcp`: the JSON-RPC server over stdio, exposing the same queries as tools.

`locate.Layout` is the trait where another build tool plugs in. It says where compiled classes
and library jars are, what to tell a person when nothing is compiled, which directories
hold tests, what a person calls the module a classes directory belongs to, and the command that
compiles modules. Mill's implementation ships; another build tool supplies its own methods.
