# outline

A command-line tool and MCP server that answers questions about a Scala 3 codebase from its
compiled TASTy. Signatures, Scaladoc and resolved references come from the compiler's output,
so names resolve the way the compiler resolves them. Bodies and docs are source text sliced at
TASTy's positions, so an answer can be used as edit input. Answers are grouped by file, with
line ranges taken from the Scaladoc.

## Queries

Each query has a CLI form and an MCP tool of the same name, taking the same arguments.
`scripts/outline <query> --help` prints that query's full help: its examples and every flag.

**show**: To read or call a definition: its signature and Scaladoc, with one-line outlines of the
project types it names. Bodies only for members named with `--body`.

    scripts/outline show Sym[,Sym…] [--depth 0|1|2] [--body m[,m…]] [--private] [--cap BYTES] [--root DIR]

**family**: To implement or change a trait: its implementations, contracts and their suites.
`--member` narrows each to one member.

    scripts/outline family Trait [--member m] [--body] [--cap BYTES] [--root DIR]

**uses**: To find who calls or names a definition before changing it. `--in` and `--outside`
filter by path prefix.

    scripts/outline uses Sym[,Sym…] [--in PREFIX] [--outside PREFIX] [--cap BYTES] [--root DIR]

**area**: To get a map of a part of the project: the areas `.outline.conf` declares, at level 0 one
line per package, at level 1 (the default) each definition with its doc's first sentence.

    scripts/outline area name[,name…] [--level 0|1] [--cap BYTES] [--root DIR]

**tests**: To add or change a test in a suite: its helpers and its tests' names with line ranges.
`--test NAME` prints the verbatim body of each test whose name starts with NAME.

    scripts/outline tests Suite [--test NAME] [--cap BYTES] [--root DIR]

MCP tool names: `show`, `family`, `uses`, `area`, `tests`.

File text: in show and family, each definition's lines after its NN-MM header are the file's own lines, unchanged: its doc and signature, and with --body its body (family: with --member and --body). In tests --test, each printed test is the file's own lines. Not file text: the ##, == and -- lines, the NN-MM header lines, the → lines, the [N KB] line, and tests' helper and test-name lines. A family line that gives an implementation, a contract or a member its collapsed signature after the NN-MM header is not file text. An Edit may take file text as old_string without a Read first; Read the line range only when an Edit fails to match. Exceptions: a file tagged [stale: ...] may print lines that no longer match its source; a cap cuts whole entries, never inside one; byte-exactness is tested for LF files only.

## Running it

- `scripts/outline <query> …` runs a query from the current checkout.
- `scripts/outline mcp` runs a stdio MCP server. Client configuration, one entry:

      {"outline": {"command": "<checkout>/scripts/outline", "args": ["mcp"]}}

The launcher is built on first use and rebuilt when a source under `dev/outline/src` is newer.

## Roots and freshness

- Every answer starts with a `## root` line naming its root, its branch and its HEAD's first
  8 characters. Read it first: it says which checkout the answer describes.
- `--root DIR` picks the root; without it, the repository above the working directory. An MCP
  server's calls use its start-up root unless a call names one.
- A query reads only that root's build output, and refuses any TASTy whose sources lie outside
  the root.
- A source newer than its `.tasty` is flagged as stale: the definition reflects an earlier compile.
- Nothing compiles on its own. With no compiled classes, the answer says how to compile the
  root, and the tool does not run the build.

## Configuration: `.outline.conf`

The file sits at the repository root. Lines starting with `#` are comments.

    hide vendor/generated
    area storage: com.example.store.Store com.example.store.Tx family:com.example.store.Repository
    test-call test

- `hide <path>`: source paths no query shows unless it names them fully.
- `area <name>: <symbols…>`: a named set of symbols, and `family:<Trait>` entries for a trait's
  implementations and contracts.
- `test-call <name>`: the call `tests` reads test names from, as in `test("name") { … }`.

A bad line is named by its line number in the error.

## Layout

One-way dependency order: model ← locate ← read; model ← trace ← render; all ← query ← cli, mcp.

- `model`: definitions, line ranges, uses, test cases, staleness.
- `locate`: a root's build output and sources, and each checkout's branch and HEAD.
- `read`: TASTy files into definitions, each with its source slices.
- `trace`: the repository types reached from named roots within a number of hops.
- `render`: the text of a family and of a traced type.
- `query`: the five queries, their caches, and the `.outline.conf` reader.
- `cli`: argument parsing and subcommands.
- `mcp`: the JSON-RPC server over stdio, exposing the same queries as tools.

`locate.Layout` is the trait where another build tool plugs in. It says where compiled classes
and library jars are, what to tell a person when nothing is compiled, and which directories
hold tests. Mill's implementation ships; another build tool supplies its own four methods.
