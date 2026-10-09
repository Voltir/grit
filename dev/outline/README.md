# outline

A command-line tool and MCP server that answers questions about a Scala 3 codebase from its
compiled TASTy. Signatures, Scaladoc and resolved references come from the compiler's output,
so names resolve the way the compiler resolves them. Bodies and docs are source text sliced at
TASTy's positions, so an answer can be used as edit input. Answers are grouped by file, with
line ranges taken from the Scaladoc.

## Queries

Each query has a CLI form and an MCP tool of the same name, taking the same arguments.

**show**: signatures with full Scaladoc for named symbols, plus one-line outlines of the
project types they mention. Bodies only for members named with `--body`.

    scripts/outline show Sym[,Sym…] [--depth N] [--body m[,m…]] [--private] [--cap BYTES] [--root DIR]

**family**: a trait or abstract class with its implementations, the abstract contracts whose
members name it, and the suites running each contract. `--member` narrows each to one member.

    scripts/outline family Trait [--member m] [--body] [--cap BYTES] [--root DIR]

**uses**: direct references to the named symbols, resolved by the compiler, one line per site
with its enclosing definition. `--in` and `--outside` filter by path prefix.

    scripts/outline uses Sym[,Sym…] [--in PREFIX] [--outside PREFIX] [--cap BYTES] [--root DIR]

**area**: the areas `.outline.conf` declares, at level 0 one line per package, at level 1
(the default) each definition with its doc's first sentence.

    scripts/outline area name[,name…] [--level 0|1] [--cap BYTES] [--root DIR]

**tests**: a suite's helpers and its test names with their line ranges. `--test NAME` prints
the verbatim body of each test whose name starts with NAME.

    scripts/outline tests Suite [--test NAME] [--cap BYTES] [--root DIR]

MCP tool names: `show`, `family`, `uses`, `area`, `tests`.

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
