# grit.knowledge

Cards: one short document per unit of a source (a repository's files and directories), each a
summary with search terms and a pointer (ADR 0028) to the hosted call that reads the unit, so a
window can show what a source holds without holding it. Names `grit.core` alone: a source is
read through the MCP tools a deployment offers, never a client of its own.

Packages, in one-way order (each names only those before it):

- **`card`**: what a card is, whatever its source. `CardKey`, a unit's key (the SHA-256 of its
  corpus and path), `UnitVersion` (a git SHA), `CardSummary` and its reply (`CardSummary.Reply`,
  the schema a summary ask is sent), `Card` (a unit's path, kind, version, summary and pointer,
  as its document's key, text and data), and `CardMoves`, the names of a sync run's moves
  (`{phase}/{key}`, `held`, `drop`).
- **`github`**: a repository as a card source, read through GitHub's MCP server's
  `get_file_contents`. `GitHubCards` (the repository, what of it becomes cards, its label and
  its `SyncSchedule`s; a unit's read call and its pointer, `{server}_get_file_contents` with
  `owner`, `repo` and `path` and no `ref`), `Listing` (a directory's answer: its entries with
  their SHAs, or `Partial` when cut or naming possible path matches) and `Fetched` (a file's
  answer: its text and blob SHA, the only version a file's card is written at, or
  `Unversioned`). Imports `card`.

The test tree mirrors it. `test/resources/github/` holds `tools/call` results GitHub's hosted
read-only server (`https://api.githubcopilot.com/mcp/readonly`) answered on 2026-10-11 (in an
event stream) to `get_file_contents` for a public repository, `github/github-mcp-server`:
`get-file-contents-directory.json` (its `docs` directory, every field),
`get-file-contents-directory-git-url.json` (the same, asking for `type`, `path`, `sha` and
`git_url`), `get-file-contents-match.json` (a path naming no file, `testing.md`, answered with
the paths that end with it) and `get-file-contents-file.json` (the file `docs/testing.md`).
Each has its server's icons removed, the repository set to `octocat/Hello-World` and its commit
to a placeholder in every URL and URI; the file's text is replaced by a short neutral text, the
SHA in its text block by that text's git blob SHA, and its `structuredContent` (a copy of the
text) removed. Keys, their order and every other value are as sent.
