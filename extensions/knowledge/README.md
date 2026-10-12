# grit.knowledge

Cards: one short document per unit of a source (a repository's files and directories), each a
summary with search terms and a pointer (ADR 0028) to the hosted call that reads the unit, so a
window can show what a source holds without holding it. Names `grit.core` alone: a source is
read through the MCP tools a deployment offers, never a client of its own.

Packages, in one-way order (each names only those before it):

- **`card`**: what a card is, whatever its source. `CardKey`, a unit's key (the SHA-256 of its
  corpus and path), `UnitVersion` (a git SHA), `CardSummary` and its reply (`CardSummary.Reply`,
  the schema a summary ask is sent), `Card` (a unit's path, kind, version, summary and pointer,
  and a directory's `Scope`, what of its source it was made under, as its document's key, text
  and data), and `CardMoves`, the names of a sync run's moves
  (`{phase}/{key}`, `held`, `drop`).
- **`github`**: a repository as a card source, read through GitHub's MCP server's
  `get_file_contents`. `GitHubCards` (the repository, what of it becomes cards, its label and
  its `SyncSchedule`s, and its `scope`; a unit's read call and its pointer, `{server}_get_file_contents` with
  `owner`, `repo` and `path` and no `ref`), `Listing` (a directory's answer: its entries with
  their SHAs, or `Partial` when cut or naming possible path matches) and `Fetched` (a file's
  answer: its text and blob SHA, the only version a file's card is written at, or
  `Unversioned`, saying why). Imports `card`.
- **`sync`**: a sync run. Its plan, pure: `Walk`, from the cards a run holds (`Held`, each read
  back from its document by `Held.read`) and each step's `Outcome`, the next `Step`: list, read,
  summarise, write, relabel, and a last drop; `Synced`, what the run came to, and its reply line;
  `SyncParams`, what a run is told. And `Sync`, the keeping job `knowledge-sync`, which makes
  each step as a move named by `CardMoves`: a call, a summary ask, or a keep, a card held at
  another label withdrawn in the keep that replaces it. Imports `card` and `github`.
- **`plugin`**: the plugin a deployment turns on. `CardSource`, where cards come from (a
  repository, `GitHubCards`), and `Knowledge`, the plugin over a deployment's sources: its
  documents' terms, the `Sync` job, and each source's sync schedules, keyed `{source}-{key}`.
  Imports `github` and `sync`.

## What a deployment declares

A deployment builds each source with `GitHubCards.of` (its corpus, which names it and supplies
its service; the name the deployment gave GitHub's MCP server; the repository, ref and what of
it becomes cards; at most how many cards; its label; the place its cards are kept at, drawn on
where a conversation's scope holds it; and its `SyncSchedule`s), and the plugin with
`Knowledge.of(name, sources, visibility)`, under the visibility it declares. It also serves an
edge hosting the MCP server's `get_file_contents` at the source's service, or every run's first
call fails and its reply says so. The reference deployment's `GRIT_PLUGINS=knowledge` is one
such declaration (`deployments/app/README.md`).

## The label rule

A source is kept at one label: the one it declares, or its service place's when it declares
none. It may be above the service's (a confidential repository behind an internal service),
never below, and never above what the deployment trusts the service with: `Knowledge.of`
refuses either, naming the source, since its runs could then not read the service or send it
a call. Its schedules run at that label, so a run reads and keeps at it, and every answer a
run's calls and asks bring back is at it: no card is kept where the next run cannot read it
back. Whether a turn reads a card is its label's business alone: a turn in a room below the
source's label holds none of its cards.

## Data handling

A sync run sends the head of each file it summarises (up to 8 KB) to the catalog's summary
model, a third party, whatever the source's label. Declaring a source is the deployment's
consent for that source's included files. A turn that reads the same file already sends all of
it to the reply model.

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
