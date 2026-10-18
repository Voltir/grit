# grit.knowledge

Cards: one short document per unit of a source (a repository's files and directories), each a
summary with search terms and a pointer (ADR 0028) to the hosted call that reads the unit, so a
window can show what a source holds without holding it. Names `grit.core` alone: a source is
read through the MCP tools a deployment offers, never a client of its own.

Strand documents: by a second plugin in the same module, one short document per strand of a
room's threads (ADR 0023), summarised from what its members said, so a window and the
relevance judge see a topic spread over threads as one thing.

Packages, in one-way order (each names only those before it):

- **`card`**: what a card is, whatever its source: a unit's key (the SHA-256 of its corpus and
  path), its version (a git SHA, or when a file last changed), its title, its summary, and its
  document's key, text and data; and the names of a sync run's moves (`{phase}/{key}`, `held`,
  `drop`).
- **`sync`**: a sync run over any source. A `Source` says how a run reads it: the tool and
  arguments of each list and read, what an answer holds, what of a listing is checked, and its
  `Walk`, the run's plan, which is pure: from the cards a run holds and each step's outcome, the
  next step (list, read, summarise, write, relabel, and a last drop). `Walking` is every
  source's walk: it reads, summarises, writes, relabels, tallies and ends, while a source's
  `Walking.Agenda` says what each of its tasks does, which tasks left untaken count, and what
  it drops. `Sync`, the keeping job `knowledge-sync`, makes each step as a move: a call, a
  summary ask, a check, or a keep; a card held at another label is withdrawn in the keep that
  replaces it. A listing or a file answered
  below `Outside` is checked by the integrity tree before anything is made from it (ADR 0036),
  since a card is kept only from `Outside` or above. Imports `card`.
- **`github`**: a repository as a card source, read through GitHub's MCP server's
  `get_file_contents`, and its walk (a `Walking` agenda), depth first from the repository's
  root. A unit's pointer is `{server}_get_file_contents` with `owner`, `repo` and `path` and no
  `ref`. A file's card is written only at its blob SHA; a file answered without one is not
  carded. Imports `card` and `sync`; another source is a package beside it, and neither names
  the other.
- **`drive`**: Drive's shared drives as a card source, read through grit's own Drive tools
  (`drive_list`, `drive_read`) by the facts their answers carry, which it decodes itself since
  it may not name grit.mcp; and its walk (a `Walking` agenda), the drives, each drive and the
  folders directly in it. A source declares a chain of labels, one run per label, and a file is carded by the run
  of the least label dominating what its listing was read at. A file's pointer is `drive_read`
  with its id; its card is written at when it last changed, titled with its name. Imports
  `card` and `sync`, never `github`.
- **`strand`**: a strand's document (ADR 0042), one per strand root of three threads or more
  with enough said, kept at its room's label: its key, text and data, and `Dream`, the
  dreaming job `strands-dream`, which scans the strands of its run's rooms by ids and counts,
  has core summarise each changed one from what its members said, and keeps or withdraws
  their documents. Its code never holds a message's text. Imports `card`.
- **`plugin`**: `Knowledge`, the plugin over a deployment's sources: its documents' terms, the
  `Sync` job, and each source's sync schedules (the keys in `Knowledge`'s doc); and
  `StrandDocuments`, the strands plugin: the `Dream` job on a daily schedule per label it is
  declared at, and its documents' terms. Imports `github`, `drive`, `sync` and `strand`.

## What a deployment declares

A deployment builds each source with `GitHubCards.of` (its corpus, which names it and supplies
its service; the name the deployment gave GitHub's MCP server; the repository, ref and what of
it becomes cards, and those files in a few plain words, which its directories' cards say they
hold; at most how many cards; its label; the place its cards are kept at, drawn on
where a conversation's scope holds it; and its `SyncSchedule`s), or with `DriveCards.of` (the
same, but Drive's two tools, and a chain of labels in place of the repository and one label),
and the plugin with
`Knowledge.of(name, sources, visibility)`, under the visibility it declares. It also serves an
edge hosting the MCP server's `get_file_contents` at the source's service, or every run's first
call fails and its reply says so. The reference deployment's `GRIT_PLUGINS=knowledge` is one
such declaration (`deployments/app/README.md`).

## The label rule

A source is kept at one label: the one it declares, or its service place's when it declares
none. It may be above the service's (a confidential repository behind an internal service),
never below, and never above what the deployment clears the service for: `Knowledge.of`
refuses either, naming the source, since its runs could then not read the service or send it
a call. Its schedules run at that label, so a run reads and keeps at it, and every answer a
run's calls and asks bring back is at it: no card is kept where the next run cannot read it
back. Whether a turn reads a card is its label's business alone: a turn in a room below the
source's label holds none of its cards.

A Drive source is kept at each of its labels, and every one of them is held to that rule: each
declares its own schedules, whose runs card what was read at most at that label and above the
one below it.

## Data handling

A sync run sends the head of each file it summarises (up to 8 KB) to the catalog's summary
model, a third party, whatever the source's label. Declaring a source is the deployment's
consent for that source's included files. A turn that reads the same file already sends all of
it to the reply model. For a source whose results are believed below `Outside`, a run also
sends each listing's paths and each file's head to the deployment's classifier, when it is
cleared for the source's label, to check them.

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

`test/resources/drive/facts/` holds the facts Drive's tools answer, as a reader keeping every
part states them (`{"answer"?:…,"parts":[…]}`, the keys `grit.mcp.drive.DriveFacts`'): the shared
drives (`drives.json`), a drive's and a folder's listing (`listing-drive-a.json`,
`listing-folder-3.json`) and a text file's read (`read-file-text.json`). Knowledge may not name
grit.mcp, so `grit.app`'s `DriveFactsPinTests` holds them equal to what grit.mcp answers over
its recordings.
