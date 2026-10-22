# grit.mcp

Implements `ServedEdge`, serving a server's tools as `Hosted` (what a deployment can
supply: [`docs/extending.md`](../../docs/extending.md#extension-points)).

grit's client of Model Context Protocol servers, at revision 2026-07-28 over Streamable
HTTP: no handshake and no session, each request a POST of its own carrying its protocol
version. A server's tools are offered to a turn by an edge hosting them at a service place
(ADR 0017), and only those the server marks read-only. A shipped extension (ADR 0021): it
names a generic protocol and no organisation, depends on `grit.core` and `grit.edge`, and is
named only by a deployment (`mcp-names-core-alone`). The JDK HTTP client's second egress
beside `grit.models` (STYLE rule 8); neither names the other.

It also holds one client that is not MCP's: Drive's REST API, until Drive's MCP server serves a
service account (`drive`, below), reusing `client`'s credentials.

In dependency order:

- **`wire`**: the protocol over ujson, pure: the requests grit sends, the results and errors
  read back, a response in an event stream, a call's result as the text the model reads, and
  `McpCalled`, the exchange a server's `Accounts` reads for its items (ADR 0036). Imports
  nothing in mcp.
- **`scope`** — a server's reach, pure: `McpScope`, which of its tools are offered, which
  calls are sent and which of their answers' results are shown, `Open` or held `Within` exact
  argument `Bound`s, so a deployment bounds a service at the edge rather than by asking the
  model (ADR 0017). A tool whose answers an `Attribution` places is offered and its results
  outside every bound withheld; an answer it cannot place is not shown at all. Imports `wire`.
- **`github`** — GitHub's side of a scope: `GitHubScope`, a repository or an owner as the
  `Bound` GitHub's tools name it, and a scope within them whose search tools are held by the
  repository each result names; and `GitHubItems`, repositories and owners as places under the
  service's and the `Accounts` reading each item of its issue, pull-request and file tools at
  its repository by its author's `author_association`, so a deployment's `Cap` on a repository
  caps them. It names a service, no organisation; which repositories, and at what, is the
  deployment's to say. Imports `scope`; nothing else in mcp imports it
  (`mcp-generic-names-no-github`).
- **`client`**: a server a deployment declares, reached over HTTP, each request a POST of its
  own held to its scope, carrying the token its `Credential` gives: a static token, or a
  service account's, got by a JWT its key signs and kept until near expiry; the secret is
  read from the environment, or a service account's key from the file a variable names
  (`KeyFiles`), and never shown. A tool that names its subject but not where it
  lies is declared with lookups, read-only calls grit makes first with the same argument,
  whose answers the server's `Accounts` read beside the call's. The JDK HTTP client lives
  here alone. Imports `wire` and `scope`.
- **`drive`**: Drive over its v3 REST API, read-only: a file's id as Drive writes it, what
  Drive says of files and their grants, `DriveRest` (a search whose query grit builds from the
  words asked, a listing of one folder's or drive's own files, a file and when it last changed,
  its grants, a Doc's, a Sheet's or a text file's text, and each failure
  as a `DriveError` whose words name no setting, its cause for the operator's log alone), how
  a deployment's Drive is read (`DriveReading`: its people's domain, its own address, its
  declared writers, and how a person names a folder in a command, by its link or id), and
  `DriveItems`, each hit, listed file and read a part at the folder or drive it lies within (a
  folder or drive itself at the service's place), a hit naming its shared drive as listed, a
  read believed at the meet over its writers (ADR 0039), each part's text rendered from its
  `DriveFacts`, the data it carries beside it and the one home of their keys. `DriveEdge` serves `drive_list`,
  `drive_search` and `drive_read` at the service's place, its tools telling
  the operator's log what the model is not. `drive_read` reads a Doc, a Sheet or a text file
  whole, as Drive exports it, and any file Drive keeps the bytes of (a PDF, a Word or an Excel
  file) by the deployment's formats (ADR 0044), from a copy kept only for that read, a part at
  a time: its start and outline, a run of pages, a section by its heading, or from an offset. Imports `wire` and `client`; nothing else in mcp imports it
  (`mcp-generic-names-no-github`).
- **`edge`**: `McpEdge`, the `ServedEdge` a deployment declares: it registers the service place,
  advertises there (with the instructions the deployment declared for the place, if any, as its
  Place layer), and runs each request by calling its server. A tool whose schema declares `to`
  (the argument core keeps for a writing tool's destination) is left out. Imports `wire` and
  `client`.

The test tree mirrors it. `client`'s and `edge`'s tests run against `FakeMcpServer`, an MCP server in the
test JVM (`com.sun.net.httpserver`) that checks every request as the spec's server rules say;
a service account's exchange runs against `FakeTokenServer`, a token endpoint held to what
Google documents of it, and not yet to Google's own.
`McpServerContract` is the exchanges a server must answer as the client reads them; the fake
keeps it in the unit tier (`McpServerFakeTests`), and `LiveProbe`, a main in test sources that
needs a token and so is no tier, runs it against GitHub's (its doc says how). It passed whole
there on 2026-09-30 and again on 2026-10-16, so the fake is held to a real server as of that
run, and between runs to the spec and go-sdk's source. `GitHubItemsContract` is what
`GitHubItems` needs of GitHub's answers: the recordings below keep it in the unit tier
(`GitHubItemsRecordedTests`), and `LiveProbe` runs it too; it passed whole there on 2026-10-16.
`DriveRestContract` is what `DriveRest` needs of Drive's answers; `FakeDriveServer`, Drive's
API in the test JVM answering from the recordings below, keeps it in the unit tier
(`DriveRestRecordedTests`), and `LiveProbe` runs it against Drive when a service account's key
is given (`DriveLive` finds the files it reads); it has not yet been run there. `DriveCapture`, a main beside it that needs a service account's key, records Drive's v3 REST
answers under `test/resources/drive/`, each written with `Neutral`'s stand-ins for its ids,
addresses, names and text, never as it came (its doc says how to run it). Drive's MCP server
answers a service account's `tools/list` but refuses each of its `tools/call`s ("The caller does
not have permission", 2026-10-16), so Drive is read over REST.

`test/resources/github/tools-list.json` is the `tools/list` result GitHub's hosted read-only
server (`https://api.githubcopilot.com/mcp/readonly`) answered on 2026-09-30, its icons
removed and nothing else changed, so the reader and the header mirroring are tested on the
entries a real server sends, `x-mcp-header` annotations included. Beside it,
`get-file-contents.json` is the `tools/call` result the same server answered the same day
(in an event stream) to `get_file_contents` for a private repository's README, its envelope
kept as sent and its icons removed; the file was replaced by a short neutral text, the SHA in
its text block by that text's git blob SHA, and the resource's `repo://` URI's owner,
repository and commit by `octocat/Hello-World` and a placeholder. `search-issues.json`,
`search-pull-requests.json` and `search-code.json` are the `tools/call` results the same server
answered on 2026-10-01 (in an event stream) to `search_issues`, `search_pull_requests` and
`search_code`, their envelopes kept as sent and their icons removed. The issue and pull-request
searches read a private repository: in each, the second of the three results was moved to a
made-up repository (`elsewhere-org/elsewhere`) and the others to `octocat/Hello-World`, in
`repository_url` and every URL; titles, bodies, ids and node ids were replaced by neutral
values and the author by `octocat`; `total_count` was set to 12; keys and their order are as
sent. The code search read a public repository, `github/github-mcp-server`, and is as sent but
for its second result's `repository`, set to `octocat/Hello-World`. The answers of the read
tools whose items `GitHubItems` reads were answered on 2026-10-16 (in an event stream) by the
same server, for a public repository: `issue-read.json` (`issue_read` get),
`issue-read-comments.json`, `issue-read-labels.json`, `pull-request-read.json`
(`pull_request_read` get), `pull-request-read-comments.json`, `-reviews.json`,
`-review-comments.json`, `-diff.json`, `-files.json` and `-status.json`, `list-issues.json` and
`list-pull-requests.json`. Each is redacted the same way: the repository moved to
`octocat/Hello-World` in every URL and name; titles, bodies, descriptions, diffs, paths, ids,
issue numbers, SHAs and cursors replaced by neutral values; every person's login `octocat`,
id 1; a bot's login (`dependabot[bot]`, `copilot-pull-request-reviewer[bot]`) kept; the
longest lists cut to their first entries; `author_association` and every key and its order as
sent, and `structuredContent` the text block's object, as the server sends it. They show which
answers name an author's association (an issue, its comments, a pull request's comments and
reviews, the searches' results) and which do not (a pull request, the lists, review comments),
and that only the searches name a user's `type`. `test/resources/toolsnaps/`
holds GitHub's write tool `issue_write`, which that server does not list, as its source lists
it: https://github.com/github/github-mcp-server at `5a1a3866d4a681d771162d35680d6c7219eac3b0`
(MIT). The source's snapshots lack the hosted server's annotations, so they are not kept for
its read tools.

`test/resources/drive/` holds what Drive's v3 REST API answered `DriveCapture` on 2026-10-16,
for a service account shared into two shared drives, every request with the fields the reader
asks for: `drives-list.json`, and `drive-a.json`/`drive-b.json` (`drives.get`);
`search-folders.json`, `search-files.json` and `search-fulltext.json` (`files.list` over every
drive: the folders, the other files, and `fullText contains`); `metadata-*.json` (`files.get` of
a drive's top file, a folder's file, a folder, and each drive's id, which Drive answers as a
folder with no parents and one generic name for both); `permissions-drive-a.json` and
`-b.json` (`permissions.list`, one page each); `export-doc.txt` and `.md` (one Doc exported as
`text/plain`, with its byte-order mark, and as `text/markdown`) and `export-sheet.csv` (a
Sheet's first sheet); and `error-export-folder.json` (a folder's export, 403
`fileNotExportable`) and `error-no-file.json` (an id naming nothing, 404 `notFound`). Each was
written through `Neutral`, never as it came: ids as `drive-a`, `folder-1` and `file-1`, a drive
named so only when listed; names as `name-1`; people as `person-1@example.com`, the account
itself as `self@project.iam.example` and another service account as
`writer-bot-1@project.iam.example`; every word of text, a message included, as `text` or
`message-1`, its punctuation and line breaks kept; lowercase single words at other keys (a
reason, a role, a type) and every key and its order as sent. They show that every item names
its drive, that a shared drive's top files have the drive's id as their only parent, that
folders carry `inheritedPermissionsDisabled` and grants `permissionDetails.inherited`, and that
the account's own address is among a file's grants. The capture came before the reader asked
for `modifiedTime`, which Drive gives every file, and `size`, which it gives a file it keeps the
bytes of: each recorded file's was added by hand, a stand-in, the `made-*` file's where one lists it (`DriveCaptureTests` checks that every
recorded file holds each field Drive gives it).

Beside them, the `made-*` files are written by hand, since the recordings hold no text file:
`made-file-text.json`, a text/plain file in `folder-3`;
`made-file-text.txt`, its content, starting with a byte-order mark;
`made-permissions-file-text.json`, grants of its own, a writer outside the domain among them;
and `made-children-drive-a.json` and `made-children-folder-3.json`, the listing of each one's own
files and folders (`'{id}' in parents`, no words), the recorded entries lying there, folders
first and then by name, each with a `modifiedTime` of its own.
