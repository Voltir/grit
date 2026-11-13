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

It also holds the server's side of that wire, both eras of it (below, "Serving"): reading a
request a client posted and writing its answer, pure, for an edge that serves grit's own tools
over MCP; nothing here listens or runs a tool.

It also holds one client that is not MCP's: Drive's REST API, until Drive's MCP server serves a
service account (`drive`, below), reusing `client`'s credentials.

In dependency order:

- **`wire`**: the protocol over ujson, pure: the requests grit sends, the results and errors
  read back, a response in an event stream, a call's result as the text the model reads, and
  `McpCalled`, the exchange a server's `Accounts` reads for its items (ADR 0036). And the
  server's half: `Rpc.received`, a POST read as one request (`Posted`), only `tools/list` and
  `tools/call` left to the server, each answered on the case that asked it (`Listing`,
  `CallResult`); `McpTool.described`, a tool as a server lists it; `Headers.decoded`, a header
  value as `encoded` wrote it. `Code` is the one table of error codes both halves use. Its
  reading of a body as one message is a private function apart from the header checks, so a
  stdio server could share it. It names `grit.edge.http`'s data types (`Request`, `Response`,
  `Status`) and nothing else of the edge; imports nothing in mcp.
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
- **`client`**: a server a deployment declares and the transport it is reached by
  (`McpTransport`). Over HTTP, each request is a POST of its
  own held to its scope, carrying the token its `Credential` gives: a static token, or a
  service account's, got by a JWT its key signs and kept until near expiry; the secret is
  read from the environment, or a service account's key from the file a variable names
  (`KeyFiles`), and never shown. A tool that names its subject but not where it
  lies is declared with lookups, read-only calls grit makes first with the same argument,
  whose answers the server's `Accounts` read beside the call's. Over stdio, the server is a
  program core's `Processes` start, kept for later requests and started again after it ends;
  requests go one a line, one at a time, each answered within `McpClient.Timeout` of being asked
  (or `McpClient.ColdStart` of its program's start, when later), or the program is stopped and
  that request and those queued behind it fail. A server may name an argument grit sets on each
  call to the asking conversation's directory (`McpServer.directoryArgument`): it is left out
  of each listed schema, and a call that gives it, or comes from a conversation working in no
  directory, is not sent. The JDK HTTP client lives here alone. Imports
  `wire` and `scope`.
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
  the operator's log what the model is not. `drive_read` reads any file Drive keeps the bytes
  of (a PDF, a Word or an Excel file), and a Doc, a Sheet or a Slides deck as the office file
  Drive exports it to (Word, Excel, PDF) when the deployment's formats read that kind, by those
  formats (ADR 0044), from the file area (ADR 0045), a part at a time: its start and outline,
  a run of pages, a section by its heading, or from an offset; any other Doc or Sheet whole, as
  Drive's text of it. The area keeps the file under the call, so a call run again answers the
  same handle, which `file_read` reads further. A file refused for its own sake (a format would
  not read it, Drive flagged it or will not export it) is answered as unread, saying why, and
  not as a failure, so a reader (the knowledge sync) tells it from Drive or the area failing. Imports `wire` and `client`; nothing else in mcp imports it
  (`mcp-generic-names-no-github`).
- **`edge`**: `McpEdge`, the `ServedEdge` a deployment declares: it registers the service place,
  advertises there (with the instructions the deployment declared for the place, if any, as its
  Place layer), and runs each request by calling its server. A tool whose schema declares `to`
  (the argument core keeps for a writing tool's destination) is skipped alone where its
  server's list is read, and logged with the other skips. `McpEdge.local` is the same over a
  stdio program, for the process that runs a console (`Console.Edge`): it reads the list at
  start, refused when the program cannot be read or offers nothing, and learns each request's
  directory from its desk (`Desk.origin`). Imports `wire` and `client`.

**Serving.** `Rpc.received` reads one POST in the revision it declares; the wire shapes are the 2026-07-28
schema's and the 2025 revisions' (the project's notes on them, read from the official pages).
It lists 2026-07-28, 2025-11-25 and 2025-06-18: 2025-03-26 and older require a server to take
batches, which this one refuses.

- **Stateless** (2026-07-28) when the request's `params._meta` holds
  `io.modelcontextprotocol/protocolVersion`, or its `MCP-Protocol-Version` is `2026-07-28`. It
  is checked in go-sdk's order, the first broken check answered: the header present (`-32020`),
  `_meta`'s version present (`-32602`), the two equal (`-32020`), `Mcp-Method` the method
  (`-32020`), for `tools/call` `Mcp-Name` (decoded) `params.name` (`-32020`), `_meta`'s
  `clientCapabilities` an object (`-32602`), the version 2026-07-28 (`-32022`), each 400; then
  the method: `server/discover` answered, `tools/list` and `tools/call` to the server, any other
  (`initialize`, `ping` and `subscriptions/listen` among them) 404 `-32601`. A result carries
  `resultType: complete`; discover's and a list's say `ttlMs: 0` (the set changes with a
  deploy, and a loopback list is cheap) and `cacheScope: private` (tools may come to differ per
  person); the server's name and version are in discover's `_meta`.
- **Older**, any other request, in the revision its `MCP-Protocol-Version` names; one naming
  an unlisted revision is 400 `-32602`. With no header it is read as 2025-06-18, the oldest
  listed, rather than refused: `ping` may precede `initialize`'s answer, and 2025-06-18 differs
  from the spec's assumed 2025-03-26 only in batches (refused anyway) and optional fields.
  `initialize` answers the revision it asks for when that is 2025-06-18 or 2025-11-25, else
  2025-11-25 (2026-07-28 has no `initialize`); `ping` an empty result; any other method
  `-32601`, and params a list or a call cannot read `-32602`, both at 200, as Claude Code
  accepts them.
- **Either era:** a body that is not JSON is 400 `-32700`; one that is not one JSON-RPC 2.0
  request (an array, which the notes leave without a status; no `"jsonrpc": "2.0"`; a method
  not a string; `params` not an object; a request with no id) 400 `-32600`. An id is a string
  or an integer within ±(2^53−1), the largest a double holds exactly, and is echoed as sent;
  any other id (null, fractional, larger, or of another type) is `-32600` answered to a null
  id. A notification or a client's response is 202 with no body, in either era; a verb other
  than POST 405. `-32603` is never sent: an answer that throws is the `Listener`'s 500.
  `-32022`'s `data.supported`, and the older era's, list all three revisions, newest first.
- **Not read:** `Origin` (judged by the caller, which knows its listen address),
  `Authorization`, `Mcp-Session-Id`, and every `Mcp-Param-*`: `McpTool.described` declares no
  `x-mcp-header`, so a server here expects none and ignores any it is sent.

A tool's own failure is a `tools/call` result with `isError: true`; arguments it could not
read are such a result from 2025-11-25 and `-32602` in 2025-06-18; a tool not offered and a
cursor never given are `-32602`, 400 statelessly and 200 before. Each choice above that the
specification leaves open is pinned in `ServedTests`, and `ServedLawTests` holds the server's
half to the client's.

The test tree mirrors it. `client`'s and `edge`'s tests run against `FakeMcpServer`, an MCP server in the
test JVM (`com.sun.net.httpserver`) that reads every request with `Rpc.received` and checks
what a hosted server adds past it (its token, its content types, `Mcp-Param-*` mirroring);
a service account's exchange runs against `FakeTokenServer`, a token endpoint held to what
Google documents of it, and not yet to Google's own.
`McpServerContract` is the exchanges any server must answer as the client reads them, and
`McpServerGuardContract` what a hosted one checks beyond them; the fake keeps both in the unit
tier (`McpServerFakeTests`, `McpServerFakeGuardTests`), grit's own wire keeps the first over a
`Listener` (`McpServerWireTests`), and `LiveProbe`, a main in test sources that
needs a token and so is no tier, runs both against GitHub's (its doc says how). It passed whole
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
for `modifiedTime` and `version`, which Drive gives every file, and `size`, which it gives a file it keeps the
bytes of: each recorded file's was added by hand, a stand-in, the `made-*` file's where one lists it (`DriveCaptureTests` checks that every
recorded file holds each field Drive gives it).

Beside them, the `made-*` files are written by hand, since the recordings hold no text file:
`made-file-text.json`, a text/plain file in `folder-3`;
`made-file-text.txt`, its content, starting with a byte-order mark;
`made-permissions-file-text.json`, grants of its own, a writer outside the domain among them;
and `made-children-drive-a.json` and `made-children-folder-3.json`, the listing of each one's own
files and folders (`'{id}' in parents`, no words), the recorded entries lying there, folders
first and then by name, each with a `modifiedTime` of its own.

`test/resources/claude-code/handshake-2.1.303.jsonl` holds the six requests Claude Code 2.1.303
sent an HTTP MCP server on 2026-11-13, as it sent them but for the `Host` (a placeholder port)
and the tool call's name, arguments and `toolUseId` (neutral stand-ins, `Content-Length`
recomputed); each answer is kept as its status and its shape alone: every string empty,
every number 0, every list its first item, and a schema's property names numbered. Beside it,
`stateless-synthesised-2.1.303.jsonl` holds no recording: the stateless `tools/list` and
`tools/call` Claude Code is expected to send once `server/discover` is answered, built from
the recorded discover's `_meta` and headers, until a real-use run records them.
