# grit.claudecode

Claude Code's adapter for the commands edge
([ADR 0062](../../docs/decisions/0062-an-outside-harness-reaches-a-deployment-over-http-through-an-edge-that-writes-no-files-each-request-from-a-caller-and-each-worker-named-by-its-credential.md)):
one value, `ClaudeCode.adapter`, core's `Adapter` for the harness `claude-code`, which a
deployment hands `CommandsEdge.of`. The edge serves its join script at `GET /join/claude-code`; a
person runs it in a checkout. It names core alone (`claudecode-names-core-alone`), never
`grit.commands`: what it asks of the edge is the routes, in plain HTTP.

One idea, one package, `grit.claudecode`. Its scripts are text, under `shell/`:

- **`hook.sh`**: the hook, one program with a subcommand per role, which the join installs.
- **`join.sh`**: the join script's body. The script served is a shebang, then one group: `{`,
  `harness=claude-code`, `hook=<hook.sh, single-quoted>`, this body and `}`, so the harness's name has one home, in Scala,
  and the hook rides in the join with no delimiter to collide with.

The build reads them into `Scripts` as string constants, so the adapter is a pure value with no
resource to read; a script over 60 KB, or holding a NUL, fails the build.

## Requires

bash 5, jq 1.6 or later, curl, git 2.36 or later, and coreutils (the hook's watchdog is its
`timeout`), on the machine the join runs on; `--retire` needs GNU coreutils' `realpath`, for its `-e` and `-m`, and is refused without
it. The unit tests run them too, with no skip.

## The join

```
curl -fsS <edge>/join/claude-code | bash -s -- <edge> [--mcp-name <name>] [--retire <dir>]...
curl -fsS <edge>/join/claude-code | bash -s -- --undo [--mcp-name <name>]
```

A script piped into bash runs as it arrives, a command at a time, and a top-level word cut short
is a command of its own (`program` cut to `pro` runs a program of that name). So the script
served is one `{ … }` group, which bash runs only once the closing brace has arrived: a download
cut short is a syntax error and runs nothing. Inside it `join.sh` only defines until its last
line, `</dev/null main "$@"`; `main` sets bash's options itself, and what it starts reads
`/dev/null`, never the rest of the script.

**Refused**, with exit 1, one line on stderr and nothing changed: bash below 5; no git, jq or
curl on PATH; outside a git checkout; a git whose `git worktree list` takes no `-z` (below 2.36),
as an undo reads every worktree's settings from that list; an address other than `http://` or
`https://`, a host (a name, or an IPv6 address in brackets) and an optional port, nothing after;
plain `http://` to a host that is not loopback (`localhost`, `127.x.x.x`, `[::1]`), since posts
carry a session's replies and summaries in clear until requests carry credentials; a checkout or
hook path holding a control character; settings that are a symbolic link, not a JSON object, or
whose `hooks` is not an object of lists of groups; `--retire` of something that is not a
directory, or of `HOME`, the checkout or an ancestor of either; an `--mcp-name` other than 1 to 64
letters, digits, `_`, `.` or `-`, starting with a letter or digit (so it is never read as an
option); and any jq call that fails, or a rewrite of the settings that is not one JSON object, as
everything that can fail is worked out before the first file is written (an undo, too, reads
every other worktree's settings and `~/.claude.json` first).

**Writes**, each file through a temporary one moved into place:

- **The hook**, `<git common dir>/grit/claude-code/hook` (absolute, by `git rev-parse
  --path-format=absolute`), executable: a shebang, `harness='claude-code'`, `edge='<edge>'`,
  `edge=${GRIT_EDGE:-$edge}`, then `hook.sh`. git never tracks or shows it, and the repository's
  worktrees share it. `GRIT_EDGE` comes from the session's environment, past every check the join
  made of its address, so the hook checks it with the join's own patterns, and does nothing when
  it fails them ([the hook](#the-hook)).
- **The settings**, the checkout's `.claude/settings.local.json` (`{}` when absent), keeping the
  file's mode, as it may hold tokens (0644 for a file the join makes). One jq program removes
  every handler whose command is the hook's, and every one whose program is under a `--retire`
  directory (a relative one read from where the join is run); drops the groups and events that
  removal left empty; and appends one group per event, 11 handlers in all, each `'<hook>' <role>`.
  Their timeouts are 5 s, SessionEnd's 2 s; the hooks bound themselves below them. Everything else
  keeps its order, and a second join writes the same bytes; but jq writes the whole file in its
  own layout, two-space indented, so the first join re-indents a file laid out otherwise.

  A handler's program is its command's first word after `exec`, as a shell reads it with its
  quotes removed: `$CLAUDE_PROJECT_DIR` or `${CLAUDE_PROJECT_DIR}` at its start is the checkout
  unless single-quoted, `~/` is `HOME` unless quoted, a relative path is read from the checkout's
  top (as a hook runs), and the path is canonicalised, symbolic links resolved. A first word with
  no `/` (`bash x.sh`, `FOO=1 x.sh`) names no program under a directory, and its handler stays.

  | Event | Handlers | |
  |---|---|---|
  | SessionStart | `brief`, `opening`, `event` | `event` async |
  | SubagentStart | `brief`, `event` | |
  | PostToolUse, PostToolUseFailure | `event` | async; matcher `^(Read\|Edit\|Write\|MultiEdit\|NotebookEdit\|Bash\|mcp__scout__.*)$` |
  | Stop, PostCompact, SubagentStop | `event` | async |
  | SessionEnd | `event` | |

- **git's hooks**, `post-commit` (`exec '<hook>' commit`) and `post-rewrite` (`exec '<hook>' rewrite
  "$@"`) in `<git common dir>/hooks`, each marked by the line `# grit claude-code join`. When
  `core.hooksPath` names another directory (one other repositories share, or one the checkout
  tracks), the join writes nothing there and prints the two lines to add. Otherwise each is written
  when the file is absent, ours, or every command line of it runs a program under a retired
  directory, and left as it is, with the line to add printed, when not (`'<hook>' commit`, with no
  `exec`, which would end that hook at the line). The repository's worktrees share them.
- **The MCP entry**: with `claude` on PATH, `claude mcp remove --scope local <name>` (its failure
  ignored), then `claude mcp add --transport http --scope local <name> <edge>/mcp`; the entry it
  replaces is read from `~/.claude.json` first and printed, a URL as its scheme, host and path
  alone (no credentials, query or fragment), a stdio entry as `the command <program>`. An add that
  fails prints the command and exits 1. Without `claude`, the command is printed. The entry
  belongs to the checkout's directory, so each worktree joins its own.

Every removed command and git hook line is printed as it was. The hook's path and the MCP entry's
key are the checkout's path at the join: **a repository moved after a join** keeps handlers naming
the old hook, and a join at the new path adds a second set, so run the undo before moving it.

**Undo** removes the checkout's handlers (rewriting the settings only when it held one) and its
MCP entry, but only an entry the join would have written, an `http://` or `https://` host and
port with the path `/mcp` and nothing after, as a checkout never joined may hold another (printed,
and left as it is); the hook and the git hooks it marked go too, unless another worktree's
settings still name the hook, when they are kept and the worktree named. Nothing retired is
restored. A `hooks` the removal leaves empty goes, but a settings file the join made is left as
`{}`: with no state file the undo cannot tell it from a person's own `{}`, and Claude Code reads
the two alike.

A join prints, for a first join (paths as they are on the machine):

```
grit claude-code: joined /work/proj to http://127.0.0.1:7317
  hook      /work/proj/.git/grit/claude-code/hook, shared by this repository's worktrees
  settings  .claude/settings.local.json: 11 handlers of ours
  git       post-commit and post-rewrite in /work/proj/.git/hooks, shared by this repository's worktrees
  mcp       grit at http://127.0.0.1:7317/mcp, local scope
Sessions pick the hooks up at once; run /mcp in a running session to connect grit.
Undo: curl -fsS http://127.0.0.1:7317/join/claude-code | bash -s -- --undo --mcp-name grit
```

with a `  removed   <command>` line for each retired one, `, replacing <name> at <url>` after the
MCP entry it replaced, and `  git       <file> is not ours and is left as it is; add the line:
<line>` for a foreign git hook. Under `core.hooksPath`, the `git` line is three:

```
  git       /work/shared-hooks is core.hooksPath, which the join does not write; add these lines
  git       post-commit: '/work/proj/.git/grit/claude-code/hook' commit
  git       post-rewrite: '/work/proj/.git/grit/claude-code/hook' rewrite "$@"
```

An undo prints:

```
grit claude-code: undid the join in /work/proj
  settings  .claude/settings.local.json: 11 handlers of ours removed
  hook      /work/proj/.git/grit/claude-code/hook removed
  git       post-commit and post-rewrite removed
  mcp       grit at http://127.0.0.1:7317/mcp removed from local scope
Nothing retired was restored.
```

or `  hook      <hook> kept: the worktree <dir> still names it` and `  git       post-commit and
post-rewrite kept, as the hook is` while another worktree is joined; and, for a foreign git hook
that still names the hook removed, `  git       <file> is not ours and still names <hook>; remove
that line`. The `mcp` line is `  mcp       <name> at <where> is not an edge's /mcp and is left
as it is` for an entry the join would not have written, and `  mcp       no entry <name> in local
scope` when there is none.

## The hook

`<hook> <role>`, one program the join installs, a role per handler. Every query carries
`harness=claude-code`, the payload's `session_id` as `session`, and as `dir` the starting
session's directory, `CLAUDE_PROJECT_DIR` (recorded: an agent in a worktree has the worktree as
`cwd`), else the payload's `cwd`; an agent's adds its `agent_id` and `agent_type` as `agent` and
`type`. A session's or agent's id is sent only when it matches `^[A-Za-z0-9_-][A-Za-z0-9._-]{0,99}$`.

| Role | Run at | Reads | Asks | Prints |
|---|---|---|---|---|
| `brief` | SessionStart | `source` (docs, recorded) | `GET /brief`, `start=fresh` at `startup` and `clear`, `start=resume` at `resume`, `compact` and `fork` (a fork has a new id and goes on from its window) | the brief as context; nothing when it is empty |
| `brief` | SubagentStart | `agent_id`, `agent_type` (recorded) | `GET /brief…&agent&type&start=fresh` | the brief as context; nothing for an agent of no type |
| `opening` | SessionStart | `source` | `GET /holder`; then after `compact` `part=digest&start=resume`, after `clear` `part=digest&start=fresh`, and at no other start | the holder lines, then the digest, as context |
| `holder [<type> --dir <dir> --session <id>]` | a Bash tool | `CLAUDE_PROJECT_DIR`, `CLAUDE_CODE_SESSION_ID` (recorded: a Bash tool has only the second) | `GET /holder`, with `session` and `type` for an agent's | the holder, one line; exit 1 when the edge did not give it |
| `event` | PostToolUse, PostToolUseFailure | `tool_name`, `tool_input`; PostToolUse's `tool_response` (recorded; a failure carries `error` and none, so it posts `ok: false` and 0 bytes) | `POST /events`: Read, `read` `{path, worktree?, ok}`, `bytes` those of `tool_response.file.content`; Edit, Write, MultiEdit, NotebookEdit, `edited` `{path, worktree?, ok}`; Bash, `ran` `{program, ok}`; an MCP tool, `read` `{named?, tool, ok}`, `bytes` those of its text blocks (recorded), with nothing named for scout's `find` and `area`; any other tool, nothing | nothing |
| `event` | SessionStart | `source` | `started` `{source}` | nothing |
| `event` | SubagentStart, SubagentStop | `agent_id`, `agent_type` | `started`, `ended`, on the agent | nothing |
| `event` | Stop | `last_assistant_message` (docs, recorded) | `closing`, `text` its first 2,000 characters; nothing when it is empty | nothing |
| `event` | PostCompact | `compact_summary`, `trigger` (docs, recorded) | `closing`, `text` the summary's first 2,000 characters, `{trigger}` | nothing |
| `event` | SessionEnd | `reason` | `ended` `{reason}` | nothing |
| `commit` | git's post-commit | `CLAUDE_CODE_SESSION_ID` | `commit` `{sha, subject}` | nothing |
| `rewrite amend\|rebase` | git's post-rewrite | its stdin, a line `<old> <new>` a commit (docs: githooks) | one post, a line per commit it left, in order, `{sha, subject, replaces}` | nothing |

**An event line** is `session`, `agent` and `type` for an agent's, `dir`, `worktree` (the top of
the checkout `cwd` is in, else `cwd`), `n`, `at`, `event`, and a read's `bytes`, a closing's
`text` and `shape`, as `/events` reads them. `n` is the microseconds of one reading of the clock
times 1000 plus the process's id modulo 1000, and `at` that instant; a rewrite's commits take
`n`, `n+1`, …. Every string is cut to fit the line's bounds, control characters written `?`: a
path is relative to its file's checkout, which is named under `worktree` when it is not the line's
and holds at most 100 bytes, else to `cwd` (outside it, the file's name alone), and keeps its last
120 bytes; a program is a command's first word after any leading `cd <dir> &&` or `;` and any
`NAME=value`, its last path segment, 40 bytes; `named` is a scout query's first symbol, suite,
target or pattern, 120 bytes; a subject 100 characters; a summary 2,000 characters as grit counts
them (UTF-16). So a payload of any size posts a line of a few hundred bytes, a closing's under
13 KB, and a rewrite's lines go a thousand to a post.

An agent whose `agent_type` is empty posts and asks nothing: it is a compaction's, which no
SubagentStart announced (recorded). A commit's `dir` is `CLAUDE_PROJECT_DIR`, else the commit's
checkout, as a Bash tool has none; outside a Claude Code session (no `CLAUDE_CODE_SESSION_ID`)
or mid-rebase (the rewrite posts it) it posts nothing. It carries no `child`: there is no source
telling an agent's commit from its session's (`CLAUDE_CODE_CHILD_SESSION`, below), so a commit is
its session's. An amend is posted twice, as made and as replacing the commit it rewrote; the
record leaves out a commit a later one replaces. A commit can be a session's first line when the
edge was down at its start, and the session's holder is then its checkout's.

The join registers PostToolUse, PostToolUseFailure, Stop, PostCompact, SubagentStop and
SessionStart's `event` as async, so they never hold a session up.

Context is printed as Claude Code reads it, one JSON object on stdout,
`{"hookSpecificOutput":{"hookEventName":<event>,"additionalContext":<text>}}`. The holder lines
are in `opening`, not `brief`, as a brief may come near the 10,000-character cap a hook's context
has; they are, with `'<hook>'`, `'<dir>'` and `'<session>'` single-quoted for a shell:

```
grit: this session works as the claim holder <holder>.
grit: an agent of type <type> that this session starts works as the claim holder this command prints: '<hook>' holder <type> --dir '<dir>' --session '<session>'
```

`--dir` and `--session` are written out because a Bash tool has neither `CLAUDE_PROJECT_DIR` nor
an agent's id (recorded).

**The edge down.** Every role but `holder` exits 0 whatever happens, and says on stderr, in one
line, what it could not do. When the edge does not answer a start, `brief` alone gives one line of
context, `grit: the edge at <edge> did not answer; this session opens without its brief.` (`this
agent` at an agent's), and `opening` prints nothing; an answer other than 200 gives `… answered
<status> for the brief; this session opens without it.` Each post is sent once: one the edge did
not answer, or answered other than 200 (a 413, a 503, any other), is dropped, and a line it
refused is named with its reason. Nothing is kept to resend, so while the edge is down a
session's events, closings and commits are lost.

**Bounds.** A whole run of the hook is stopped 3 s after it starts, by `timeout`, which stops what
it started too; each request is given what is left of that, less a quarter second, and connects
within 0.3 s. A SessionEnd's requests end 1.2 s after the start, within the 1.5 s Claude Code
gives SessionEnd's hooks by default (docs `#sessionend`).

**Its address.** `GRIT_EDGE`, when set, is checked with the join's own patterns and loopback rule;
an address they refuse makes the hook ask nothing and say so (`holder` exits 1).

## Smoke run

`SmokeEdge`, in the test sources, serves the edge on a free port over scripted sessions on the
system's clock: `java -cp <grit.claudecode.test's runClasspath> grit.claudecode.SmokeEdge
<checkout> <brief>` prints its address, keeps `<brief>` as a session's brief in `<checkout>`, and
prints what it recorded at each line on its stdin. Join a scratch checkout to it (with a `HOME` of
its own, or no `claude` on PATH, so the join leaves the person's `~/.claude.json` alone), then run
`claude -p --setting-sources project,local` there with a cheap model and `--max-turns`.

## Claude Code's hook contract

What the scripts rest on, each fact tagged by its source: **docs**, the anchor of
code.claude.com/docs/en/hooks (or the page named) that says it; **recorded**, a payload or an
environment Claude Code 2.1 handed a recording hook in `claude -p`, kept in
`test/resources/payloads/` with ids, paths and text replaced by neutral ones; **observed**, seen in
a trial and not kept.

- Every event's stdin is one JSON object with `session_id`, `transcript_path`, `cwd` and
  `hook_event_name`; most add `prompt_id`, `permission_mode`, `effort` and `scratchpad_dir`
  (docs `#common-input-fields`; recorded). Inside an agent, `agent_id` and `agent_type` are added,
  and `session_id` stays the starting session's (recorded: every agent payload). There is no
  timestamp and no counter (recorded).
- SessionStart's `source` is `startup`, `resume`, `clear`, `compact` or `fork` (docs
  `#sessionstart`; recorded, all five). A fork has a new `session_id` (recorded). A `/clear` ends
  the old session (SessionEnd `reason` `clear`) and starts a new id with `source` `clear`
  (recorded). A compaction of a resumed `-p` session fires SessionStart `resume`, then
  `compact` on the same id (recorded).
- SubagentStart carries `agent_id` and `agent_type`; an agent in a worktree has the worktree as
  `cwd` while `CLAUDE_PROJECT_DIR` stays the starting checkout (recorded:
  `subagent-start-worktree.json`).
- PostToolUse carries `tool_name`, `tool_input`, `tool_response`, `tool_use_id` and
  `duration_ms` (docs `#posttooluse`; recorded). Read's `tool_response.file` has `content`,
  `filePath`, `numLines`, `startLine` and `totalLines`; Write's and Edit's have `filePath`; Bash's
  `stdout`, `stderr` and `interrupted`; an MCP tool's is a list of content blocks,
  `[{"type":"text","text":…}]`, with `mcp_server` beside it (recorded).
- **PostToolUseFailure carries `error`, a string, and no `tool_response`**, with `is_interrupt`
  (docs `#posttoolusefailure`; recorded).
- Stop and SubagentStop carry `last_assistant_message` and `stop_hook_active` (docs `#stop`,
  `#subagentstop`; recorded). **A compaction fires a SubagentStop whose `agent_type` is `""`, for
  an agent no SubagentStart announced** (recorded: `subagent-stop-compaction.json`).
- PreCompact carries `trigger`; PostCompact `trigger` and `compact_summary` (docs
  `#postcompact`; recorded).
- SessionEnd's `reason` is `clear`, `resume`, `logout`, `prompt_input_exit` or `other` (docs
  `#sessionend`; recorded: `clear`, `other`). Its hooks have 1.5 s unless a hook's own `timeout`
  raises it, up to 60 s (docs `#sessionend`).
- `hookSpecificOutput.additionalContext` on SessionStart and SubagentStart is added to the
  window, at most 10,000 characters a hook; past that, a 2,000-character preview (docs
  `#sessionstart-decision-control`, `#subagentstart`). Exit 0 is silent, exit 2 shows stderr to
  the person, another exit a hook-error notice (docs `#exit-code-2-behavior-per-event`).
- `timeout` is in seconds; an event's matching hooks run in parallel; `async: true` runs a
  command hook in the background, its timeout unenforced, killed at a `-p` session's teardown
  (docs `#timeouts`, `#configure-an-async-hook`); so a `-p` session's async Stop post did not
  arrive, and it recorded no closing (observed, in a smoke run). A `matcher` holding a character beyond letters,
  digits, `_`, `-`, space, comma and `|` is an unanchored regular expression (docs
  `#hook-locations`), so the join anchors its own.
- Hooks of user, project and local settings all run; settings are reloaded when a file changes
  (docs: settings `#settings-precedence`, `#when-edits-take-effect`). `claude -p` in a folder never
  trusted ran `.claude/settings.local.json`'s hooks (recorded).
- A hook has `CLAUDE_PROJECT_DIR` (docs; recorded). A Bash tool, and a git hook it runs, have
  `CLAUDE_CODE_SESSION_ID`, the session's id, and no `CLAUDE_PROJECT_DIR` and no agent id
  (recorded: `bash-tool-env.txt`). **`CLAUDE_CODE_CHILD_SESSION=1` is set in a session's own
  Bash tool and hooks as in an agent's** (recorded), so it does not tell an agent's commit from
  its session's.
- `claude mcp add --transport http --scope local <name> <url>` keeps the entry in
  `~/.claude.json` under the checkout's path, and refuses a name it has; `claude mcp remove
  --scope local <name>` exits 1 when there is none (docs: mcp `#local-scope`; observed, with
  `HOME` a temporary directory). `claude mcp get` connects to the server first, up to 30 s
  (observed), so the join reads the old entry from `~/.claude.json` instead.

## What the code cannot say

- **A hook never fails a session.** Whatever the edge does, down, slow or refusing, a hook exits 0
  within its bound, and a session goes on without what it would have brought.
