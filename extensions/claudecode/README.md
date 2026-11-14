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

bash 5, jq 1.6 or later, curl, git 2.36 or later, and coreutils, on the machine the join runs
on. The unit tests run them too, with no skip.

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
as an undo reads every worktree's settings from that list; an address other than `http://` or `https://`, a host (a
name, or an IPv6 address in brackets) and an optional port, nothing after; plain `http://` to a
host that is not loopback (`localhost`, `127.x.x.x`, `[::1]`), since posts carry a session's
replies and summaries in clear until requests carry credentials; a checkout or hook path holding a
control character; settings that are a symbolic link, not a JSON object, or whose `hooks` is not
an object of lists of groups; `--retire` of something that is not a directory, of `/` or of
`HOME`; an `--mcp-name` other than 1 to 64 letters, digits, `_`, `.` or `-`, starting with a letter or
digit (so it is never read as an option); any jq call that fails, or
a rewrite of the settings that is not one JSON object, as everything that can fail is worked out
before the first file is written (an undo, too, reads every other worktree's settings first).

**Writes**, each file through a temporary one moved into place:

- **The hook**, `<git common dir>/grit/claude-code/hook` (absolute, by `git rev-parse
  --path-format=absolute`), executable: a shebang, `harness='claude-code'`, `edge='<edge>'`,
  `edge=${GRIT_EDGE:-$edge}`, then `hook.sh`. git never tracks or shows it, and the repository's
  worktrees share it.
- **The settings**, the checkout's `.claude/settings.local.json` (`{}` when absent), keeping the
  file's mode, as it may hold tokens (0644 for a file the join makes). One jq program removes
  every handler whose command is the hook's, and every one whose program is under a `--retire`
  directory (a relative one read from where the join is run); drops the groups and events that
  removal left empty; and appends one group per event, 11 handlers in all, each `'<hook>' <role>`. Their
  timeouts are 5 s, SessionEnd's 2 s; the hooks bound themselves below them. Everything else keeps
  its order, and a second join writes the same bytes.

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
  tracks), the join writes nothing there and prints the two lines to add. Written when the file is
  absent, ours, or every command line of it
  runs a program under a retired directory; otherwise left as it is, with the line to add printed
  (`'<hook>' commit`, with no `exec`, which would end that hook at the line).
  The repository's worktrees share them.
- **The MCP entry**: with `claude` on PATH, `claude mcp remove --scope local <name>` (its failure
  ignored), then `claude mcp add --transport http --scope local <name> <edge>/mcp`; the entry it
  replaces is read from `~/.claude.json` first and printed, a URL as its scheme, host and path
  alone (no credentials, query or fragment), a stdio entry as `the command <program>`. An add that
  fails prints the command and exits 1. Without `claude`, the command is printed. The entry belongs to the checkout's
  directory, so each worktree joins its own.

Every removed command and git hook line is printed as it was. **Undo** removes the checkout's
handlers (rewriting the settings only when it held one) and its MCP entry, but only an entry the
join would have written, an `http://` or `https://` host and port with the path `/mcp` and nothing
after, as a checkout never joined may hold another (printed, and left as it is); the hook and the git
hooks it marked go too, unless another worktree's settings still name the hook, when they are
kept and the worktree named. Nothing retired is restored.

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
that line`.

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
  (docs `#timeouts`, `#configure-an-async-hook`). A `matcher` holding a character beyond letters,
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
