# grit.claudecode

Claude Code's adapter for the commands edge
([ADR 0062](../../docs/decisions/0062-an-outside-harness-reaches-a-deployment-over-http-through-an-edge-that-writes-no-files-each-request-from-a-caller-and-each-worker-named-by-its-credential.md)):
one value, `ClaudeCode.adapter`, core's `Adapter` for the harness `claude-code`, which a
deployment hands `CommandsEdge.of`. The edge serves its join script at `GET /join/claude-code`; a
person runs it in a checkout. It names core alone (`claudecode-names-core-alone`), never
`grit.commands`: what it asks of the edge is the routes, in plain HTTP.

One idea, one package, `grit.claudecode`. Its scripts are text, under `shell/`:

- **`hook.sh`**: the hook, one program with a subcommand per role, which the join installs.
- **`join.sh`**: the join script's body. The script served is a shebang, `harness=claude-code`,
  `hook=<hook.sh, single-quoted>`, then this body, so the harness's name has one home, in Scala,
  and the hook rides in the join with no delimiter to collide with.

The build reads them into `Scripts` as string constants, so the adapter is a pure value with no
resource to read; a script over 60 KB, or holding a NUL, fails the build.

## Requires

bash 5, jq 1.6 or later, curl, git 2.31 or later, and coreutils, on the machine the join runs
on. The unit tests run them too, with no skip.

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
