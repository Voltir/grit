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

## What the code cannot say

- **A hook never fails a session.** Whatever the edge does, down, slow or refusing, a hook exits 0
  within its bound, and a session goes on without what it would have brought.
