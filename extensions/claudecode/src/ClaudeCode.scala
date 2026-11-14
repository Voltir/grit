package grit.claudecode

import grit.core.edge.Adapter
import grit.core.host.Argv
import grit.core.id.Harness

/** Claude Code's adapter for the commands edge (ADR 0062). */
object ClaudeCode {

  private val Named: Harness =
    Harness.of("claude-code").fold(why => throw new IllegalStateException(why), identity)

  /** For the harness `claude-code`. Its join script, run in a git checkout as `bash -s --
    * <edge's address> [--mcp-name <name>] [--retire <dir>]…`, installs the hook that briefs
    * Claude Code's sessions and agents from the edge and posts what they do to it; registers it
    * in `.claude/settings.local.json` and git's `post-commit` and `post-rewrite`, removing the
    * handlers whose program is under a retired directory; and registers the edge's `/mcp` as
    * `<name>` (`grit` by default). `--undo` removes what it added. What it refuses, writes and
    * prints, and what each hook asks, posts and prints: extensions/claudecode/README.md.
    */
  val adapter: Adapter = Adapter(
    Named,
    s"#!/usr/bin/env bash\n{\nharness=${Argv.quote(Harness.value(Named))}\n" +
      s"hook=${Argv.quote(Scripts.Hook)}\n" + Scripts.Join + "}\n"
  )
}
