package grit.claudecode

import grit.core.edge.Adapter
import grit.core.host.Argv
import grit.core.id.Harness

/** Claude Code's adapter for the commands edge (ADR 0062). */
object ClaudeCode {

  private val Named: Harness =
    Harness.of("claude-code").fold(why => throw new IllegalStateException(why), identity)

  /** For the harness `claude-code`. Its join script, run in a git checkout as `bash -s --
    * <edge's address> [--credential <file>] [--mcp-name <name>] [--retire <dir>]…`, or `bash -s
    * -- --undo [--mcp-name <name>]`. What it does: extensions/claudecode/README.md.
    */
  val adapter: Adapter = Adapter(
    Named,
    s"#!/usr/bin/env bash\n{\nharness=${Argv.quote(Harness.value(Named))}\n" +
      s"hook=${Argv.quote(Scripts.Hook)}\n" + Scripts.Join + "}\n"
  )
}
