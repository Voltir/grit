package grit.claudecode

import java.nio.file.{Files, Path}

import grit.core.id.Harness

import utest.*

/** [[ClaudeCode.adapter]]: its harness, and the join script's head, which carries the hook. */
object ClaudeCodeTests extends TestSuite {

  private def shell(file: String): String =
    Files.readString(
      Path.of(
        sys.env.getOrElse(
          "GRIT_CLAUDECODE_SHELL",
          throw new java.lang.AssertionError("GRIT_CLAUDECODE_SHELL is unset: run through Mill")
        ),
        file
      )
    )

  val tests = Tests {
    test("the adapter is for the harness claude-code") {
      Harness.value(ClaudeCode.adapter.harness) ==> "claude-code"
    }
    test(
      "the join opens with bash's shebang and the harness's name, then the hook as bash reads it"
    ) {
      Bash.inTemp { dir =>
        val join = ClaudeCode.adapter.join
        val head = join.stripSuffix(Scripts.Join)
        val read =
          Bash.run(dir, dir, dir, Seq("-s"), head + "printf '%s|' \"$harness\" \"$hook\"\n")
        (
          join.linesIterator.take(2).toVector,
          join.endsWith(Scripts.Join),
          read.exit,
          read.out.mkString("\n")
        ) ==> (
          Vector("#!/usr/bin/env bash", "harness=claude-code"),
          true,
          0,
          "claude-code|" + Scripts.Hook + "|"
        )
      }
    }
    test("Scripts holds each shell script's text as it is") {
      (Scripts.Hook, Scripts.Join) ==> (shell("hook.sh"), shell("join.sh"))
    }
    test("bash parses the hook, the join, and the join as served") {
      Bash.inTemp { dir =>
        Vector(Scripts.Hook, Scripts.Join, ClaudeCode.adapter.join)
          .map(script => Bash.run(dir, dir, dir, Seq("-n"), script).exit) ==> Vector(0, 0, 0)
      }
    }
  }
}
