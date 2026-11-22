package grit.claudecode

import java.nio.file.{Files, Path}

import grit.core.id.Harness
import grit.core.session.Session

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
      "the join is bash's shebang, then one group: the harness's name, the hook as bash reads it, led by the session record's unended seconds, the body"
    ) {
      Bash.inTemp { dir =>
        val join = ClaudeCode.adapter.join
        val head = join.take(join.indexOf(Scripts.Shared + Scripts.Join))
        val read =
          Bash.run(dir, dir, dir, Seq("-s"), head + "printf '%s|' \"$harness\" \"$hook\"\n}\n")
        (
          join.linesIterator.take(3).toVector,
          join.endsWith(Scripts.Shared + Scripts.Join + "}\n"),
          read.exit,
          read.out.mkString("\n")
        ) ==> (
          Vector("#!/usr/bin/env bash", "{", "harness=claude-code"),
          true,
          0,
          s"claude-code|unended=${Session.Unended.toSeconds}\n" + Scripts.Shared + Scripts.Hook + "|"
        )
      }
    }
    test("Scripts holds each shell script's text as it is") {
      (Scripts.Shared, Scripts.Hook, Scripts.Join) ==>
        (shell("shared.sh"), shell("hook.sh"), shell("join.sh"))
    }
    test("bash parses the hook, the join, and the join as served") {
      Bash.inTemp { dir =>
        Vector(Scripts.Hook, Scripts.Join, ClaudeCode.adapter.join)
          .map(script => Bash.run(dir, dir, dir, Seq("-n"), script).exit) ==> Vector(0, 0, 0)
      }
    }
  }
}
