package grit.claudecode

import java.nio.file.{Files, Path}

import grit.commands.routes.RouteFixtures.AgentText

import utest.*

/** The pinned shellcheck (bash scripts/fetch-shellcheck.sh) over the scripts as they run: the join
  * as the adapter serves it, and the hook as the join installs it. A finding the scripts mean is
  * annotated beside it with its reason, so the lint runs with every check on.
  */
object ShellcheckTests extends TestSuite {

  // Every finding in `script`, as `-:<line>:<column>: <level>: <message> [SC<code>]`.
  private def findings(dir: Path, script: String): Vector[String] = {
    val shellcheck = sys.env
      .get("GRIT_SHELLCHECK")
      .filter(p => Files.isExecutable(Path.of(p)))
      .getOrElse(
        throw new java.lang.AssertionError(
          "no tools/shellcheck: run bash scripts/fetch-shellcheck.sh"
        )
      )
    val done = Bash.run(dir, dir, dir, Seq("-c", "exec \"$0\" --norc -f gcc -", shellcheck), script)
    done.out ++ Option.when(done.exit != 0 && done.out.isEmpty)(s"exit ${done.exit}: ${done.err}")
  }

  val tests = Tests {
    test("shellcheck finds nothing in the join as served") {
      Bash.inTemp(dir => findings(dir, ClaudeCode.adapter.join) ==> Vector.empty)
    }
    test("shellcheck finds nothing in the hook as the join installs it") {
      JoinFixtures { f =>
        val dir = f.checkout("proj")
        f.joined(dir).exit ==> 0
        findings(f.root, Files.readString(f.hook(dir))) ==> Vector.empty
      }
    }
    test("shellcheck finds nothing in the hook as a join with a credential installs it") {
      HookFixtures.credentialed(AgentText) { f =>
        findings(f.machine.root, Files.readString(f.hook)) ==> Vector.empty
      }
    }
  }
}
