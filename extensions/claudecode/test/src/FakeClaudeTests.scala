package grit.claudecode

import java.nio.charset.StandardCharsets.UTF_8

import utest.*

/** [[JoinFixtures]]' fake `claude`, held to the real CLI: `claude/mcp-local.json` records each
  * `claude mcp` call the join makes, run by the CLI it names in a fresh checkout and `HOME`,
  * with its exit status and what `~/.claude.json` keeps of each project's MCP entries after it.
  * Record it again when Claude Code changes how it keeps them.
  */
object FakeClaudeTests extends TestSuite {

  private val Recorded: ujson.Value = ujson.read(
    new String(getClass.getResourceAsStream("/claude/mcp-local.json").readAllBytes(), UTF_8)
  )

  /** Each recorded step's arguments, and its exit status and projects after it. */
  private val Steps: Vector[(Vector[String], (Int, ujson.Value))] =
    Recorded("steps").arr.toVector.map(s =>
      s("args").arr.toVector.map(_.str) -> (s("exit").num.toInt -> s("projects"))
    )

  val tests = Tests {
    test("the fake keeps a checkout's MCP entries, and exits, as the recorded claude does") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val at = f.sh(dir, "pwd -P").mkString
        val ran = Steps.map((args: Vector[String], _: (Int, ujson.Value)) => {
          val done = Bash.run(dir, f.home, f.bin, Seq("-c", "claude \"$@\"", "claude") ++ args)
          val projects = f
            .read(f.home.resolve(".claude.json"))
            .flatMap(t => ujson.read(t).objOpt.flatMap(_.get("projects")))
            .flatMap(_.objOpt)
            .fold(ujson.Obj())(ps =>
              ujson.Obj.from(ps.map((k, v) => (if (k == at) "$CHECKOUT" else k) -> v))
            )
          args -> (done.exit -> projects)
        })
        ran ==> Steps
      }
    }
  }
}
