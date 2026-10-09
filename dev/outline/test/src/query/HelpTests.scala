package grit.outline.query

import grit.outline.cli.Main

import utest.*

object HelpTests extends TestSuite {

  /** Each query's flags as its parser reads them, with whether each takes a value. The parser matches string
    * literals, so it cannot be listed from the code: this table is the parser's spelling, and the test checks
    * each row against the parser itself.
    */
  private val accepted: Vector[(String, Vector[(String, Boolean)])] = Vector(
    "show" -> Vector(
      "--depth" -> true,
      "--body" -> true,
      "--private" -> false,
      "--cap" -> true,
      "--root" -> true
    ),
    "family" -> Vector("--member" -> true, "--body" -> false, "--cap" -> true, "--root" -> true),
    "uses" -> Vector("--in" -> true, "--outside" -> true, "--cap" -> true, "--root" -> true),
    "area" -> Vector("--level" -> true, "--cap" -> true, "--root" -> true),
    "tests" -> Vector("--test" -> true, "--cap" -> true, "--root" -> true)
  )

  /** A directory with no build output: a flag that parses reaches the root check and fails there, not as an unknown argument. */
  private lazy val emptyRoot: os.Path = os.temp.dir(prefix = "outline-help-")

  def tests = Tests {

    test(
      "every flag a query's parser accepts is named by that query's help, and the parser accepts it"
    ) {
      accepted.foreach { case (query, flags) =>
        val (code, help) = Main.run(Vector(query, "--help"), os.pwd)
        assert(code == 0)
        val entry = Help.entry(query)
        assert(entry.isDefined)
        flags.foreach { case (flag, takesValue) =>
          assert(help.linesIterator.exists(_.contains(flag)))
          assert(entry.exists(_.flags.exists(_.name == flag)))
          val args = if (takesValue) Vector(flag, "1") else Vector(flag)
          val (_, text) = Main.run(
            Vector(query, "A.keep") ++ args ++ Vector("--root", emptyRoot.toString),
            os.pwd
          )
          assert(!text.contains("unknown argument"))
        }
      }
    }

    test("each query's help is its usage, purpose and examples as the CLI runs them") {
      Help.entries.foreach { e =>
        val (code, help) = Main.run(Vector(e.query, "-h"), os.pwd)
        assert(code == 0)
        assert(help.startsWith(s"usage: ${e.query} "))
        assert(help.contains(e.purpose))
        assert(e.examples.forall(x => help.contains(s"scripts/outline ${x.args}")))
      }
    }
  }
}
