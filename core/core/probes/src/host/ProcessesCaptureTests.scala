package grit.core.host

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about a started program: its [[Started]] handle, or a stream of it,
  * kept past the block of [[Processes.started]] that started it. Pinned by compiling probe
  * sources against core, as `OpenFileCaptureTests` does.
  */
object ProcessesCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import scala.concurrent.duration.*
      |import grit.core.host.*
      |""".stripMargin

  private val params = "(processes: Processes^, argv: Argv)"

  private def start(body: String): String = s"processes.started(argv, RelPath.Root)($body)"

  private val awaited = s"def f$params: Any = ${start("s => s.await(1.second)")}\n"

  private val keptInVar =
    s"""def f$params: Unit = {
       |  var kept: Option[Started^] = None
       |  val _ = ${start("s => { kept = Some(s) }")}
       |}
       |""".stripMargin

  private val returned =
    s"""def f$params: Unit = {
       |  val t = ${start("s => s")}
       |}
       |""".stripMargin

  private val outputReturned =
    s"""def f$params: Unit = {
       |  val t = ${start("s => s.output")}
       |}
       |""".stripMargin

  private val probes = List(awaited, keptInVar, returned, outputReturned)

  private lazy val compiled = Probes.batch(prelude, probes)

  private def errors(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("control: a started program's block may wait for it and return what it said") {
      errors(awaited) ==> Nil
    }

    test("a started program's handle kept in a variable outside its block is rejected") {
      assert(
        errors(keptInVar).exists(e =>
          e.contains("Started^") && e.contains("cannot flow into capture set {any}") &&
            e.contains("variable kept")
        )
      )
    }

    test("a started program's handle returned from its block is rejected") {
      assert(
        errors(returned).exists(e => e.contains("outlives its scope") && e.contains("Started"))
      )
    }

    test("a started program's output returned from its block is rejected") {
      assert(errors(outputReturned).exists(_.contains("outlives its scope")))
    }
  }
}
