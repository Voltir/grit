package grit.act.moves

import grit.core.durable.Probes
import grit.core.durable.Probes.classpath

import utest.*

/** A planner's moves take what they act on as believed ([[grit.core.flow.Tainted]]), never raw:
  * a keep from a raw value, a call with raw arguments and an ask of a raw posing are rejected,
  * and the same moves made from the run they were handed compile. Pinned by compiling probe
  * sources against act with act's own flags.
  */
object MovesTaintTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.act.*
      |import grit.core.flow.Trusted
      |import grit.core.job.JobRun
      |import grit.core.place.Service
      |import grit.core.provider.ModelRequest
      |import grit.core.tool.ToolName
      |final case class Noted(n: Int) extends caps.Pure
      |object Noted {
      |  given grit.core.durable.Journaled[Noted] =
      |    grit.core.durable.Journaled.json(n => ujson.Num(n.n), _ => Right(Noted(0)))
      |}
      |""".stripMargin

  /** `body` as a planner's: over the run it was handed and its moves. */
  private def planner(body: String): String =
    "def plan(run: Trusted[JobRun[Noted]], moves: Keeping^, at: Service, tool: ToolName, " +
      s"name: MoveName): String = {\n$body\n}"

  private def errors(body: String): List[String] = compiled.errors(planner(body))

  private val fromRun =
    """val args = run.tainted.map(_ => ujson.Obj("path" -> "a"))
      |val posed = run.tainted.map(_ => Posed.Text(ModelRequest("", Vector.empty)))
      |val kept = moves.keep(name, run.tainted)((r, _, _) => Right(Noted(r.params.n)))
      |s"${moves.call(name, at, tool, args)} ${moves.ask(name, posed)} $kept"""".stripMargin

  private val rawKeep =
    """moves.keep(name, Noted(1))((n, _, _) => Right(n)).toString""".stripMargin

  private val rawCall =
    """moves.call(name, at, tool, ujson.Obj("path" -> "a")).toString""".stripMargin

  private val rawAsk =
    """moves.ask(name, Posed.Text(ModelRequest("", Vector.empty))).toString""".stripMargin

  private lazy val compiled =
    Probes.batch(prelude, List(fromRun, rawKeep, rawCall, rawAsk).map(planner))

  /** Whether `errs` holds a refusal of a raw value where a believed one is required. */
  private def unbelieved(errs: List[String]): Boolean =
    errs.exists(_.contains("Required: grit.core.flow.Tainted["))

  val tests = Tests {
    test("moves made from the run a planner was handed compile") {
      val errs = errors(fromRun)
      assert(classpath.nonEmpty, errs.isEmpty)
    }

    test("a keep from a raw value is rejected") {
      val errs = errors(rawKeep)
      assert(unbelieved(errs))
    }

    test("a call with raw arguments is rejected") {
      val errs = errors(rawCall)
      assert(unbelieved(errs))
    }

    test("an ask of a raw posing is rejected") {
      val errs = errors(rawAsk)
      assert(unbelieved(errs))
    }
  }
}
