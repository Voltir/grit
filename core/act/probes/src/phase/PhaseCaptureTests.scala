package grit.act.phase

import grit.core.durable.Probes
import grit.core.durable.Probes.classpath

import utest.*

/** What separation checking rejects about a phase's steps, a model's reply ([[Asking]]) or a
  * JSON ask's shaped reply ([[Shaping]]), pinned by compiling probe sources against act with
  * act's own flags: a step body may not mention the `Durable` it runs on, not even to read a
  * patch, so a phase computes what its steps need before them (docs/capture-checking.md).
  */
object PhaseCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.act.phase.*
      |import grit.core.clock.Clock
      |import grit.core.durable.Durable
      |import grit.core.model.NameRepair
      |import grit.core.provider.{ModelRequest, Provider}
      |import grit.core.schema.{JsonSchema, Typed}
      |""".stripMargin

  /** `body` as a method's over a provider, a request, a schema and a clock, in a workflow. */
  private def asked(body: String): String =
    "def asked(p: Provider^, request: ModelRequest, schema: JsonSchema, c: Clock^)" +
      s"(using d: Durable^): String = {\n$body\n}"

  /** The error messages from compiling `body` as `asked`'s, with every probe below. */
  private def errors(body: String): List[String] = compiled.errors(asked(body))

  /** A reply asked in a step, the new branch taken when `patched` says so. */
  private def asking(patched: String): String =
    s"""d.step("call-model") { () =>
       |  if ($patched) Asking.reply(p, request, Hearing.silent(), c).fold(identity, _.model)
       |  else "as before"
       |}""".stripMargin

  /** A JSON ask shaped in a step, repaired once when `patched` says so. */
  private def shaping(patched: String): String =
    s"""d.step("move:a") { () =>
       |  val repairs = if ($patched) 1 else 0
       |  Shaping.shaped(p, request, "reply", Typed.json(schema), NameRepair.AsSent, Set.empty, repairs, c) match {
       |    case Shaped.Read(reply, _, _) => reply.text
       |    case other => other.toString
       |  }
       |}""".stripMargin

  private val askPatchedFirst = "val fresh = d.patch(\"ask-again\")\n" + asking("fresh")
  private val askPatchedInStep = asking("d.patch(\"ask-again\")")
  private val shapePatchedFirst = "val fresh = d.patch(\"repair\")\n" + shaping("fresh")

  private lazy val compiled =
    Probes.batch(prelude, List(askPatchedFirst, askPatchedInStep, shapePatchedFirst).map(asked))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("a phase's step whose patch is read before it compiles") {
      val errs = errors(askPatchedFirst)
      assert(classpath.nonEmpty, errs.isEmpty)
    }

    test("a phase's step whose body reads a patch is rejected") {
      val errs = errors(askPatchedInStep)
      assert(errs.exists(_.contains("Separation failure")))
    }

    test("a JSON ask's step whose patch is read before it compiles") {
      val errs = errors(shapePatchedFirst)
      assert(errs.isEmpty)
    }
  }
}
