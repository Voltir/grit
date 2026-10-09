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
      |object Probe {
      |  def asked(p: Provider^, request: ModelRequest, schema: JsonSchema, c: Clock^)(using d: Durable^): String = {
      |""".stripMargin

  private def errors(body: String): List[String] =
    Probes.errors(prelude + body + "\n  }\n}\n")

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

  val tests = Tests {
    test("a phase's step whose patch is read before it compiles") {
      val errs = errors("val fresh = d.patch(\"ask-again\")\n" + asking("fresh"))
      assert(classpath.nonEmpty, errs.isEmpty)
    }

    test("a phase's step whose body reads a patch is rejected") {
      val errs = errors(asking("d.patch(\"ask-again\")"))
      assert(errs.exists(_.contains("Separation failure")))
    }

    test("a JSON ask's step whose patch is read before it compiles") {
      val errs = errors("val fresh = d.patch(\"repair\")\n" + shaping("fresh"))
      assert(errs.isEmpty)
    }

    test("a JSON ask's step whose body reads a patch is rejected") {
      val errs = errors(shaping("d.patch(\"repair\")"))
      assert(errs.exists(_.contains("Separation failure")))
    }
  }
}
