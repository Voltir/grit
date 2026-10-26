package grit.knowledge.declassify

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects of a plugin's code around a declassification: it calls
  * [[grit.core.act.Dreaming.declassify]] and keeps what it hands back, but never reads a rewrite's
  * text or reaches the move's raise or its primitives. That it makes neither the declassification
  * nor the row a keeper keeps is pinned in core's `DowngradeCaptureTests`. Each probe holds one
  * breach; the control must compile.
  *
  * Expires: delete once capture and separation checking are no longer experimental. Until then
  * an upgrade can change what they reject silently; this suite is re-read at each Scala bump.
  */
object DeclassifyCaptureTests extends TestSuite {

  private val prelude =
    """package grit.knowledge.probe
      |import grit.core.act.*
      |import grit.core.document.*
      |import grit.core.id.DocumentVersionId
      |""".stripMargin

  private val control =
    """def f(m: Dreaming^, v: DocumentVersionId, a: PolicyApplication) =
      |  m.declassify(MoveName("d"), v, "r1", a)
      |""".stripMargin
  private val textRead = "def f(d: Declassified) = d.text"
  private val raiseReached =
    "def f(m: Dreaming^) = m.declassifier"
  private val primitiveCalled =
    """def f(m: Dreaming^, v: DocumentVersionId, a: PolicyApplication) =
      |  m.rewrite(MoveName("d"), v, "r1", a)
      |""".stripMargin
  private val screenCalled =
    """def f(m: Dreaming^, v: DocumentVersionId, a: PolicyApplication, t: DocText) =
      |  m.screen(MoveName("d"), v, "r1", a, t, Declassified.Attempt.First)
      |""".stripMargin
  private val baselineCalled =
    """def f(m: Dreaming^, v: DocumentVersionId, a: PolicyApplication) =
      |  m.baseline(MoveName("d"), v, "r1", a)
      |""".stripMargin

  private lazy val compiled = Probes.batch(
    prelude,
    List(control, textRead, raiseReached, primitiveCalled, screenCalled, baselineCalled)
  )

  private def says(text: String)(found: List[String]): Boolean = found.exists(_.contains(text))

  val tests = Tests {
    test("control: a plugin's code calls declassify") {
      compiled.errors(control) ==> Nil
    }

    test("a plugin reads no rewrite's text") {
      assert(says("private[core] value text can only be accessed")(compiled.errors(textRead)))
    }

    test("a plugin reaches neither a declassify's raise nor its primitives") {
      assert(
        says("protected method declassifier")(compiled.errors(raiseReached)),
        says("protected method rewrite")(compiled.errors(primitiveCalled)),
        says("protected method screen")(compiled.errors(screenCalled)),
        says("protected method baseline")(compiled.errors(baselineCalled))
      )
    }
  }
}
