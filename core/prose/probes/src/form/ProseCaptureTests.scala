package grit.prose.form

import grit.core.durable.Probes

import utest.*

/** What capture checking rejects of a doc's notes: a note is pure, so no capability (a
  * transaction, a client) rides in prose to wherever the doc is rendered or kept. Pinned by
  * compiling probe sources against prose with its own flags; `assertCompileError` cannot see
  * capture errors (docs/capture-checking.md). Prose names no core, so a local
  * `SharedCapability` stands in for a transaction.
  */
object ProseCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.prose.form.{Block, Doc, Span, Text}
      |
      |class C extends caps.SharedCapability
      |final case class P(n: Int) extends caps.Pure
      |""".stripMargin

  private val capabilityDoc =
    """def f(c: C^): Boolean = {
      |  val d: Doc[C^] = Doc(Vector.empty)
      |  d.isEmpty
      |}
      |""".stripMargin

  private val capabilityNote =
    """def f(c: C^): String = Span("x", Set.empty, Vector(c)).text
      |""".stripMargin

  private val pureNote =
    """val d: Doc[P] = Doc(Vector(Block.Paragraph(Text(Vector(Span("x", Set.empty, Vector(P(1))))))))
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(capabilityDoc, capabilityNote, pureNote))

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.options.contains("-language:experimental.captureChecking"))
    }

    test("a doc whose notes are a capability is rejected") {
      compiled.errors(capabilityDoc) ==> List(
        "Type argument probe.C^ does not conform to upper bound scala.caps.Pure"
      )
    }

    test("a span noting a capability is rejected") {
      val errs = compiled.errors(capabilityNote)
      assert(
        errs.size == 1,
        errs.forall(e =>
          e.contains("Found:    (c : probe.C^)") && e.contains("Required: scala.caps.Pure")
        )
      )
    }

    test("a doc noting a pure case class is accepted") {
      compiled.errors(pureNote) ==> Nil
    }
  }
}
