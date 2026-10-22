package grit.prose.json

import grit.core.durable.Probes

import utest.*

/** What capture checking rejects of stored prose's note codecs: each is a pure function, so a
  * codec cannot reach a capability (a transaction, a client) while a doc is written or read.
  * Pinned by compiling probe sources against prose with its own flags; `assertCompileError`
  * cannot see capture errors (docs/capture-checking.md). A local `SharedCapability` stands in
  * for a transaction.
  */
object ProseJsonCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.prose.form.Doc
      |import grit.prose.json.ProseJson
      |
      |class C extends caps.SharedCapability { def tag: String = "c" }
      |final case class P(n: Int) extends caps.Pure
      |""".stripMargin

  private val pureCodec =
    """val doc: Doc[P] = Doc(Vector.empty)
      |val json: ujson.Value = ProseJson.write(doc, (p: P) => ujson.Num(p.n))
      |val back = ProseJson.read(json, (v: ujson.Value) => v.numOpt.map(n => P(n.toInt)).toRight("no"))
      |""".stripMargin

  private val capturingWriter =
    """def f(c: C^): ujson.Value =
      |  ProseJson.write(Doc(Vector.empty): Doc[P], (p: P) => ujson.Str(c.tag))
      |""".stripMargin

  private val capturingReader =
    """def f(c: C^, json: ujson.Value) =
      |  ProseJson.read(json, (v: ujson.Value) => Right(P(c.tag.length)))
      |""".stripMargin

  private lazy val compiled =
    Probes.batch(prelude, List(pureCodec, capturingWriter, capturingReader))

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.options.contains("-language:experimental.captureChecking"))
    }

    test("pure note codecs are accepted") {
      compiled.errors(pureCodec) ==> Nil
    }

    test("a note writer capturing a capability is rejected") {
      val errs = compiled.errors(capturingWriter)
      assert(
        errs.size == 1,
        errs.forall(e =>
          e.contains("Required: probe.P -> ujson.Value") &&
            e.contains("capability `c` cannot flow into capture set {}")
        )
      )
    }

    test("a note reader capturing a capability is rejected") {
      val errs = compiled.errors(capturingReader)
      assert(
        errs.size == 1,
        errs.forall(e =>
          e.contains("Required: ujson.Value -> Either[String, probe.P]") &&
            e.contains("capability `c` cannot flow into capture set {}")
        )
      )
    }
  }
}
