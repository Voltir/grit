package grit.core.tool

import grit.core.durable.Probes

import utest.*

/** What capture checking rejects about a tool that keeps in grit's store ([[Hosted.keeping]]):
  * the transaction its turn opens for a call is the call's alone, so a run that keeps it past
  * the call does not compile. Compiled against core.
  */
object KeepingToolCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.store.*
      |import grit.core.tool.*
      |import grit.core.visibility.{Integrity, Label}
      |val spec: ToolSpec[Unit] = ToolSpec(ToolName("probe"), "A probe.", Args.of(NamedTuple.Empty).map(_ => ()))
      |""".stripMargin

  private val keepsThrough =
    """def tool: Tool[Unit] =
      |  Hosted.keeping(spec, _ => "", Integrity.Outside, Believed.Default)((_, _, _) =>
      |    Right(Outcome.Done(Label.shown(Tx.floor(summon[Tx^])))))""".stripMargin
  private val keptInVar =
    """def f(): Unit = {
      |  var kept: Option[Tx^] = None
      |  val _ = Hosted.keeping(spec, _ => "", Integrity.Outside, Believed.Default)((_, _, _) => {
      |    kept = Some(summon[Tx^]); Right(Outcome.Done(""))
      |  })
      |}""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(keepsThrough, keptInVar))

  val tests = Tests {
    test("control: a run that keeps through the transaction it is given compiles, as a pure tool") {
      compiled.errors(keepsThrough) ==> Nil
    }

    test("a run that keeps the transaction it is given past its call is rejected") {
      compiled
        .errors(keptInVar)
        .exists(e => e.contains("Tx^") && e.contains("variable kept")) ==> true
    }
  }
}
