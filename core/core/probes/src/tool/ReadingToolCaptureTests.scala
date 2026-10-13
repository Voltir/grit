package grit.core.tool

import grit.core.durable.Probes

import utest.*

/** What capture checking rejects about a tool that reads grit's store ([[Hosted.reading]]):
  * the reads its turn hands a call are the call's alone, so a run that keeps them past the
  * call does not compile. Compiled against core.
  */
object ReadingToolCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.store.*
      |import grit.core.tool.*
      |val described: Hosted[Unit] =
      |  Hosted(ToolSpec(ToolName("probe"), "A probe.", Args.of(NamedTuple.Empty).map(_ => ())), Gate.Free, _ => "")
      |""".stripMargin

  private val readsThrough =
    """def tool: Tool[Unit] =
      |  described.reading((_, _, reads) => reads.read(Right("x")).fold(_ => Outcome.Failed("x"), Outcome.Done(_)))""".stripMargin
  private val keptInVar =
    """def f(): Unit = {
      |  var kept: Option[Reads^] = None
      |  val _ = described.reading((_, _, reads) => { kept = Some(reads); Outcome.Done("") })
      |}""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(readsThrough, keptInVar))

  val tests = Tests {
    test("control: a run that reads through the reads it is handed compiles, as a pure tool") {
      compiled.errors(readsThrough) ==> Nil
    }

    test("a run that keeps the reads it is handed past its call is rejected") {
      compiled
        .errors(keptInVar)
        .exists(e => e.contains("Reads^") && e.contains("variable kept")) ==> true
    }
  }
}
