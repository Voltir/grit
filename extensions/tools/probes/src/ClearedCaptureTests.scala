package grit.tools

import grit.core.durable.Probes

import utest.*

/** `clearance` captures nothing: it reads through the reads its turn hands each call, so its
  * declared type, `Tool[Unit]`, refuses a tool that also reads a store of its own.
  */
object ClearedCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.store.{Askers, Db, Tx}
      |import grit.core.tool.{Args, Gate, Hosted, Outcome, Tool, ToolName, ToolSpec}
      |import grit.core.visibility.Subject
      |import grit.tools.Cleared
      |""".stripMargin

  /* A model of `Cleared.tool` at its declared type, its run reading through `reads`. */
  private def model(reads: String): String =
    s"""def tool(askers: Askers, store: Db^): Tool[Unit] =
       |    Hosted[Unit](
       |      ToolSpec(ToolName("clearance"), "d", Args.of(NamedTuple.Empty).map(_ => ())),
       |      Gate.Free,
       |      _ => ""
       |    ).reading((_, at, handed) =>
       |      $reads.read((tx: Tx^) ?=> Right("x")).fold(_ => Outcome.Failed("x"), Outcome.Done(_))
       |    )""".stripMargin

  private val itself = "def made(askers: Askers): Tool[Unit] = Cleared.tool(askers)"
  private val handed = model("handed")
  private val ownStore = model("store.as(Subject.Turn(at.turn))")
  private lazy val compiled = Probes.batch(prelude, List(itself, handed, ownStore))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: the tool itself, and a model reading what it is handed, compile at that type") {
      (compiled.errors(itself), compiled.errors(handed)) ==> (Nil, Nil)
    }

    test("a model that reads a store of its own is rejected at that type") {
      val errs = compiled.errors(ownStore)
      assert(errs.exists(e => e.contains("store") && e.contains("cannot flow into capture set")))
    }
  }
}
