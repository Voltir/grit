package grit.tools

import grit.core.durable.Probes

import utest.*

/** `clearance` captures the store it is given and nothing else: its declared type,
  * `Tool[Unit]^{store}`, refuses a tool that also calls another capability.
  */
object ClearedCaptureTests extends TestSuite {

  private def probe(body: String): List[String] =
    Probes.errors(
      s"""package outside
         |import grit.core.store.{Askers, Db, Tx}
         |import grit.core.tool.{Args, Gate, Hosted, Outcome, Tool, ToolName, ToolSpec}
         |import grit.core.visibility.Subject
         |import grit.tools.Cleared
         |trait Provider extends caps.SharedCapability { def call(): String }
         |object Probe {
         |  $body
         |}
         |""".stripMargin
    )

  /* A model of `Cleared.tool` at its declared type, whose run does `runs` before reading. */
  private def model(runs: String): String =
    s"""def tool(askers: Askers, store: Db^, p: Provider^): Tool[Unit]^{store} =
       |    Hosted[Unit](
       |      ToolSpec(ToolName("clearance"), "d", Args.of(NamedTuple.Empty).map(_ => ())),
       |      Gate.Free,
       |      _ => ""
       |    ).calling((_, at) => {
       |      $runs
       |      store.read(Subject.Turn(at.turn))((tx: Tx^) ?=> Right("x")).fold(_ => Outcome.Failed("x"), Outcome.Done(_))
       |    })""".stripMargin

  val tests = Tests {
    test("control: the tool itself, and a model reading only the store, compile at that type") {
      (
        probe(
          "def made(askers: Askers, store: Db^): Tool[Unit]^{store} = Cleared.tool(askers, store)"
        ),
        probe(model("()"))
      ) ==> (Nil, Nil)
    }

    test("a model that also calls another capability is rejected at that type") {
      val errs = probe(model("val _ = p.call()"))
      assert(errs.exists(e => e.contains("p") && e.contains("cannot flow into capture set")))
    }
  }
}
