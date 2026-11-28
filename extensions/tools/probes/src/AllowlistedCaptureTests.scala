package grit.tools

import grit.core.durable.Probes

import utest.*

/** What `Coding.allowlisted`'s declared type refuses: its toolbox acts only through the checkout,
  * its edits and one [[grit.core.host.Allowlisted]], so a tool beside them that starts programs
  * through a whole `Processes` or `Runs`, or holds a transaction, does not fit it.
  */
object AllowlistedCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import scala.concurrent.duration.Duration
      |import grit.core.host.{Allowlisted, Argv, Edits, Processes, RelPath, Runs, Uncommitted, Workspace}
      |import grit.core.store.Tx
      |import grit.core.tool.{Args, Believed, Gate, Hosted, Outcome, SetRefusal, ToolName, ToolSpec, Toolbox}
      |import grit.tools.{Coding, MachineGuard, Refusals}
      |""".stripMargin

  /* A model of `Coding.allowlisted` at its declared type, one of its tools running `body`. */
  private def model(params: String, body: String): String =
    s"""def made(ws: Workspace^, e: Edits^, r: Allowlisted^$params): Either[SetRefusal, Toolbox[caps.CapSet^{ws, e, r}]] =
       |  Toolbox.of(
       |    Coding.read(ws),
       |    Coding.edit(e),
       |    Hosted[Unit](
       |      ToolSpec(ToolName("run"), "d", Args.of(NamedTuple.Empty).map(_ => ())),
       |      Gate.Free,
       |      _ => "",
       |      Believed.Default
       |    ).over(_ => { $body; Outcome.Done("") })
       |  )""".stripMargin

  private val itself =
    "def made(ws: Workspace^, e: Edits^, r: Allowlisted^, g: MachineGuard^, f: Refusals^, t: Uncommitted^): Either[SetRefusal, Toolbox[caps.CapSet^{ws, e, r, g, f, t}]] = Coding.allowlisted(ws, e, r, g, f, None, t)"
  private val forgettingRefusals =
    "def forgetting(ws: Workspace^, e: Edits^, r: Allowlisted^, g: MachineGuard^, f: Refusals^, t: Uncommitted^): Either[SetRefusal, Toolbox[caps.CapSet^{ws, e, r, g, t}]] = Coding.allowlisted(ws, e, r, g, f, None, t)"
  private val forgettingTree =
    "def untreed(ws: Workspace^, e: Edits^, r: Allowlisted^, g: MachineGuard^, f: Refusals^, t: Uncommitted^): Either[SetRefusal, Toolbox[caps.CapSet^{ws, e, r, g, f}]] = Coding.allowlisted(ws, e, r, g, f, None, t)"
  private val throughAllowlisted =
    model(
      "",
      "val _ = r.start(Argv.of(\"true\").toOption.get, Duration(1, \"s\"), Duration(1, \"s\"))"
    )
  private val throughRuns =
    model(
      ", rs: Runs^",
      "val _ = rs.start(Argv.of(\"true\").toOption.get, Duration(1, \"s\"), Duration(1, \"s\"))"
    )
  private val throughProcesses =
    model(
      ", p: Processes^",
      "val _ = p.run(Argv.of(\"true\").toOption.get, RelPath.Root, Duration(1, \"s\"))"
    )
  private val holdingTx = model(", tx: Tx^", "val _ = tx")
  private lazy val compiled =
    Probes.batch(
      prelude,
      List(
        itself,
        forgettingRefusals,
        forgettingTree,
        throughAllowlisted,
        throughProcesses,
        throughRuns,
        holdingTx
      )
    )

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test(
      "control: the set itself, and a model running through its Allowlisted, compile at that type"
    ) {
      (compiled.errors(itself), compiled.errors(throughAllowlisted)) ==> (Nil, Nil)
    }

    test("a model whose tool starts programs through a whole Processes is rejected at that type") {
      assert(
        compiled
          .errors(throughProcesses)
          .exists(_.contains("capability `p` cannot flow into capture set {ws, e, r}"))
      )
    }

    test(
      "a model whose tool starts programs through a whole Runs, past the allowlist, is rejected"
    ) {
      assert(
        compiled
          .errors(throughRuns)
          .exists(_.contains("capability `rs` cannot flow into capture set {ws, e, r}"))
      )
    }

    test("the set typed without the refusals its run keeps is rejected") {
      assert(
        compiled
          .errors(forgettingRefusals)
          .exists(_.contains("capability `f` cannot flow into capture set {ws, e, r, g, t}"))
      )
    }

    test("the set typed without the tree its commits read is rejected") {
      assert(
        compiled
          .errors(forgettingTree)
          .exists(_.contains("capability `t` cannot flow into capture set {ws, e, r, g, f}"))
      )
    }

    test("a model whose tool holds a transaction in scope is rejected at that type") {
      assert(
        compiled
          .errors(holdingTx)
          .exists(_.contains("capability `tx` cannot flow into capture set {ws, e, r}"))
      )
    }
  }
}
