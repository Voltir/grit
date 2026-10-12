package grit.core.tool

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, erased, flowsInto, options}

import utest.*

/** What capture checking rejects about a [[Tool]]'s power, pinned by compiling probe sources
  * against core with core's own flags: a tool or toolbox typed as acting only through a
  * [[grit.core.host.Workspace]] cannot hold one that edits, whether or not its run is told its
  * call ([[Hosted.calling]]). `assertCompileError` cannot see
  * capture errors (docs/capture-checking.md); the pattern is
  * `grit.core.durable.SeparationTests`.
  */
object ToolCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.host.*
      |import grit.core.tool.*
      |object Probe {
      |  val spec: ToolSpec[String] = ToolSpec(ToolName("probe"), "A probe.", Args.of((path = Field.text("A path."))).map(_.path))
      |  def reads(ws: Workspace^): Tool[String]^{ws} =
      |    Tool(spec, Gate.Free, p => p, p => RelPath.of(p).flatMap(ws.read(_, Lines.All)).fold(e => Outcome.Failed(e.toString), c => Outcome.Done(c.show)))
      |  def writes(e: Edits^): Tool[String]^{e} =
      |    Tool(spec, Gate.Free, p => p, p => RelPath.of(p).flatMap(e.write(_, "x")).fold(_ => Outcome.Failed("no"), _ => Outcome.Done("ok")))
      |""".stripMargin

  /** The error messages from compiling `body` inside `object Probe`. */
  private def errors(body: String, flags: List[String] = options): List[String] =
    Probes.errors(prelude + body + "\n}\n", flags)

  /** A read-only toolbox that is handed a tool that edits. */
  private val smuggled =
    """def readOnly(ws: Workspace^, e: Edits^): Either[DuplicateName, Toolbox[{ws}]] =
      |  Toolbox.of(reads(ws), writes(e))
      |""".stripMargin

  /** A toolbox of the store handed a tool, told its call, whose run also edits. */
  private val toldEdits =
    """def told(store: Workspace^, e: Edits^): Either[DuplicateName, Toolbox[{store}]] =
      |  Toolbox.of(new Hosted(spec, Gate.Free, p => p).calling((p, at) => { val _ = RelPath.of(p).map(e.write(_, at.key)); reads(store); Outcome.Done(at.key) }))
      |""".stripMargin

  private def rejected(errs: List[String]): Boolean =
    errs.exists(e => e.contains("Found:") && e.contains("Required:"))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("a read-only toolbox of reading tools compiles, and so does one that edits") {
      val errs = errors(
        """def readOnly(ws: Workspace^): Either[DuplicateName, Toolbox[{ws}]] = Toolbox.of(reads(ws))
          |def all(ws: Workspace^, e: Edits^): Either[DuplicateName, Toolbox[{ws, e}]] =
          |  Toolbox.of(reads(ws), writes(e))
          |def told(store: Workspace^): Either[DuplicateName, Toolbox[{store}]] =
          |  Toolbox.of(new Hosted(spec, Gate.Free, p => p).calling((p, at) => { reads(store); Outcome.Done(at.key) }))
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a read-only toolbox holding a tool that edits is rejected") {
      assert(rejected(errors(smuggled)))
    }

    test("a tool typed as reading that edits is rejected") {
      val errs = errors(
        """def sneaky(ws: Workspace^, e: Edits^): Tool[String]^{ws} =
          |  Tool(spec, Gate.Free, p => p, p => { writes(e); reads(ws); Outcome.Done(p) })
          |""".stripMargin
      )
      assert(rejected(errs))
    }

    test("a store's toolbox holding a tool, told its call, whose run edits is rejected") {
      assert(flowsInto("{store}")(errors(toldEdits)))
    }

    test("capture checking is what rejects the smuggled tools") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val errs = Probes.errors(erased(prelude + smuggled + toldEdits + "\n}\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
