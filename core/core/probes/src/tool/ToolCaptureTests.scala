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
      |val spec: ToolSpec[String] = ToolSpec(ToolName("probe"), "A probe.", Args.of((path = Field.text("A path."))).map(_.path))
      |def reads(ws: Workspace^): Tool[String]^{ws} =
      |  Tool(spec, Gate.Free, p => p, Believed.Default, p => RelPath.of(p).flatMap(ws.read(_, Lines.All)).fold(e => Outcome.Failed(e.toString), c => Outcome.Done(c.show)))
      |def writes(e: Edits^): Tool[String]^{e} =
      |  Tool(spec, Gate.Free, p => p, Believed.Default, p => RelPath.of(p).flatMap(e.write(_, "x")).fold(_ => Outcome.Failed("no"), _ => Outcome.Done("ok")))
      |""".stripMargin

  /** The error messages from compiling `body` after the prelude, with every probe below. */
  private def errors(body: String): List[String] = compiled.errors(body)

  /** A read-only toolbox that is handed a tool that edits. */
  private val smuggled =
    """def readOnly(ws: Workspace^, e: Edits^): Either[SetRefusal, Toolbox[{ws}]] =
      |  Toolbox.of(reads(ws), writes(e))
      |""".stripMargin

  /** A toolbox of the store handed a tool, told its call, whose run also edits. */
  private val toldEdits =
    """def told(store: Workspace^, e: Edits^): Either[SetRefusal, Toolbox[{store}]] =
      |  Toolbox.of(new Hosted(spec, Gate.Free, p => p, Believed.Default).calling((p, at) => { val _ = RelPath.of(p).map(e.write(_, at.key)); reads(store); Outcome.Done(at.key) }))
      |""".stripMargin

  private def rejected(errs: List[String]): Boolean =
    errs.exists(e => e.contains("Found:") && e.contains("Required:"))

  private val readOnlyAndAll =
    """def readOnly(ws: Workspace^): Either[SetRefusal, Toolbox[{ws}]] = Toolbox.of(reads(ws))
      |def all(ws: Workspace^, e: Edits^): Either[SetRefusal, Toolbox[{ws, e}]] =
      |  Toolbox.of(reads(ws), writes(e))
      |def told(store: Workspace^): Either[SetRefusal, Toolbox[{store}]] =
      |  Toolbox.of(new Hosted(spec, Gate.Free, p => p, Believed.Default).calling((p, at) => { reads(store); Outcome.Done(at.key) }))
      |""".stripMargin

  private val sneaky =
    """def sneaky(ws: Workspace^, e: Edits^): Tool[String]^{ws} =
      |  Tool(spec, Gate.Free, p => p, Believed.Default, p => { writes(e); reads(ws); Outcome.Done(p) })
      |""".stripMargin

  /** A hosted tool whose logging function, meant to be pure, edits. */
  private val loggingEdits =
    """def logs(e: Edits^): Hosted[String] =
      |  new Hosted(spec, Gate.Free, p => p, Believed.Default).logging((p, _, _) => { val _ = RelPath.of(p).map(e.write(_, "x")); None })
      |""".stripMargin

  /** A hosted tool logged as what its argument names, purely. */
  private val loggingPure =
    """def logs: Hosted[String] =
      |  new Hosted(spec, Gate.Free, p => p, Believed.Default).logging((p, _, _) => Some(grit.core.session.Logged.Read(grit.core.session.Logged.shaped("path", p))))
      |""".stripMargin

  private lazy val compiled =
    Probes.batch(
      prelude,
      List(readOnlyAndAll, smuggled, sneaky, toldEdits, loggingEdits, loggingPure)
    )

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("a read-only toolbox of reading tools compiles, and so does one that edits") {
      val errs = errors(readOnlyAndAll)
      assert(errs.isEmpty)
    }

    test("a read-only toolbox holding a tool that edits is rejected") {
      assert(rejected(errors(smuggled)))
    }

    test("a tool typed as reading that edits is rejected") {
      val errs = errors(sneaky)
      assert(rejected(errs))
    }

    test("a store's toolbox holding a tool, told its call, whose run edits is rejected") {
      assert(flowsInto("{store}")(errors(toldEdits)))
    }

    test("a hosted tool logged purely compiles") {
      assert(errors(loggingPure).isEmpty)
    }

    test("a hosted tool whose logging function edits is rejected") {
      assert(rejected(errors(loggingEdits)))
    }

    test("capture checking is what rejects the smuggled tools") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val errs = Probes.errors(erased(prelude + smuggled + toldEdits + loggingEdits), flags)
      assert(errs.isEmpty)
    }
  }
}
