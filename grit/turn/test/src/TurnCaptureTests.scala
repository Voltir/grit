package grit.turn

import java.nio.file.Files

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import utest.*

/** What capture checking rejects about the tools a turn is offered, pinned by compiling
  * probe sources against turn with turn's own flags: a [[TurnTooling]]'s tools act only
  * through its workspace, so a turn cannot be handed one that edits. The pattern is
  * `grit.core.tool.ToolCaptureTests` (docs/capture-checking.md).
  */
object TurnCaptureTests extends TestSuite {

  private val classpath = sys.env.getOrElse("GRIT_PROBE_CLASSPATH", "")
  private val options = sys.env.getOrElse("GRIT_PROBE_OPTIONS", "").split(" ").toList

  private val prelude =
    """package probe
      |import grit.core.host.*
      |import grit.core.tool.*
      |import grit.turn.*
      |object Probe {
      |  val spec: ToolSpec[String] = ToolSpec(ToolName("probe"), "A probe.", Args.of((path = Field.text("A path."))).map(_.path))
      |  def reads(ws: Workspace^): Tool[String]^{ws} =
      |    new Tool(spec, Gate.Free, p => RelPath.of(p).flatMap(ws.read(_, Lines.All)).fold(e => Outcome.Failed(e.toString), c => Outcome.Done(c.show)))
      |  def writes(e: Edits^): Tool[String]^{e} =
      |    new Tool(spec, Gate.Free, p => RelPath.of(p).flatMap(e.write(_, "x")).fold(_ => Outcome.Failed("no"), _ => Outcome.Done("ok")))
      |""".stripMargin

  private def errors(body: String, flags: List[String] = options): List[String] =
    compile(prelude + body + "\n}\n", flags)

  /** `source` with its capture sets erased, as code written without capture checking. */
  private def erased(source: String): String =
    source
      .replaceAll("Toolbox\\[\\{[^}]*\\}\\]", "Toolbox[?]")
      .replaceAll("\\^\\{[^}]*\\}", "")
      .replace("^", "")

  private def compile(code: String, flags: List[String]): List[String] = {
    val dir = Files.createTempDirectory("grit-turn-probe")
    try {
      val source = Files.writeString(dir.resolve("Probe.scala"), code)
      val args = flags ++ List("-classpath", classpath, "-d", dir.toString, source.toString)
      // The compiler only reads the array; separation checking treats any array as mutable.
      val argv = caps.unsafe.unsafeAssumePure(args.toArray)
      new Driver().process(argv, StoreReporter(), null).allErrors.map(_.message)
    } finally {
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
    }
  }

  /** A turn handed a toolbox that edits. */
  private val smuggled =
    """def offered(ws: Workspace^, e: Edits^, box: Toolbox[{ws, e}], b: TurnLoop.Budget) =
      |  TurnTooling(ws, box, b, strict = false)
      |""".stripMargin

  private def rejected(errs: List[String]): Boolean =
    errs.exists(e => e.contains("Found:") && e.contains("Required:"))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("a turn offered tools that read its workspace compiles") {
      val errs = errors(
        """def offered(ws: Workspace^, b: TurnLoop.Budget): Option[TurnTooling^{ws}] =
          |  Toolbox.of[{ws}](reads(ws)).toOption.map(box => TurnTooling(ws, box, b, strict = false))
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a turn offered a toolbox that edits is rejected") {
      assert(rejected(errors(smuggled)))
    }

    test("capture checking is what rejects the toolbox that edits") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val errs = compile(erased(prelude + smuggled + "\n}\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
