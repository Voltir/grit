package grit.turn

import java.nio.file.Files

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import utest.*

/** What capture checking rejects about the tools a turn is offered, pinned by compiling
  * probe sources against turn with turn's own flags: a [[TurnTooling]]'s tools act only
  * through the capabilities it names, so a read-only turn cannot be handed a tool that
  * edits, and no turn a tool that acts through anything else. The pattern is
  * `grit.core.tool.ToolCaptureTests` (docs/capture-checking.md).
  */
object TurnCaptureTests extends TestSuite {

  private val classpath = sys.env.getOrElse("GRIT_PROBE_CLASSPATH", "")
  private val options = sys.env.getOrElse("GRIT_PROBE_OPTIONS", "").split(" ").toList

  private val prelude =
    """package probe
      |import grit.core.host.*
      |import grit.core.store.Jot
      |import grit.core.tool.*
      |import grit.turn.*
      |object Probe {
      |  val spec: ToolSpec[String] = ToolSpec(ToolName("probe"), "A probe.", Args.of((path = Field.text("A path."))).map(_.path))
      |  def reads(ws: Workspace^): Tool[String]^{ws} =
      |    new Tool(spec, Gate.Free, p => p, p => RelPath.of(p).flatMap(ws.read(_, Lines.All)).fold(e => Outcome.Failed(e.toString), c => Outcome.Done(c.show)))
      |  def writes(e: Edits^): Tool[String]^{e} =
      |    new Tool(spec, Gate.Free, p => p, p => RelPath.of(p).flatMap(e.write(_, "x")).fold(_ => Outcome.Failed("no"), _ => Outcome.Done("ok")))
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

  /** The planted breach: [[TurnTooling.ReadOnly]]'s shape, its tools not tied to its
    * workspace.
    */
  private val breach =
    """final case class Breached[C^](workspace: Workspace^, tools: Toolbox[C], jot: Jot^, budget: TurnLoop.Budget)
      |""".stripMargin

  /** A toolbox that edits through `e`, handed to `tooling` as a read-only turn's tools over
    * `ws`, typed `result`.
    */
  private def editing(tooling: String, result: String) =
    s"""def offered(ws: Workspace^, e: Edits^, box: Toolbox[{ws, e}], j: Jot^, b: TurnLoop.Budget): $result =
      |  $tooling(ws, box, j, b)
      |""".stripMargin

  /** A toolbox that edits through `other`, not the tooling's `e`, handed to a full turn. */
  private val smuggled =
    """def offered(ws: Workspace^, e: Edits^, s: Shell^, other: Edits^, box: Toolbox[{ws, other}], j: Jot^, b: TurnLoop.Budget): TurnTooling^{ws, e, s, other, j} =
      |  TurnTooling.Full(ws, e, s, box, j, b)
      |""".stripMargin

  private def rejected(errs: List[String]): Boolean =
    errs.exists(e => e.contains("Found:") && e.contains("Required:"))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("a read-only turn offered reading tools, and a full one offered editing, compile") {
      val errs = errors(
        """def reading(ws: Workspace^, j: Jot^, b: TurnLoop.Budget): Option[TurnTooling^{ws, j}] =
          |  Toolbox.of[{ws}](reads(ws)).toOption.map(box => TurnTooling.ReadOnly(ws, box, j, b))
          |def editing(ws: Workspace^, e: Edits^, s: Shell^, j: Jot^, b: TurnLoop.Budget): Option[TurnTooling^{ws, e, s, j}] =
          |  Toolbox.of[{ws, e}](reads(ws), writes(e)).toOption.map(box => TurnTooling.Full(ws, e, s, box, j, b))
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a read-only turn offered a tool that edits is rejected") {
      assert(rejected(errors(editing("TurnTooling.ReadOnly", "TurnTooling^{ws, e, j}"))))
    }

    test("a read-only tooling whose tools are not tied to its workspace takes that tool") {
      // The breach the test above guards against, planted in a fixture: watched making that
      // test fail, kept so the rejection is known to come from the link alone.
      val errs = errors(breach + editing("Breached", "Breached[{ws, e}]^{ws, j}"))
      assert(errs.isEmpty)
    }

    test("a full turn offered a tool that edits through another capability is rejected") {
      assert(rejected(errors(smuggled)))
    }

    test("capture checking is what rejects the tool that edits") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val sources = Vector(editing("TurnTooling.ReadOnly", "TurnTooling^{ws, e, j}"), smuggled)
      val errs = sources.flatMap(s => compile(erased(prelude + s + "\n}\n"), flags))
      assert(errs.isEmpty)
    }
  }
}
