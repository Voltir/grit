package grit.turn

import java.nio.file.Files

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import utest.*

/** What capture checking rejects about the tools a turn is offered, pinned by compiling
  * probe sources against turn with turn's own flags: a [[TurnTooling]]'s own tools act only
  * through the capabilities it names, so a turn cannot be handed a tool that edits unless
  * its type says so, and its hosted tools, which an edge runs, act through nothing here.
  * The pattern is `grit.core.tool.ToolCaptureTests` (docs/capture-checking.md).
  */
object TurnCaptureTests extends TestSuite {

  private val classpath = sys.env.getOrElse("GRIT_PROBE_CLASSPATH", "")
  private val options = sys.env.getOrElse("GRIT_PROBE_OPTIONS", "").split(" ").toList

  private val prelude =
    """package probe
      |import grit.core.host.*
      |import grit.core.model.ModelSettings
      |import grit.core.provider.Models
      |import grit.core.store.{Db, Jot}
      |import grit.core.tool.*
      |import grit.turn.*
      |object Probe {
      |  val spec: ToolSpec[String] = ToolSpec(ToolName("probe"), "A probe.", Args.of((path = Field.text("A path."))).map(_.path))
      |  extension (s: ToolSpec[String]) def hosted: Hosted[String] = new Hosted(s, Gate.Free, p => p)
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
      .replaceAll("(Toolbox|TurnTooling|Breached)\\[\\{[^}]*\\}\\]", "$1[?]")
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

  /** The planted breach: [[TurnTooling]]'s shape, its hosted tools allowed to capture. */
  private val breach =
    """final case class Breached[C^](tools: Toolbox[C], operator: Toolbox[C], hosted: Vector[Tool.Offered^], jot: Jot^, budget: TurnLoop.Budget)
      |""".stripMargin

  /** A toolbox that edits through `e`, handed to `tooling` as the tools of a turn whose own
    * tools act through `ws` alone.
    */
  private val editing =
    """def offered(ws: Workspace^, e: Edits^, box: Toolbox[{ws, e}], j: Jot^, b: TurnLoop.Budget): TurnTooling[{ws}]^{j} =
      |  TurnTooling(box, Toolbox.Empty, Vector.empty, j, b)
      |""".stripMargin

  /** A tool that edits through `e`, handed to `tooling` as a hosted tool: one the edge runs,
    * which must act through nothing here.
    */
  private def smuggled(tooling: String) =
    s"""def offered(e: Edits^, j: Jot^, b: TurnLoop.Budget): Option[$tooling[{}]^{j, e}] =
      |  Toolbox.of().toOption.map(box => $tooling(box, Toolbox.Empty, Vector(writes(e)), j, b))
      |""".stripMargin

  private def rejected(errs: List[String]): Boolean =
    errs.exists(e => e.contains("Found:") && e.contains("Required:"))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("a turn offered tools through what it names, and pure hosted tools, compiles") {
      val errs = errors(
        """def reading(ws: Workspace^, e: Edits^, j: Jot^, b: TurnLoop.Budget): Option[TurnTooling[{ws, e}]^{j}] =
          |  Toolbox.of[{ws, e}](reads(ws), writes(e)).toOption.map(box => TurnTooling[{ws, e}](box, Toolbox.Empty, Vector(spec.hosted), j, b))
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a turn whose tools act through its workspace alone is not handed a tool that edits") {
      assert(rejected(errors(editing)))
    }

    test("a hosted tool that acts through a capability here is rejected") {
      assert(rejected(errors(smuggled("TurnTooling"))))
    }

    test("a tooling whose hosted tools may capture takes that tool") {
      // The breach the test above guards against, planted in a fixture: watched making that
      // test fail, kept so the rejection is known to come from the hosted list's type alone.
      val errs = errors(breach + smuggled("Breached"))
      errs ==> List()
    }

    test("capture checking is what rejects the tool that edits") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val sources = Vector(editing, smuggled("TurnTooling"))
      val errs = sources.flatMap(s => compile(erased(prelude + s + "\n}\n"), flags))
      errs ==> Vector()
    }
  }
}
