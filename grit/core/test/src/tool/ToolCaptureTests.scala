package grit.core.tool

import java.nio.file.Files

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import utest.*

/** What capture checking rejects about a [[Tool]]'s power, pinned by compiling probe sources
  * against core with core's own flags: a tool or toolbox typed as acting only through a
  * [[grit.core.host.Workspace]] cannot hold one that edits. `assertCompileError` cannot see
  * capture errors (docs/capture-checking.md); the pattern is
  * `grit.core.durable.SeparationTests`.
  */
object ToolCaptureTests extends TestSuite {

  private val classpath = sys.env.getOrElse("GRIT_PROBE_CLASSPATH", "")
  private val options = sys.env.getOrElse("GRIT_PROBE_OPTIONS", "").split(" ").toList

  private val prelude =
    """package probe
      |import grit.core.host.*
      |import grit.core.tool.*
      |object Probe {
      |  val spec: ToolSpec[String] = ToolSpec(ToolName("probe"), "A probe.", Args.of((path = Field.text("A path."))).map(_.path))
      |  def reads(ws: Workspace^): Tool[String]^{ws} =
      |    new Tool(spec, Gate.Free, p => RelPath.of(p).flatMap(ws.read(_, Lines.All)).fold(e => Outcome.Failed(e.toString), c => Outcome.Done(c.show)))
      |  def writes(e: Edits^): Tool[String]^{e} =
      |    new Tool(spec, Gate.Free, p => RelPath.of(p).flatMap(e.write(_, "x")).fold(_ => Outcome.Failed("no"), _ => Outcome.Done("ok")))
      |""".stripMargin

  /** The error messages from compiling `body` inside `object Probe`. */
  private def errors(body: String, flags: List[String] = options): List[String] =
    compile(prelude + body + "\n}\n", flags)

  /** `source` with its capture sets erased, as code written without capture checking. */
  private def erased(source: String): String =
    source
      .replaceAll("Toolbox\\[\\{[^}]*\\}\\]", "Toolbox[?]")
      .replaceAll("\\^\\{[^}]*\\}", "")
      .replace("^", "")

  private def compile(code: String, flags: List[String]): List[String] = {
    val dir = Files.createTempDirectory("grit-tool-probe")
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

  /** A read-only toolbox that is handed a tool that edits. */
  private val smuggled =
    """def readOnly(ws: Workspace^, e: Edits^): Either[DuplicateName, Toolbox[{ws}]] =
      |  Toolbox.of(reads(ws), writes(e))
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
          |  new Tool(spec, Gate.Free, p => { writes(e); reads(ws); Outcome.Done(p) })
          |""".stripMargin
      )
      assert(rejected(errs))
    }

    test("capture checking is what rejects the smuggled tool") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val errs = compile(erased(prelude + smuggled + "\n}\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
