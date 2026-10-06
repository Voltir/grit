package grit.core.plugin

import java.nio.file.Files

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import utest.*

/** What capture checking rejects about a plugin (ADR 0027): a plugin, its tools' runs and the
  * services it exports hold no capability, so a deployment can be built outside any and a
  * service handed between plugins carries none. Pinned by compiling probe sources against
  * core with core's own flags, as `grit.core.tool.ToolCaptureTests` does; `assertCompileError`
  * cannot see capture errors (docs/capture-checking.md).
  */
object PluginCaptureTests extends TestSuite {

  private val classpath = sys.env.getOrElse("GRIT_PROBE_CLASSPATH", "")
  private val options = sys.env.getOrElse("GRIT_PROBE_OPTIONS", "").split(" ").toList

  private val prelude =
    """package probe
      |import grit.core.document.*
      |import grit.core.id.{CallSlot, PluginName}
      |import grit.core.job.*
      |import grit.core.plugin.*
      |import grit.core.store.*
      |import grit.core.tool.*
      |
      |trait Activity extends caps.Pure {
      |  def recent(n: Int)(using Tx^): Either[StoreError, Vector[String]]
      |}
      |final class Lines(own: PluginReads) extends Activity {
      |  def recent(n: Int)(using Tx^): Either[StoreError, Vector[String]] =
      |    own.cache.newest("", n).map(_.map(_._1))
      |}
      |object Recent extends PluginTool[Int] {
      |  val described: Hosted[Int] =
      |    new Hosted(ToolSpec(ToolName("recent"), "Recent lines.", Args.of((n = Field.count("How many.", 1, 5))).map(_.n)), Gate.Free, n => n.toString)
      |  def bind(own: PluginReads, needs: Needs, jobs: OwnJobs): Either[Unneeded | NotOwn, PluginRun[Int]] =
      |    Right(new PluginRun[Int] {
      |      def run(n: Int, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome =
      |        db.read(new Lines(own).recent(n)).fold(e => Outcome.Failed(e.toString), l => Outcome.Done(l.mkString("\n")))
      |    })
      |}
      |object Noted extends Documents {
      |  val terms: DocumentTerms =
      |    DocLabel.of("notes").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, scala.concurrent.duration.FiniteDuration(1, "day"), 10)).fold(sys.error, identity)
      |  def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit] = Right(())
      |}
      |final class Kept(val name: PluginName) extends Exports[Activity] {
      |  val version: Int = 1
      |  override val cache: Option[CachePosting] = Some(new CachePosting {
      |    def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] = docs.put("k", ujson.Str("v"))
      |  })
      |  override val documents: Option[Documents] = Some(Noted)
      |  override val tools: Vector[PluginTool[?]] = Vector(Recent)
      |  def service(own: PluginReads): Activity = new Lines(own)
      |}
      |final class Reader(val name: PluginName, kept: Kept) extends Plugin {
      |  val version: Int = 1
      |  override val needs: Vector[Exports[?]] = Vector(kept)
      |}
      |""".stripMargin

  /** The error messages from compiling `body` after the prelude. */
  private def errors(body: String, flags: List[String] = options): List[String] =
    compile(prelude + body + "\n", flags)

  /** `source` with its capture sets erased, as code written without capture checking. */
  private def erased(source: String): String =
    source
      .replaceAll("Toolbox\\[\\{[^}]*\\}\\]", "Toolbox[?]")
      .replaceAll("\\^\\{[^}]*\\}", "")
      .replace("^", "")

  private def compile(code: String, flags: List[String]): List[String] = {
    val dir = Files.createTempDirectory("grit-plugin-probe")
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

  /** A plugin holding a store it was built with. */
  private val holdsDb =
    """final class Holds(val name: PluginName, val db: Db^) extends Plugin {
      |  val version: Int = 1
      |}
      |""".stripMargin

  /** A plugin holding a function that reads a store. */
  private val holdsClosure =
    """final class Closes(val name: PluginName, val peek: () => Int) extends Plugin {
      |  val version: Int = 1
      |}
      |""".stripMargin

  /** A plugin's documents posting through a store it was built with. */
  private val documentsHoldDb =
    """final class DocsHold(db: Db^) extends Documents {
      |  val terms: DocumentTerms = Noted.terms
      |  def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit] = db.read(Right(()))
      |}
      |""".stripMargin

  /** A service that reads through a store it captured, rather than the transaction it is given. */
  private val serviceCapturesDb =
    """final class Leaky(db: Db^) extends Activity {
      |  def recent(n: Int)(using Tx^): Either[StoreError, Vector[String]] = db.read(Right(Vector.empty))
      |}
      |""".stripMargin

  /** A service type with no purity, exported. */
  private val serviceImpure =
    """trait Loose { def recent(n: Int): Vector[String] }
      |final class Exporting(val name: PluginName) extends Exports[Loose] {
      |  val version: Int = 1
      |  def service(own: PluginReads): Loose = new Loose { def recent(n: Int): Vector[String] = Vector.empty }
      |}
      |""".stripMargin

  /** A run that keeps the store it is handed for a later call. */
  private val runKeepsDb =
    """final class Keeps extends PluginRun[Int] {
      |  var kept: Option[Db^] = None
      |  def run(n: Int, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome = { kept = Some(db); Outcome.Done("") }
      |}
      |""".stripMargin

  /** A run that stashes the store it is handed in a mutable collection. */
  private val runStashesDb =
    """final class Stashes extends PluginRun[Int] {
      |  val kept: scala.collection.mutable.ArrayBuffer[Db^] = scala.collection.mutable.ArrayBuffer.empty
      |  def run(n: Int, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome = { kept += db; Outcome.Done("") }
      |}
      |""".stripMargin

  /** A service that keeps the transaction it is handed. */
  private val serviceKeepsTx =
    """final class KeepsTx extends Activity {
      |  var kept: Option[Tx^] = None
      |  def recent(n: Int)(using t: Tx^): Either[StoreError, Vector[String]] = { kept = Some(t); Right(Vector.empty) }
      |}
      |""".stripMargin

  /** A tool whose run reads a store it captured, not the one the kit hands it. */
  private val toolHoldsDb =
    """final class ToolHolds(db: Db^) extends PluginTool[Int] {
      |  val described: Hosted[Int] = Recent.described
      |  def bind(own: PluginReads, needs: Needs, jobs: OwnJobs): Either[Unneeded | NotOwn, PluginRun[Int]] =
      |    Right(new PluginRun[Int] {
      |      def run(n: Int, call: CallSlot, handed: Db^, desk: ScheduleDesk^): Outcome = db.read(Right(n)).fold(_ => Outcome.Failed("no"), _ => Outcome.Done("ok"))
      |    })
      |}
      |""".stripMargin

  /** Whether `errs` reject a class for holding a capability its pure self type excludes. */
  private def heldImpure(errs: List[String]): Boolean =
    errs.exists(_.contains("is not included in the allowed capture set {} of the self type"))

  /** Whether `errs` reject a capability flowing into a capture set that may not hold it. */
  private def flowsInto(set: String)(errs: List[String]): Boolean =
    errs.exists(_.contains(s"cannot flow into capture set $set"))

  private val breaches: Vector[String] = Vector(
    holdsDb,
    holdsClosure,
    documentsHoldDb,
    serviceCapturesDb,
    runKeepsDb,
    runStashesDb,
    serviceKeepsTx,
    toolHoldsDb
  )

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("a pure plugin with a tool, a cache, documents, needs and an exported service compiles") {
      val errs = errors(
        """object Store {
          |  def toolbox(store: Db^, desk: ScheduleDesk^, r: PluginRun[Int]): Either[DuplicateName, Toolbox[{store, desk}]] =
          |    Toolbox.of(Recent.described.calling((n, at) => r.run(n, at, store, desk)))
          |  def through(needs: Needs, kept: Kept): Either[Unneeded, Activity] = needs.of(kept)
          |}
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a plugin holding a Db is rejected") {
      val errs = errors(holdsDb)
      assert(heldImpure(errs))
    }

    test("a plugin holding a closure over a Db is rejected") {
      val errs = errors(holdsClosure)
      assert(heldImpure(errs))
    }

    test("a plugin's documents holding a Db are rejected") {
      val errs = errors(documentsHoldDb)
      assert(heldImpure(errs))
    }

    test("a service that captures a Db is rejected where it is declared") {
      val errs = errors(serviceCapturesDb)
      assert(heldImpure(errs))
    }

    test("a service type that is not pure is rejected where it is exported") {
      val errs = errors(serviceImpure)
      assert(errs.exists(_.contains("does not conform to upper bound scala.caps.Pure")))
    }

    test("a plugin run that keeps the Db it is given is rejected") {
      val errs = errors(runKeepsDb)
      assert(flowsInto("{any}")(errs))
    }

    test("a plugin run that stashes the Db it is given in a collection field is rejected") {
      val errs = errors(runStashesDb)
      assert(flowsInto("{any}")(errs))
    }

    test("a service that keeps the Tx it is given is rejected") {
      val errs = errors(serviceKeepsTx)
      assert(flowsInto("{any}")(errs))
    }

    test("a PluginTool holding a Db is rejected") {
      val errs = errors(toolHoldsDb)
      assert(heldImpure(errs))
    }

    test("capture checking is what rejects each breach") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val errs = compile(erased(prelude + breaches.mkString("\n") + "\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
