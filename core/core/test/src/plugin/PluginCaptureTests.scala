package grit.core.plugin

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, erased, flowsInto, heldImpure, options}

import utest.*

/** What capture checking rejects about a plugin (ADR 0027): a plugin, its tools' runs and the
  * services it exports hold no capability, so a deployment can be built outside any and a
  * service handed between plugins carries none. Pinned by compiling probe sources against
  * core with core's own flags, as `grit.core.tool.ToolCaptureTests` does; `assertCompileError`
  * cannot see capture errors (docs/capture-checking.md).
  */
object PluginCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.document.*
      |import grit.core.id.{CallSlot, PluginName}
      |import grit.core.job.*
      |import grit.core.plugin.*
      |import grit.core.store.*
      |import grit.core.tool.*
      |import grit.core.visibility.Subject
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
      |      def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome =
      |        reads.read(new Lines(own).recent(n)).fold(e => Outcome.Failed(e.toString), l => Outcome.Done(l.mkString("\n")))
      |    })
      |}
      |object Noted extends Documents {
      |  val terms: DocumentTerms =
      |    DocLabel.of("notes").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, scala.concurrent.duration.FiniteDuration(1, "day"), 10)).fold(sys.error, identity)
      |  val posting: Option[DocumentPosting] = None
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
    Probes.errors(prelude + body + "\n", flags)

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
    """final class DocsHold(db: Db^) extends DocumentPosting {
      |  def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit] = db.read(Subject.Public)(Right(()))
      |}
      |""".stripMargin

  /** A service that reads through a store it captured, rather than the transaction it is given. */
  private val serviceCapturesDb =
    """final class Leaky(db: Db^) extends Activity {
      |  def recent(n: Int)(using Tx^): Either[StoreError, Vector[String]] = db.read(Subject.Public)(Right(Vector.empty))
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

  /** A run that keeps the reads it is handed, its turn's, for a later call. */
  private val runKeepsReads =
    """final class Keeps extends PluginRun[Int] {
      |  var kept: Option[Reads^] = None
      |  def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome = { kept = Some(reads); Outcome.Done("") }
      |}
      |""".stripMargin

  /** A run that stashes the reads it is handed in a mutable collection. */
  private val runStashesReads =
    """final class Stashes extends PluginRun[Int] {
      |  val kept: scala.collection.mutable.ArrayBuffer[Reads^] = scala.collection.mutable.ArrayBuffer.empty
      |  def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome = { kept += reads; Outcome.Done("") }
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
      |      def run(n: Int, call: CallSlot, handed: Reads^, desk: ScheduleDesk^): Outcome = db.read(Subject.Public)(Right(n)).fold(_ => Outcome.Failed("no"), _ => Outcome.Done("ok"))
      |    })
      |}
      |""".stripMargin

  private val breaches: Vector[String] = Vector(
    holdsDb,
    holdsClosure,
    documentsHoldDb,
    serviceCapturesDb,
    runKeepsReads,
    runStashesReads,
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
          |    Toolbox.of(Recent.described.calling((n, at) => r.run(n, at, store.as(Subject.Turn(at.turn)), desk)))
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

    test("a plugin run that keeps the reads it is given is rejected") {
      val errs = errors(runKeepsReads)
      assert(flowsInto("{any}")(errs))
    }

    test("a plugin run that stashes the reads it is given in a collection field is rejected") {
      val errs = errors(runStashesReads)
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
      val errs = Probes.errors(erased(prelude + breaches.mkString("\n") + "\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
