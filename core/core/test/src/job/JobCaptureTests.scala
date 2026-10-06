package grit.core.job

import java.nio.file.Files

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import utest.*

/** What capture checking rejects about jobs and schedules (ADR 0029): a job, its parameters and
  * a declared schedule hold no capability, so a deployment declares them outside any; a plugin
  * tool's run is handed its desk per call and keeps none; and the toolbox the kit offers is the
  * store's and the desk's alone. Pinned by compiling probe sources against core with core's own
  * flags, as `grit.core.plugin.PluginCaptureTests` does; `assertCompileError` cannot see capture
  * errors (docs/capture-checking.md).
  */
object JobCaptureTests extends TestSuite {

  private val classpath = sys.env.getOrElse("GRIT_PROBE_CLASSPATH", "")
  private val options = sys.env.getOrElse("GRIT_PROBE_OPTIONS", "").split(" ").toList

  private val prelude =
    """package probe
      |import java.time.Instant
      |import grit.core.host.*
      |import grit.core.id.*
      |import grit.core.job.*
      |import grit.core.plugin.*
      |import grit.core.store.*
      |import grit.core.tool.*
      |
      |final case class Note(text: String) extends caps.Pure
      |object Remind extends Job[Note] {
      |  val name: JobName = JobName.of("remind").fold(sys.error, identity)
      |  val version: Int = 1
      |  def write(params: Note): ujson.Value = ujson.Str(params.text)
      |  def read(params: ujson.Value): Either[String, Note] = params.strOpt.map(Note(_)).toRight("no text")
      |  def reply(run: JobRun[Note]): String = run.params.text
      |}
      |object Remember extends PluginTool[Int] {
      |  val described: Hosted[Int] =
      |    new Hosted(ToolSpec(ToolName("remember"), "Remember.", Args.of((n = Field.count("In how many minutes.", 1, 5))).map(_.n)), Gate.Free, n => n.toString)
      |  def bind(own: PluginReads, needs: Needs, jobs: OwnJobs): Either[Unneeded | NotOwn, PluginRun[Int]] =
      |    jobs.of(Remind).map(booking =>
      |      new PluginRun[Int] {
      |        def run(n: Int, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome =
      |          desk.ask(call, booking, When.In(scala.concurrent.duration.FiniteDuration(n.toLong, "minutes")), Grace.Zero, Note("tea"))
      |            .fold(r => Outcome.Failed(r.said), a => Outcome.Done(ScheduleId.value(a.id)))
      |      })
      |}
      |final class Reminders(val name: PluginName) extends Plugin {
      |  val version: Int = 1
      |  override val jobs: Vector[Job[?]] = Vector(Remind)
      |  override val tools: Vector[PluginTool[?]] = Vector(Remember)
      |  override val schedules: Vector[Declared[?]] = Vector(
      |    Declared(ScheduleKey.of("tea").fold(sys.error, identity), Remind, SlotRule.Once(Instant.EPOCH, Grace.Zero), Note("tea")))
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
    val dir = Files.createTempDirectory("grit-job-probe")
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

  /** 1. A job holding a store it was built with. */
  private val jobHoldsDb =
    """final class Holds(db: Db^) extends Job[Note] {
      |  val name: JobName = Remind.name
      |  val version: Int = 1
      |  def write(params: Note): ujson.Value = Remind.write(params)
      |  def read(params: ujson.Value): Either[String, Note] = Remind.read(params)
      |  def reply(run: JobRun[Note]): String = db.read(Right("")).fold(_ => "", identity)
      |}
      |""".stripMargin

  /** 2. A declared schedule whose parameters hold a store. */
  private val declaredHolder =
    """final class Holder(val db: Db^)
      |def declared(job: Job[Note], h: Holder): Vector[Declared[Holder]] = Vector.empty
      |""".stripMargin

  /** 3. A run that keeps the desk it is handed for a later call. */
  private val runKeepsDesk =
    """final class Keeps extends PluginRun[Int] {
      |  var kept: Option[ScheduleDesk^] = None
      |  def run(n: Int, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome = { kept = Some(desk); Outcome.Done("") }
      |}
      |""".stripMargin

  /** 4. A run that stashes the desk it is handed in a mutable collection. */
  private val runStashesDesk =
    """final class Stashes extends PluginRun[Int] {
      |  val kept: scala.collection.mutable.ArrayBuffer[ScheduleDesk^] = scala.collection.mutable.ArrayBuffer.empty
      |  def run(n: Int, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome = { kept += desk; Outcome.Done("") }
      |}
      |""".stripMargin

  /** 5. A plugin tool built over a desk it holds. */
  private val toolHoldsDesk =
    """final class ToolHoldsDesk(desk: ScheduleDesk^) extends PluginTool[Int] {
      |  val described: Hosted[Int] = Remember.described
      |  def bind(own: PluginReads, needs: Needs, jobs: OwnJobs): Either[Unneeded | NotOwn, PluginRun[Int]] =
      |    jobs.of(Remind).map(booking =>
      |      new PluginRun[Int] {
      |        def run(n: Int, call: CallSlot, db: Db^, handed: ScheduleDesk^): Outcome =
      |          desk.pending(call, booking).fold(r => Outcome.Failed(r.said), _ => Outcome.Done(""))
      |      })
      |}
      |""".stripMargin

  /** 6. The toolbox of a store and a desk handed a plugin tool whose run also edits. */
  private val toolboxEdits =
    """def box(store: Db^, desk: ScheduleDesk^, e: Edits^, r: PluginRun[Int]): Either[DuplicateName, Toolbox[{store, desk}]] =
      |  Toolbox.of(Remember.described.calling((n, at) => { val _ = RelPath.of("x").map(e.write(_, "x")); r.run(n, at, store, desk) }))
      |""".stripMargin

  /** Whether `errs` reject a class for holding a capability its pure self type excludes. */
  private def heldImpure(errs: List[String]): Boolean =
    errs.exists(_.contains("is not included in the allowed capture set {} of the self type"))

  /** Whether `errs` reject a capability flowing into a capture set that may not hold it. */
  private def flowsInto(set: String)(errs: List[String]): Boolean =
    errs.exists(_.contains(s"cannot flow into capture set $set"))

  /** Every breach capture checking rejects; [[declaredHolder]] is the type's bound, rejected
    * with the checker off too.
    */
  private val breaches: Vector[String] =
    Vector(jobHoldsDb, runKeepsDesk, runStashesDesk, toolHoldsDesk, toolboxEdits)

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test(
      "a pure job, a declared schedule, and a plugin tool booking its own job, run with the desk handed in, compile"
    ) {
      val errs = errors(
        """object Kit {
          |  def toolbox(store: Db^, desk: ScheduleDesk^, r: PluginRun[Int]): Either[DuplicateName, Toolbox[{store, desk}]] =
          |    Toolbox.of(Remember.described.calling((n, at) => r.run(n, at, store, desk)))
          |}
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a job holding a Db is rejected") {
      assert(heldImpure(errors(jobHoldsDb)))
    }

    test("a declared schedule whose parameters hold a Db is rejected by its bound") {
      val errs = errors(declaredHolder)
      assert(errs.exists(_.contains("does not conform to upper bound scala.caps.Pure")))
    }

    test("a plugin run that keeps the desk it is given is rejected") {
      assert(flowsInto("{any}")(errors(runKeepsDesk)))
    }

    test("a plugin run that stashes the desk it is given in a collection field is rejected") {
      assert(flowsInto("{any}")(errors(runStashesDesk)))
    }

    test("a plugin tool built over a desk it holds is rejected") {
      assert(heldImpure(errors(toolHoldsDesk)))
    }

    test("a toolbox of the store and a desk holding a plugin tool that also edits is rejected") {
      assert(flowsInto("{store, desk}")(errors(toolboxEdits)))
    }

    test("capture checking is what rejects each breach but the bound") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val errs = compile(erased(prelude + breaches.mkString("\n") + "\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
