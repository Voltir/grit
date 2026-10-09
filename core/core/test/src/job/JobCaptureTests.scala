package grit.core.job

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, erased, flowsInto, heldImpure, options}

import utest.*

/** What capture checking rejects about jobs and schedules (ADR 0029): a job, its parameters and
  * a declared schedule hold no capability, so a deployment declares them outside any; a plugin
  * tool's run is handed its desk per call and keeps none; and the toolbox the kit offers is the
  * store's and the desk's alone. Pinned by compiling probe sources against core with core's own
  * flags, as `grit.core.plugin.PluginCaptureTests` does; `assertCompileError` cannot see capture
  * errors (docs/capture-checking.md).
  */
object JobCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import java.time.Instant
      |import grit.core.act.*
      |import grit.core.host.*
      |import grit.core.id.*
      |import grit.core.job.*
      |import grit.core.plugin.*
      |import grit.core.store.*
      |import grit.core.tool.*
      |import grit.core.visibility.Subject
      |
      |final case class Note(text: String) extends caps.Pure
      |object Remind extends PlainJob[Note] {
      |  val name: JobName = JobName.of("remind").fold(sys.error, identity)
      |  val version: Int = 1
      |  def write(params: Note): ujson.Value = ujson.Str(params.text)
      |  def read(params: ujson.Value): Either[String, Note] = params.strOpt.map(Note(_)).toRight("no text")
      |  def run(run: JobRun[Note], moves: Moves^): String = run.params.text
      |}
      |object Remember extends PluginTool[Int] {
      |  val described: Hosted[Int] =
      |    new Hosted(ToolSpec(ToolName("remember"), "Remember.", Args.of((n = Field.count("In how many minutes.", 1, 5))).map(_.n)), Gate.Free, n => n.toString)
      |  def bind(own: PluginReads, needs: Needs, jobs: OwnJobs): Either[Unneeded | NotOwn, PluginRun[Int]] =
      |    jobs.of(Remind).map(booking =>
      |      new PluginRun[Int] {
      |        def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome =
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
    Probes.errors(prelude + body + "\n", flags)

  /** 1. A job holding a store it was built with. */
  private val jobHoldsDb =
    """final class Holds(db: Db^) extends PlainJob[Note] {
      |  val name: JobName = Remind.name
      |  val version: Int = 1
      |  def write(params: Note): ujson.Value = Remind.write(params)
      |  def read(params: ujson.Value): Either[String, Note] = Remind.read(params)
      |  def run(run: JobRun[Note], moves: Moves^): String = db.read(Subject.Public)(Right("")).fold(_ => "", identity)
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
      |  def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome = { kept = Some(desk); Outcome.Done("") }
      |}
      |""".stripMargin

  /** 4. A run that stashes the desk it is handed in a mutable collection. */
  private val runStashesDesk =
    """final class Stashes extends PluginRun[Int] {
      |  val kept: scala.collection.mutable.ArrayBuffer[ScheduleDesk^] = scala.collection.mutable.ArrayBuffer.empty
      |  def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome = { kept += desk; Outcome.Done("") }
      |}
      |""".stripMargin

  /** 5. A plugin tool built over a desk it holds. */
  private val toolHoldsDesk =
    """final class ToolHoldsDesk(desk: ScheduleDesk^) extends PluginTool[Int] {
      |  val described: Hosted[Int] = Remember.described
      |  def bind(own: PluginReads, needs: Needs, jobs: OwnJobs): Either[Unneeded | NotOwn, PluginRun[Int]] =
      |    jobs.of(Remind).map(booking =>
      |      new PluginRun[Int] {
      |        def run(n: Int, call: CallSlot, reads: Reads^, handed: ScheduleDesk^): Outcome =
      |          desk.pending(call, booking).fold(r => Outcome.Failed(r.said), _ => Outcome.Done(""))
      |      })
      |}
      |""".stripMargin

  /** 6. The toolbox of a store and a desk handed a plugin tool whose run also edits. */
  private val toolboxEdits =
    """def box(store: Db^, desk: ScheduleDesk^, e: Edits^, r: PluginRun[Int]): Either[DuplicateName, Toolbox[{store, desk}]] =
      |  Toolbox.of(Remember.described.calling((n, at) => { val _ = RelPath.of("x").map(e.write(_, "x")); r.run(n, at, store.as(Subject.Turn(at.turn)), desk) }))
      |""".stripMargin

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
          |    Toolbox.of(Remember.described.calling((n, at) => r.run(n, at, store.as(Subject.Turn(at.turn)), desk)))
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
      val errs = Probes.errors(erased(prelude + breaches.mkString("\n") + "\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
