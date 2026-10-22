package grit.kit.run

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.id.{Declarer, JobName, PluginName, ScheduleId, ScheduleKey}
import grit.core.job.{Declared, Ending, Grace, Slot, SlotRule}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Entry, Payload, StoreError}
import grit.core.visibility.{Label, Subject}
import grit.dbos.engine.Engine
import grit.kit.deployment.Deployment

/** What [[Kit.runOnce]] does between opening an engine and closing it, and the reads a run by
  * hand shares: whether runs are a plugin's to run, how a schedule ended, and what its run
  * replied.
  */
private[grit] object OnceRuns {

  /** 10 minutes: how late a once run may start after it is due. */
  val Late: Grace = Grace.of(10.minutes).getOrElse(Grace.Zero)

  /** How often a run is looked at while it is waited for: 2 s. */
  val Every: FiniteDuration = 2.seconds

  /** Why `runs` are not `plugin`'s to run in `d`, the first that is not: `plugin` is not
    * `d`'s, a run's job is not `plugin`'s, a run's key is one of `d`'s declared schedules', or
    * its clearance is none of `plugin`'s own schedules'; `None` when all are.
    */
  def unrunnable(d: Deployment, plugin: PluginName, runs: Vector[Declared[?]]): Option[String] =
    d.plugins.find(_.name == plugin) match {
      case None => Some(s"the deployment has no plugin ${PluginName.value(plugin)}")
      case Some(p) =>
        val name = PluginName.value(plugin)
        val keys = d.declared.map(_._2.key).toSet
        val cleared = p.schedules.map(_.clearance).toSet
        runs.iterator
          .flatMap { r =>
            val key = ScheduleKey.value(r.key)
            if (!p.jobs.exists(_.name == r.job.name))
              Some(s"the job ${JobName.value(r.job.name)} is not the plugin $name's")
            else if (keys.contains(r.key)) Some(s"the schedule $key is one the deployment declares")
            else if (!cleared.contains(r.clearance))
              Some(
                s"the schedule $key's clearance ${Label.shown(r.clearance)} is none of the plugin $name's schedules'"
              )
            else None
          }
          .nextOption()
    }

  /** Whether any of `plugin`'s own declared schedules in `d` is stored on `engine`'s database:
    * whether `d` has been served there.
    */
  def served(engine: Engine^, d: Deployment, plugin: PluginName): Either[StoreError, Boolean] =
    engine.db.read(Subject.Public) {
      val ids = d.declared.collect {
        case (by @ Declarer.Plugin(p), s) if p == plugin => s.id(by)
      }
      ids.foldLeft[Either[StoreError, Boolean]](Right(false))((acc, id) =>
        acc.flatMap(found => if (found) Right(true) else engine.schedules.read(id).map(_.nonEmpty))
      )
    }

  /** `run` made due at `at`, a once slot that may start up to [[Late]] after it. */
  def dated(run: Declared[?], at: Instant): Declared[?] = run match {
    case r: Declared[p] => r.copy(rule = SlotRule.Once(at, Late))
  }

  /** How `plugin`'s schedule `key` ended, waited for until `clock` reads `until`, `tick` made
    * every [[Every]] meanwhile; `None` when it had not ended by then, or could not be read.
    * Ended means its run replied and nothing the engine runs is still queued or running.
    */
  def waited(
      engine: Engine^,
      plugin: PluginName,
      key: ScheduleKey,
      clock: Clock^,
      until: Instant,
      tick: () => Unit
  ): Option[Ending] = {
    val id = ScheduleId.declared(Declarer.Plugin(plugin), key)
    def ended: Option[Ending] =
      engine.db.read(Subject.Public)(engine.schedules.read(id)).toOption.flatten.flatMap(_.ended)
    def idle: Boolean = engine.unfinished().contains(0)
    while ((ended.isEmpty || !idle) && clock.now().isBefore(until)) {
      tick()
      Thread.sleep(Every.toMillis)
    }
    ended
  }

  /** The last reply the run of `plugin`'s schedule `key`, of `job`, its once slot due `at`,
    * wrote; `None` when it wrote none, or it could not be read.
    */
  def replied(
      engine: Engine^,
      plugin: PluginName,
      key: ScheduleKey,
      job: JobName,
      at: Instant
  ): Option[String] = {
    val slot = Slot(ScheduleId.declared(Declarer.Plugin(plugin), key), Slot.kept(at))
    engine.db
      .read(Subject.Public) {
        engine.conversations
          .find(slot.origin(job))
          .flatMap(_.fold(Right(Vector.empty))(c => engine.entries.list(c.id)))
      }
      .toOption
      .flatMap(_.reverseIterator.collectFirst {
        case Entry(_, _, _, _, _, Payload.Message(a: Message.Assistant), _, _) =>
          a.blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      })
  }
}
