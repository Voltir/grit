package grit.kit.deployment

import grit.core.id.{CallSlot, PluginName, ScheduleId}
import grit.core.job.{
  Asked,
  Booking,
  DeskRefusal,
  Grace,
  NotOwn,
  OwnJobs,
  Pending,
  ScheduleDesk,
  When
}
import grit.core.plugin.{Needs, Plugin, PluginReads, PluginRun, PluginTool, Unneeded}
import grit.core.store.{Db, StoreError}
import grit.core.tool.{Hosted, Tool}

/** A plugin's tool bound to its run: what it is, and what a call does. */
private[kit] final case class BoundTool[A](described: Hosted[A], run: PluginRun[A]) {

  /** This tool, each call run through `store`, told the call it runs. Its schedules go nowhere:
    * every desk call is refused [[DeskRefusal.Unavailable]], since no store keeps schedules
    * yet.
    */
  def over(store: Db^): Tool.Offered^{store} =
    described.calling((a, at) => run.run(a, at, store, new Unkept))
}

/** A desk over no store: it refuses every call, writing nothing. */
private final class Unkept extends ScheduleDesk {
  private val refused = DeskRefusal.Unavailable(StoreError.DatabaseError("no schedules are kept"))
  def ask[P <: caps.Pure](
      call: CallSlot,
      booking: Booking[P],
      when: When,
      grace: Grace,
      params: P
  ): Either[DeskRefusal, Asked[P]] = Left(refused)
  def pending[P <: caps.Pure](
      call: CallSlot,
      booking: Booking[P]
  ): Either[DeskRefusal, Pending[P]] =
    Left(refused)
  def cancel(call: CallSlot, booking: Booking[?], id: ScheduleId): Either[DeskRefusal, Unit] =
    Left(refused)
}

/** How the kit binds the deployment's plugins' tools (ADR 0027). */
private[kit] object PluginBinding {

  /** Every tool of `plugins`, each bound over its own plugin's documents, its needs' services
    * and its plugin's jobs, as `reads` gives each plugin's documents by name; the first
    * [[Unneeded]] when a tool asks for a plugin its own does not list, or [[NotOwn]] when it
    * books a job not its plugin's.
    */
  def bound(
      plugins: Vector[Plugin],
      reads: PluginName -> PluginReads
  ): Either[Unneeded | NotOwn, Vector[BoundTool[?]]] =
    plugins.foldLeft[Either[Unneeded | NotOwn, Vector[BoundTool[?]]]](Right(Vector.empty)) {
      (acc, p) =>
        val needs = Needs.over(p.name, p.needs.map(n => n.name -> reads(n.name)))
        val own = reads(p.name)
        val jobs = OwnJobs.over(p.name, p.jobs)
        p.tools.foldLeft(acc)((done, t) =>
          done.flatMap(ts => one(t, own, needs, jobs).map(ts :+ _))
        )
    }

  private def one[A](
      t: PluginTool[A],
      own: PluginReads,
      needs: Needs,
      jobs: OwnJobs
  ): Either[Unneeded | NotOwn, BoundTool[A]] =
    t.bind(own, needs, jobs).map(BoundTool(t.described, _))
}
