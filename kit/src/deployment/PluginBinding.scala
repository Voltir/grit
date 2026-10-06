package grit.kit.deployment

import grit.core.id.{JobName, PluginName}
import grit.core.job.{NotOwn, OwnJobs, ScheduleDesk}
import grit.core.plugin.{Needs, Plugin, PluginReads, PluginRun, PluginTool, Unneeded}
import grit.core.store.Db
import grit.core.tool.{Hosted, Tool}

/** A plugin's tool bound to its run: what it is, what a call does, and its plugin, whose jobs
  * are named `jobs`.
  */
private[kit] final case class BoundTool[A](
    described: Hosted[A],
    run: PluginRun[A],
    plugin: PluginName,
    jobs: Vector[JobName]
) {

  /** This tool, each call run through `store`, told the call it runs, and writing its
    * plugin's schedules through its plugin's desk from `desks`.
    */
  def over(store: Db^, desks: Desks^): Tool.Offered^{store, desks} =
    described.calling((a, at) => run.run(a, at, store, desks.of(plugin, jobs)))
}

/** Each plugin's desk (ADR 0029). */
private[kit] trait Desks {

  /** `plugin`'s desk, refusing a booking of any job not named in `jobs`. */
  def of(plugin: PluginName, jobs: Vector[JobName]): ScheduleDesk^
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
          done.flatMap(ts => one(t, p, own, needs, jobs).map(ts :+ _))
        )
    }

  private def one[A](
      t: PluginTool[A],
      p: Plugin,
      own: PluginReads,
      needs: Needs,
      jobs: OwnJobs
  ): Either[Unneeded | NotOwn, BoundTool[A]] =
    t.bind(own, needs, jobs).map(BoundTool(t.described, _, p.name, p.jobs.map(_.name)))
}
