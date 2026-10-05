package grit.kit.deployment

import grit.core.id.PluginName
import grit.core.plugin.{Needs, Plugin, PluginReads, PluginRun, PluginTool, Unneeded}
import grit.core.store.Db
import grit.core.tool.{Hosted, Tool}

/** A plugin's tool bound to its run: what it is, and what a call does. */
private[kit] final case class BoundTool[A](described: Hosted[A], run: PluginRun[A]) {

  /** This tool, each call run through `store`. */
  def over(store: Db^): Tool.Offered^{store} = described.over(a => run.run(a, store))
}

/** How the kit binds the deployment's plugins' tools (ADR 0027). */
private[kit] object PluginBinding {

  /** Every tool of `plugins`, each bound over its own plugin's documents and its needs'
    * services, as `reads` gives each plugin's documents by name; the first [[Unneeded]] when a
    * tool asks for a plugin its own does not list.
    */
  def bound(
      plugins: Vector[Plugin],
      reads: PluginName -> PluginReads
  ): Either[Unneeded, Vector[BoundTool[?]]] =
    plugins.foldLeft[Either[Unneeded, Vector[BoundTool[?]]]](Right(Vector.empty)) { (acc, p) =>
      val needs = Needs.over(p.name, p.needs.map(n => n.name -> reads(n.name)))
      val own = reads(p.name)
      p.tools.foldLeft(acc)((done, t) => done.flatMap(ts => one(t, own, needs).map(ts :+ _)))
    }

  private def one[A](
      t: PluginTool[A],
      own: PluginReads,
      needs: Needs
  ): Either[Unneeded, BoundTool[A]] =
    t.bind(own, needs).map(BoundTool(t.described, _))
}
