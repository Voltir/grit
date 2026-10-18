package grit.core.need

import grit.core.id.PluginName

/** A plugin another may need: named `name`, it exports `S`, a service over its own documents,
  * the only way a plugin reads another's (ADR 0027).
  */
trait Exported[S <: caps.Pure] {
  def name: PluginName

  /** Its service, reading `own`. */
  def service(own: PluginReads): S
}

/** The services of one plugin's needs, each over the needed plugin's own documents. */
trait Needs extends caps.Pure {

  /** `dependency`'s service; [[Unneeded]] when no plugin of its name is among the asking
    * plugin's needs. Reached in [[grit.core.plugin.PluginTool.bind]], in a dreaming job's scans
    * ([[grit.core.act.Dreaming.scan]]), when a deployment is checked and when the engine starts.
    */
  def of[S <: caps.Pure](dependency: Exported[S]): Either[Unneeded, S]

  /** The plugins whose services it gives. */
  def plugins: Vector[PluginName]
}

object Needs {

  /** `asker`'s needs, each read through the documents `reads` pairs with its name; a
    * dependency whose name `reads` does not hold is [[Unneeded]].
    */
  def over(asker: PluginName, reads: Vector[(PluginName, PluginReads)]): Needs =
    new Needs {
      def of[S <: caps.Pure](dependency: Exported[S]): Either[Unneeded, S] =
        reads
          .collectFirst { case (n, own) if n == dependency.name => dependency.service(own) }
          .toRight(Unneeded(asker, dependency.name))
      def plugins: Vector[PluginName] = reads.map(_._1)
    }
}

/** `plugin`'s tool asked for `dependency`, which `plugin` does not list in its needs. */
final case class Unneeded(plugin: PluginName, dependency: PluginName)
