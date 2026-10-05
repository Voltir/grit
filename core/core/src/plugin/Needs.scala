package grit.core.plugin

import grit.core.document.DocumentShelf
import grit.core.id.PluginName

/** One plugin's own documents as its tools and its service read them: its cache documents of
  * its cursor's generation, and its current documents.
  */
final case class PluginReads(cache: PluginDocs, documents: DocumentShelf)

/** A plugin another may need: it exports `S`, a service over its own documents, the only way a
  * plugin reads another's (ADR 0027).
  */
trait Exports[S <: caps.Pure] extends Plugin {

  /** Its service, reading `own`. */
  def service(own: PluginReads): S
}

/** The services of one plugin's needs, each over the needed plugin's own documents. */
trait Needs extends caps.Pure {

  /** `dependency`'s service; [[Unneeded]] when no plugin of its name is among the asking
    * plugin's needs. Reached only in [[PluginTool.bind]], when a deployment is checked and
    * when the engine starts, never on a call.
    */
  def of[S <: caps.Pure](dependency: Exports[S]): Either[Unneeded, S]
}

object Needs {

  /** `asker`'s needs, each read through the documents `reads` pairs with its name; a
    * dependency whose name `reads` does not hold is [[Unneeded]].
    */
  def over(asker: PluginName, reads: Vector[(PluginName, PluginReads)]): Needs =
    new Needs {
      def of[S <: caps.Pure](dependency: Exports[S]): Either[Unneeded, S] =
        reads
          .collectFirst { case (n, own) if n == dependency.name => dependency.service(own) }
          .toRight(Unneeded(asker, dependency.name))
    }
}

/** `plugin`'s tool asked for `dependency`, which `plugin` does not list in its needs. */
final case class Unneeded(plugin: PluginName, dependency: PluginName)
