package grit.kit.deployment

import grit.core.id.PluginName
import grit.core.plugin.{Exports, Needs, PluginReads, PluginRun, PluginTool, Unneeded}
import grit.core.store.{Db, StoreError, Tx}
import grit.core.tool.{Args, Field, Gate, Hosted, Outcome, ToolName, ToolSpec}

/** Plugins for tests of how a deployment takes them: one that exports its documents' keys, and
  * tools that read through a service or their own documents.
  */
object TestPlugins {

  def name(s: String): PluginName = PluginName.of(s).fold(e => sys.error(e), identity)

  /** What [[Keys]] exports: the keys of its documents. */
  trait KeyList extends caps.Pure {
    def keys()(using Tx^): Either[StoreError, Vector[String]]
  }

  /** A tool named `called` whose call answers the keys `list` gives, joined by commas. */
  final class Listing(called: ToolName, list: (PluginReads, Needs) -> Either[Unneeded, KeyList])
      extends PluginTool[Int] {
    val described: Hosted[Int] =
      new Hosted(
        ToolSpec(called, "Lists keys.", Args.of((n = Field.count("How many.", 1, 9))).map(_.n)),
        Gate.Free,
        n => n.toString
      )
    def bind(own: PluginReads, needs: Needs): Either[Unneeded, PluginRun[Int]] =
      list(own, needs).map { l =>
        new PluginRun[Int] {
          def run(n: Int, db: Db^): Outcome =
            db.read(l.keys())
              .fold(e => Outcome.Failed(e.toString), k => Outcome.Done(k.mkString(",")))
        }
      }
  }

  private def ownKeys(own: PluginReads): KeyList = new KeyList {
    def keys()(using Tx^): Either[StoreError, Vector[String]] =
      own.cache.newest("", 100).map(_.map(_._1))
  }

  /** Exports its own documents' keys, and offers them as the tool `called`. */
  final class Keys(val name: PluginName, called: ToolName = ToolName("keys"))
      extends Exports[KeyList] {
    val version: Int = 1
    def service(own: PluginReads): KeyList = ownKeys(own)
    override val tools: Vector[PluginTool[?]] =
      Vector(new Listing(called, (own, _) => Right(ownKeys(own))))
  }

  /** Needs `keys` when `declared`, and offers `called`, which lists `keys`' keys through it. */
  final class Reading(
      val name: PluginName,
      keys: Keys,
      declared: Boolean,
      called: ToolName = ToolName("theirs")
  ) extends grit.core.plugin.Plugin {
    val version: Int = 1
    override val needs: Vector[Exports[?]] = if (declared) Vector(keys) else Vector.empty
    override val tools: Vector[PluginTool[?]] =
      Vector(new Listing(called, (_, needs) => needs.of(keys)))
  }
}
