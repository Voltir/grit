package grit.core.retention

import scala.concurrent.duration.FiniteDuration

import grit.core.id.{ConversationId, PeriodRef, PeriodSeq, PluginName}
import grit.core.period.{CloseOrdinal, Windows}

/** Something grit has decided to delete. */
enum Target {

  /** A closed period's raw entries (every entry of its turns but its closing entry), the
    * verdicts on it, and its turn, close and settle workflows.
    */
  case Raw(period: PeriodRef)

  /** A closed period whose closing the next one has replaced: its row and closing entry, its
    * turns' usage and profiles, and every document posted from it.
    */
  case Superseded(period: PeriodRef)

  /** A conversation's newest period, closed: the whole conversation, with its usage, profiles,
    * the documents posted from its closings, and its place when no other conversation is
    * there. Spared when a later period has opened.
    */
  case Quiet(period: PeriodRef)

  /** The finished runs posting to `plugin` at `version` from `cursor`, which it has moved
    * past. The run that writes it is one of them.
    */
  case PostRuns(plugin: PluginName, version: Int, cursor: CloseOrdinal)

  /** `plugin`'s documents from before its cursor last started again, and its runs at other
    * versions.
    */
  case Restarted(plugin: PluginName)

  /** A plugin not enabled: its documents, cursor and runs. Spared while it is enabled. */
  case Disabled(plugin: PluginName)

  def kind: Target.Kind = this match {
    case Raw(_) => Target.Kind.Raw
    case Superseded(_) => Target.Kind.Superseded
    case Quiet(_) => Target.Kind.Quiet
    case PostRuns(_, _, _) => Target.Kind.PostRuns
    case Restarted(_) => Target.Kind.Restarted
    case Disabled(_) => Target.Kind.Disabled
  }
}

object Target {

  /** The kinds, in the order the collector takes them. */
  enum Kind(val name: String) {
    case Raw extends Kind("raw")
    case Superseded extends Kind("superseded")
    case Quiet extends Kind("quiet")
    case PostRuns extends Kind("post-runs")
    case Restarted extends Kind("restarted")
    case Disabled extends Kind("disabled")

    /** How long after its tombstone is written a target of this kind is deleted: `windows`'
      * retention for Raw, PostRuns and Restarted; its ledger window for Superseded, Quiet and
      * Disabled.
      */
    def retention(windows: Windows): FiniteDuration = this match {
      case Kind.Raw | Kind.PostRuns | Kind.Restarted => windows.retention
      case Kind.Superseded | Kind.Quiet | Kind.Disabled => windows.ledger
    }
  }

  object Kind {

    /** The kind stored as `name`, or `None`. */
    def named(name: String): Option[Kind] = values.find(_.name == name)
  }

  /** `target`'s stored key, beside its kind's name: `{conversation}:{period}` for a period's,
    * `{plugin}:{version}:{cursor}` for posting runs, `{plugin}` for a plugin's.
    */
  def key(target: Target): String = target match {
    case Raw(p) => period(p)
    case Superseded(p) => period(p)
    case Quiet(p) => period(p)
    case PostRuns(plugin, version, cursor) =>
      s"${PluginName.value(plugin)}:$version:${CloseOrdinal.value(cursor)}"
    case Restarted(plugin) => PluginName.value(plugin)
    case Disabled(plugin) => PluginName.value(plugin)
  }

  /** The target of `kind` stored under `key`, or why it is none. */
  def read(kind: Kind, key: String): Either[String, Target] = {
    def periodOf(make: PeriodRef => Target) =
      key.split(':') match {
        case Array(c, s) if c.nonEmpty =>
          s.toLongOption
            .flatMap(PeriodSeq.of)
            .map(seq => make(PeriodRef(ConversationId(c), seq)))
            .toRight(s"${kind.name} $key: not a period")
        case _ => Left(s"${kind.name} $key: not a period")
      }
    def pluginOf(make: PluginName => Target) =
      PluginName.of(key).map(make).left.map(why => s"${kind.name} $key: $why")
    kind match {
      case Kind.Raw => periodOf(Raw(_))
      case Kind.Superseded => periodOf(Superseded(_))
      case Kind.Quiet => periodOf(Quiet(_))
      case Kind.Restarted => pluginOf(Restarted(_))
      case Kind.Disabled => pluginOf(Disabled(_))
      case Kind.PostRuns =>
        key.split(':') match {
          case Array(p, v, c) =>
            (for {
              plugin <- PluginName.of(p).toOption
              version <- v.toIntOption
              cursor <- c.toLongOption.flatMap(CloseOrdinal.of)
            } yield PostRuns(plugin, version, cursor)).toRight(s"post-runs $key: not a run's")
          case _ => Left(s"post-runs $key: not a run's")
        }
    }
  }

  private def period(p: PeriodRef): String =
    s"${ConversationId.value(p.conversationId)}:${PeriodSeq.value(p.seq)}"
}
