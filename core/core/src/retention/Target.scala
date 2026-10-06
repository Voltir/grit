package grit.core.retention

import grit.core.id.{ConversationId, DocumentVersion, PeriodRef, PeriodSeq, PluginName, ScheduleId}
import grit.core.period.{CloseOrdinal, Windows}

/** Something grit has decided to delete. */
enum Target {

  /** A closed period's raw entries (every entry of its turns but its closing entry) with
    * what triage and its shadows made of them, the verdicts on it, and its workflows: its
    * turns', every attempt to close it, every question asked about it, and every triage and
    * shadow of a message heard in it ([[grit.core.period.Purgeable]]).
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

  /** A plugin not enabled: its documents, their terms, its cursor and runs. Spared while it is
    * enabled.
    */
  case Disabled(plugin: PluginName)

  /** A version of a plugin's document that stopped being current (superseded, or withdrawn)
    * or is itself a withdrawal.
    */
  case Document(version: DocumentVersion)

  /** An ended schedule's row (ADR 0029). Spared when it was declared again since it ended. */
  case Schedule(id: ScheduleId)

  def kind: Target.Kind = this match {
    case Raw(_) => Target.Kind.Raw
    case Superseded(_) => Target.Kind.Superseded
    case Quiet(_) => Target.Kind.Quiet
    case PostRuns(_, _, _) => Target.Kind.PostRuns
    case Restarted(_) => Target.Kind.Restarted
    case Disabled(_) => Target.Kind.Disabled
    case Document(_) => Target.Kind.Document
    case Schedule(_) => Target.Kind.Schedule
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
    case Document extends Kind("document")
    case Schedule extends Kind("schedule")

    /** How long after its tombstone is written a target of this kind is deleted: `windows`'
      * retention for Raw, PostRuns, Restarted and Schedule; its ledger window for Superseded,
      * Quiet and Disabled; for Document, its plugin's declared retention.
      */
    def retention(windows: Windows): Retention = this match {
      case Kind.Raw | Kind.PostRuns | Kind.Restarted | Kind.Schedule =>
        Retention.For(windows.retention)
      case Kind.Superseded | Kind.Quiet | Kind.Disabled => Retention.For(windows.ledger)
      case Kind.Document => Retention.Declared
    }
  }

  object Kind {

    /** The kind stored as `name`, or `None`. */
    def named(name: String): Option[Kind] = values.find(_.name == name)
  }

  /** `target`'s stored key, beside its kind's name: `{conversation}:{period}` for a period's,
    * `{plugin}:{version}:{cursor}` for posting runs, `{plugin}` for a plugin's, `{version}`
    * for a document's, its id for a schedule's.
    */
  def key(target: Target): String = target match {
    case Raw(p) => period(p)
    case Superseded(p) => period(p)
    case Quiet(p) => period(p)
    case PostRuns(plugin, version, cursor) =>
      s"${PluginName.value(plugin)}:$version:${CloseOrdinal.value(cursor)}"
    case Restarted(plugin) => PluginName.value(plugin)
    case Disabled(plugin) => PluginName.value(plugin)
    case Document(version) => DocumentVersion.value(version).toString
    case Schedule(id) => ScheduleId.value(id)
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
      case Kind.Document =>
        key.toLongOption
          .flatMap(DocumentVersion.of)
          .map(Document(_))
          .toRight(s"document $key: not a version")
      case Kind.Schedule =>
        ScheduleId.of(key).map(Schedule(_)).left.map(why => s"schedule $key: $why")
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
