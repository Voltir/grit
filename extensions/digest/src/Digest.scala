package grit.digest

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import grit.core.id.PluginName
import grit.core.period.{CloseOrdinal, CloseReason}
import grit.core.plugin.{
  CacheDocs,
  CachePosting,
  Needs,
  Plugin,
  PluginReads,
  PluginRun,
  PluginTool,
  Unneeded
}
import grit.core.store.{ClosedPeriod, Db, Origin, StoreError, Tx}
import grit.core.tool.{Args, Field, Gate, Hosted, Outcome, ToolName, ToolSpec}

/** The digest: one line per closed period, across every conversation: when it closed,
  * where, why, and what it came to ([[grit.core.period.Closing.headline]]). The hello-world
  * plugin, kept under `name`: one document per closed period, keyed by its close ordinal;
  * none for a period closed unearned ([[CloseReason.Unearned]]), whose closing says only
  * that nothing was kept.
  */
final class Digest(val name: PluginName) extends Plugin {

  val version: Int = 1

  override val cache: Option[CachePosting] = Some(Digest.Posted)

  override val tools: Vector[PluginTool[?]] = Vector(Digest.RecentActivity)
}

object Digest {

  /** One line per closed period, kept under its close ordinal; none for one closed unearned. */
  private object Posted extends CachePosting {
    def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] =
      why(closed.reason) match {
        case None => Right(())
        case Some(why) => docs.put(key(closed.order), doc(closed, why))
      }
  }

  /** How many lines `recent_activity` shows when not told. */
  val DefaultShown = 10

  /** The most lines `recent_activity` shows. */
  val MaxShown = 50

  /** A document's key: its close ordinal, zero-padded, so keys order as closes do. */
  private def key(o: CloseOrdinal): String = f"${CloseOrdinal.value(o)}%020d"

  /** Why a period closed, as its line says it; `None` for one that keeps no line. */
  private def why(reason: CloseReason): Option[String] = reason match {
    case CloseReason.Resolved(_) => Some("resolved")
    case CloseReason.Lapsed => Some("lapsed")
    case CloseReason.Unearned => None
  }

  /** The document kept for `closed`, which closed for `why`: `at`, `where`, `why`, `line`. */
  private def doc(closed: ClosedPeriod, why: String): ujson.Value =
    ujson.Obj(
      "at" -> closed.at.toString,
      "where" -> where(closed.origin),
      "why" -> why,
      "line" -> closed.closing.headline
    )

  private def where(origin: Origin): String = origin match {
    case Origin.Tui(_, session) => s"tui $session"
    case Origin.Slack(_, channel, _) => s"slack #$channel"
    case Origin.Task(name, _) => s"task $name"
  }

  private val Minute = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(ZoneOffset.UTC)

  /** One kept document as a line: `{date} {time} · {where} · {why} · {headline}`, UTC;
    * `None` for a document that is not one of the digest's.
    */
  def shown(doc: ujson.Value): Option[String] =
    for {
      o <- doc.objOpt
      at <- o.get("at").flatMap(_.strOpt).flatMap(s => scala.util.Try(Instant.parse(s)).toOption)
      where <- o.get("where").flatMap(_.strOpt)
      why <- o.get("why").flatMap(_.strOpt)
      line <- o.get("line").flatMap(_.strOpt)
    } yield s"${Minute.format(at)} · $where · $why · $line"

  /** `recent_activity`: the newest lines of the digest it is bound to, newest first. Free: it
    * only reads.
    */
  val RecentActivity: PluginTool[Int] = new PluginTool[Int] {
    val described: Hosted[Int] =
      new Hosted(
        ToolSpec(
          ToolName("recent_activity"),
          "List the conversations that closed most recently, across every place grit works " +
            "(this chat, Slack threads, tasks), newest first, one line each: when it closed " +
            "(UTC), where, whether it was resolved or lapsed, and what it came to. Only what " +
            "each closing recorded is known; the conversations themselves are not.",
          Args
            .of(
              (n =
                Field.count(s"How many lines; $DefaultShown when not given.", 1, MaxShown).optional
              )
            )
            .map(_.n.getOrElse(DefaultShown))
        ),
        Gate.Free,
        n => n.toString
      )

    def bind(own: PluginReads, needs: Needs): Either[Unneeded, PluginRun[Int]] =
      Right(new PluginRun[Int] {
        def run(n: Int, db: Db^): Outcome =
          db.read(own.cache.newest("", n)) match {
            case Left(error) => Outcome.Failed(s"the digest could not be read: $error")
            case Right(kept) =>
              val lines = kept.flatMap((_, d) => shown(d))
              Outcome.Done(
                if (lines.isEmpty) "No conversation has closed yet." else lines.mkString("\n")
              )
          }
      })
  }
}
