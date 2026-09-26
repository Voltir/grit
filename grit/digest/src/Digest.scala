package grit.digest

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import grit.core.period.{CloseOrdinal, CloseReason}
import grit.core.plugin.{Plugin, PluginDocs, PluginName}
import grit.core.store.{ClosedPeriod, Db, Origin, StoreError, Tx}
import grit.core.tool.{Args, Field, Gate, Outcome, Tool, ToolName, ToolSpec}

/** The digest: one line per closed period, across every conversation: when it closed,
  * where, why, and what it came to ([[grit.core.period.Closing.headline]]). The hello-world
  * plugin, kept under `name`: one document per closed period, keyed by its close ordinal.
  */
final class Digest(val name: PluginName) extends Plugin {

  val version: Int = 1

  def post(closed: ClosedPeriod, docs: PluginDocs)(using Tx^): Either[StoreError, Unit] =
    docs.put(Digest.key(closed.order), Digest.doc(closed))
}

object Digest {

  /** How many lines `recent_activity` shows when not told. */
  val DefaultShown = 10

  /** The most lines `recent_activity` shows. */
  val MaxShown = 50

  /** A document's key: its close ordinal, zero-padded, so keys order as closes do. */
  private def key(o: CloseOrdinal): String = f"${CloseOrdinal.value(o)}%020d"

  /** The document kept for `closed`: `at`, `where`, `why`, `line`. */
  private def doc(closed: ClosedPeriod): ujson.Value =
    ujson.Obj(
      "at" -> closed.at.toString,
      "where" -> where(closed.origin),
      "why" -> (closed.reason match {
        case CloseReason.Resolved(_) => "resolved"
        case CloseReason.Lapsed => "lapsed"
      }),
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

  /** `recent_activity`: the newest lines of the digest `docs` holds, read through `db`, newest
    * first. Free: it only reads.
    */
  def recentActivity(db: Db^, docs: PluginDocs): Tool[Int]^{db} =
    new Tool(
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
      n => n.toString,
      n =>
        db.read(docs.newest("", n)) match {
          case Left(error) => Outcome.Failed(s"the digest could not be read: $error")
          case Right(kept) =>
            val lines = kept.flatMap((_, d) => shown(d))
            Outcome.Done(
              if (lines.isEmpty) "No conversation has closed yet." else lines.mkString("\n")
            )
        }
    )
}
