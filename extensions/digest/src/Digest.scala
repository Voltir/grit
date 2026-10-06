package grit.digest

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocText, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.id.{CallSlot, DocKey, PluginName}
import grit.core.job.{NotOwn, OwnJobs, ScheduleDesk}
import grit.core.period.{CloseOrdinal, CloseReason}
import grit.core.place.Place
import grit.core.plugin.{Documents, Exports, Needs, PluginReads, PluginRun, PluginTool, Unneeded}
import grit.core.store.{ClosedPeriod, Db, Origin, StoreError, Tx}
import grit.core.tool.{Args, Field, Gate, Hosted, Outcome, ToolName, ToolSpec}

/** The digest (ADR 0011's hello-world plugin, kept as documents, ADR 0028): one document per
  * room ([[grit.core.store.Origin.room]]) where conversations closed, kept at that room,
  * holding its newest [[Digest.Lines]] closings' lines (when each closed, where, why, and what
  * it came to: [[grit.core.period.Closing.headline]]), newest first, written as of the newest
  * one's close; none for a period whose closing stays in its own conversation
  * ([[CloseReason.Unshown]]: unearned, or a job's run). Retrieval finds a room's document where a window's scope
  * reaches the room; `recent_activity` reads them all.
  */
final class Digest(val name: PluginName) extends Exports[Activity] {

  val version: Int = 2

  override val documents: Option[Documents] = Some(Digest.Kept)

  override val tools: Vector[PluginTool[?]] = Vector(Digest.RecentActivity)

  def service(own: PluginReads): Activity = Digest.activity(own)
}

/** What the digest exports: its lines, read from its documents. */
trait Activity extends caps.Pure {

  /** Its newest `n` lines across every room, newest first; at most [[Digest.Lines]] of any one
    * room, since a room keeps no more. None when `n` is not positive.
    */
  def recent(n: Int)(using Tx^): Either[StoreError, Vector[String]]
}

object Digest {

  /** How many closings' lines a room's document holds: 10. */
  val Lines: Int = 10

  /** Label "digest: conversations closed here most recently", unscaled, kept 30 days, at most
    * 1000 rooms.
    */
  val Terms: DocumentTerms =
    // Literals both constructors accept, read when the object is first used: every test of
    // the digest throws here if not.
    (for {
      label <- DocLabel.of("digest: conversations closed here most recently")
      terms <- DocumentTerms.of(label, DocWeight.Unscaled, 30.days, 1000)
    } yield terms).fold(why => throw new IllegalStateException(why), identity)

  /** Each closing's line in its room's document. */
  private object Kept extends Documents {
    val terms: DocumentTerms = Terms
    def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit] =
      why(closed.reason) match {
        case None => Right(())
        case Some(why) =>
          val room = closed.origin.room
          val line = Line(
            CloseOrdinal.value(closed.order),
            closed.at,
            where(closed.origin),
            why,
            closed.closing.headline
          )
          for {
            key <- DocKey.of(roomKey(room)).left.map(StoreError.Invalid(_))
            current <- keeper.current(key)
            kept = current.fold(Vector.empty[Line])(d => Line.all(d.data))
            lines = (kept.filterNot(_.order == line.order) :+ line)
              .sortBy(-_.order)
              .take(Lines)
            text <- DocText
              .of((Header +: lines.map(_.shown)).mkString("\n"))
              .left
              .map(StoreError.Invalid(_))
            newest = lines.headOption.fold(closed.at)(_.at)
            _ <- keeper.write(key, room, text, Line.data(lines), newest)
          } yield ()
      }
  }

  /** The lines `own`'s documents hold. */
  private def activity(own: PluginReads): Activity = new Activity {
    def recent(n: Int)(using Tx^): Either[StoreError, Vector[String]] =
      if (n < 1) Right(Vector.empty)
      else
        // The rooms holding the newest n lines are among the n written last: each is written
        // as of its newest line.
        own.documents
          .newest(n)
          .map(
            _.flatMap(d => Line.all(d.data))
              .sortBy(l => (l.at, l.order))(using Ordering[(Instant, Long)].reverse)
              .take(n)
              .map(_.shown)
          )
  }

  /** What heads a room's document, above its lines. */
  private val Header = "Closed here, newest first (UTC):"

  /** A room's key: the SHA-256 of its written place, in hex, so that a place too long or not
    * one line for a [[DocKey]] still has one.
    */
  private def roomKey(room: Place): String =
    java.security.MessageDigest
      .getInstance("SHA-256")
      .digest(room.written.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      .map(b => f"$b%02x")
      .mkString

  /** One closing's line, under its close ordinal `order`. */
  private final case class Line(
      order: Long,
      at: Instant,
      where: String,
      why: String,
      line: String
  ) {

    /** `{date} {time} · {where} · {why} · {headline}`, UTC. */
    def shown: String = s"${Minute.format(at)} · $where · $why · $line"
  }

  private object Line {

    /** `lines` as a document's data. */
    def data(lines: Vector[Line]): ujson.Value =
      ujson.Obj(
        "lines" -> ujson.Arr.from(
          lines.map(l =>
            ujson.Obj(
              "order" -> ujson.Num(l.order.toDouble),
              "at" -> l.at.toString,
              "where" -> l.where,
              "why" -> l.why,
              "line" -> l.line
            )
          )
        )
      )

    /** The lines a document's data holds; one unreadable is left out. */
    def all(data: ujson.Value): Vector[Line] =
      data.objOpt
        .flatMap(_.get("lines"))
        .flatMap(_.arrOpt)
        .fold(Vector.empty[Line])(_.toVector.flatMap(one))

    private def one(v: ujson.Value): Option[Line] =
      for {
        o <- v.objOpt
        order <- o.get("order").flatMap(_.numOpt).map(_.toLong)
        at <- o.get("at").flatMap(_.strOpt).flatMap(s => scala.util.Try(Instant.parse(s)).toOption)
        where <- o.get("where").flatMap(_.strOpt)
        why <- o.get("why").flatMap(_.strOpt)
        line <- o.get("line").flatMap(_.strOpt)
      } yield Line(order, at, where, why, line)
  }

  /** How many lines `recent_activity` shows when not told. */
  val DefaultShown = 10

  /** The most lines `recent_activity` shows. */
  val MaxShown = 50

  /** Why a period closed, as its line says it; `None` for one that keeps no line. */
  private def why(reason: CloseReason): Option[String] = reason match {
    case CloseReason.Resolved(_) => Some("resolved")
    case CloseReason.Lapsed => Some("lapsed")
    case CloseReason.Unearned | CloseReason.Ran => None
  }

  private def where(origin: Origin): String = origin match {
    case Origin.Tui(_, session) => s"tui $session"
    case Origin.Slack(_, channel, _) => s"slack #$channel"
    case Origin.Task(name, _) => s"task $name"
  }

  private val Minute = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(ZoneOffset.UTC)

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

    def bind(
        own: PluginReads,
        needs: Needs,
        jobs: OwnJobs
    ): Either[Unneeded | NotOwn, PluginRun[Int]] =
      Right(new PluginRun[Int] {
        private val read = activity(own)
        def run(n: Int, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome =
          db.read(read.recent(n)) match {
            case Left(error) => Outcome.Failed(s"the digest could not be read: $error")
            case Right(lines) =>
              Outcome.Done(
                if (lines.isEmpty) "No conversation has closed yet." else lines.mkString("\n")
              )
          }
      })
  }
}
