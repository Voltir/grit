package grit.digest

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Utc
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.id.{CallSlot, DocKey, PluginName, Sha256}
import grit.core.job.{NotOwn, OwnJobs, ScheduleDesk}
import grit.core.need.{Needs, PluginReads, Unneeded}
import grit.core.period.{CloseOrdinal, CloseReason}
import grit.core.place.Place
import grit.core.plugin.{DocumentPosting, Documents, Exports, PluginRun, PluginTool}
import grit.core.store.{ClosedPeriod, Origin, Reads, StoreError, Tx}
import grit.core.tool.{Args, Believed, Field, Gate, Hosted, Outcome, ToolName, ToolSpec}
import grit.core.visibility.Integrity

/** The digest (ADR 0011's hello-world plugin, kept as documents, ADR 0028): one document per
  * room ([[grit.core.store.Origin.room]]) where conversations closed and per label those
  * conversations were created at, placed at that room and kept in it at that label, holding
  * its newest [[Digest.Lines]] closings' lines (when each closed, where, why, and what it came
  * to: [[grit.core.period.Closing.headline]]), newest first, written as of the newest one's
  * close; none for a period whose closing stays in its own conversation
  * ([[CloseReason.Unshown]]: unearned, or a job's run). Retrieval finds a room's document where
  * a window's turn reads it; `recent_activity` lists the lines
  * of every one the calling turn reads.
  */
final class Digest(val name: PluginName) extends Exports[Activity] {

  val version: Int = 3

  override val documents: Option[Documents] = Some(Digest.Kept)

  override val tools: Vector[PluginTool[?]] = Vector(Digest.RecentActivity)

  def service(own: PluginReads): Activity = Digest.activity(own)
}

/** What the digest exports: its lines, read from its documents. */
trait Activity extends caps.Pure {

  /** Its newest `n` lines across every document its transaction reads, newest first; at most
    * [[Digest.Lines]] of any one, since a document keeps no more. None when `n` is not
    * positive.
    */
  def recent(n: Int)(using Tx^): Either[StoreError, Vector[String]]
}

object Digest {

  /** How many closings' lines a room's document holds: 10. */
  val Lines: Int = 10

  /** Label "digest: conversations closed here most recently", unscaled, kept 30 days, at most
    * 1000 documents, every room's and label's together.
    */
  val Terms: DocumentTerms =
    // Literals both constructors accept, read when the object is first used: every test of
    // the digest throws here if not.
    (for {
      label <- DocLabel.of("digest: conversations closed here most recently")
      terms <- DocumentTerms.of(label, DocWeight.Unscaled, 30.days, 1000)
    } yield terms).fold(why => throw new IllegalStateException(why), identity)

  /** Its documents, under [[Terms]], each room's posted [[Posted]]. */
  private object Kept extends Documents {
    val terms: DocumentTerms = Terms
    val posting: Option[DocumentPosting] = Some(Posted)
  }

  /** Each closing's line in its room's document. */
  private object Posted extends DocumentPosting {
    def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using
        tx: Tx^
    ): Either[StoreError, Unit] =
      why(closed.reason) match {
        case None => Right(())
        case Some(why) =>
          val room = closed.origin.room
          // Posting reads and keeps at the closed conversation's label (Tx.cleared): a
          // thread of the room at another label keeps its own document under the same key.
          val label = Tx.cleared(tx)
          val line = Line(
            CloseOrdinal.value(closed.order),
            closed.at,
            where(closed.origin),
            why,
            closed.closing.headline
          )
          for {
            key <- DocKey.of(roomKey(room)).left.map(StoreError.Invalid(_))
            current <- keeper.current(key, label)
            kept = current.fold(Vector.empty[Line])(d => Line.all(d.data))
            lines = (kept.filterNot(_.order == line.order) :+ line)
              .sortBy(-_.order)
              .take(Lines)
            text <- DocText
              .of((Header +: lines.map(_.shown)).mkString("\n"))
              .left
              .map(StoreError.Invalid(_))
            newest = lines.headOption.fold(closed.at)(_.at)
            // Made from the lines kept before and this closing: believed as the least of them.
            believed = current.fold(closed.integrity)(_.integrity.meet(closed.integrity))
            _ <- keeper.write(key, label, believed, room, text, Line.data(lines), None, newest)
          } yield ()
      }
  }

  /** The lines `own`'s documents hold. */
  private def activity(own: PluginReads): Activity = new Activity {
    def recent(n: Int)(using Tx^): Either[StoreError, Vector[String]] = newest(own, n).map(_._1)
  }

  /** The newest `n` lines of `own`'s documents, newest first, and the least integrity of the
    * documents read for them; `None` when none was read.
    */
  private def newest(own: PluginReads, n: Int)(using
      Tx^
  ): Either[StoreError, (Vector[String], Option[Integrity])] =
    if (n < 1) Right((Vector.empty, None))
    else
      // The rooms holding the newest n lines are among the n written last: each is written
      // as of its newest line.
      own.documents
        .newest(n)
        .map { documents =>
          val lines = documents
            .flatMap(d => Line.all(d.data))
            .sortBy(l => (l.at, l.order))(using Ordering[(Instant, Long)].reverse)
            .take(n)
            .map(_.shown)
          (lines, documents.map(_.integrity).reduceOption(_.meet(_)))
        }

  /** What heads a room's document, above its lines. */
  private val Header = "Closed here, newest first:"

  /** A room's key: the SHA-256 of its written place, in hex, so that a place too long or not
    * one line for a [[DocKey]] still has one.
    */
  private def roomKey(room: Place): String = Sha256.text(room.written).hex

  /** One closing's line, under its close ordinal `order`. */
  private final case class Line(
      order: Long,
      at: Instant,
      where: String,
      why: String,
      line: String
  ) {

    /** `{date} {time} UTC · {where} · {why} · {headline}` ([[Utc.toMinute]]). */
    def shown: String = s"${Utc.toMinute(at)} · $where · $why · $line"
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
    case Origin.Direct(_, _) => "a direct message"
  }

  /** `recent_activity`: the newest lines of the digest it is bound to that the calling turn
    * reads (the [[Reads]] its run is handed, for that turn), newest first, believed at the
    * least integrity of the documents read, and as grit's own words when it read none or could
    * not read ([[grit.core.tool.Believed.Own]]). Free: it only reads.
    */
  val RecentActivity: PluginTool[Int] = new PluginTool[Int] {
    val described: Hosted[Int] =
      new Hosted(
        ToolSpec(
          ToolName("recent_activity"),
          "List the conversations that closed most recently, across the places grit works " +
            "that this conversation may see (this chat, Slack threads, tasks), newest first, one line each: when it closed " +
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
        Believed.Own
      )

    def bind(
        own: PluginReads,
        needs: Needs,
        jobs: OwnJobs
    ): Either[Unneeded | NotOwn, PluginRun[Int]] =
      Right(new PluginRun[Int] {
        def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome =
          reads.read(newest(own, n)) match {
            case Left(error) => Outcome.Failed(s"the digest could not be read: $error")
            case Right((lines, believed)) =>
              Outcome.Done(
                if (lines.isEmpty) "No conversation has closed yet." else lines.mkString("\n"),
                believed
              )
          }
      })
  }
}
