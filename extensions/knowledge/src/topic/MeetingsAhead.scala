package grit.knowledge.topic

import java.time.{Instant, LocalDate, LocalTime, ZoneId}

import grit.core.act.{Keeping, MoveError, MoveLimits, MoveName, Occasioned}
import grit.core.document.Document
import grit.core.durable.Journaled
import grit.core.flow.Trusted
import grit.core.id.{DocumentVersionId, JobName, ShortHash}
import grit.core.job.{JobRun, KeepingJob}
import grit.core.occasion.{Occasion, OccasionKey}
import grit.core.place.Place
import grit.core.speech.SpeechJson
import grit.knowledge.ref.TopicKey

/** What a run of `meetings-ahead` is told: the `day` it notes, its slot's day in its job's zone
  * when none.
  */
final case class AheadParams(day: Option[LocalDate] = None) extends caps.Pure

/** The job `meetings-ahead`, its days in `zone`, reading at most `bound` of its plugin's current
  * documents: a run for its day names at most [[MeetingsAhead.Most]] occasions (`occasion-0`,
  * `occasion-1`, …), ahead ones first, each about the topic it read, in the room of that topic's
  * newest passage written from a strand (a topic with none names no occasion):
  *   - for each meeting planned for that day, `Ahead`, keyed `ahead:{topic}:{day}`, until its
  *     start, or the day's end when it has none; soonest start first, one with none last;
  *   - for each meeting proposed with no day whose newest passage is 1 to
  *     [[MeetingsAhead.Talked]] days before that day, `Open`, keyed `open:{topic}`, until
  *     [[MeetingsAhead.OpenFor]] hours after its slot; most recently talked of first.
  *
  * `{topic}` is [[MeetingsAhead.keyed]] of the topic's key, so a meeting folded into another
  * topic after it was noted may be noted again under that topic's. Replies
  * [[MeetingsAhead.reply]]. Its `limits`: `Most` occasions, no asks or calls.
  */
final class MeetingsAhead(zone: ZoneId, bound: Int) extends KeepingJob[AheadParams] {

  val name: JobName = MeetingsAhead.Name

  /** 1: the first version. */
  val version: Int = 1

  def write(params: AheadParams): ujson.Value =
    params.day.fold[ujson.Value](ujson.Obj())(d => ujson.Obj("day" -> d.toString))

  def read(params: ujson.Value): Either[String, AheadParams] =
    params.objOpt
      .toRight(s"a meetings-ahead run's parameters are an object: ${ujson.write(params).take(100)}")
      .flatMap(_.get("day") match {
        case None => Right(AheadParams(None))
        case Some(d) =>
          d.strOpt
            .flatMap(Passage.day)
            .toRight(s"a meetings-ahead run's day is a date: ${ujson.write(d).take(100)}")
            .map(day => AheadParams(Some(day)))
      })

  override val limits: MoveLimits =
    MoveLimits.Zero.withOccasions(MeetingsAhead.Most).getOrElse(MoveLimits.Zero)

  def run(run: Trusted[JobRun[AheadParams]], moves: Keeping^): String = {
    val r = run.extract
    val day = r.params.day.getOrElse(LocalDate.ofInstant(r.nominal, zone))
    moves.keep(MeetingsAhead.Read, run.tainted)((_, keeper, _) =>
      keeper.newest(bound).map(MeetingsAhead.named(_, day, zone, r.nominal))
    ) match {
      case Left(e) => s"Meetings ahead for $day: its meetings were not read: ${MoveError.said(e)}"
      case Right(named) =>
        val (now, past) = named.occasions.splitAt(MeetingsAhead.Most)
        val made = now.zipWithIndex.map((o, i) =>
          MoveName
            .of(s"occasion-$i")
            .left
            .map(MoveError.Store(_))
            .flatMap(n => moves.occasion(n, run.tainted.map(_ => o)))
        )
        MeetingsAhead.reply(day, made, named.unkeyed, past.size)
    }
  }
}

object MeetingsAhead {

  /** `meetings-ahead`. */
  val Name: JobName = JobName.literal("meetings-ahead")

  /** 3: the most occasions one run names. */
  val Most: Int = 3

  /** 12: the hours after its slot an `Open` occasion stays due. */
  val OpenFor: Long = 12

  /** 7: the most days before a run's day a meeting proposed with no day may last have been
    * talked of and still be asked about.
    */
  val Talked: Long = 7

  /** The keep a run reads its meetings in. */
  private val Read: MoveName = MoveName.literal("meetings")

  /** What a run read of its meetings: the occasions due, in the order it names them, and how
    * many meetings due named no key ([[OccasionKey.of]] refused it).
    */
  final case class Named(occasions: Vector[Occasion], unkeyed: Int) extends caps.Pure

  /** The first 16 hex digits of the SHA-256 of `topic`'s key: a topic's part of an occasion's
    * key, short and of the characters a key holds, however long its key.
    */
  def keyed(topic: TopicKey): String = ShortHash.of(TopicKey.value(topic))

  /** The occasions due on `day` in `zone` among `documents`, its run's slot `nominal`, as
    * [[MeetingsAhead]] says, ahead ones first; a document that is not a readable meeting topic
    * is passed over.
    */
  private[topic] def named(
      documents: Vector[Document],
      day: LocalDate,
      zone: ZoneId,
      nominal: Instant
  ): Named = {
    val meetings = documents.flatMap(d =>
      for {
        key <- TopicKey.read(d.key)
        topic <- Topic.read(key, d.data).toOption
        meeting <- topic.kind match {
          case Topic.Kind.Meeting(m) => Some(m)
          case Topic.Kind.Subject => None
        }
        room <- topic.passages.reverse.view.flatMap(roomOf).headOption
      } yield (d.version, key, topic, meeting, room)
    )
    val ends = day.plusDays(1).atStartOfDay(zone).toInstant
    val ahead = meetings
      .collect {
        case (v, key, _, Meeting(Meeting.State.Planned(w), _), room) if w.day == day =>
          val until = w.start.fold(ends)(t => day.atTime(t).atZone(zone).toInstant)
          (
            w.start,
            keyed(key),
            Due(room, s"ahead:${keyed(key)}:$day", Occasion.Reason.Ahead, v, until)
          )
      }
      .sortBy((start, k, _) => (start.isEmpty, start.getOrElse(LocalTime.MIN), k))
      .map(_._3)
    val open = meetings
      .flatMap { (v, key, topic, meeting, room) =>
        val talked = topic.passages.lastOption.map(_.day)
        meeting.state match {
          case Meeting.State.Proposed(None)
              if talked.exists(t => t.isBefore(day) && !t.isBefore(day.minusDays(Talked))) =>
            talked.map(t =>
              (
                t,
                keyed(key),
                Due(
                  room,
                  s"open:${keyed(key)}",
                  Occasion.Reason.Open,
                  v,
                  nominal.plusSeconds(OpenFor * 3600)
                )
              )
            )
          case _ => None
        }
      }
      .sortBy((t, k, _) => (-t.toEpochDay, k))
      .map(_._3)
    val due = ahead ++ open
    val occasions = due.flatMap(_.occasion)
    Named(occasions, due.size - occasions.size)
  }

  /** An occasion due, its key not yet checked. */
  private final case class Due(
      room: Place,
      key: String,
      reason: Occasion.Reason,
      about: DocumentVersionId,
      until: Instant
  ) {
    def occasion: Option[Occasion] =
      OccasionKey.of(key).map(Occasion(room, _, reason, about, until))
  }

  /** The room of `passage`'s last strand, when it was written from one. */
  private def roomOf(passage: Passage): Option[Place] =
    passage.from.tags.reverse.collectFirst { case Lineage.Tag.Strand(_, room, _, _) => room }

  /** A run's reply: `Meetings ahead for {day}: {n} started, {m} held ({why, counted}), {a}
    * already noted, {k} past the {Most} a run.`, then, when any failed, `, {f} failed ({why,
    * counted})` before the full stop. Held: each silence as speech names it, `mislabelled`
    * and `unread`; failed: `unstarted`, `unkeyed` for each of `unkeyed`, and each move refused,
    * as its error says. `past`, the occasions past `Most`.
    */
  // Draft wording, for Nick (W4, and its failed clause).
  def reply(
      day: LocalDate,
      made: Vector[Either[MoveError, Occasioned]],
      unkeyed: Int,
      past: Int
  ): String = {
    def counted(whys: Vector[String]): String =
      whys.distinct.sorted.map(w => s"${whys.count(_ == w)} $w").mkString(", ")
    val held = made.collect {
      case Right(Occasioned.Held(_, why)) =>
        SpeechJson
          .writeSilence(why)
          .objOpt
          .flatMap(_.get("kind"))
          .flatMap(_.strOpt)
          .getOrElse("held")
      case Right(Occasioned.Mislabelled(_)) => "mislabelled"
      case Right(Occasioned.Unread(_)) => "unread"
    }
    val failed = made.collect {
      case Right(Occasioned.Unstarted(_, _)) => "unstarted"
      case Left(e) => MoveError.said(e)
    } ++ Vector.fill(unkeyed)("unkeyed")
    val started = made.count {
      case Right(Occasioned.Started(_)) => true
      case _ => false
    }
    val already = made.count {
      case Right(Occasioned.Already) => true
      case _ => false
    }
    val heldPart = if (held.isEmpty) "0 held" else s"${held.size} held (${counted(held)})"
    val failedPart =
      if (failed.isEmpty) "" else s", ${failed.size} failed (${counted(failed)})"
    s"Meetings ahead for $day: $started started, $heldPart, $already already noted, " +
      s"$past past the $Most a run$failedPart."
  }

  /** `[{"room","key","reason","about","until"}, …]` beside `"unkeyed"`: what a run's keep read,
    * as its step record keeps it, the room as [[Place.written]] writes it and the reason `ahead`
    * or `open`.
    */
  given Journaled[Named] = Journaled.json(
    n =>
      ujson.Obj(
        "occasions" -> ujson.Arr.from(
          n.occasions.map(o =>
            ujson.Obj(
              "room" -> o.room.written,
              "key" -> OccasionKey.value(o.key),
              "reason" -> (o.reason match {
                case Occasion.Reason.Ahead => "ahead"
                case Occasion.Reason.Open => "open"
              }),
              "about" -> ujson.Num(DocumentVersionId.value(o.about).toDouble),
              "until" -> o.until.toString
            )
          )
        ),
        "unkeyed" -> n.unkeyed
      ),
    v =>
      for {
        o <- v.objOpt.toRight("meetings: expected an object")
        all <- o.get("occasions").flatMap(_.arrOpt).toRight("meetings: expected occasions")
        unkeyed <- o
          .get("unkeyed")
          .flatMap(_.numOpt)
          .map(_.toInt)
          .toRight("meetings: expected unkeyed")
        occasions <- all.toVector.foldLeft[Either[String, Vector[Occasion]]](Right(Vector.empty))(
          (acc, e) => acc.flatMap(done => occasionOf(e).map(done :+ _))
        )
      } yield Named(occasions, unkeyed)
  )

  private def occasionOf(v: ujson.Value): Either[String, Occasion] =
    for {
      o <- v.objOpt.toRight("meetings: an occasion is an object")
      room <- o.get("room").flatMap(_.strOpt).toRight("meetings: no room").flatMap(Place.read)
      key <- o.get("key").flatMap(_.strOpt).flatMap(OccasionKey.of).toRight("meetings: no key")
      reason <- o.get("reason").flatMap(_.strOpt) match {
        case Some("ahead") => Right(Occasion.Reason.Ahead)
        case Some("open") => Right(Occasion.Reason.Open)
        case _ => Left("meetings: no reason")
      }
      about <- o
        .get("about")
        .flatMap(_.numOpt)
        .flatMap(n => DocumentVersionId.of(n.toLong))
        .toRight("meetings: no version")
      until <- o
        .get("until")
        .flatMap(_.strOpt)
        .flatMap(t => scala.util.Try(Instant.parse(t)).toOption)
        .toRight("meetings: no until")
    } yield Occasion(room, key, reason, about, until)
}
