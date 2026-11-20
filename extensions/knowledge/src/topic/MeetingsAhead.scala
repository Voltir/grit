package grit.knowledge.topic

import java.time.{Instant, LocalDate, ZoneId}

import scala.concurrent.duration.*

import grit.core.act.Sorting
import grit.core.classify.{Criterion, Decision}
import grit.core.document.Document
import grit.core.durable.Journaled
import grit.core.id.{DocumentVersionId, ShortHash}
import grit.core.occasion.{Occasion, OccasionBrief, OccasionKey}
import grit.core.place.Place
import grit.knowledge.ref.TopicKey
import grit.std.collect.Collect
import grit.std.either.Proven

object MeetingsAhead {

  val Asks: Int = 3

  val Window: FiniteDuration = 24.hours

  val YesAt: Double = 0.5

  enum Judged extends caps.Pure {
    case Unsettled, Settled
  }

  val Unsettled: Sorting[Judged, Decision[Judged]] = Proven.right(
    Sorting.choice(
      "This document is what a team said about one meeting it planned. Does the talk since it " +
        "was arranged leave something unsettled for it, or a detail missing?",
      Occasion.DocumentChars,
      Occasion.DocumentChars,
      1,
      Criterion(
        Judged.Unsettled,
        "unsettled",
        Some(
          "Something is still open or missing: a question nobody answered, a choice not yet " +
            "made, a document someone said they would share, or where it is or how to join."
        )
      ),
      Criterion(
        Judged.Settled,
        "settled",
        Some("Everything the meeting needs is settled and stated.")
      )
    )
    // Two keys, neither a number, and 1 is within Chosen.
  )

  val Brief: OccasionBrief = OccasionBrief.literal(
    "A meeting the team planned starts within the next few hours: the one in the document " +
      "below. Talk since it was arranged left something unsettled for it, or a detail missing. " +
      "Say in two or three lines what is still open or missing (a question nobody answered, a " +
      "choice not yet made, a document someone said they would share, where it is or how to " +
      "join), naming the meeting and its time as the document does, so the team can settle it " +
      "before it starts. Do not repeat what is already settled, and do not answer the open " +
      "points yourself."
  )

  /** `ahead:{the topic key's ShortHash}:{day}`: 33 characters, so always a key. */
  def key(topic: TopicKey, day: LocalDate): OccasionKey =
    Proven.right(
      OccasionKey
        .of(s"ahead:${ShortHash.of(TopicKey.value(topic))}:$day")
        .toRight(s"no occasion key for ${TopicKey.value(topic)} on $day")
    )

  final case class Due(version: DocumentVersionId, room: Place, start: Instant, key: OccasionKey)
      extends caps.Pure

  /** A scan's meetings to sort, and how many due each check passed over. */
  final case class Found(
      at: Instant,
      asking: Vector[Due],
      within: Int,
      fewer: Int,
      left: Int
  ) extends caps.Pure

  /** Of `documents`, each meeting planned to start within [[Window]] after `at` (one with a day
    * and no start: on `at`'s day in `zone`, until that day's end), in the one of `rooms` its
    * topic's strands were most written from, the latest on a tie, and naming two people or
    * more; the soonest of each room, at most [[Asks]].
    */
  def due(documents: Vector[Document], rooms: Vector[Place], zone: ZoneId, at: Instant): Found = {
    val until = at.plusNanos(Window.toNanos)
    val today = at.atZone(zone).toLocalDate
    val within = documents.flatMap(d =>
      for {
        topicKey <- TopicKey.read(d.key)
        topic <- Topic.read(topicKey, d.data).toOption
        meeting <- topic.kind match {
          case Topic.Kind.Meeting(m @ Meeting(Meeting.State.Planned(_), _)) => Some(m)
          case Topic.Kind.Meeting(_) | Topic.Kind.Subject => None
        }
        when <- meeting.when
        ends <- when.start match {
          case Some(t) =>
            Some(when.day.atTime(t).atZone(zone).toInstant)
              .filter(s => s.isAfter(at) && !s.isAfter(until))
          case None =>
            Option.when(when.day == today)(when.day.plusDays(1).atStartOfDay(zone).toInstant)
        }
        room <- roomOf(topic, rooms)
      } yield (Due(d.version, room, ends, key(topicKey, when.day)), meeting.who.size >= 2)
    )
    val named = within.collect { case (d, true) => d }
    val asking = named.sortBy(_.start).distinctBy(_.room).take(Asks)
    Found(at, asking, within.size, within.size - named.size, named.size - asking.size)
  }

  /** The one of `rooms` the most of `topic`'s strands, each once, were written from; the latest
    * written on a tie.
    */
  private def roomOf(topic: Topic, rooms: Vector[Place]): Option[Place] =
    Collect
      .counted(
        topic.passages.reverse
          .flatMap(
            _.from.tags.reverse
              .collect { case t @ Lineage.Tag.Strand(_, room, _, _) =>
                t.strand.map((id, _) => (id, room))
              }
              .flatten
          )
          .distinctBy(_._1)
          .map(_._2)
          .filter(rooms.contains)
      )
      .maxByOption(_._2)
      .map(_._1)

  private[topic] def occasion(due: Due): Occasion =
    Occasion(due.room, due.key, Brief, due.version, due.start)

  given Journaled[Found] = Journaled.json(
    f =>
      ujson.Obj(
        "at" -> f.at.toString,
        "asking" -> ujson.Arr.from(
          f.asking.map(d =>
            ujson.Obj(
              "version" -> ujson.Num(DocumentVersionId.value(d.version).toDouble),
              "room" -> d.room.written,
              "start" -> d.start.toString,
              "key" -> OccasionKey.value(d.key)
            )
          )
        ),
        "within" -> f.within,
        "fewer" -> f.fewer,
        "left" -> f.left
      ),
    v =>
      for {
        o <- v.objOpt.toRight("meetings: expected an object")
        at <- instant(o.get("at"))
        all <- o.get("asking").flatMap(_.arrOpt).toRight("meetings: expected asking")
        asking <- all.toVector.foldLeft[Either[String, Vector[Due]]](Right(Vector.empty))(
          (acc, e) => acc.flatMap(done => dueOf(e).map(done :+ _))
        )
        counts <- Vector("within", "fewer", "left")
          .foldLeft[Either[String, Vector[Int]]](Right(Vector.empty))((acc, k) =>
            acc.flatMap(done =>
              o.get(k)
                .flatMap(_.numOpt)
                .map(_.toInt)
                .toRight(s"meetings: expected $k")
                .map(done :+ _)
            )
          )
        found <- counts match {
          case Vector(within, fewer, left) => Right(Found(at, asking, within, fewer, left))
          case _ => Left("meetings: expected three counts")
        }
      } yield found
  )

  private def instant(v: Option[ujson.Value]): Either[String, Instant] =
    v.flatMap(_.strOpt)
      .flatMap(t => scala.util.Try(Instant.parse(t)).toOption)
      .toRight("meetings: expected an instant")

  private def dueOf(v: ujson.Value): Either[String, Due] =
    for {
      o <- v.objOpt.toRight("meetings: a meeting is an object")
      version <- o
        .get("version")
        .flatMap(_.numOpt)
        .flatMap(n => DocumentVersionId.of(n.toLong))
        .toRight("meetings: no version")
      room <- o.get("room").flatMap(_.strOpt).toRight("meetings: no room").flatMap(Place.read)
      start <- instant(o.get("start"))
      key <- o.get("key").flatMap(_.strOpt).flatMap(OccasionKey.of).toRight("meetings: no key")
    } yield Due(version, room, start, key)
}
