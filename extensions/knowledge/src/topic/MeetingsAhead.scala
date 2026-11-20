package grit.knowledge.topic

import java.time.{Instant, LocalDate, LocalTime, ZoneId}

import scala.concurrent.duration.*

import grit.core.act.{Bookable, Booked, Dreaming, MoveError, MoveName, Sorting}
import grit.core.classify.{Criterion, Decision}
import grit.core.document.Document
import grit.core.durable.Journaled
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.{DocKey, DocumentVersionId, ScheduleId, ShortHash}
import grit.core.occasion.{Occasion, OccasionBrief, OccasionKey}
import grit.core.place.Place
import grit.knowledge.card.CardMoves
import grit.knowledge.ref.TopicKey
import grit.std.collect.Collect
import grit.std.either.Proven

object MeetingsAhead {

  val Asks: Int = 3

  val Window: FiniteDuration = 24.hours

  val YesAt: Double = 0.5

  /** 10 minutes: how long before its start an occasion is named for a meeting, and how late its
    * booked run may start.
    */
  val Lead: FiniteDuration = 10.minutes

  /** 08:45: when, on its day, an occasion is named for a meeting with no start. */
  val Undated: LocalTime = LocalTime.of(8, 45)

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
    "A meeting the team planned starts in about ten minutes (or is today, when it has no time " +
      "set): the one in the document below. Talk since it was arranged left something " +
      "unsettled for it, or a detail missing. Say in two or three lines what is still open or " +
      "missing (a question nobody answered, a choice not yet made, a document someone said " +
      "they would share, where it is or how to join), naming the meeting and its time as the " +
      "document does, so it can be settled as it starts. Do not repeat what is already " +
      "settled, and do not answer the open points yourself."
  )

  /** `ahead:{the topic key's ShortHash}:{day}`: 33 characters, so always a key. */
  def key(topic: TopicKey, day: LocalDate): OccasionKey =
    Proven.right(
      OccasionKey
        .of(s"ahead:${ShortHash.of(TopicKey.value(topic))}:$day")
        .toRight(s"no occasion key for ${TopicKey.value(topic)} on $day")
    )

  /** The meeting of `topic`'s document at `version` on `day`, due in `room` at `slot`, its
    * occasion lapsing at `until`.
    */
  final case class Due(
      topic: TopicKey,
      day: LocalDate,
      version: DocumentVersionId,
      room: Place,
      slot: Instant,
      until: Instant
  ) extends caps.Pure {
    def key: OccasionKey = MeetingsAhead.key(topic, day)
  }

  /** A scan's meetings to sort, and how many due each check passed over. */
  final case class Found(
      at: Instant,
      asking: Vector[Due],
      within: Int,
      fewer: Int,
      left: Int
  ) extends caps.Pure

  /** Of `documents`, each meeting [[qualifying]] in `rooms` at `at` and naming two people or
    * more; the soonest of each room, at most [[Asks]].
    */
  def due(documents: Vector[Document], rooms: Vector[Place], zone: ZoneId, at: Instant): Found = {
    val within = documents.flatMap(qualifying(_, rooms, zone, at))
    val named = within.collect { case (d, true) => d }
    val asking = named.sortBy(_.until).distinctBy(_.room).take(Asks)
    Found(at, asking, within.size, within.size - named.size, named.size - asking.size)
  }

  /** `d`'s meeting, when planned to start within [[Window]] after `at`, in the one of `rooms` its
    * topic's strands were most written from, the latest on a tie; and whether it names two
    * people or more. Due [[Lead]] before its start, or with a day and no start, when that day is
    * `at`'s in `zone`, at [[Undated]] until the day's end; at `at` when that is past.
    */
  def qualifying(
      d: Document,
      rooms: Vector[Place],
      zone: ZoneId,
      at: Instant
  ): Option[(Due, Boolean)] = {
    val window = at.plusNanos(Window.toNanos)
    for {
      topicKey <- TopicKey.read(d.key)
      topic <- Topic.read(topicKey, d.data).toOption
      meeting <- topic.kind match {
        case Topic.Kind.Meeting(m @ Meeting(Meeting.State.Planned(_), _)) => Some(m)
        case Topic.Kind.Meeting(_) | Topic.Kind.Subject => None
      }
      when <- meeting.when
      (slot, until) <- when.start match {
        case Some(t) =>
          Some(when.day.atTime(t).atZone(zone).toInstant)
            .filter(s => s.isAfter(at) && !s.isAfter(window))
            .map(s => (s.minusNanos(Lead.toNanos), s))
        case None =>
          Option.when(when.day == at.atZone(zone).toLocalDate)(
            (
              when.day.atTime(Undated).atZone(zone).toInstant,
              when.day.plusDays(1).atStartOfDay(zone).toInstant
            )
          )
      }
      room <- roomOf(topic, rooms)
    } yield (
      Due(topicKey, when.day, d.version, room, if (slot.isBefore(at)) at else slot, until),
      meeting.who.size >= 2
    )
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
    Occasion(due.room, due.key, Brief, due.version, due.until)

  private val Scan: MoveName = MoveName.literal("ahead")

  private enum Sorted {
    case Refused(why: String)
    case Unasked
    case Settled
    case Booked(booked: Either[MoveError, ScheduleId])
  }

  /** A topics run's last phase, for a day ending at `end` in `zone`: its documents scanned
    * (`ahead`) for the meetings [[due]] in `rooms`, none when `end` is before the scan; each
    * sorted (`ahead-sort-{i}`) by [[Unsettled]], and a run of `ahead` booked (`later-{i}`) at
    * its slot, missed past its until, for each judged unsettled at [[YesAt]] or more. Replies
    * what each came to, to follow the run's own line.
    */
  private[topic] def named(
      rooms: Tainted[Vector[Place]],
      zone: ZoneId,
      end: Instant,
      ahead: Bookable[Due],
      moves: Dreaming^
  ): String =
    moves.scan(Scan, rooms)((rs, keeper, _, _, at) =>
      if (end.isBefore(at)) Right(Found(at, Vector.empty, 0, 0, 0))
      else keeper.newest(Int.MaxValue).map(due(_, rs, zone, at))
    ) match {
      case Left(e) => s"; meetings ahead not read: ${MoveError.said(e)}"
      case Right(found) =>
        reply(
          found,
          found.asking.zipWithIndex.map((d, i) =>
            moves.sort(
              CardMoves.numbered("ahead-sort", i),
              Dreaming.Item.Document(d.version),
              Sorting.Among(Vector.empty, Vector.empty, 0),
              rooms.map(_ => Unsettled)
            ) match {
              case Left(e) => Sorted.Refused(MoveError.said(e))
              case Right(None) => Sorted.Unasked
              case Right(Some(sorted)) =>
                val answer = sorted.asked.reply
                if (Trusted.asIs(answer).extract.probability(Judged.Unsettled) < YesAt)
                  Sorted.Settled
                else
                  Sorted.Booked(
                    moves.later(
                      CardMoves.numbered("later", i),
                      answer.map(_ => Booked(ahead, d.slot, d.until, d))
                    )
                  )
            }
          )
        )
    }

  private def reply(found: Found, sorted: Vector[Sorted]): String = {
    def counted(whys: Vector[String]): String =
      Collect.counted(whys).map((w, n) => s"$n $w").mkString(", ")
    val failed = sorted.collect {
      case Sorted.Refused(why) => why
      case Sorted.Unasked => "unsorted"
      case Sorted.Booked(Left(e)) => MoveError.said(e)
    }
    val booked = sorted.count {
      case Sorted.Booked(Right(_)) => true
      case _ => false
    }
    val settled = sorted.count(_ == Sorted.Settled)
    val failedPart = if (failed.isEmpty) "" else s", ${failed.size} failed (${counted(failed)})"
    s"; meetings ahead: ${found.within} in its rooms within ${Window.toHours} hours, " +
      s"${found.fewer} naming fewer than two people, ${found.left} past the $Asks a run asks " +
      s"about; $settled judged settled, $booked booked$failedPart"
  }

  given Journaled[Found] = Journaled.json(
    f =>
      ujson.Obj(
        "at" -> f.at.toString,
        "asking" -> ujson.Arr.from(f.asking.map(writeDue)),
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

  /** `{"topic", "day", "version", "room", "slot", "until"}`. */
  private[topic] def writeDue(d: Due): ujson.Value =
    ujson.Obj(
      "topic" -> TopicKey.value(d.topic),
      "day" -> d.day.toString,
      "version" -> ujson.Num(DocumentVersionId.value(d.version).toDouble),
      "room" -> d.room.written,
      "slot" -> d.slot.toString,
      "until" -> d.until.toString
    )

  /** The meeting [[writeDue]] wrote, or why `v` holds none. */
  private[topic] def dueOf(v: ujson.Value): Either[String, Due] =
    for {
      o <- v.objOpt.toRight("meetings: a meeting is an object")
      topic <- o
        .get("topic")
        .flatMap(_.strOpt)
        .flatMap(t => DocKey.of(t).toOption)
        .flatMap(TopicKey.read)
        .toRight("meetings: no topic")
      day <- o
        .get("day")
        .flatMap(_.strOpt)
        .flatMap(t => scala.util.Try(LocalDate.parse(t)).toOption)
        .toRight("meetings: no day")
      version <- o
        .get("version")
        .flatMap(_.numOpt)
        .flatMap(n => DocumentVersionId.of(n.toLong))
        .toRight("meetings: no version")
      room <- o.get("room").flatMap(_.strOpt).toRight("meetings: no room").flatMap(Place.read)
      slot <- instant(o.get("slot"))
      until <- instant(o.get("until"))
    } yield Due(topic, day, version, room, slot, until)
}
