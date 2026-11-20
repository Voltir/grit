package grit.knowledge.topic

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, ZoneId}

import scala.concurrent.duration.*

import grit.core.act.{Dreaming, MoveError, MoveLimits, MoveName, Occasioned, Sorting}
import grit.core.classify.{Criterion, Decision}
import grit.core.document.DocumentShelf
import grit.core.durable.Journaled
import grit.core.flow.Trusted
import grit.core.id.{DocumentVersionId, JobName, ShortHash}
import grit.core.job.{DreamingJob, JobRun}
import grit.core.occasion.{Occasion, OccasionBrief, OccasionKey}
import grit.core.place.Place
import grit.core.speech.SpeechJson
import grit.core.stitch.{Strand, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.knowledge.card.CardMoves
import grit.knowledge.ref.TopicKey
import grit.std.collect.Collect
import grit.std.either.Proven

/** What a run of `meetings-ahead` is told: the rooms it may name occasions in. */
final case class AheadParams(rooms: Vector[Place]) extends caps.Pure

/** The job `meetings-ahead`, its days in `zone`, reading at most `bound` of its plugin's current
  * documents. A run names occasions for the meetings its topics plan, each passing these checks
  * in order, cheapest first, measured from its scan's recorded instant `at`:
  *   - its topic's room, that of its newest passage written from a strand, is one of `rooms`;
  *   - it starts after `at` and within [[MeetingsAhead.Lead]] (one with no start is passed
  *     over);
  *   - it names two people or more ([[Meeting.who]]);
  *   - not covered: of the strands spoken in within [[MeetingsAhead.Since]] before `at` (top
  *     levels cut by [[Strand.Cut.Default]]), none its topic was written from (by root and first
  *     message) was touched within [[MeetingsAhead.Covered]], or holds messages its topic has not
  *     read (a digest other than its lineage's);
  *   - sorted by the deployment's classifier ([[Dreaming.sort]] of its document by
  *     [[MeetingsAhead.Unsettled]], its head within [[Occasion.DocumentChars]]), `unsettled` at
  *     [[MeetingsAhead.YesAt]] or more; a sort that asks nothing names none.
  *
  * Only the soonest in each room is sorted, at most [[MeetingsAhead.Asks]] a run, soonest first.
  * Each occasion (`occasion-{i}`) is in its topic's room, keyed [[MeetingsAhead.key]], worded
  * [[MeetingsAhead.Brief]], until its start, believed as its sort's answer. A meeting arranged
  * today is not seen until the nightly topics run writes it. Replies [[MeetingsAhead.reply]].
  */
final class MeetingsAhead(zone: ZoneId, bound: Int) extends DreamingJob[AheadParams] {

  val name: JobName = MeetingsAhead.Name

  val version: Int = 1

  def write(params: AheadParams): ujson.Value =
    ujson.Obj("rooms" -> ujson.Arr.from(params.rooms.map(r => ujson.Str(r.written))))

  def read(params: ujson.Value): Either[String, AheadParams] =
    params.objOpt
      .flatMap(_.get("rooms"))
      .flatMap(_.arrOpt)
      .toRight(s"a meetings-ahead run's parameters name its rooms: ${params.render().take(100)}")
      .flatMap(rooms =>
        rooms.toVector.foldLeft[Either[String, Vector[Place]]](Right(Vector.empty))((acc, r) =>
          acc.flatMap(done =>
            r.strOpt
              .toRight(s"a room is a place: ${r.render().take(100)}")
              .flatMap(Place.read)
              .map(done :+ _)
          )
        )
      )
      .map(AheadParams(_))

  override val limits: MoveLimits =
    Proven.right(MoveLimits.of(0, 0, MeetingsAhead.Asks, MeetingsAhead.Asks))

  def run(run: Trusted[JobRun[AheadParams]], moves: Dreaming^): String =
    if (run.extract.params.rooms.isEmpty) MeetingsAhead.Roomless
    else
      moves.scan(MeetingsAhead.Scan, run.tainted.map(_.params))((p, keeper, strands, _, at) =>
        MeetingsAhead.scanned(p.rooms, keeper, strands, bound, zone, at)
      ) match {
        case Left(e) => s"Meetings ahead: its meetings were not read: ${MoveError.said(e)}"
        case Right(found) =>
          val asked = found.asking.zipWithIndex.map((due, i) =>
            moves.sort(
              CardMoves.numbered("sort", i),
              Dreaming.Item.Document(due.version),
              Sorting.Among(Vector.empty, Vector.empty, 0),
              run.tainted.map(_ => MeetingsAhead.Unsettled)
            ) match {
              case Left(e) => MeetingsAhead.Sorted.Refused(MoveError.said(e))
              case Right(None) => MeetingsAhead.Sorted.Unasked
              case Right(Some(sorted)) =>
                val reply = sorted.asked.reply
                if (
                  Trusted
                    .asIs(reply)
                    .extract
                    .probability(MeetingsAhead.Judged.Unsettled) < MeetingsAhead.YesAt
                )
                  MeetingsAhead.Sorted.Settled
                else
                  MeetingsAhead.Sorted.Named(
                    MoveName
                      .of(s"occasion-$i")
                      .left
                      .map(MoveError.Store(_))
                      .flatMap(n => moves.occasion(n, reply.map(_ => MeetingsAhead.occasion(due))))
                  )
            }
          )
          MeetingsAhead.reply(found, zone, asked)
      }
}

object MeetingsAhead {

  val Name: JobName = JobName.literal("meetings-ahead")

  /** 3 hours: how soon before its start a meeting is looked at. */
  val Lead: FiniteDuration = 3.hours

  /** 3 hours: how recently talk in its topic's strand covers a meeting. */
  val Covered: FiniteDuration = 3.hours

  /** 24 hours: how far back a run reads the strands spoken in. */
  val Since: FiniteDuration = 24.hours

  /** 3: the most meetings one run sorts. */
  val Asks: Int = 3

  /** 0.5: `unsettled`'s probability at or above which an occasion is named. */
  val YesAt: Double = 0.5

  private val Scan: MoveName = MoveName.literal("meetings")

  /** What a meeting's sort may answer. */
  enum Judged extends caps.Pure {
    case Unsettled, Settled
  }

  // Draft wording, for Nick (J1).
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

  // Draft wording, for Nick (B1).
  val Brief: OccasionBrief = OccasionBrief.literal(
    "A meeting the team planned starts within the next few hours: the one in the document " +
      "below. Talk since it was arranged left something unsettled for it, or a detail missing. " +
      "Say in two or three lines what is still open or missing (a question nobody answered, a " +
      "choice not yet made, a document someone said they would share, where it is or how to " +
      "join), naming the meeting and its time as the document does, so the team can settle it " +
      "before it starts. Do not repeat what is already settled, and do not answer the open " +
      "points yourself."
  )

  /** `ahead:{topic}:{day}`: the topic key's [[ShortHash]] and the day, ISO; 33 characters a key
    * holds, so always one.
    */
  def key(topic: TopicKey, day: LocalDate): OccasionKey =
    Proven.right(
      OccasionKey
        .of(s"ahead:${ShortHash.of(TopicKey.value(topic))}:$day")
        .toRight(s"no occasion key for ${TopicKey.value(topic)} on $day")
    )

  /** A meeting to sort: its topic's current version, its room, its start, and its key. */
  final case class Due(version: DocumentVersionId, room: Place, start: Instant, key: OccasionKey)
      extends caps.Pure

  /** What a run's scan found at `at`: the meetings to sort, soonest first; of the meetings in
    * its rooms starting within [[Lead]] (`within`), how many named fewer than two people
    * (`fewer`), were covered (`covered`), or were left past one a room and [[Asks]] a run
    * (`left`).
    */
  final case class Found(
      at: Instant,
      asking: Vector[Due],
      within: Int,
      fewer: Int,
      covered: Int,
      left: Int
  ) extends caps.Pure

  /** What became of a meeting's sort. */
  enum Sorted {
    case Refused(why: String)
    case Unasked
    case Settled
    case Named(occasioned: Either[MoveError, Occasioned])
  }

  /** The scan [[MeetingsAhead]] makes at `at`, over `keeper`'s newest `bound` documents. */
  private[topic] def scanned(
      rooms: Vector[Place],
      keeper: DocumentShelf,
      strands: StrandShelf,
      bound: Int,
      zone: ZoneId,
      at: Instant
  )(using Tx^): Either[StoreError, Found] =
    keeper.newest(bound).flatMap { documents =>
      val until = at.plusNanos(Lead.toNanos)
      val within = documents.flatMap(d =>
        for {
          key <- TopicKey.read(d.key)
          topic <- Topic.read(key, d.data).toOption
          meeting <- topic.kind match {
            case Topic.Kind.Meeting(m) => Some(m)
            case Topic.Kind.Subject => None
          }
          room <- topic.passages.reverse.view.flatMap(roomOf).headOption
          if rooms.contains(room)
          when <- meeting.when
          time <- when.start
          start = when.day.atTime(time).atZone(zone).toInstant
          if start.isAfter(at) && !start.isAfter(until)
        } yield (Due(d.version, room, start, MeetingsAhead.key(key, when.day)), meeting, topic)
      )
      val named = within.filter(_._2.who.size >= 2)
      if (named.isEmpty) Right(Found(at, Vector.empty, within.size, within.size, 0, 0))
      else
        strands.units(at.minusNanos(Since.toNanos), at, Strand.Cut.Default).map { spoken =>
          val recent = at.minusNanos(Covered.toNanos)
          val open = named.filterNot { (_, _, topic) =>
            val written = topic.passages.flatMap(_.from.tags.flatMap(_.strand)).toSet
            val ids = written.map(_._1)
            spoken.exists(s =>
              ids.contains(s.id) && (s.touched.isAfter(recent) || !written
                .contains((s.id, s.digest)))
            )
          }
          val soonest = open.map(_._1).sortBy(_.start).distinctBy(_.room)
          val asking = soonest.take(Asks)
          Found(
            at,
            asking,
            within.size,
            within.size - named.size,
            named.size - open.size,
            open.size - asking.size
          )
        }
    }

  /** The occasion `due` names. */
  private[topic] def occasion(due: Due): Occasion =
    Occasion(due.room, due.key, Brief, due.version, due.start)

  /** The room of `passage`'s last strand, when it was written from one. */
  private def roomOf(passage: Passage): Option[Place] =
    passage.from.tags.reverse.collectFirst { case Lineage.Tag.Strand(_, room, _, _) => room }

  private val Clock = DateTimeFormatter.ofPattern("HH:mm")

  /** A run's reply when it lists no room: it reads nothing. */
  // Draft wording, for Nick (R1).
  val Roomless: String = "Meetings ahead: no room to raise occasions in."

  /** A run's reply, at its scan's instant in `zone` (Wording for Nick, R1): `Meetings ahead at
    * {HH:mm}: {n} in its rooms starting within 3 hours, {p} naming fewer than two people, {c}
    * talked of in the last 3 hours, {k} left for a later run; {u} judged settled, {s} started,
    * {h} held ({why, counted}), {a} already noted`, then `, {f} failed ({why, counted})` when
    * any failed, and a full stop. Held: each silence as speech names it, `mislabelled` and
    * `unread`; failed: `unstarted`, `unsorted` for a sort that asked nothing, and each move
    * refused, as its error says.
    */
  // Draft wording, for Nick (R1).
  def reply(found: Found, zone: ZoneId, sorted: Vector[Sorted]): String = {
    def counted(whys: Vector[String]): String =
      Collect.counted(whys).map((w, n) => s"$n $w").mkString(", ")
    val occasioned = sorted.collect { case Sorted.Named(o) => o }
    val held = occasioned.collect {
      case Right(Occasioned.Held(_, why)) => SpeechJson.silenceName(why)
      case Right(Occasioned.Mislabelled(_)) => "mislabelled"
      case Right(Occasioned.Unread(_)) => "unread"
    }
    val failed = sorted.collect {
      case Sorted.Refused(why) => why
      case Sorted.Unasked => "unsorted"
      case Sorted.Named(Left(e)) => MoveError.said(e)
      case Sorted.Named(Right(Occasioned.Unstarted(_, _))) => "unstarted"
    }
    val started = occasioned.count {
      case Right(Occasioned.Started(_)) => true
      case _ => false
    }
    val already = occasioned.count {
      case Right(Occasioned.Already) => true
      case _ => false
    }
    val settled = sorted.count(_ == Sorted.Settled)
    val time = Clock.format(found.at.atZone(zone))
    val heldPart = if (held.isEmpty) "0 held" else s"${held.size} held (${counted(held)})"
    val failedPart = if (failed.isEmpty) "" else s", ${failed.size} failed (${counted(failed)})"
    s"Meetings ahead at $time: ${found.within} in its rooms starting within ${Lead.toHours} " +
      s"hours, ${found.fewer} naming fewer than two people, ${found.covered} talked of in the " +
      s"last ${Covered.toHours} hours, ${found.left} left for a later run; $settled judged " +
      s"settled, $started started, $heldPart, $already already noted$failedPart."
  }

  /** `{"at", "asking": [{"version", "room", "start", "key"}], "within", "fewer", "covered",
    * "left"}`, a room as [[Place.written]] writes it and instants ISO-8601.
    */
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
        "covered" -> f.covered,
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
        counts <- Vector("within", "fewer", "covered", "left")
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
          case Vector(within, fewer, covered, left) =>
            Right(Found(at, asking, within, fewer, covered, left))
          case _ => Left("meetings: expected four counts")
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
