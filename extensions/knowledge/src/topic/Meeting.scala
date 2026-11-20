package grit.knowledge.topic

import java.time.format.TextStyle
import java.time.{DayOfWeek, LocalDate, LocalTime}
import java.util.Locale

import grit.core.clock.Utc

/** One meeting a topic tracks ([[Topic.Kind.Meeting]]): where it stands, and who is in it, as
  * written, each name once.
  */
final case class Meeting private (state: Meeting.State, who: Vector[String]) extends caps.Pure {

  /** Its day and start: proposed for a day, planned, or held (held has no start); `None` when
    * proposed with no day yet.
    */
  def when: Option[Meeting.When] = state match {
    case Meeting.State.Proposed(w) => w
    case Meeting.State.Planned(w) => Some(w)
    case Meeting.State.Held(d) => Some(Meeting.When(d, None))
  }
}

object Meeting {

  /** When a meeting is: its day, and its start when known, in the job's zone. */
  final case class When(day: LocalDate, start: Option[LocalTime]) extends caps.Pure

  /** Where a meeting stands: proposed, its day tentative or not yet known; planned for a day;
    * or held on a day.
    */
  enum State extends caps.Pure {
    case Proposed(when: Option[When])
    case Planned(when: When)
    case Held(day: LocalDate)
  }

  /** 12: the most names a meeting holds. */
  val Who: Int = 12

  /** 80: the most characters of one name. */
  val NameChars: Int = 80

  /** A meeting at `state` with `who`, trimmed, leaving out a blank name, one past [[NameChars]],
    * and any after the first of a name or the first [[Who]].
    */
  def of(state: State, who: Vector[String]): Meeting =
    Meeting(
      state,
      who.map(_.trim).filter(n => n.nonEmpty && n.length <= NameChars).distinct.take(Who)
    )

  /** A meeting newly opened: proposed, its day not yet known, with no one named. */
  val Opened: Meeting = Meeting(State.Proposed(None), Vector.empty)

  /** The days after a meeting's day that talk of it is still taken: a write may date it so far
    * back, and a strand said so late is still offered it.
    */
  val Grace: Long = 2

  /** How a message names a meeting's day other than by its date, as a write reply's `as` gives
    * it.
    */
  enum As extends caps.Pure {
    case Today
    case Tomorrow
    case On(day: DayOfWeek)
    case Next(day: DayOfWeek)
  }

  object As {

    private def named(d: DayOfWeek): String = d.getDisplayName(TextStyle.FULL, Locale.ENGLISH)

    private val Read: Vector[(String, As)] =
      Vector("today" -> Today, "tomorrow" -> Tomorrow) ++
        DayOfWeek.values.toVector.map(d => named(d) -> On(d)) ++
        DayOfWeek.values.toVector.map(d => s"next ${named(d)}" -> Next(d))

    /** Its words, `today`, `tomorrow`, `Monday` … `Sunday` and `next Monday` … `next Sunday`:
      * a write reply's schema offers these alone.
      */
    val Words: Vector[String] = Read.map(_._1)

    /** The one of [[Words]] `word` is, written exactly so. */
    def read(word: String): Option[As] = Read.collectFirst { case (w, a) if w == word => a }
  }

  /** The day `as`, said on `said`, names: `Today` `said` and `Tomorrow` the day after; `On(d)`
    * the first `d` after `said` (a week on when `said` is a `d`), or when `held` the last on or
    * before it; `Next(d)` the first `d` after `said`. None when `said` is after the run's `day`.
    */
  private[topic] def reckon(
      said: LocalDate,
      as: As,
      held: Boolean,
      day: LocalDate
  ): Option[LocalDate] = Option.when(!said.isAfter(day))(as match {
    case As.Today => said
    case As.Tomorrow => said.plusDays(1)
    case As.On(d) if held =>
      said.minusDays(Math.floorMod(said.getDayOfWeek.ordinal - d.ordinal, 7).toLong)
    case As.On(d) => after(said, d)
    case As.Next(d) => after(said, d)
  })

  // The first `d` after `said`: a week on when `said` is one.
  private def after(said: LocalDate, d: DayOfWeek): LocalDate = {
    val ahead = Math.floorMod(d.ordinal - said.getDayOfWeek.ordinal, 7)
    said.plusDays(if (ahead == 0) 7L else ahead.toLong)
  }

  /** `was` as a write's `reply` refines it on the run's `day`: `{"state": "proposed" | "planned"
    * | "held", "day": "YYYY-MM-DD", "said": "YYYY-MM-DD", "as": one of [[As.Words]], "start":
    * "HH:MM", "who": [name]}`, each field optional, a missing, bad or empty one leaving `was`'s;
    * with no `day`, the day `said` and `as` reckon ([[reckon]]), bounded alike; a day more than [[Grace]] days before `day`, or more than
    * `ahead` days after it, left out; planned with no day proposed; held with no day held on
    * `day`; held after `day` planned; and a meeting once held kept held as it was, whatever the
    * reply.
    */
  private[topic] def refined(
      was: Meeting,
      reply: Option[ujson.Value],
      day: LocalDate,
      ahead: Long
  ): Meeting = was.state match {
    case State.Held(_) => was
    case _ =>
      val o = reply.flatMap(_.objOpt)
      def field(key: String) = o.flatMap(_.get(key))
      val wasWhen = was.state match {
        case State.Proposed(w) => w
        case State.Planned(w) => Some(w)
        case State.Held(d) => Some(When(d, None))
      }
      def date(key: String) =
        field(key).flatMap(_.strOpt).flatMap(t => scala.util.Try(LocalDate.parse(t)).toOption)
      val said = for {
        s <- date("said")
        as <- field("as").flatMap(_.strOpt).flatMap(As.read)
        d <- reckon(s, as, field("state").flatMap(_.strOpt).contains("held"), day)
      } yield d
      val newDay = date("day")
        .orElse(said)
        .filter(d => !d.isBefore(day.minusDays(Grace)) && !d.isAfter(day.plusDays(ahead)))
      val newStart =
        field("start").flatMap(_.strOpt).flatMap(t => scala.util.Try(LocalTime.parse(t)).toOption)
      val when = newDay
        .map(d => When(d, newStart.orElse(wasWhen.filter(_.day == d).flatMap(_.start))))
        .orElse(wasWhen.map(w => newStart.fold(w)(s => w.copy(start = Some(s)))))
      val state = field("state").flatMap(_.strOpt) match {
        case Some("held") =>
          when.filter(_.day.isAfter(day)) match {
            case Some(later) => State.Planned(later)
            case None => State.Held(when.fold(day)(_.day))
          }
        case Some("planned") => when.fold[State](State.Proposed(None))(State.Planned(_))
        case Some("proposed") => State.Proposed(when)
        case _ =>
          was.state match {
            case State.Planned(_) => when.fold[State](State.Proposed(None))(State.Planned(_))
            case _ => State.Proposed(when)
          }
      }
      val who = field("who")
        .flatMap(_.arrOpt)
        .filter(names => names.nonEmpty && names.forall(_.strOpt.nonEmpty))
        .map(_.toVector.flatMap(_.strOpt))
        .getOrElse(was.who)
      of(state, who)
  }

  /** `m` as a topic's data holds it: `{"state", "day"?, "start"?, "who": [...]}`. */
  private[topic] def write(m: Meeting): ujson.Value = {
    val state = m.state match {
      case State.Proposed(_) => "proposed"
      case State.Planned(_) => "planned"
      case State.Held(_) => "held"
    }
    ujson.Obj.from(
      Vector("state" -> ujson.Str(state)) ++
        m.when.toVector.flatMap(w =>
          ("day" -> ujson.Str(w.day.toString)) +: w.start.toVector.map(s =>
            "start" -> ujson.Str(s.toString)
          )
        ) :+ ("who" -> ujson.Arr.from(m.who.map(ujson.Str(_))))
    )
  }

  /** A meeting [[write]] wrote, or why `v` is not one. */
  private[topic] def read(v: ujson.Value): Either[String, Meeting] = {
    val o = v.objOpt
    def text(key: String) = o.flatMap(_.get(key)).flatMap(_.strOpt)
    val day = text("day").map(t => scala.util.Try(LocalDate.parse(t)).toOption)
    val start = text("start").map(t => scala.util.Try(LocalTime.parse(t)).toOption)
    for {
      d <- day.fold[Either[String, Option[LocalDate]]](Right(None))(
        _.map(Some(_)).toRight("a meeting's day is no date")
      )
      s <- start.fold[Either[String, Option[LocalTime]]](Right(None))(
        _.map(Some(_)).toRight("a meeting's start is no time")
      )
      when = d.map(When(_, s))
      state <- (text("state"), when) match {
        case (Some("proposed"), w) => Right(State.Proposed(w))
        case (Some("planned"), Some(w)) => Right(State.Planned(w))
        case (Some("held"), Some(w)) => Right(State.Held(w.day))
        case _ => Left("a meeting's state is not one it can be in")
      }
      who <- o
        .flatMap(_.get("who"))
        .flatMap(_.arrOpt)
        .filter(_.forall(_.strOpt.nonEmpty))
        .map(_.toVector.flatMap(_.strOpt))
        .toRight("a meeting's who is not names")
    } yield of(state, who)
  }

  /** Its head line, the first line of its document's text. */
  private[topic] def head(m: Meeting): String = {
    def at(w: When) =
      Utc.toWeekday(w.day) +
        w.start.fold("")(s => s" at $s")
    val names =
      if (m.who.size <= 1) m.who.mkString
      else s"${m.who.init.mkString(", ")} and ${m.who.last}"
    def who(sep: String) = if (m.who.isEmpty) "" else s"$sep with $names"
    m.state match {
      // Its own comma would blur a comma before `with`.
      case State.Proposed(None) => s"Meeting: proposed, day not yet known${who(";")}."
      case State.Proposed(Some(w)) => s"Meeting, proposed for ${at(w)}${who(",")}."
      case State.Planned(w) => s"Meeting, planned for ${at(w)}${who(",")}."
      case State.Held(d) => s"Meeting, held ${at(When(d, None))}${who(",")}."
    }
  }
}
