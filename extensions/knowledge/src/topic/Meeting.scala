package grit.knowledge.topic

import java.time.{LocalDate, LocalTime}

/** One meeting a topic tracks ([[Topic.Kind.Meeting]]): where it stands, and who is in it, as
  * written, each name once.
  */
final case class Meeting private (state: Meeting.State, who: Vector[String]) extends caps.Pure

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

  /** `was` as a write's `reply` refines it on the run's `day`: `{"state": "proposed" | "planned"
    * | "held", "day": "YYYY-MM-DD", "start": "HH:MM", "who": [name]}`, each field optional, a
    * missing or bad one leaving `was`'s; a day more than 2 days before `day`, or more than
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
      val newDay = field("day")
        .flatMap(_.strOpt)
        .flatMap(t => scala.util.Try(LocalDate.parse(t)).toOption)
        .filter(d => !d.isBefore(day.minusDays(2)) && !d.isAfter(day.plusDays(ahead)))
      val newStart =
        field("start").flatMap(_.strOpt).flatMap(t => scala.util.Try(LocalTime.parse(t)).toOption)
      val when = newDay
        .map(d => When(d, newStart.orElse(wasWhen.filter(_.day == d).flatMap(_.start))))
        .orElse(wasWhen.map(w => newStart.fold(w)(s => w.copy(start = Some(s)))))
      val state = field("state").flatMap(_.strOpt) match {
        case Some("held") =>
          when.map(_.day).filter(_.isAfter(day)) match {
            case Some(_) => when.fold[State](State.Proposed(None))(State.Planned(_))
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
        .filter(_.forall(_.strOpt.nonEmpty))
        .map(_.toVector.flatMap(_.strOpt))
        .getOrElse(was.who)
      of(state, who)
  }

  /** `m` as a topic's data holds it: `{"state", "day"?, "start"?, "who": [...]}`. */
  private[topic] def write(m: Meeting): ujson.Value = {
    val (state, when) = m.state match {
      case State.Proposed(w) => ("proposed", w)
      case State.Planned(w) => ("planned", Some(w))
      case State.Held(d) => ("held", Some(When(d, None)))
    }
    ujson.Obj.from(
      Vector("state" -> ujson.Str(state)) ++
        when.toVector.flatMap(w =>
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
  // Draft wording, for Nick: the meeting head line.
  private[topic] def head(m: Meeting): String = {
    def at(w: When) =
      s"${w.day.getDayOfWeek.toString.toLowerCase.capitalize} ${w.day}" +
        w.start.fold("")(s => s" at $s")
    val state = m.state match {
      case State.Proposed(None) => "proposed, its day not yet known"
      case State.Proposed(Some(w)) => s"proposed for ${at(w)}"
      case State.Planned(w) => s"planned for ${at(w)}"
      case State.Held(d) => s"held ${at(When(d, None))}"
    }
    val who =
      if (m.who.isEmpty) ""
      else if (m.who.size == 1) s", with ${m.who.mkString}"
      else s", with ${m.who.init.mkString(", ")} and ${m.who.last}"
    s"Meeting, $state$who."
  }
}
