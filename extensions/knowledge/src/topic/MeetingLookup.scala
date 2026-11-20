package grit.knowledge.topic

import java.time.{LocalDate, LocalTime, ZoneId}

import scala.util.chaining.*

import grit.core.clock.Utc
import grit.core.document.{DocText, Document}
import grit.core.message.Cut
import grit.core.visibility.Integrity
import grit.knowledge.ref.TopicKey

/** What a turn asks of the meetings: the days `from` to `until`, both included, in `zone`, the
  * words (lower-cased) each one listed must mention, at most how many to list, and whether the
  * latest come first.
  */
final case class MeetingLookup private (
    from: LocalDate,
    until: LocalDate,
    zone: ZoneId,
    about: Vector[String],
    most: Int,
    latestFirst: Boolean
) extends caps.Pure {

  /** The answer over `documents`: a header naming the days, their zone and `about` when given;
    * then the meetings among them whose day ([[Meeting.when]]) is from `from` to `until`, soonest
    * first by day and then start (on a day, one with no start after those with one), or the
    * reverse when `latestFirst`; then, under [[MeetingLookup.UndatedHeader]], those proposed with
    * no day, or proposed for a day before `from`, whose newest passage is dated from
    * [[MeetingLookup.Lately]] days before `from` to `until`, most recently talked of first. Each is
    * kept only when every word of `about` starts some word of its document's text (both split by
    * [[MeetingLookup.words]]), and is shown as that text cut to [[MeetingLookup.ShownChars]]. At
    * most `most` are shown in all, the rest counted on a last line. A document that is not a
    * readable topic (an aside, data of another version, a subject) is passed over. `believed` is
    * the meet of every matched document's integrity, listed or counted; `None` when none matched.
    */
  def answer(documents: Vector[Document]): MeetingLookup.Answer = {
    val meetings = documents.flatMap(d =>
      for {
        key <- TopicKey.read(d.key)
        topic <- Topic.read(key, d.data).toOption
        meeting <- topic.kind match {
          case Topic.Kind.Meeting(m) => Some(m)
          case Topic.Kind.Subject => None
        }
        text = DocText.value(d.text)
        if mentions(text)
      } yield (d, topic, meeting, text)
    )
    val dated = meetings
      .flatMap((d, _, m, text) => m.when.filter(within).map(w => (d, w, text)))
      .sortBy((_, w, _) => (w.day, w.start.isEmpty, w.start.getOrElse(LocalTime.MIN)))
      .pipe(ds => if (latestFirst) ds.reverse else ds)
      .map((d, _, text) => (d, text))
    val earliest = from.minusDays(MeetingLookup.Lately)
    val undated = meetings
      .flatMap { (d, t, m, text) =>
        val open = m.state match {
          case Meeting.State.Proposed(w) => w.forall(_.day.isBefore(from))
          case _ => false
        }
        t.passages.lastOption
          .map(_.day)
          .filter(talked => open && !talked.isBefore(earliest) && !talked.isAfter(until))
          .map(talked => (d, talked, text))
      }
      .sortBy(_._2)(using Ordering[LocalDate].reverse)
      .map((d, _, text) => (d, text))
    val matched = dated ++ undated
    val believed = Integrity.meetAll(matched.map(_._1.integrity))
    if (matched.isEmpty) MeetingLookup.Answer(none, believed)
    else {
      val listed = dated.take(most)
      val alsoUndated = undated.take(most - listed.size)
      def shown(ms: Vector[(Document, String)]) =
        ms.map((_, text) => Cut.head(text, MeetingLookup.ShownChars)).mkString(MeetingLookup.Rule)
      val left = matched.size - listed.size - alsoUndated.size
      val parts =
        Vector(header) ++
          Option.when(listed.nonEmpty)(shown(listed)) ++
          Option.when(alsoUndated.nonEmpty)(
            s"${MeetingLookup.UndatedHeader}\n\n${shown(alsoUndated)}"
          ) ++
          Option.when(left > 0)(
            s"$left more ${
                if (left == 1) "matches" else "match"
              }; give fewer days, more words in about, or a larger most."
          )
      MeetingLookup.Answer(parts.mkString("\n\n"), believed)
    }
  }

  private def within(w: Meeting.When): Boolean = !w.day.isBefore(from) && !w.day.isAfter(until)

  private def mentions(text: String): Boolean = {
    val said = MeetingLookup.words(text)
    about.forall(w => said.exists(_.startsWith(w)))
  }

  private def days: String = s"from ${Utc.toWeekday(from)} to ${Utc.toWeekday(until)}"

  private def quoted: String = s"\"${about.mkString(" ")}\""

  private def header: String =
    s"Meetings $days (${Utc.canonical(zone).getId} time)" +
      (if (about.isEmpty) "" else s" that mention $quoted") +
      s", ${if (latestFirst) "latest" else "soonest"} first, as of the last write-up:"

  private def none: String =
    s"No meeting this conversation may see $days " +
      (if (about.isEmpty) "has been written up." else s"mentions $quoted.")
}

object MeetingLookup {

  /** 5: how many meetings when not told. */
  val Shown: Int = 5

  /** 8: the most meetings one answer shows. */
  val Most: Int = 8

  /** 13: the days after `from` that `until` is when not given (two weeks, both ends included). */
  val Span: Long = 13

  /** 14: how many days before `from` a meeting proposed with no day, or a day before `from`,
    * may last have been talked of and still be listed.
    */
  val Lately: Long = 14

  /** 1,200: the most characters one meeting is shown in. */
  val ShownChars: Int = 1200

  /** What stands between two meetings an answer shows. */
  private val Rule: String = "\n\n---\n\n"

  /** The header of the meetings proposed with no day yet. */
  val UndatedHeader: String = "Proposed, with no day yet:"

  /** `s` lower-cased, split at anything not a letter or digit, blanks dropped. */
  def words(s: String): Vector[String] =
    s.toLowerCase.split("[^\\p{L}\\p{N}]+").toVector.filter(_.nonEmpty)

  /** The lookup: `until` [[Span]] days after `from` when `None`; `about` as [[words]], none when
    * `None`; `most` [[Shown]] when `None`. Why not: `until` before `from`, or `most` outside 1 to
    * [[Most]].
    */
  def of(
      from: LocalDate,
      until: Option[LocalDate],
      zone: ZoneId,
      about: Option[String],
      most: Option[Int],
      latestFirst: Boolean
  ): Either[String, MeetingLookup] = {
    val last = until.getOrElse(from.plusDays(Span))
    val n = most.getOrElse(Shown)
    if (last.isBefore(from)) Left("`until` is before `from`; give a day on or after it.")
    else if (n < 1 || n > Most) Left(s"`most` is a whole number from 1 to $Most.")
    else Right(MeetingLookup(from, last, zone, about.fold(Vector.empty)(words), n, latestFirst))
  }

  /** What a lookup answers: its text, and what that is believed at. */
  final case class Answer(text: String, believed: Option[Integrity]) extends caps.Pure
}
