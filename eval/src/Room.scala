package grit.eval

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Try

/** A synthetic Slack room, as written under `eval/resources/rooms/`: its `team` and `channel`,
  * its `messages` in the order said (by time, then id), and the `cards` of the files its team
  * shares, in the order written. A top-level message is an opening, of its `topic` or of none
  * (chatter, belonging nowhere); a thread reply names its `thread` and is never one. A card is
  * of a topic or of none. Made only by [[Room.read]].
  */
final case class Room private (
    name: String,
    team: String,
    channel: String,
    messages: Vector[Room.Message],
    cards: Vector[Room.Card]
) {

  /** The top-level messages, in the order said. */
  def openings: Vector[Room.Message] = messages.filter(_.thread.isEmpty)
}

object Room {

  /** One message: said `at` by `who`; a reply's `thread`, the opening it replies under; an
    * opening's `topic`; `ambiguous` when where it belongs is a judgement call.
    */
  final case class Message(
      id: String,
      at: Instant,
      who: String,
      text: String,
      thread: Option[String],
      topic: Option[String],
      ambiguous: Boolean
  )

  /** A card of a file the team shares: written `at`, its file's `title`, what it says (`text`),
    * and its `topic`, `None` when it belongs to none.
    */
  final case class Card(
      id: String,
      at: Instant,
      title: String,
      text: String,
      topic: Option[String]
  )

  /** What placing `opening` should give: `follows` the first opening of its topic said before
    * it, `None` when it begins (its topic's first, or chatter); and its `burst`.
    */
  final case class Truth(opening: Message, follows: Option[String], burst: Option[Burst])

  /** The opening its author said last, `previous`, at most [[BurstGap]] before (`gap`), and
    * whether that one is of the same topic (`continues`; chatter continues nothing).
    */
  final case class Burst(previous: String, gap: FiniteDuration, continues: Boolean)

  /** How soon after an author's previous opening the next is in a burst with it. */
  val BurstGap: FiniteDuration = 2.minutes

  /** The room `name` written as `text`: `{team, channel, messages: [{id, at, who, text,
    * thread?, topic?, ambiguous?}], cards?: [{id, at, title, text, topic?}]}`. `Left`, naming
    * the message or card, when it is not JSON of that shape, an id repeats (across messages and
    * cards), a reply's thread names no top-level message said before it, or a reply has a
    * topic.
    */
  def read(name: String, text: String): Either[String, Room] =
    Try(ujson.read(text)).toOption.toRight(s"$name: not JSON").flatMap { j =>
      for {
        team <- str(j, "team").toRight(s"$name: no team")
        channel <- str(j, "channel").toRight(s"$name: no channel")
        all <- j.objOpt.flatMap(_.get("messages")).flatMap(_.arrOpt).toRight(s"$name: no messages")
        read <- all.toVector.foldLeft[Either[String, Vector[Message]]](Right(Vector.empty)) {
          (acc, m) => acc.flatMap(done => message(name, m).map(done :+ _))
        }
        sorted = read.sortBy(m => (m.at, m.id))
        cards <- j.objOpt
          .flatMap(_.get("cards"))
          .fold[Either[String, Vector[ujson.Value]]](Right(Vector.empty))(
            _.arrOpt.map(_.toVector).toRight(s"$name: cards is no list")
          )
          .flatMap(_.foldLeft[Either[String, Vector[Card]]](Right(Vector.empty)) { (acc, c) =>
            acc.flatMap(done => card(name, c).map(done :+ _))
          })
          .map(_.sortBy(c => (c.at, c.id)))
        _ <- valid(name, sorted, cards.map(_.id))
      } yield Room(name, team, channel, sorted, cards)
    }

  /** Each of `room`'s openings, in the order said, with what placing it should give. */
  def truth(room: Room): Vector[Truth] = {
    val openings = room.openings
    openings.zipWithIndex.map { (m, i) =>
      val before = openings.take(i)
      val follows = m.topic.flatMap(t => before.find(_.topic.contains(t)).map(_.id))
      val burst = before.reverse.find(_.who == m.who).flatMap { p =>
        val gap = (m.at.toEpochMilli - p.at.toEpochMilli).millis
        Option.when(gap <= BurstGap)(Burst(p.id, gap, m.topic.isDefined && p.topic == m.topic))
      }
      Truth(m, follows, burst)
    }
  }

  private def str(j: ujson.Value, key: String): Option[String] =
    j.objOpt.flatMap(_.get(key)).flatMap(_.strOpt)

  private def message(name: String, m: ujson.Value): Either[String, Message] = {
    val id = str(m, "id")
    def field(key: String): Either[String, String] =
      str(m, key).toRight(s"$name: message ${id.getOrElse("?")} has no $key")
    for {
      id <- field("id")
      at <- field("at").flatMap(a => Try(Instant.parse(a)).toOption.toRight(s"$name: $id: at"))
      who <- field("who")
      text <- field("text")
    } yield Message(
      id,
      at,
      who,
      text,
      str(m, "thread"),
      str(m, "topic"),
      m.objOpt.flatMap(_.get("ambiguous")).flatMap(_.boolOpt).getOrElse(false)
    )
  }

  private def card(name: String, c: ujson.Value): Either[String, Card] = {
    val id = str(c, "id")
    def field(key: String): Either[String, String] =
      str(c, key).toRight(s"$name: card ${id.getOrElse("?")} has no $key")
    for {
      id <- field("id")
      at <- field("at").flatMap(a => Try(Instant.parse(a)).toOption.toRight(s"$name: $id: at"))
      title <- field("title")
      text <- field("text")
    } yield Card(id, at, title, text, str(c, "topic"))
  }

  private def valid(
      name: String,
      sorted: Vector[Message],
      cards: Vector[String]
  ): Either[String, Unit] = {
    val repeated =
      (sorted.map(_.id) ++ cards).groupBy(identity).collect { case (id, ms) if ms.size > 1 => id }
    val misplaced = sorted.zipWithIndex.collectFirst {
      case (m, i)
          if m.thread.exists(t => !sorted.take(i).exists(o => o.id == t && o.thread.isEmpty)) =>
        s"$name: ${m.id}'s thread is no opening said before it"
      case (m, _) if m.thread.isDefined && m.topic.isDefined =>
        s"$name: ${m.id} is a reply with a topic"
    }
    repeated.headOption match {
      case Some(id) => Left(s"$name: id $id repeats")
      case None => misplaced.toLeft(())
    }
  }
}
