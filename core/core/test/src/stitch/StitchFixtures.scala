package grit.core.stitch

import java.time.Instant

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.place.{Namespace, Place}
import grit.core.store.{Entry, Payload}

/** Messages in Slack channels, for the stitching tests. */
object StitchFixtures {

  val Now: Instant = Instant.parse("2026-09-30T22:32:26Z")

  /** The channel `channel` of team T. */
  def room(channel: String = "C1"): Place = Place.under(Namespace.Slack, Vector("T", channel))

  /** Heard `text`, said `secondsAgo` before [[Now]] in thread `thread` of `channel`. */
  def heard(
      thread: String,
      text: String,
      secondsAgo: Long,
      channel: String = "C1",
      seq: Long = 0
  ): Said =
    Said(
      ConversationId(s"$channel/$thread"),
      Place.under(Namespace.Slack, Vector("T", channel, thread)),
      Entry(
        EntryId(s"$channel/$thread:$seq"),
        ConversationId(s"$channel/$thread"),
        TurnSeq.First,
        None,
        EntrySeq(seq),
        Payload.Heard(text),
        Now.minusSeconds(secondsAgo)
      )
    )

  val free: Usage = Usage(Tokens(900), Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))

  /** Answers every request with `answer` (or fails), keeping the states it was sent. */
  final class Scripted(answer: Option[Answer]) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var states: Vector[ujson.Value] = Vector.empty

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      states = states :+ state
      answer
        .map(a => Answers(Vector(a), free, "jev"))
        .toRight(ClassifierError.Unavailable("down"))
    }
  }

  /** A choice of `key` with these weights. */
  def chose(key: String, weights: (String, Double)*): Answer =
    Answer.Choice(key, weights.toVector.map((k, p) => Answer.Weight(k, p)), 0.5)
}
