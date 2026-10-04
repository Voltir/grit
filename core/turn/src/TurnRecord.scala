package grit.turn

import grit.core.durable.StepRecord
import grit.core.edge.RequestState
import grit.core.id.{EntryId, TurnRef}
import grit.core.speech.Outcome

/** A turn after the fact, read from the steps it recorded ([[Turn.Step]]) and the ids its
  * entries and ledger rows are kept under, by the turn's own codecs: for a tool reading a turn
  * it did not run, never for the turn itself.
  */
object TurnRecord {

  /** The offer `steps` recorded; `None` when no `offer` step is recorded, or it recorded a
    * failure ([[failure]] says which). `Left` naming the step when its output does not read.
    */
  def offer(steps: Vector[StepRecord]): Either[String, Option[TurnOffer.Recorded]] = {
    import TurnJournal.given
    read[Either[TurnFailure, TurnOffer.Recorded]](steps, Turn.Step.Offer).map(_.flatMap(_.toOption))
  }

  /** What the `weigh` step recorded the turn's root was weighed with, or why asking failed;
    * `None` when no `weigh` step is recorded or it weighed nothing. `Left` naming the step when its output does not
    * read.
    */
  def weighed(steps: Vector[StepRecord]): Either[String, Option[TurnWeighing.Weighed]] = {
    import TurnJournal.given
    read[Option[TurnWeighing.Weighed]](steps, Turn.Step.Weigh).map(_.flatten)
  }

  /** The first of `steps` whose output is a [[TurnFailure]]: its step's family
    * ([[Turn.Step.family]]; the name as recorded when it has none) and the failure. A failure
    * ends the turn only when no reply follows it: a failed summary leaves the reply standing.
    */
  def failure(steps: Vector[StepRecord]): Option[(String, TurnFailure)] =
    steps.iterator
      .flatMap(s =>
        s.output.flatMap(TurnJournal.failure).map(Turn.Step.family(s.name).getOrElse(s.name) -> _)
      )
      .nextOption()

  /** What the `record-speech` step recorded became of a heard-rooted turn's draft; `None` when
    * the step is not recorded, or recorded a failure. `Left` when its output does not read.
    */
  def speech(steps: Vector[StepRecord]): Either[String, Option[Outcome]] = {
    import TurnJournal.given
    read[Either[TurnFailure, Outcome]](steps, Turn.Step.RecordSpeech).map(_.flatMap(_.toOption))
  }

  /** The tool loop's calls `steps` show given up on, by round and index, each from 0 (the
    * `n` and `j` of `tool:n:j`). A call whose edge answered in the end is not among them.
    */
  def givenUp(steps: Vector[StepRecord]): Map[(Int, Int), GivenUp] = {
    import TurnJournal.given
    val journal = summon[grit.core.durable.Journaled[Either[TurnFailure, RequestState]]]
    def state(s: StepRecord): Option[RequestState] =
      s.output.flatMap(journal.decode(_).toOption).flatMap(_.toOption)
    steps.flatMap { s =>
      s.name match {
        case Expire(n, j) =>
          state(s).collect { case RequestState.Expired => (n.toInt, j.toInt) -> GivenUp.Expired }
        case Abandon(n, j) =>
          state(s).collect { case RequestState.Expired | RequestState.Claimed =>
            (n.toInt, j.toInt) -> GivenUp.Abandoned
          }
        case _ => None
      }
    }.toMap
  }

  /** A tool call the turn stopped waiting for. */
  enum GivenUp {

    /** No edge claimed it in time. */
    case Expired

    /** Its edge claimed it and did not answer in time. */
    case Abandoned
  }

  /** What a ledger row of `turn` paid for, by the id of the entry it is kept under; `None`
    * for an id none of `turn`'s calls is kept under.
    */
  def role(turn: TurnRef, entry: EntryId): Option[Role] = {
    val id = EntryId.value(entry)
    // The last part of an id, as a number: a candidate index, confirmed only by the id it makes.
    val last = id.lastIndexOf(':') match {
      case -1 => None
      case k => id.drop(k + 1).toIntOption.filter(_ >= 0)
    }
    if (entry == turn.replyId || entry == turn.draftId) Some(Role.Reply)
    else if (entry == TurnJudge.id(turn)) Some(Role.Judge)
    else if (entry == TurnSummary.id(turn)) Some(Role.Summary)
    else if (entry == TurnWeighing.id(turn)) Some(Role.Weigh)
    else if (
      Vector(TurnTopics.placedId(turn), TurnTopics.describedId(turn), TurnVerdict.verdictId(turn))
        .contains(entry)
    ) Some(Role.Topic)
    else if ((0 +: last.toVector).exists(i => Turn.queryId(turn, i) == entry)) Some(Role.Query)
    else last.filter(n => TurnTools.callId(turn, TurnLoop.Round.at(n)) == entry).map(Role.Round(_))
  }

  /** What a model call of a turn was for. */
  enum Role {

    /** Assembly's search query. */
    case Query

    /** Placing the turn's message among its conversation's topics, describing a topic, or
      * the reply's verdict on one.
      */
    case Topic

    /** The reply to round `index` (from 0) of the tool loop, which called tools. */
    case Round(index: Int)

    /** The reply, or a heard-rooted turn's draft. */
    case Reply

    /** The judge of a heard-rooted turn's draft. */
    case Judge

    /** The turn's summary. */
    case Summary

    /** Live triage's set asked of the message said to grit the turn answers, before its offer
      * ([[TurnWeighing.Weighed.Asked]]).
      */
    case Weigh
  }

  private val Expire = """expire:(\d+):(\d+)""".r
  private val Abandon = """abandon:(\d+):(\d+)""".r

  /** The output of the step named `name` among `steps`, read by `A`'s journal; `None` when no
    * such step recorded one.
    */
  private def read[A](steps: Vector[StepRecord], name: String)(using
      journal: grit.core.durable.Journaled[A]
  ): Either[String, Option[A]] =
    steps.find(_.name == name).flatMap(_.output) match {
      case None => Right(None)
      case Some(output) => journal.decode(output).map(Some(_)).left.map(why => s"$name: $why")
    }
}
