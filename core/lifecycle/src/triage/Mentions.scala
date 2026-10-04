package grit.lifecycle.triage

import scala.concurrent.duration.*

import grit.core.classify.{Classifier, ClassifierError, Request}
import grit.core.clock.Clock
import grit.core.id.TurnRef
import grit.core.provider.TokenEstimator
import grit.core.recipe.RoomReads
import grit.core.stitch.{Opening, Placements, StitchReads, Tuning}
import grit.core.store.Db
import grit.core.triage.{KnowledgeSources, Tags, Weighing}

/** [[Weighing]] as live triage asks: its question set ([[TriageQuestions.Shipped]]) put to the
  * state [[TriageInput.read]] builds for the message, as for one heard (the shipped recipe,
  * `tuning`), read through `reads` and `rooms`, with the knowledge sources of `sources`
  * covering its conversation's place, of `classifier`; an opening is asked about once its
  * placement through `placements` has ended, waited for at most [[Mentions.PlacedWithin]] on
  * `clock`. Its request's input is estimated by `estimator`. It writes nothing.
  */
final class Mentions(
    reads: StitchReads,
    rooms: RoomReads,
    sources: KnowledgeSources,
    classifier: Classifier^,
    placements: Placements^,
    db: Db^,
    clock: Clock^,
    estimator: TokenEstimator,
    tuning: Tuning
) extends Weighing {

  def weigh(turn: TurnRef): Either[Weighing.Unweighed, Weighing.Weighed] =
    for {
      _ <- placed(turn)
      read <- TriageInput
        .read(reads, rooms, db, turn, tuning, TriageRecipe.Shipped)
        .left
        .map(_ => Weighing.Unweighed.Unread)
      // A conversation not found is at no place, so no source covers it.
      asked = read.place.fold(KnowledgeSources.Empty)(sources.at)
      answered <- TriageQuestions.Shipped.ask(classifier, read.state, asked).left.map {
        case ClassifierError.Unavailable(_) => Weighing.Unweighed.Unavailable
        case ClassifierError.Unreadable(_) => Weighing.Unweighed.Unreadable
      }
    } yield {
      val request = TriageQuestions.Shipped.request(read.state, asked)
      Weighing.Weighed(
        Tags.Weighed(answered.value, answered.model, answered.usage),
        estimator.system(Request.json(request).render())
      )
    }

  /** Once `turn`'s message's placement has ended, when it is an opening ([[Opening.of]]);
    * at once otherwise. Why not, when the thread cannot be read, or the placement failed or
    * did not end within [[Mentions.PlacedWithin]].
    */
  private def placed(turn: TurnRef): Either[Weighing.Unweighed, Unit] =
    db.read {
      for {
        all <- reads.entries.list(turn.conversationId)
        conversation <- reads.conversations.get(turn.conversationId)
      } yield conversation.flatMap(Opening.of(_, all, turn))
    }.left
      .map(_ => Weighing.Unweighed.Unread)
      .flatMap(
        _.fold[Either[Weighing.Unweighed, Unit]](Right(()))(
          placements
            .awaitedWithin(_, Mentions.PlacedWithin, clock)
            .left
            .map {
              case Placements.Unplaced.Failed(_) => Weighing.Unweighed.PlacementFailed
              case Placements.Unplaced.Late => Weighing.Unweighed.PlacementLate
            }
            .map(_ => ())
        )
      )
}

object Mentions {

  /** How long a mention's weighing waits for its opening's placement before giving up,
    * unweighed. A placement is one classifier call, after its room's earlier ones; past this
    * a mention's turn goes on, offered everything, and waits out the rest of the placement in
    * its own `stitched` step.
    */
  val PlacedWithin: FiniteDuration = 5.seconds
}
