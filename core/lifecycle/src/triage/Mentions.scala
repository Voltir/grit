package grit.lifecycle.triage

import java.util.concurrent.{ExecutionException, FutureTask, TimeUnit, TimeoutException}

import scala.concurrent.duration.*

import grit.core.classify.{Classifier, ClassifierError, Request}
import grit.core.clock.Clock
import grit.core.id.TurnRef
import grit.core.provider.TokenEstimator
import grit.core.recipe.RoomReads
import grit.core.stitch.{Opening, Placements, StitchReads, Tuning}
import grit.core.store.Db
import grit.core.triage.{Corpora, Tags, Weighing}

/** [[Weighing]] as live triage asks: its question set, `questions`, put to the
  * state [[TriageInput.read]] builds for the message, as for one heard (the shipped recipe,
  * `tuning`), read through `reads` and `rooms`, with the corpora of `sources`
  * covering its conversation's place, of `classifier`; an opening is asked about once its
  * placement through `placements` has ended, waited for at most [[Mentions.PlacedWithin]] on
  * `clock`. The classifier is given `askWithin` to answer, past which the message is
  * `Late`, unweighed. Its request's input is estimated by `estimator`. It writes nothing.
  */
final class Mentions(
    reads: StitchReads,
    rooms: RoomReads,
    sources: Corpora,
    questions: TriageQuestions,
    classifier: Classifier^,
    placements: Placements^,
    db: Db^,
    clock: Clock^,
    estimator: TokenEstimator,
    tuning: Tuning,
    askWithin: FiniteDuration = Mentions.AskWithin
) extends Weighing {

  def weigh(turn: TurnRef): Either[Weighing.Unweighed, Weighing.Weighed] =
    for {
      _ <- placed(turn)
      read <- TriageInput
        .read(reads, rooms, db, turn, tuning, TriageRecipe.Shipped)
        .left
        .map(_ => Weighing.Unweighed.Unread)
      // A conversation not found is at no place, so no source covers it.
      asked = read.place.fold(Corpora.Empty)(sources.at)
      answered <- within(questions.ask(classifier, read.state, asked)).flatMap(
        _.left.map {
          case ClassifierError.Unavailable(_) => Weighing.Unweighed.Unavailable
          case ClassifierError.Unreadable(_) => Weighing.Unweighed.Unreadable
        }
      )
    } yield {
      val request = questions.request(read.state, asked)
      Weighing.Weighed(
        Tags.Weighed(answered.value, answered.model, answered.usage),
        estimator.system(Request.json(request).render())
      )
    }

  /** What `call` returns, made on a thread of its own and waited for at most `askWithin`:
    * `Late` past it, the call left to end on its own, its answer unused; `Unavailable` when
    * it throws.
    */
  private def within[A](call: => A): Either[Weighing.Unweighed, A] = {
    val task = new FutureTask[A](() => call)
    val _ = Thread.ofVirtual().name("grit-weigh").start(task)
    try Right(task.get(askWithin.toMillis, TimeUnit.MILLISECONDS))
    catch {
      case _: TimeoutException => Left(Weighing.Unweighed.Late)
      case _: ExecutionException | _: InterruptedException => Left(Weighing.Unweighed.Unavailable)
    }
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

  /** How long a mention's weighing waits for the classifier's answer before giving up,
    * unweighed, so a person waiting on a reply waits at most this, past [[PlacedWithin]],
    * before its first model call. Jev answers live triage's set in well under a second (a
    * weigh step measured 557 ms end to end); this leaves room for a slow call without
    * waiting out Jev's own 30 s timeout.
    */
  val AskWithin: FiniteDuration = 3.seconds
}
