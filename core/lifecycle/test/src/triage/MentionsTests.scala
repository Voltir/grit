package grit.lifecycle.triage

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Classifier, Request}
import grit.core.clock.Clock
import grit.core.id.TurnRef
import grit.core.message.{Message, Tokens}
import grit.core.provider.TokenEstimator
import grit.core.stitch.{Opening, Placements, Tuning}
import grit.core.triage.{KnowledgeSources, Weighing}

import utest.*

/** One token a character: what the tests pin an estimate against. */
private object ByChar extends TokenEstimator {
  def message(message: Message): Tokens = Tokens(0)
  def system(prompt: String): Tokens = Tokens(prompt.length.toLong)
}

/** Placements whose every bounded wait returns `outcome`, each wait's bound kept. */
private final class Waits(outcome: Either[Placements.Unplaced, String]) extends Placements {
  @caps.unsafe.untrackedCaptures
  var within = Vector.empty[FiniteDuration]
  def awaited(opening: Opening): Either[String, String] = outcome.left.map(_.toString)
  def awaitedWithin(
      opening: Opening,
      bound: FiniteDuration,
      clock: Clock^
  ): Either[Placements.Unplaced, String] = {
    within = within :+ bound
    outcome
  }
}

/** A message said to grit weighed as live triage asks a heard one ([[Mentions]]). */
object MentionsTests extends TestSuite {
  import grit.lifecycle.shadow.ShadowFixtures.{Catalog, Github}

  import TriageFixtures.*

  private def mentions(w: World, classifier: Classifier^, placements: Placements^) =
    new Mentions(
      w.reads,
      w.rooms,
      Catalog,
      classifier,
      placements,
      FakeDb,
      new Stopped(at(5)),
      ByChar,
      Tuning.Default
    )

  /** The request live triage's set makes of `turn`'s message in `w`, with only [[Github]]. */
  private def asked(w: World, turn: TurnRef) =
    TriageInput
      .build(w.reads, w.rooms, FakeDb, turn, Tuning.Default, TriageRecipe.Shipped)
      .toOption
      .map((_, state) =>
        TriageQuestions.Shipped.request(
          state,
          KnowledgeSources.of(Vector(Github)).getOrElse(sys.error("one source"))
        )
      )

  val tests = Tests {
    test(
      "a message said to grit is asked live triage's set with the sources covering its conversation, its tags and its request's estimate returned"
    ) {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      val t = w.say("is the release branch cut?", 1, Some("Ana"))
      val requests = new Requests
      val classifier = Classifier.around(
        new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9))
      ) { (request, ask) =>
        requests.sent = requests.sent :+ request
        ask()
      }
      val weighed = mentions(w, classifier, new Waits(Right("placed"))).weigh(t)
      requests.sent ==> asked(w, t).toVector
      weighed.map(_.estimate) ==>
        Right(Tokens(asked(w, t).fold(0L)(r => Request.json(r).render().length.toLong)))
      weighed.map(_.tags.model) ==> Right("jev-1.13.0")
      weighed.map(_.tags.usage) ==> Right(Spent)
    }

    test(
      "an opening is asked about once its placement has ended, waited for at most PlacedWithin"
    ) {
      val w = new World
      val t = w.say("is the release branch cut?", 0, Some("Ana"))
      val waits = new Waits(Right("placed"))
      val classifier = new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9))
      mentions(w, classifier, waits).weigh(t).isRight ==> true
      (waits.within, classifier.calls) ==> (Vector(Mentions.PlacedWithin), 1)
      // A reply waits for no placement.
      val reply = w.say("and the tag?", 1, Some("Ana"))
      val again = new Waits(Right("placed"))
      mentions(w, classifier, again).weigh(reply).isRight ==> true
      again.within ==> Vector.empty
    }

    test(
      "an opening whose placement fails or does not end in time is not asked about, saying which"
    ) {
      val w = new World
      val t = w.say("is the release branch cut?", 0, Some("Ana"))
      val classifier = new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9))
      Vector(Placements.Unplaced.Late, Placements.Unplaced.Failed("queue down"))
        .map(u => mentions(w, classifier, new Waits(Left(u))).weigh(t)) ==>
        Vector(Left(Weighing.Unweighed.PlacementLate), Left(Weighing.Unweighed.PlacementFailed))
      classifier.calls ==> 0
    }

    test("a classifier that fails, or answers what does not read, weighs nothing, saying which") {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      val t = w.say("is the release branch cut?", 1, Some("Ana"))
      mentions(w, new Scripted(Vector.empty, Vector.empty), new Waits(Right("placed"))).weigh(t) ==>
        Left(Weighing.Unweighed.Unavailable)
      // One yes/no answer to five questions.
      mentions(w, new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1)), new Waits(Right("placed")))
        .weigh(t) ==> Left(Weighing.Unweighed.Unreadable)
    }

    test("a turn holding no person's message weighs nothing, as unread") {
      val w = new World
      val classifier = new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9))
      mentions(w, classifier, new Waits(Right("placed")))
        .weigh(grit.core.id.TurnRef(c, grit.core.id.TurnSeq(7))) ==>
        Left(Weighing.Unweighed.Unread)
      classifier.calls ==> 0
    }
  }
}
