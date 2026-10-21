package grit.lifecycle.triage

import scala.concurrent.duration.FiniteDuration

import grit.core.allot.TaskKind
import grit.core.classify.{Classifier, Request}
import grit.core.clock.Clock
import grit.core.id.TurnRef
import grit.core.message.{Message, Tokens}
import grit.core.period.Probability
import grit.core.provider.TokenEstimator
import grit.core.stitch.{Opening, Placements, Tuning}
import grit.core.triage.{Corpora, Weighing}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*

/** One token a character: what the tests pin an estimate against. */
private object ByChar extends TokenEstimator {
  def message(message: Message): Tokens = Tokens(0)
  def system(prompt: String): Tokens = Tokens(prompt.length.toLong)
}

/** Placements whose every bounded wait returns `outcome`, each wait's bound kept. */
private final class Waits(outcome: Either[Placements.Unplaced, String]) extends Placements {
  // Written and read only on the test's own thread, through the waits the triage makes on it.
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

  /** Weighing over `w`, asking `classifier`, reading at
    * [[grit.core.classify.TestClassifiers.Above]] when `above`.
    */
  private def mentions(
      w: World,
      classifier: Classifier^,
      placements: Placements^,
      above: Boolean = false
  ) =
    new Mentions(
      w.reads,
      w.rooms,
      w.documents,
      Catalog,
      grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit),
      grit.core.classify.TestClassifiers.cleared(classifier),
      placements,
      if (above) grit.core.classify.TestClassifiers.above() else FakeDb,
      new Stopped(at(5)),
      ByChar,
      Tuning.Default
    )

  /** The request live triage's set makes of `turn`'s message in `w`, with only [[Github]]. */
  private def asked(w: World, turn: TurnRef) =
    TriageInput
      .build(
        w.reads,
        w.rooms,
        w.documents,
        FakeDb.as(Subject.Public),
        turn,
        Tuning.Default,
        TriageRecipe.Shipped
      )
      .toOption
      .map((_, state) =>
        TriageQuestions
          .shipped(grit.core.persona.Persona.Grit)
          .request(
            state,
            Corpora.of(Vector(Github)).getOrElse(sys.error("one source"))
          )
      )

  /** The task questions' request for `turn`'s message in `w`, its state read with no pool. */
  private def taskAsked(w: World, turn: TurnRef) =
    TriageInput
      .build(
        w.reads,
        w.rooms,
        w.documents,
        FakeDb.as(Subject.Public),
        turn,
        Tuning.Default,
        TriageRecipe.Bare
      )
      .toOption
      .map((_, state) => Request.of(state, TaskQuestions.Shipped))

  /** A conversation whose opening, heard at `minutes`, was stitched to [[c]]'s strand. */
  private def stitched(w: World, minutes: Long) = {
    val b = w.thread("2.0")
    val t = w.hear("what about the pricing page?", "David", minutes, in = b)
    // Exchange 1 (c) at 0.9: its opening follows c.
    val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
    new grit.core.durable.InMemoryDurable().run(t.workflowId)(w.body(classifier, minutes + 1))
    b
  }

  /** Answers the task questions: analysis most, p(hard) 0.72. */
  private def tasks = new Scripted(Vector(0.05, 0.05, 0.2, 0.6, 0.05, 0.0, 0.05), Vector(0.72))

  val tests = Tests {
    test(
      "a root is asked the task questions about live triage's state with no pool, once its placement ended, saying whether a strand was shown"
    ) {
      val w = new World
      // A record the shipped recipe would show beside the message.
      w.note("Signups dropped in March after the pricing change")(using TestTx.fake).isRight ==>
        true
      val t = w.say("why did signups drop in March?", 0, Some("Ana"))
      val requests = new Requests
      val scripted = tasks
      val classifier = Classifier.around(scripted) { (request, ask) =>
        requests.sent = requests.sent :+ request
        ask()
      }
      val waits = new Waits(Right("placed"))
      val asked = mentions(w, classifier, waits).task(t)
      requests.sent ==> taskAsked(w, t).toVector
      requests.sent.map(r => Request.json(r)("state").obj.keys.toVector) ==>
        Vector(Vector("new_message", "author", "thread"))
      waits.within ==> Vector(Mentions.PlacedWithin)
      asked.map(a => (a.features.kind, a.features.hard, a.strand, a.model, a.usage)) ==>
        Right((TaskKind.Analysis, Probability.clamped(0.72), false, "jev-1.13.0", Spent))
      asked.map(_.estimate) ==>
        Right(Tokens(taskAsked(w, t).fold(0L)(r => Request.json(r).render().length.toLong)))
      // In a conversation stitched to another, the strand is shown, and said.
      val b = stitched(w, 1)
      val joined = w.say("and the checkout?", 3, Some("Ana"), in = b)
      mentions(w, tasks, new Waits(Right("placed"))).task(joined).map(_.strand) ==> Right(true)
    }

    test(
      "a classifier whose service is not cleared for what the read knew is not asked the task questions"
    ) {
      val w = new World
      val t = w.say("why did signups drop in March?", 0, Some("Ana"))
      val classifier = tasks
      mentions(w, classifier, new Waits(Right("placed")), above = true).task(t) ==>
        Left(Weighing.Unweighed.Unavailable)
      classifier.calls ==> 0
    }

    test("a task ask the classifier does not answer within the bound is late") {
      val w = new World
      val t = w.say("why did signups drop in March?", 0, Some("Ana"))
      val release = new java.util.concurrent.CountDownLatch(1)
      val slow = new Scripted(
        Vector(0.05, 0.05, 0.2, 0.6, 0.05, 0.0, 0.05),
        Vector(0.72),
        () => { val _ = release.await(2, java.util.concurrent.TimeUnit.SECONDS) }
      )
      val bounded = new Mentions(
        w.reads,
        w.rooms,
        w.documents,
        Catalog,
        grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit),
        grit.core.classify.TestClassifiers.cleared(slow),
        new Waits(Right("placed")),
        FakeDb,
        new Stopped(at(5)),
        ByChar,
        Tuning.Default,
        scala.concurrent.duration.Duration(100, "millis")
      )
      try bounded.task(t) ==> Left(Weighing.Unweighed.Late)
      finally release.countDown()
    }

    test(
      "a message said to grit is asked live triage's set with the sources covering its conversation, its tags and its request's estimate returned"
    ) {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      val t = w.say("is the release branch cut?", 1, Some("Ana"))
      val requests = new Requests
      val classifier = Classifier.around(
        new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9, 0.9))
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
      val classifier =
        new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9, 0.9))
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
      val classifier =
        new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9, 0.9))
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
      // One yes/no answer to the set's seven.
      mentions(w, new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1)), new Waits(Right("placed")))
        .weigh(t) ==> Left(Weighing.Unweighed.Unreadable)
    }

    test("a classifier whose service is not cleared for what the weighing read is not asked") {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      val t = w.say("is the release branch cut?", 1, Some("Ana"))
      val classifier = new Scripted(Vector(0.1, 0.9), Vector(0.9))
      mentions(w, classifier, new Waits(Right("placed")), above = true).weigh(t) ==>
        Left(Weighing.Unweighed.Unavailable)
      classifier.calls ==> 0
    }

    test("a classifier that does not answer within the bound weighs nothing, as late") {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      val t = w.say("is the release branch cut?", 1, Some("Ana"))
      val release = new java.util.concurrent.CountDownLatch(1)
      // Answers once released, or after 2 s: far past the 100 ms bound.
      val slow = new Scripted(
        Vector(0.5, 0.5, 0, 0),
        Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9, 0.9),
        () => { val _ = release.await(2, java.util.concurrent.TimeUnit.SECONDS) }
      )
      val bounded = new Mentions(
        w.reads,
        w.rooms,
        w.documents,
        Catalog,
        grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit),
        grit.core.classify.TestClassifiers.cleared(slow),
        new Waits(Right("placed")),
        FakeDb,
        new Stopped(at(5)),
        ByChar,
        Tuning.Default,
        scala.concurrent.duration.Duration(100, "millis")
      )
      try bounded.weigh(t) ==> Left(Weighing.Unweighed.Late)
      finally release.countDown()
    }

    test("a turn holding no person's message weighs nothing, as unread") {
      val w = new World
      val classifier =
        new Scripted(Vector(0.5, 0.5, 0, 0), Vector(0.1, 0.2, 0.3, 0.4, 0.9, 0.9, 0.9))
      mentions(w, classifier, new Waits(Right("placed")))
        .weigh(grit.core.id.TurnRef(c, grit.core.id.TurnSeq(7))) ==>
        Left(Weighing.Unweighed.Unread)
      classifier.calls ==> 0
    }
  }
}
