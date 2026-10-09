package grit.act.phase

import java.time.Instant

import grit.core.classify.{
  Answer,
  Answers,
  Ask,
  Classifier,
  ClassifierError,
  Question,
  Request,
  StateJson
}
import grit.core.clock.SetClock
import grit.core.message.{Tokens, Usage}

import utest.*

/** A judgment's phase: a classifier's answers and its retries. */
object ClassifyingTests extends TestSuite {

  private val start = Instant.parse("2026-10-08T12:00:00Z")

  private given StateJson[String] = StateJson.instance(s => ujson.Obj("text" -> s))

  private val request = Request.of("x", Ask.yesNo[String]("Is `text` urgent?", None, None))

  private val yes =
    Answers(Vector(Answer.YesNo(0.9)), Usage(Tokens(5), Tokens(1), Tokens.Zero, None), "j")

  private val down = Left(ClassifierError.Unavailable("upstream down"))

  /** A classifier that answers each call with the next of `script`, the last again after. */
  private final class Scripted(script: Vector[Either[ClassifierError, Answers]])
      extends Classifier {
    @caps.unsafe.untrackedCaptures
    var calls = 0
    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      val i = calls
      calls += 1
      script
        .lift(i)
        .orElse(script.lastOption)
        .getOrElse(Left(ClassifierError.Unavailable("none")))
    }
  }

  val tests = Tests {
    test(
      "an unavailable classifier is asked again after 2 s and 6 s, then fails naming its tries"
    ) {
      val classifier = new Scripted(Vector(down))
      val clock = new SetClock(start)
      val got = Classifying.answers(classifier, request, clock)
      (got, classifier.calls, clock.at) ==>
        (
          Left(ClassifierError.Unavailable("upstream down (after 3 tries)")),
          3,
          start.plusSeconds(8)
        )
    }

    test("a classifier back on its second try answers, after the clock slept 2 s") {
      val classifier = new Scripted(Vector(down, Right(yes)))
      val clock = new SetClock(start)
      val got = Classifying.answers(classifier, request, clock)
      (got, classifier.calls, clock.at) ==> (Right(yes), 2, start.plusSeconds(2))
    }

    test("an unreadable reply is not asked again, and names no tries") {
      val classifier = new Scripted(Vector(Left(ClassifierError.Unreadable("garbled")), Right(yes)))
      val clock = new SetClock(start)
      val got = Classifying.answers(classifier, request, clock)
      (got, classifier.calls, clock.at) ==> (Left(ClassifierError.Unreadable("garbled")), 1, start)
    }

    test("unreadable after an unavailable try is the last failure, naming both tries") {
      val classifier = new Scripted(Vector(down, Left(ClassifierError.Unreadable("garbled"))))
      val clock = new SetClock(start)
      val got = Classifying.answers(classifier, request, clock)
      (got, classifier.calls, clock.at) ==>
        (Left(ClassifierError.Unreadable("garbled (after 2 tries)")), 2, start.plusSeconds(2))
    }
  }
}
