package grit.core.classify

import grit.core.message.{Tokens, Usage}

import utest.*

/** [[Classifier.around]] and the [[Request]] it hands on. */
object AroundTests extends TestSuite {

  /** A ticket, sent as `{"ticket": text}`. */
  private final case class Ticket(text: String)
  private given StateJson[Ticket] = StateJson.instance(t => ujson.Obj("ticket" -> t.text))

  private def urgent(words: String = "Is `ticket` urgent?") =
    Ask.yesNo[Ticket](words, None, None)

  private val free = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))

  /** Answers yes at `p` to its one question, keeping each request it was sent. */
  private final class Inner(p: Double) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var sent: Vector[(ujson.Value, Vector[Question])] = Vector.empty

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      sent = sent :+ (state, questions)
      Right(Answers(Vector(Answer.YesNo(p)), free, "inner"))
    }
  }

  /** The requests `around`'s function was handed. */
  private final class Seen {
    @caps.unsafe.untrackedCaptures
    var requests: Vector[Request] = Vector.empty
  }

  val tests = Tests {
    test("f is handed the request exactly as the inner classifier receives it") {
      val inner = new Inner(0.25)
      val seen = new Seen
      val wrapped = Classifier.around(inner) { (request, ask) =>
        seen.requests = seen.requests :+ request
        ask()
      }
      wrapped.ask(Ticket("the site is down"), urgent()) ==>
        Right(Answered(0.25, free, "inner"))
      seen.requests.map(r => (r.state, r.questions)) ==> inner.sent
      seen.requests ==> Vector(Request.of(Ticket("the site is down"), urgent()))
    }

    test("f's answer is the answer, and an f that never asks leaves the inner one unasked") {
      val inner = new Inner(0.25)
      val wrapped = Classifier.around(inner) { (_, _) =>
        Right(Answers(Vector(Answer.YesNo(0.875)), free, "cached"))
      }
      wrapped.ask(Ticket("x"), urgent()) ==> Right(Answered(0.875, free, "cached"))
      inner.sent.size ==> 0
    }

    test("ask asks the inner classifier once each time it is called") {
      val inner = new Inner(0.25)
      val wrapped = Classifier.around(inner) { (_, ask) =>
        val _ = ask()
        ask()
      }
      wrapped.ask(Ticket("x"), urgent())
      inner.sent.size ==> 2
    }

    test("a score is hashed as its kind, its words and its levels, in order") {
      val frustration = Question
        .score("How frustrated is `ticket`?", "calm", "angry")
        .getOrElse(throw new java.lang.AssertionError("levels"))
      // A pin of the hashed form: a new kind beside the others, so their digests stay.
      Request.json(Request.of(Ticket("x"), Ask.answer[Ticket](frustration))).render() ==>
        """{"state":{"ticket":"x"},"questions":[{"kind":"score","instructions":"How frustrated is `ticket`?","levels":["calm","angry"]}]}"""
    }

    test("equal requests have equal digests; one changed word changes it") {
      val a = Request.of(Ticket("the site is down"), urgent())
      val b = Request.of(Ticket("the site is down"), urgent())
      val worded = Request.of(Ticket("the site is down"), urgent("Is `ticket` urgent now?"))
      val stated = Request.of(Ticket("the site is up"), urgent())
      a.digest ==> b.digest
      assert(a.digest.matches("[0-9a-f]{64}"))
      assert(worded.digest != a.digest, stated.digest != a.digest)
    }
  }
}
