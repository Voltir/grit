package grit.models

import grit.core.classify.{Answer, Answered, Ask, Criterion, Question, StateJson}

import utest.*

object StubClassifierTests extends TestSuite {

  /** A message, sent as `{"new_message": text}`, the field the stub reads. */
  private final case class State(text: String)
  private given StateJson[State] = StateJson.instance(s => ujson.Obj("new_message" -> s.text))

  private def state(message: String) = State(message)

  private val same = Ask.yesNo[State]("?", None, None)

  private val which = Ask
    .choice[State, Int](
      "?",
      Criterion(1, "Knots", None),
      Criterion(2, "Redis eviction", None),
      Criterion(3, "new", None)
    )
    .fold(d => throw new java.lang.AssertionError(s"keys repeat: $d"), identity)

  private val frustration = Question
    .score("?", "calm", "frustrated", "very angry")
    .fold(t => throw new java.lang.AssertionError(s"levels: $t"), identity)

  val tests = Tests {
    test("a score weighs the ~level: marker's level 0.9, or the last; the rest share 0.1") {
      // Its position and confidence in hundredths, as they are sums of floating weights.
      def scored(message: String) =
        StubClassifier
          .answers(ujson.Obj("new_message" -> message), Vector(frustration))
          .answers
          .map {
            case Answer.Score(score, ps, confidence) =>
              Some((math.round(score * 100), ps, math.round(confidence * 100)))
            case _ => None
          }
      scored("~level:1 now") ==> Vector(Some((100L, Vector(0.05, 0.9, 0.05), 85L)))
      scored("anything") ==> Vector(Some((185L, Vector(0.05, 0.05, 0.9), 85L)))
      // A level past the last is no marker.
      scored("~level:3") ==> scored("anything")
    }

    test("p(same) is the ~ marker's, or 0.9") {
      val c = new StubClassifier
      Vector("hello", "hi ~0.1", "hm ~0.5 there", "~1", "x~0.25").map(m =>
        c.ask(state(m), same).map(_.value)
      ) ==> Vector(Right(0.9), Right(0.1), Right(0.5), Right(1.0), Right(0.25))
    }

    test("a choice picks the ~back: option, or the last") {
      val c = new StubClassifier
      c.ask(state("back to it ~0.1 ~back:Redis eviction"), which).map(_.value.choice) ==> Right(2)
      c.ask(state("anything ~0.1"), which).map(_.value.choice) ==> Right(3)
      c.ask(state("~back:nothing like it"), which).map(_.value.choice) ==> Right(3)
      // Several markers: each choice takes the first that names one of its own keys.
      c.ask(state("real? ~back:question ~back:Redis eviction"), which).map(_.value.choice) ==>
        Right(2)
      c.ask(state("~back:Knots"), which) match {
        case Right(Answered(d, _, model)) =>
          assert(math.abs(d.top - 0.9) < 1e-9, model == StubClassifier.Model)
        case Left(e) => assert(e.toString == "an answer")
      }
    }
  }
}
