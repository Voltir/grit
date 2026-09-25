package grit.models

import grit.core.classify.{Answered, Ask, Criterion, QuestionId}

import utest.*

object StubClassifierTests extends TestSuite {

  private def state(message: String) = ujson.Obj("new_message" -> message)

  private val same = Ask.noul(QuestionId("same"), "?", None, None)

  private val which = Ask.choice(
    QuestionId("which"),
    "?",
    Vector(
      Criterion(1, "Knots", None),
      Criterion(2, "Redis eviction", None),
      Criterion(3, "new", None)
    )
  )

  val tests = Tests {
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
      c.ask(state("~back:Knots"), which) match {
        case Right(Answered(d, _, model)) =>
          assert(math.abs(d.top - 0.9) < 1e-9, model == StubClassifier.Model)
        case Left(e) => assert(e.toString == "an answer")
      }
    }
  }
}
