package grit.eval.harness.log

import grit.core.classify.{Answer, Ask, Criterion, Question}

import utest.*

/** An answer kept by position reads back, against its question, as the answer it was. */
object WeightsTests extends TestSuite {

  private val choice: Question =
    Ask
      .choice[Unit, Int](
        "pick",
        Criterion(1, "red", None),
        Criterion(2, "green", None),
        Criterion(3, "blue", None)
      )
      .fold(e => sys.error(e.toString), _.questions.head)

  private val yesNo: Question = Ask.yesNo[Unit]("is it", None, None).questions.head

  val tests = Tests {
    test("a choice is kept in its question's key order and reads back with the keys") {
      // The classifier weighed the keys in another order than the question's.
      val answer = Answer.Choice(
        "blue",
        Vector(Answer.Weight("blue", 0.6), Answer.Weight("red", 0.3), Answer.Weight("green", 0.1)),
        0.4
      )
      Weights.of(choice, answer) ==> Some(Weights.Choice(2, Vector(0.3, 0.1, 0.6), 0.4))
      Weights.of(choice, answer).flatMap(Weights.answer(choice, _)) ==> Some(
        Answer.Choice(
          "blue",
          Vector(
            Answer.Weight("red", 0.3),
            Answer.Weight("green", 0.1),
            Answer.Weight("blue", 0.6)
          ),
          0.4
        )
      )
    }

    test("an answer of the other kind, or choosing no key, is not kept") {
      Weights.of(choice, Answer.YesNo(0.5)) ==> None
      Weights.of(choice, Answer.Choice("violet", Vector.empty, 1.0)) ==> None
      Weights.answer(yesNo, Weights.Choice(0, Vector(1.0), 1.0)) ==> None
      Weights.answer(choice, Weights.Choice(0, Vector(0.5, 0.5), 0.0)) ==> None
    }

    test("a yes/no reads back as itself") {
      Weights.of(yesNo, Answer.YesNo(0.25)).flatMap(Weights.answer(yesNo, _)) ==>
        Some(Answer.YesNo(0.25))
    }
  }
}
