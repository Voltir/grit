package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName

import utest.*

/** v7's placement gate ([[Tags.V7.toRoom]]) over `for-room` and the facts code read: a reply
  * goes at its room's top level when `for-room` is strong, or when its message opened its
  * thread in a calm room and `for-room` is fair; a fact never places one alone.
  */
object V7GateTests extends TestSuite {

  private def answers(forRoom: Double): VectorMap[QuestionName, Answer] =
    VectorMap(Tags.V7.forRoom -> Answer.YesNo(forRoom))

  /** Cues of a message at its room's top level or not, in a room of `pace`. */
  private def cues(atTop: Boolean, pace: Option[Int]) =
    Cues.Empty.copy(atTop = atTop, pace = pace)

  private def places(forRoom: Double, atTop: Boolean, pace: Option[Int]): Option[Boolean] =
    Tags.V7.toRoom.drafts(answers(forRoom), cues(atTop, pace))

  val tests = Tests {
    test("for-room at 0.8 or more places a reply at the top level, in any room") {
      Vector(
        places(0.8, atTop = false, Some(20)),
        places(0.95, atTop = false, None),
        places(0.79, atTop = false, Some(0))
      ) ==> Vector(Some(true), Some(true), Some(false))
    }

    test(
      "for-room at 0.6 places it only for a message that opened its thread in a room at 5 messages an hour or fewer"
    ) {
      Vector(
        places(0.6, atTop = true, Some(5)),
        places(0.6, atTop = true, Some(6)),
        places(0.6, atTop = true, None),
        places(0.6, atTop = false, Some(0)),
        places(0.59, atTop = true, Some(0))
      ) ==> Vector(Some(true), Some(false), Some(false), Some(false), Some(false))
    }

    test("the facts alone place nothing, and an unasked for-room is unread") {
      (
        places(0.0, atTop = true, Some(0)),
        Tags.V7.toRoom.drafts(VectorMap.empty, cues(atTop = true, Some(0)))
      ) ==> (Some(false), None)
    }
  }
}
