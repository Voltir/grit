package grit.core.agent

import grit.core.visibility.Integrity

import utest.*

object SubagentTests extends TestSuite {
  import SubagentFixtures.*

  val tests = Tests {
    test("a coding subagent is refused unless its brief must carry the integrity to write out") {
      subagent(coder, coding, Integrity.Outside).map(_.agent) ==> Left(
        s"the coding subagent coder must require at least ${Integrity.shown(Integrity.WritesOut)}, " +
          s"not ${Integrity.shown(Integrity.Outside)}"
      )
      assert(subagent(coder, coding, Integrity.Member).isRight)
    }

    test("a subagent offered services alone may take a brief from outside") {
      assert(subagent(scholar, services, Integrity.Outside).isRight)
    }

    test("two subagents of one type are refused, naming it") {
      val one = ok(subagent(coder, coding, Integrity.Member))
      val other = ok(subagent(scholar, services, Integrity.Outside))
      Subagents.of(Vector(one, other, one)).map(_(coder)) ==> Left(
        "two subagents are declared of the type coder"
      )
    }
    test("a subagent is found by the type it was declared of") {
      val one = ok(subagent(coder, coding, Integrity.Member))
      val other = ok(subagent(scholar, services, Integrity.Outside))
      Subagents.of(Vector(one, other)).map(s => (s(coder), s(scholar))) ==>
        Right((Some(one), Some(other)))
    }
  }
}
