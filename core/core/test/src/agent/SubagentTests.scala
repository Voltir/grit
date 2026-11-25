package grit.core.agent

import grit.core.visibility.Integrity

import utest.*

object SubagentTests extends TestSuite {
  import SubagentFixtures.*

  val tests = Tests {
    test("a coding subagent is refused unless its brief must carry the integrity to write out") {
      val refused = subagent(coder, coding, Integrity.Outside)
      assert(refused.left.exists(_.contains("coder")))
      assert(subagent(coder, coding, Integrity.Member).isRight)
    }

    test("a subagent offered services alone may take a brief from outside") {
      assert(subagent(scholar, services, Integrity.Outside).isRight)
    }

    test("two subagents of one type are refused, naming it; a type declared once is found") {
      val one = ok(subagent(coder, coding, Integrity.Member))
      val other = ok(subagent(scholar, services, Integrity.Outside))
      val twice = Subagents.of(Vector(one, other, one))
      assert(twice.left.exists(_.contains("coder")))
      val both = ok(Subagents.of(Vector(one, other)))
      both(coder) ==> Some(one)
      both(scholar) ==> Some(other)
      Subagents.empty(coder) ==> None
    }
  }
}
