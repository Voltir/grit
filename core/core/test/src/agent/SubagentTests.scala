package grit.core.agent

import scala.concurrent.duration.*

import grit.core.host.{Allowlist, Argv}
import grit.core.id.AgentType
import grit.core.model.*
import grit.core.persona.Persona
import grit.core.place.{Repository, Service}
import grit.core.spend.Carve
import grit.core.visibility.Integrity

import utest.*

object SubagentTests extends TestSuite {

  private def ok[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val coder = ok(AgentType.of("coder"))
  private val scholar = ok(AgentType.of("scholar"))
  private val persona = ok(Persona.of("Coder"))
  private val carve = ok(Carve.of(BigDecimal("0.05")))

  private val pins: Pins = {
    val ref = ModelRef(ok(ModelId.of("anthropic/claude-haiku").toRight("a model id")), None)
    val ladder = ok(Ladder.of(Vector(Rung(Grade.Standard, Assignment(ref, 100, None)))))
    Catalog.of(Policy(ladder, ladder, ladder, ladder, ladder), Vector.empty).pin.turn
  }

  private val coding = Subagent.Tools.Coding(
    Vector.empty,
    ok(Repository.of("scratch")),
    ok(
      Allowlist.of(
        Vector(Allowlist.Rule.Exactly(ok(Argv.of("git", "status")))),
        1.minute
      )
    )
  )
  private val services = Subagent.Tools.Services(Vector(ok(Service.of("docs"))))

  private def subagent(agent: AgentType, tools: Subagent.Tools, requires: Integrity) =
    Subagent.of(agent, persona, pins, tools, carve, requires)

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
