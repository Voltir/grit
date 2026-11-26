package grit.core.agent

import scala.concurrent.duration.*

import grit.core.host.{Allowlist, Argv}
import grit.core.id.AgentType
import grit.core.message.SpendCap
import grit.core.model.*
import grit.core.persona.Persona
import grit.core.place.{Repository, Service}
import grit.core.visibility.Integrity

/** Subagents' parts as tests build them: two types, one persona and ladder, a spend cap of
  * $0.05, and tools of each kind, coding in [[scratch]].
  */
object SubagentFixtures {
  def ok[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  val coder: AgentType = ok(AgentType.of("coder"))
  val scholar: AgentType = ok(AgentType.of("scholar"))
  val persona: Persona = ok(Persona.of("Coder"))
  val spendCap: SpendCap = ok(SpendCap.of(BigDecimal("0.05")))

  val ladder: Ladder = Ladder.one(
    Assignment(
      ModelRef(ok(ModelId.of("anthropic/claude-haiku").toRight("a model id")), None),
      100,
      None
    )
  )

  val scratch: Repository = ok(Repository.of("scratch"))

  val coding: Subagent.Tools.Coding = Subagent.Tools.Coding(
    Vector.empty,
    scratch,
    ok(
      Allowlist.of(
        Vector(Allowlist.Rule.Exactly(ok(Argv.of("git", "status")))),
        1.minute
      )
    )
  )

  val services: Subagent.Tools.Services =
    Subagent.Tools.Services(Vector(ok(Service.of("docs"))))

  /** A subagent of `agent` offered `tools`, requiring `requires`, of this file's persona, ladder
    * and spend cap; or why not.
    */
  def subagent(
      agent: AgentType,
      tools: Subagent.Tools,
      requires: Integrity
  ): Either[String, Subagent] =
    Subagent.of(agent, persona, ladder, tools, spendCap, requires)
}
