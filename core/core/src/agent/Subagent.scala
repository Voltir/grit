package grit.core.agent

import grit.core.host.Allowlist
import grit.core.id.AgentType
import grit.core.model.Pins
import grit.core.persona.Persona
import grit.core.place.{Repository, Service}
import grit.core.spend.SpendCap
import grit.core.visibility.Integrity

/** A kind of agent a turn may start for one task: the persona it is told it is, the tools it is
  * offered, the pins that answer it (met with routing's clearances, ADR 0031), the most one may
  * spend, and the least integrity its brief must carry. A deployment declares it.
  */
final case class Subagent private (
    agent: AgentType,
    persona: Persona,
    pins: Pins,
    tools: Subagent.Tools,
    spendCap: SpendCap,
    requires: Integrity
) extends caps.Pure

object Subagent {

  /** What it is offered besides `task_status`; never grit's conversing tools (ADR 0056). */
  enum Tools extends caps.Pure {

    /** The services it reaches by name. */
    case Services(services: Vector[Service])

    /** Those, and `repository`, worked in under `allowlist`. */
    case Coding(services: Vector[Service], repository: Repository, allowlist: Allowlist)
  }

  /** This subagent, or why not: `Coding` tools with `requires` short of
    * [[Integrity.WritesOut]].
    */
  def of(
      agent: AgentType,
      persona: Persona,
      pins: Pins,
      tools: Tools,
      spendCap: SpendCap,
      requires: Integrity
  ): Either[String, Subagent] = tools match {
    // A coding type pushes to a forge: its brief must be believed enough to write out of grit.
    case _: Tools.Coding if !requires.mayWriteOut =>
      Left(
        s"the coding subagent ${AgentType.value(agent)} must require at least " +
          s"${Integrity.shown(Integrity.WritesOut)}, not ${Integrity.shown(requires)}"
      )
    case _ => Right(Subagent(agent, persona, pins, tools, spendCap, requires))
  }
}
