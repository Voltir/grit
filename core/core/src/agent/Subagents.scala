package grit.core.agent

import grit.core.id.AgentType
import grit.std.collect.Collect

/** A deployment's subagents, one per type. */
final case class Subagents private (byType: Map[AgentType, Subagent]) extends caps.Pure {
  def apply(agent: AgentType): Option[Subagent] = byType.get(agent)
}

object Subagents {
  val empty: Subagents = Subagents(Map.empty)

  /** These, or why not: two of one type, named. */
  def of(each: Vector[Subagent]): Either[String, Subagents] =
    Collect
      .firstRepeat(each.map(_.agent))
      .map(t => s"two subagents are declared of the type ${AgentType.value(t)}")
      .toLeft(Subagents(each.map(s => s.agent -> s).toMap))
}
