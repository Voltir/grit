package grit.turn

import grit.act.phase.{Delegated, Spawning}
import grit.core.act.Delegation
import grit.core.agent.Subagent
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.AgentType
import grit.core.place.Service
import grit.core.prompt.{Fenced, Tag}
import grit.core.tool.{Args, ArgsError, Field, Hosted, Outcome, Tool, ToolName, ToolSpec}
import grit.core.visibility.Integrity

/** The `delegate` tool, which hands one task to a run of a type the deployment declares, a child
  * turn its turn starts and waits for ([[Spawning.spawn]]). Its words call that child a run, and
  * what it gives back a reply.
  */
object TurnDelegate {

  /** The tool's name. */
  val Name: ToolName = ToolName("delegate")

  /** The tool, offered over `subagents`' types: its `agent` must name one of them and its `task`
    * is not blank, else the call is refused unrun; `None` when `subagents` declares none.
    */
  def tool(subagents: Spawnable): Option[Tool.Offered] =
    spec(subagents).map(s =>
      // Shown as its run's type, after the tool's name: as [[shown]] writes it.
      Hosted.delegating(s, (a: (AgentType, String)) => AgentType.value(a._1))(identity)
    )

  /** What a call that handed its task to a run of the type `agent` is answered when that run came
    * to `came`: `Done` holding its reply fenced in [[Tag.Reply]], or `Failed` saying only that no
    * reply came and, where the model can act on it, why.
    */
  def answer(agent: AgentType, came: Delegated): Outcome = {
    val run = s"The ${AgentType.value(agent)} run"
    came match {
      case Delegated.Replied(_, reply, spent) =>
        Outcome.Done(s"$run replied, spending ${spent.written}:\n${fenced(reply)}")
      case Delegated.Limited(_, Some(reply), spent) =>
        Outcome.Done(
          s"$run stopped before finishing, at its spending or step limit, after spending " +
            s"${spent.written}. What it handed back:\n${fenced(reply)}"
        )
      case Delegated.Limited(_, None, spent) =>
        Outcome.Done(
          s"$run stopped before finishing, at its spending or step limit, after spending " +
            s"${spent.written}, and handed back nothing."
        )
      case Delegated.Refused(Delegation.Refused.Below(_, _)) =>
        Outcome.Failed(
          s"$run was not started: what this conversation has read is not trusted enough to " +
            "hand to it."
        )
      case Delegated.Refused(Delegation.Refused.Above(_, _)) =>
        Outcome.Failed(
          s"$run was not started: this conversation holds material more confidential than " +
            "where it works may receive."
        )
      case Delegated.Refused(Delegation.Refused.NotWorking(repository)) =>
        Outcome.Failed(
          s"$run was not started: it works only in a conversation working in the repository " +
            s"${repository.name}, and this one is not."
        )
      case Delegated.OverDailyCap =>
        Outcome.Failed(s"$run was not started: today's spending limit is reached.")
      case Delegated.OverLimit(most) =>
        Outcome.Failed(
          s"No run was started: you have already started $most runs while replying to this " +
            "message, the most allowed."
        )
      // A turn is offered no delegate in a direct message's room, nor held to a spend cap;
      // whatever else kept it from starting is journaled, never shown.
      case Delegated.Refused(Delegation.Refused.Direct(_)) |
          Delegated.Refused(Delegation.Refused.OverCap(_, _, _)) | Delegated.Unspawned(_) =>
        Outcome.Failed(s"$run could not be started.")
      case Delegated.Unanswered(_) =>
        Outcome.Failed(
          s"$run did not finish within ${Spawning.Within.toMinutes} minutes; its reply will " +
            "not reach you here."
        )
      case Delegated.Unread(_) | Delegated.Failed(_, _) =>
        Outcome.Failed(s"$run ended without a reply.")
    }
  }

  /** A call of the tool handing its task to a run of `agent`, in one line, as a transcript
    * shows it ([[grit.core.tool.Bound.shown]]).
    */
  def shown(agent: AgentType): String = s"${ToolName.value(Name)} ${AgentType.value(agent)}"

  /** The integrity a call's result is kept at when its run came to `came`: its reply's, met
    * with `level`, the turn's; `level` when no reply came.
    */
  def believed(came: Delegated, level: Integrity): Integrity =
    came match {
      case Delegated.Replied(_, reply, _) => reply.integrity.meet(level)
      case Delegated.Limited(_, Some(reply), _) => reply.integrity.meet(level)
      case _ => level
    }

  /** `reply`, a run's own words, fenced so it cannot close its fence. */
  private def fenced(reply: Tainted[String]): String =
    Fenced.in(Tag.Reply, Trusted.asIs(reply).extract).rendered

  private def spec(subagents: Spawnable): Option[ToolSpec[(AgentType, String)]] = {
    val types = subagents.byType.toVector.sortBy((a, _) => AgentType.value(a))
    types.headOption.map { (first, _) =>
      val kinds = types.map((a, c) => line(a, c.subagent))
      val agent = Field.oneOf(
        ("Which kind of run:" +: kinds).mkString("\n"),
        AgentType.value(first),
        types.drop(1).map((a, _) => AgentType.value(a))*
      )
      val task = Field.text("The task, whole: the run reads nothing else.")
      ToolSpec(
        Name,
        "Start a run of one of the kinds listed under agent on one task, and wait for its " +
          "reply, which comes back as this call's result. The run sees nothing of this " +
          "conversation, only the task you write, so say everything it needs: what to do, " +
          "what to reply with, and what it must know. Its reply comes back inside <reply> " +
          s"tags. You can start at most ${Spawning.PerTurn} runs while replying to one " +
          "message.",
        Args
          .of((agent = agent, task = task))
          .refine(a =>
            for {
              kind <- AgentType.of(a.agent).left.map(ArgsError.Refused("agent", _))
              brief <- Option(a.task.trim)
                .filter(_.nonEmpty)
                .toRight(ArgsError.Missing("task", task.accepts))
            } yield (kind, brief)
          )
      )
    }
  }

  /** What a run of `agent`, typed as `as`, does, in one line. */
  private def line(agent: AgentType, as: Subagent): String = {
    def reads(services: Vector[Service]) =
      if (services.isEmpty) "reads no outside service"
      else s"reads ${services.map(_.name).mkString(", ")}"
    val does = as.tools match {
      case Subagent.Tools.Coding(_, repository, _) =>
        s"writes code in the repository ${repository.name}"
      case Subagent.Tools.Services(services) => reads(services)
    }
    s"${AgentType.value(agent)}: $does"
  }
}
