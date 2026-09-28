package grit.core.store

import grit.core.id.PrincipalId
import grit.core.place.{Directory, Namespace, Place}

/** Where a conversation's work comes from. Each origin names exactly one
  * conversation: the same origin always finds the same one.
  */
enum Origin {

  /** A TUI session named `session`, run in `directory`: the same name in another directory
    * is another conversation.
    */
  case Tui(directory: Directory, session: String)

  /** A Slack thread, named by its root message's timestamp. */
  case Slack(team: String, channel: String, threadTs: String)

  /** One run of a triggered task, such as one schedule slot. */
  case Task(name: String, run: String)

  /** Where its conversation happens: a TUI session's directory under `fs`; a Slack thread
    * as `slack:{team}/{channel}/{thread}`; a task's run as `task:{name}/{run}`.
    */
  def place: Place = this match {
    case Tui(directory, _) => Place.of(directory)
    case Slack(team, channel, threadTs) =>
      Place.under(Namespace.Slack, Vector(team, channel, threadTs))
    case Task(name, run) => Place.under(Namespace.Task, Vector(name, run))
  }

  /** Whether its conversation is held with the person running grit, at a terminal on this
    * machine, who answers what a tool asks first and may tune grit itself: a TUI session.
    * Not a Slack thread, whose readers are colleagues, nor a task's run, which has no one.
    */
  // The second audience-shaped match on Origin, after `assistant`: when a third appears,
  // fold them into one `audience`.
  def operator: Boolean = this match {
    case Tui(_, _) => true
    case Slack(_, _, _) | Task(_, _) => false
  }

  /** Who the assistant is in this origin's workspace, going by the name given it there
    * ([[Principals.name]]): `slack:{team}` for a Slack thread; none for a TUI session or a
    * task, where it is simply grit.
    */
  def assistant: Option[PrincipalId] = this match {
    case Slack(team, _, _) => Some(Origin.slackAssistant(team))
    case Tui(_, _) | Task(_, _) => None
  }
}

object Origin {

  /** The assistant of every thread in Slack team `team` ([[Origin.assistant]]). */
  def slackAssistant(team: String): PrincipalId = PrincipalId(s"slack:$team")
}
