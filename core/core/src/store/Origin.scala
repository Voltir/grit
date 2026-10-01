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

  /** Who its conversation's messages are for: the operator in a TUI session, colleagues in a
    * Slack thread, nobody in a task's run.
    */
  def audience: Audience = this match {
    case Tui(_, _) => Audience.Operator
    case Slack(_, _, _) => Audience.Colleagues
    case Task(_, _) => Audience.Nobody
  }

  /** The place its conversation shares with its neighbours, what a scope's `room` stands
    * for ([[grit.core.place.Scope]]): a TUI session's directory, a Slack thread's channel
    * (`slack:{team}/{channel}`), a task's name (`task:{name}`).
    */
  def room: Place = this match {
    case Tui(directory, _) => Place.of(directory)
    case Slack(team, channel, _) => Place.under(Namespace.Slack, Vector(team, channel))
    case Task(name, _) => Place.under(Namespace.Task, Vector(name))
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

/** Who a conversation's messages are for, and what that decides. */
enum Audience {

  /** The person running grit, at a terminal on this machine: a TUI session. */
  case Operator

  /** Colleagues in a workspace: a Slack thread, begun by a message said at a channel's top
    * level.
    */
  case Colleagues

  /** No one: a task's run. */
  case Nobody

  /** Whether it answers what a tool asks first and may tune grit itself: the operator. */
  def operator: Boolean = this match {
    case Operator => true
    case Colleagues | Nobody => false
  }

  /** Whether a conversation's first message may continue an exchange elsewhere in its room
    * ([[grit.core.stitch.Stitching]]): colleagues' threads. A TUI session or a task's run is
    * begun on purpose.
    */
  def stitchable: Boolean = this match {
    case Colleagues => true
    case Operator | Nobody => false
  }
}
