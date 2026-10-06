package grit.core.store

import grit.core.id.EdgeName
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

  /** The edge its conversation's messages arrive through, and its turns' replies leave by. */
  def edge: EdgeName = this match {
    case Tui(_, _) => EdgeName.Tui
    case Slack(_, _, _) => EdgeName.Slack
    case Task(_, _) => EdgeName.Task
  }

  /** Who its conversation's messages are for: the operator in a TUI session, colleagues in a
    * Slack thread, nobody in a task's run.
    */
  def audience: Audience = this match {
    case Tui(_, _) => Audience.Operator
    case Slack(_, _, _) => Audience.Colleagues
    case Task(_, _) => Audience.Nobody
  }

  /** The focus of a message said `at` its position in its conversation: a Slack thread's
    * opening is said at its channel's top level, so `Open`, and its replies `Focused`; every
    * message of a TUI session or a task's run `Focused`.
    */
  def focus(at: Position): Focus = (this, at) match {
    case (Slack(_, _, _), Position.Opening) => Focus.Open
    case (Slack(_, _, _), Position.Reply) => Focus.Focused
    case (Tui(_, _) | Task(_, _), _) => Focus.Focused
  }

  /** Whether its conversation's first message may continue an exchange elsewhere in its room
    * ([[grit.core.stitch.Stitching]]): when that message is said where topics interleave.
    */
  def stitchable: Boolean = focus(Position.Opening) == Focus.Open

  /** The place its conversation shares with its neighbours, what a scope's `room` stands
    * for ([[grit.core.place.Scope]]): a TUI session's directory, a Slack thread's channel
    * (`slack:{team}/{channel}`), a task's name (`task:{name}`).
    */
  def room: Place = this match {
    case Tui(directory, _) => Place.of(directory)
    case Slack(team, channel, _) => Place.under(Namespace.Slack, Vector(team, channel))
    case Task(name, _) => Place.under(Namespace.Task, Vector(name))
  }

}

/** Who a conversation's messages are for, and what that decides. */
enum Audience {

  /** The person running grit, at a terminal on this machine: a TUI session. */
  case Operator

  /** Colleagues in a workspace: a Slack thread. */
  case Colleagues

  /** No one: a task's run. */
  case Nobody

  /** Whether it answers what a tool asks first and may tune grit itself: the operator. */
  def operator: Boolean = this match {
    case Operator => true
    case Colleagues | Nobody => false
  }
}

/** How many topics interleave where a message is said. */
enum Focus {

  /** One: the container is the topic (a thread's reply, a TUI session, a task's run). */
  case Focused

  /** Many: a channel's top level, where each post may continue any recent exchange or begin
    * one.
    */
  case Open
}

/** Where a message stands in its conversation: its first message, or one after it. */
enum Position {
  case Opening, Reply
}
