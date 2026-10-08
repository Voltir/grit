package grit.core.store

import grit.core.id.EdgeName
import grit.core.identity.Account
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

  /** A direct message to grit with the person `account` is linked to, in the thread `thread`:
    * its room is spelled by the account (`direct:{namespace}/{name}`), its edge is the one
    * named for the account's namespace (`slack`), its asker is that person whoever wrote a
    * turn's first entry, and it is labelled at their clearance as each transaction resolves it
    * (ADR 0032). Only for an account whose source never reissues its ids (a Slack user id, not
    * an email address): a reissued id would give its new holder this room and all said in it.
    */
  case Direct(account: Account.Sourced, thread: String)

  /** Where its conversation happens: a TUI session's directory under `fs`; a Slack thread
    * as `slack:{team}/{channel}/{thread}`; a task's run as `task:{name}/{run}`; a direct
    * message's thread as `direct:{namespace}/{name}/{thread}`.
    */
  def place: Place = this match {
    case Tui(directory, _) => Place.of(directory)
    case Slack(team, channel, threadTs) =>
      Place.under(Namespace.Slack, Vector(team, channel, threadTs))
    case Task(name, run) => Place.under(Namespace.Task, Vector(name, run))
    case Direct(account, thread) => Origin.direct(account, Vector(thread))
  }

  /** The edge its conversation's messages arrive through, and its turns' replies leave by. */
  def edge: EdgeName = this match {
    case Tui(_, _) => EdgeName.Tui
    case Slack(_, _, _) => EdgeName.Slack
    case Task(_, _) => EdgeName.Task
    case Direct(account, _) => EdgeName(Account.Sourced.namespace(account))
  }

  /** Who its conversation's messages are for: the operator in a TUI session, colleagues in a
    * Slack thread, nobody in a task's run, one person in a direct message.
    */
  def audience: Audience = this match {
    case Tui(_, _) => Audience.Operator
    case Slack(_, _, _) => Audience.Colleagues
    case Task(_, _) => Audience.Nobody
    case Direct(_, _) => Audience.Person
  }

  /** The focus of a message said `at` its position in its conversation: a Slack thread's
    * opening is said at its channel's top level, so `Open`, and its replies `Focused`; every
    * message of a TUI session, a task's run or a direct message `Focused`.
    */
  def focus(at: Position): Focus = (this, at) match {
    case (Slack(_, _, _), Position.Opening) => Focus.Open
    case (Slack(_, _, _), Position.Reply) => Focus.Focused
    case (Tui(_, _) | Task(_, _) | Direct(_, _), _) => Focus.Focused
  }

  /** Whether its conversation's first message may continue an exchange elsewhere in its room
    * ([[grit.core.stitch.Stitching]]): when that message is said where topics interleave.
    */
  def stitchable: Boolean = focus(Position.Opening) == Focus.Open

  /** The place its conversation shares with its neighbours, what a scope's `room` stands
    * for ([[grit.core.place.Scope]]): a TUI session's directory, a Slack thread's channel
    * (`slack:{team}/{channel}`), a task's name (`task:{name}`), a direct message's person's
    * account (`direct:{namespace}/{name}`).
    */
  def room: Place = this match {
    case Tui(directory, _) => Place.of(directory)
    case Slack(team, channel, _) => Origin.channel(team, channel)
    case Task(name, _) => Place.under(Namespace.Task, Vector(name))
    case Direct(account, _) => Origin.direct(account, Vector.empty)
  }

}

object Origin {

  /** The Slack channel `channel` of team `team` as a place, `slack:{team}/{channel}`: the room
    * of every thread in it ([[Origin.room]]).
    */
  def channel(team: String, channel: String): Place =
    Place.under(Namespace.Slack, Vector(team, channel))

  /** `account`'s direct room, `direct:{namespace}/{name}`, with `below` under it. */
  private def direct(account: Account.Sourced, below: Vector[String]): Place =
    Place.under(
      Namespace.Direct,
      Vector(Account.Sourced.namespace(account), Account.Sourced.name(account)) ++ below
    )
}

/** Who a conversation's messages are for, and what that decides. */
enum Audience {

  /** The person running grit, at a terminal on this machine: a TUI session. */
  case Operator

  /** Colleagues in a workspace: a Slack thread. */
  case Colleagues

  /** No one: a task's run. */
  case Nobody

  /** One person, writing to grit directly: a direct message. */
  case Person

  /** Whether it answers what a tool asks first and may tune grit itself: the operator. */
  def operator: Boolean = this match {
    case Operator => true
    case Colleagues | Nobody | Person => false
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
